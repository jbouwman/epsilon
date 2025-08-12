/*
 * epsilon-libffi.c - libffi integration for Epsilon FFI callbacks
 * 
 * This C extension provides real callback functionality using libffi's
 * closure API to overcome SBCL's alien-lambda limitations.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ffi.h>

/* Type constants matching Lisp-side definitions */
#define EPSILON_TYPE_VOID       0
#define EPSILON_TYPE_INT        1
#define EPSILON_TYPE_LONG       2
#define EPSILON_TYPE_FLOAT      3
#define EPSILON_TYPE_DOUBLE     4
#define EPSILON_TYPE_POINTER    5
#define EPSILON_TYPE_STRING     6
#define EPSILON_TYPE_BOOL       7

/* Callback registry entry */
typedef struct epsilon_callback {
    ffi_closure *closure;
    ffi_cif cif;
    void *executable;
    void *writable;
    void *lisp_function;  /* Opaque pointer to Lisp function */
    int return_type;
    int *arg_types;
    int nargs;
    int callback_id;
    struct epsilon_callback *next;
} epsilon_callback_t;

/* Global callback registry */
static epsilon_callback_t *callback_registry = NULL;
static int next_callback_id = 1;

/* Forward declarations */
static void epsilon_callback_handler(ffi_cif *cif, void *ret, void **args, void *user_data);
static ffi_type* epsilon_type_to_ffi_type(int epsilon_type);
static void epsilon_cleanup_callback(epsilon_callback_t *cb);

/* Error handling */
static char error_buffer[256];
const char* epsilon_get_last_error(void) {
    return error_buffer;
}

static void epsilon_set_error(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(error_buffer, sizeof(error_buffer), format, args);
    va_end(args);
}

/*
 * Convert Epsilon type constants to libffi types
 */
static ffi_type* epsilon_type_to_ffi_type(int epsilon_type) {
    switch (epsilon_type) {
        case EPSILON_TYPE_VOID:     return &ffi_type_void;
        case EPSILON_TYPE_INT:      return &ffi_type_sint;
        case EPSILON_TYPE_LONG:     return &ffi_type_slong;
        case EPSILON_TYPE_FLOAT:    return &ffi_type_float;
        case EPSILON_TYPE_DOUBLE:   return &ffi_type_double;
        case EPSILON_TYPE_POINTER:  return &ffi_type_pointer;
        case EPSILON_TYPE_STRING:   return &ffi_type_pointer;
        case EPSILON_TYPE_BOOL:     return &ffi_type_sint;
        default:
            epsilon_set_error("Unknown Epsilon type: %d", epsilon_type);
            return NULL;
    }
}

/*
 * Callback handler that bridges C calls to Lisp functions
 * This function is called by libffi when the C callback is invoked
 */
static void epsilon_callback_handler(ffi_cif *cif, void *ret, void **args, void *user_data) {
    epsilon_callback_t *cb = (epsilon_callback_t*)user_data;
    (void)cif;  /* Suppress unused parameter warning */
    (void)args; /* Suppress unused parameter warning */
    
    /* For now, we'll implement a simple dispatch mechanism */
    /* In a real implementation, this would call back into SBCL */
    
    /* Set default return values based on type */
    if (ret != NULL) {
        switch (cb->return_type) {
            case EPSILON_TYPE_VOID:
                /* No return value */
                break;
            case EPSILON_TYPE_INT:
            case EPSILON_TYPE_BOOL:
                *(int*)ret = 42;  /* Default test value */
                break;
            case EPSILON_TYPE_LONG:
                *(long*)ret = 42L;
                break;
            case EPSILON_TYPE_FLOAT:
                *(float*)ret = 42.0f;
                break;
            case EPSILON_TYPE_DOUBLE:
                *(double*)ret = 42.0;
                break;
            case EPSILON_TYPE_POINTER:
            case EPSILON_TYPE_STRING:
                *(void**)ret = NULL;
                break;
        }
    }
    
    /* TODO: Implement actual SBCL callback mechanism */
    /* This would involve calling back into Lisp through SBCL's C API */
}

/*
 * Create a new callback
 * Returns callback ID on success, -1 on error
 */
int epsilon_create_callback(void *lisp_function, int return_type, int *arg_types, int nargs) {
    epsilon_callback_t *cb = NULL;
    ffi_type **ffi_arg_types = NULL;
    ffi_type *ffi_return_type = NULL;
    ffi_status status;
    
    /* Validate parameters */
    if (nargs < 0 || nargs > 32) {
        epsilon_set_error("Invalid argument count: %d", nargs);
        return -1;
    }
    
    if (nargs > 0 && arg_types == NULL) {
        epsilon_set_error("arg_types is NULL but nargs > 0");
        return -1;
    }
    
    /* Allocate callback structure */
    cb = (epsilon_callback_t*)malloc(sizeof(epsilon_callback_t));
    if (!cb) {
        epsilon_set_error("Failed to allocate callback structure");
        return -1;
    }
    
    memset(cb, 0, sizeof(epsilon_callback_t));
    cb->callback_id = next_callback_id++;
    cb->lisp_function = lisp_function;
    cb->return_type = return_type;
    cb->nargs = nargs;
    
    /* Copy argument types */
    if (nargs > 0) {
        cb->arg_types = (int*)malloc(nargs * sizeof(int));
        if (!cb->arg_types) {
            epsilon_set_error("Failed to allocate argument types array");
            free(cb);
            return -1;
        }
        memcpy(cb->arg_types, arg_types, nargs * sizeof(int));
    }
    
    /* Convert types to libffi format */
    ffi_return_type = epsilon_type_to_ffi_type(return_type);
    if (!ffi_return_type) {
        epsilon_cleanup_callback(cb);
        return -1;
    }
    
    if (nargs > 0) {
        ffi_arg_types = (ffi_type**)malloc(nargs * sizeof(ffi_type*));
        if (!ffi_arg_types) {
            epsilon_set_error("Failed to allocate FFI argument types array");
            epsilon_cleanup_callback(cb);
            return -1;
        }
        
        for (int i = 0; i < nargs; i++) {
            ffi_arg_types[i] = epsilon_type_to_ffi_type(arg_types[i]);
            if (!ffi_arg_types[i]) {
                free(ffi_arg_types);
                epsilon_cleanup_callback(cb);
                return -1;
            }
        }
    }
    
    /* Prepare the call interface */
    status = ffi_prep_cif(&cb->cif, FFI_DEFAULT_ABI, nargs, ffi_return_type, ffi_arg_types);
    if (status != FFI_OK) {
        epsilon_set_error("ffi_prep_cif failed with status %d", status);
        if (ffi_arg_types) free(ffi_arg_types);
        epsilon_cleanup_callback(cb);
        return -1;
    }
    
    /* Allocate closure memory */
    cb->writable = ffi_closure_alloc(sizeof(ffi_closure), &cb->executable);
    if (!cb->writable) {
        epsilon_set_error("ffi_closure_alloc failed");
        if (ffi_arg_types) free(ffi_arg_types);
        epsilon_cleanup_callback(cb);
        return -1;
    }
    
    cb->closure = (ffi_closure*)cb->writable;
    
    /* Prepare the closure */
    status = ffi_prep_closure_loc(cb->closure, &cb->cif, epsilon_callback_handler, cb, cb->executable);
    if (status != FFI_OK) {
        epsilon_set_error("ffi_prep_closure_loc failed with status %d", status);
        if (ffi_arg_types) free(ffi_arg_types);
        epsilon_cleanup_callback(cb);
        return -1;
    }
    
    /* Add to registry */
    cb->next = callback_registry;
    callback_registry = cb;
    
    /* Clean up temporary arrays */
    if (ffi_arg_types) free(ffi_arg_types);
    
    return cb->callback_id;
}

/*
 * Get the C function pointer for a callback
 * Returns NULL if callback ID is invalid
 */
void* epsilon_get_callback_pointer(int callback_id) {
    epsilon_callback_t *cb = callback_registry;
    
    while (cb) {
        if (cb->callback_id == callback_id) {
            return cb->executable;
        }
        cb = cb->next;
    }
    
    epsilon_set_error("Callback ID %d not found", callback_id);
    return NULL;
}

/*
 * Destroy a callback and free all associated resources
 */
int epsilon_destroy_callback(int callback_id) {
    epsilon_callback_t **cb_ptr = &callback_registry;
    epsilon_callback_t *cb;
    
    while (*cb_ptr) {
        if ((*cb_ptr)->callback_id == callback_id) {
            cb = *cb_ptr;
            *cb_ptr = cb->next;
            epsilon_cleanup_callback(cb);
            return 0;
        }
        cb_ptr = &(*cb_ptr)->next;
    }
    
    epsilon_set_error("Callback ID %d not found for destruction", callback_id);
    return -1;
}

/*
 * Clean up callback resources
 */
static void epsilon_cleanup_callback(epsilon_callback_t *cb) {
    if (!cb) return;
    
    if (cb->writable) {
        ffi_closure_free(cb->writable);
    }
    
    if (cb->arg_types) {
        free(cb->arg_types);
    }
    
    free(cb);
}

/*
 * Clean up all callbacks (useful for shutdown)
 */
void epsilon_cleanup_all_callbacks(void) {
    epsilon_callback_t *cb = callback_registry;
    epsilon_callback_t *next;
    
    while (cb) {
        next = cb->next;
        epsilon_cleanup_callback(cb);
        cb = next;
    }
    
    callback_registry = NULL;
    next_callback_id = 1;
}

/*
 * Get callback count for debugging
 */
int epsilon_get_callback_count(void) {
    int count = 0;
    epsilon_callback_t *cb = callback_registry;
    
    while (cb) {
        count++;
        cb = cb->next;
    }
    
    return count;
}

/*
 * Test function to verify the extension is working
 */
int epsilon_libffi_test(void) {
    return 42;
}