/*
 * epsilon-libffi.h - Header for libffi integration
 */

#ifndef EPSILON_LIBFFI_H
#define EPSILON_LIBFFI_H

#ifdef __cplusplus
extern "C" {
#endif

/* Type constants matching Lisp-side definitions */
#define EPSILON_TYPE_VOID       0
#define EPSILON_TYPE_INT        1
#define EPSILON_TYPE_LONG       2
#define EPSILON_TYPE_FLOAT      3
#define EPSILON_TYPE_DOUBLE     4
#define EPSILON_TYPE_POINTER    5
#define EPSILON_TYPE_STRING     6
#define EPSILON_TYPE_BOOL       7

/* Core API functions */
int epsilon_create_callback(void *lisp_function, int return_type, int *arg_types, int nargs);
void* epsilon_get_callback_pointer(int callback_id);
int epsilon_destroy_callback(int callback_id);
void epsilon_cleanup_all_callbacks(void);

/* Utility functions */
int epsilon_get_callback_count(void);
const char* epsilon_get_last_error(void);
int epsilon_libffi_test(void);

#ifdef __cplusplus
}
#endif

#endif /* EPSILON_LIBFFI_H */