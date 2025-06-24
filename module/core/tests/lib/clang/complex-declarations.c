typedef struct node {
    int data;
    struct node *next;
    struct node *prev;
} node_t;

union data {
    int i;
    float f;
    char c[4];
};

enum state {
    IDLE = 0,
    RUNNING = 1,
    STOPPED = 2
};

static const int MAX_SIZE = 1024;
extern volatile int global_flag;