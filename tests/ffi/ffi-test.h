/* -*-C-*- */

/* Header for a test library; used to test the C/Unix FFI. */

typedef struct {

  char first;

  double second;

  char third;

  char * fourth;
} TestStruct;

typedef union {

  TestStruct s;
  double d;
} TestUnion;

typedef double (* TestDoubleCallback) (double d, void *user_data);

extern double test_double (double d, TestStruct *s);
extern char * test_string (char *c, TestStruct *s);
extern void test_register_double (TestDoubleCallback callback, void *id);
extern TestStruct test_struct (TestStruct s);
extern TestUnion test_union (TestUnion u);
