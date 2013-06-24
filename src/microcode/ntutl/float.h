/* float.h */
/* Produced by hard-params version 4.1, CWI, Amsterdam */

#define FLT_RADIX 2
#define FLT_MANT_DIG 24
#define FLT_DIG 6
#define FLT_ROUNDS 1
#define FLT_EPSILON ((float)1.19209290e-007)
#define FLT_MIN_EXP (-125)
#define FLT_MIN ((float)1.17549435e-038)
#define FLT_MIN_10_EXP (-37)
#define FLT_MAX_EXP 128
#define FLT_MAX ((float)3.40282347e+038)
#define FLT_MAX_10_EXP 38

#define DBL_MANT_DIG 53
#define DBL_DIG 15
#define DBL_EPSILON 2.2204460492503130e-016

/* *** WARNING: Possibly bad output from printf above */
/*     expected value around 2.2204460492503130e-16, bit pattern:
    00000000 00000000 00000000 00000000 00000000 00000000 10110000 00111100 */
/*     sscanf gave           2.2204460492503126e-016, bit pattern:
    11111111 11111111 11111111 11111111 11111111 11111111 10101111 00111100 */
/*     difference= 2.4651903288156620e-032 */

#define DBL_MIN_EXP (-1021)
#define DBL_MIN 2.2250738585072018e-308
#define DBL_MIN_10_EXP (-307)
#define DBL_MAX_EXP 1024
#define DBL_MAX 1.7976931348623155e+308

/* *** WARNING: Possibly bad output from printf above */
/*     expected value around 1.7976931348623164e308, bit pattern:
    11111111 11111111 11111111 11111111 11111111 11111111 11101111 01111111 */
/*     sscanf gave           inf, bit pattern:
    00000000 00000000 00000000 00000000 00000000 00000000 11110000 01111111 */
/*     difference= -inf */

#define DBL_MAX_10_EXP 308

