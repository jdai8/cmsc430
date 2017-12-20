

//#include "gc.h"    // Add back in and change tags if we want to use GC
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "hamt/hamt.h"

// non-heap allocated values: can be tagged
#define INT_TAG 1
#define STR_TAG 2
#define SYM_TAG 3
// tag other values using vector[0]
#define CLO_TAG 4
#define CONS_TAG 5
#define VECTOR_TAG 6
#define HASH_TAG 7

// #define ENUM_TAG 7
/* #define VECTOR_OTHERTAG 1 */
/* #define HASH_OTHERTAG 2 */
// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0  

#define MASK64 0xffffffffffffffff // useful for tagging related operations

#define ASSERT_TAG(v,tag,msg) \
    if(((v) & 1) || ((u64*)(v))[0] != (tag)) \
        fatal_err(msg);

    /* if(((v)&7ULL) != (tag)) \ */
    /*     fatal_err(msg); */

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);


// #define DECODE_CLO(v) ((u64*)(v))
// #define DECODE_CLO(v) ((u64*)((v)&(7ULL^MASK64)))
// #define ENCODE_CLO(v) (((u64)(v)) | CLO_TAG)

// decode u64*s
#define DECODE_INT(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
#define ENCODE_INT(v) ((((u64)((u32)(v))) << 32) | INT_TAG)

#define DECODE_STR(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_STR(v) (((u64)(v)) | STR_TAG)

#define DECODE_SYM(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_SYM(v) (((u64)(v)) | SYM_TAG)

#define DECODE_CONS(v) ((u64*)(((u64*)(v))[1]))
#define ENCODE_CONS(v) (heap_encode(((u64)(v)), CONS_TAG))

#define DECODE_VECTOR(v) ((u64*)(((u64*)(v))[1]))
#define ENCODE_VECTOR(v) (heap_encode(((u64)(v)), VECTOR_TAG))

#define DECODE_HASH(v) ((u64*)(((u64*)(v))[1]))
#define ENCODE_HASH(v) (heap_encode(((u64)(v)), HASH_TAG))

/* #define DECODE_CONS(v) ((u64*)((v)&(7ULL^MASK64))) */
/* #define ENCODE_CONS(v) (((u64)(v)) | CONS_TAG) */

/* #define DECODE_OTHER(v) ((u64*)((v)&(7ULL^MASK64))) */
/* #define ENCODE_OTHER(v) (((u64)(v)) | OTHER_TAG) */


// some apply-prim macros for expecting 1 argument or 2 arguments
#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    } 

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    } 

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    } 





// No mangled names
extern "C"
{



typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;


    
// UTILS

u64* alloc(const u64 m)
{
    return new u64[m];
    //return (u64*)GC_MALLOC(m);
}

u64 get_tag(u64 v) {

   u64 tag = v & 7ULL;
   if (tag)
     return tag;

   u64 *ptr = (u64*)v;
   return ((u64) (ptr[1]));
}

u64 heap_encode(u64 v, u64 tag) {
    u64 *p = alloc(sizeof(u64) * 2);
    p[0] = tag;
    p[1] = v;
    return ((u64)p);
}


void fatal_err(const char* msg)
{
    printf("library run-time error: ");
    printf("%s", msg);
    printf("\n");
    exit(1);
}

void print_u64(u64 i)
{
    printf("%lu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    ASSERT_TAG(args, CONS_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.")
    u64* p = DECODE_CONS(args);
    ASSERT_VALUE((p[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.")
    return p[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest                          
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p) 
    ASSERT_TAG(p, CONS_TAG, "Expected a cons value. (expect_cons)")               

    u64* pp = DECODE_CONS(p);
    *rest = pp[1];
    return pp[0];
}

/* u64 expect_other(u64 v, u64* rest) */
/* { */
/*     // returns the runtime tag value */
/*     // puts the untagged value at *rest */
/*     ASSERT_TAG(v, OTHER_TAG, "Expected a vector or special value. (expect_other)") */
    
/*     u64* p = DECODE_OTHER(v); */
/*     *rest = p[1]; */
/*     return p[0]; */
/* } */


/////// CONSTANTS
    
    
u64 const_init_int(s64 i)
{
    return ENCODE_INT((s32)i);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}

    
u64 const_init_false()
{
    return V_FALSE;
}

    
u64 const_init_string(const char* s)
{
    return ENCODE_STR(s);
}
        
u64 const_init_symbol(const char* s)
{
    return ENCODE_SYM(s);
}







/////////// PRIMS

    
///// effectful prims:

    
u64 prim_print_aux(u64 v) 
{
    if (v == V_NULL)
        printf("()");
    else if (get_tag(v) == CLO_TAG)
        printf("#<procedure>");
    else if (get_tag(v) == CONS_TAG)
    {
        u64* p = DECODE_CONS(v);
        printf("(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if (get_tag(v) == INT_TAG)
    {
        printf("%d", (int)(DECODE_INT(v)));
    }
    else if (get_tag(v) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if (get_tag(v) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", DECODE_SYM(v));
    }
    else if (get_tag(v) == VECTOR_TAG)
             //&& (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("#(");
        u64* vec = (u64*)DECODE_VECTOR(v);
        u64 len = vec[0];
        prim_print_aux(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print_aux(vec[i]);
        }
        printf(")");
    }
    else
        printf("(print.. v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}

u64 prim_print(u64 v) 
{
    if (v == V_NULL)
        printf("'()");
    else if (v == V_VOID)
        printf("(void)");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
    else if (get_tag(v) == CONS_TAG)
    {
        u64* p = DECODE_CONS(v);
        printf("'(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if (get_tag(v) == INT_TAG)
    {
        printf("%d", DECODE_INT(v));
    }
    else if (get_tag(v) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if (get_tag(v) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", DECODE_SYM(v));
    }
    else if (get_tag(v) == VECTOR_TAG)
             //&& (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("#(");
        u64* vec = DECODE_VECTOR(v);
        u64 len = vec[0];
        prim_print(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print(vec[i]);
        }
        printf(")");
    }
    else if ((v&7) == 0)
        printf("#<procedure>");
    else
        printf("(print v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL; 
}


u64 applyprim_vector(u64 lst)
{
    // pretty terrible, but works
    u64* buffer = new u64[256];
    u64 i = 0;
    while (get_tag(lst) == CONS_TAG && i < 256)
        buffer[i++] = expect_cons(lst, &lst);
    u64* mem = alloc(i+1);
    mem[0] = VECTOR_TAG;
    for (u64 j = 1; j <= i; ++j)
        mem[j] = buffer[j-1];
    delete [] buffer;
    return ENCODE_VECTOR(mem);
}

u64 prim_make_45vector(u64 lenv, u64 iv)
{
    ASSERT_TAG(lenv, INT_TAG, "first argument to make-vector must be an integer")

    const u64 l = DECODE_INT(lenv);
    u64* vec = (u64*)alloc(1 + (l * sizeof(u64)));
    vec[0] = l;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return ENCODE_VECTOR(vec);
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(v, VECTOR_TAG, "first argument to vector-ref must be a vector")

    return DECODE_VECTOR(v)[1+DECODE_INT(i)];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-set! must be an integer")
    ASSERT_TAG(a, VECTOR_TAG, "first argument to vector-set! must be a vector")

    DECODE_VECTOR(a)[1+DECODE_INT(i)] = v;
        
    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)

// HAMT

// u64 prim_hash_45ref(u64 h, u64


///// void, ...

    
u64 prim_void()
{
    return V_VOID;
}


    



///// eq?, eqv?, equal?

    
u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    //else if  // optional extra logic
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/*
u64 prim_equal_63(u64 a, u64 b)
{
    return 0;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
*/


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if (get_tag(a) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if (get_tag(a) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if (get_tag(a) == CLO_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)    


u64 prim_cons_63(u64 p) // cons?
{
    if (get_tag(p) == CONS_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)    


u64 prim_cons(u64 a, u64 b)
{
    u64* p = alloc(2*sizeof(u64));
    p[0] = a;
    p[1] = b;
    return ENCODE_CONS(p);
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)


u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)


u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)


///// s32 prims, +, -, *, =, ...

    
u64 prim__43(u64 a, u64 b) // +
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim + a b); b is not an integer")

        //printf("sum: %ld\n", DECODE_INT(a) + DECODE_INT(b));
    
    return ENCODE_INT(DECODE_INT(a) + DECODE_INT(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) + DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__45(u64 a, u64 b) // -
{
    ASSERT_TAG(a, INT_TAG, "(prim - a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim - a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) - DECODE_INT(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply - on non list value.")
        u64* pp = DECODE_CONS(p);
        if (pp[1] == V_NULL)
            return ENCODE_INT(0 - DECODE_INT(pp[0]));
        else // ideally would be properly left-to-right
            return ENCODE_INT(DECODE_INT(pp[0]) - DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__42(u64 a, u64 b) // *
{
    ASSERT_TAG(a, INT_TAG, "(prim * a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim * a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) * DECODE_INT(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(1);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply * on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) * DECODE_INT(applyprim__42(pp[1])));
    }
}
    
u64 prim__47(u64 a, u64 b) // /
{
    ASSERT_TAG(a, INT_TAG, "(prim / a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim / a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) / DECODE_INT(b));
}
    
u64 prim__61(u64 a, u64 b)  // =
{
    ASSERT_TAG(a, INT_TAG, "(prim = a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim = a b); b is not an integer")
        
    if (DECODE_INT(a) == DECODE_INT(b))
    // if ((s32)((a&(7ULL^MASK64)) >> 32) == (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    ASSERT_TAG(a, INT_TAG, "(prim < a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim < a b); b is not an integer")
    
    if (DECODE_INT(a) < DECODE_INT(b))
    // if ((s32)((a&(7ULL^MASK64)) >> 32) < (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}
    
u64 prim__60_61(u64 a, u64 b) // <=
{
    ASSERT_TAG(a, INT_TAG, "(prim <= a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim <= a b); b is not an integer")
        
    if (DECODE_INT(a) <= DECODE_INT(b))
    // if ((s32)((a&(7ULL^MASK64)) >> 32) <= (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a) 
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)

