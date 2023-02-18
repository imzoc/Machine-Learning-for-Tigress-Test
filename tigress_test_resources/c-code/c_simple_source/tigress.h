/* This is tigress.h */

/*******************************************************************************/
/* We don't support clang extensions.                                          */
/*  https://clang.llvm.org/docs/BlockLanguageSpec.html#the-block-type          */
/*******************************************************************************/

#ifdef __clang__ 
#undef __BLOCKS__
#endif

/*******************************************************************************/
/* We don't support C11 features.                                              */
/*  https://en.wikipedia.org/wiki/C11_(C_standard_revision)#Changes_from_C99   */
/*******************************************************************************/

#define _Noreturn

/*******************************************************************************/
/* User annotations.                                                           */
/*******************************************************************************/

#define TIGRESS_IDENT(x) x
#define TIGRESS_QUOTE(x) #x
#define TIGRESS_STR(x) TIGRESS_QUOTE(x)

#define CHECKSUM_INSERT asm volatile ("##_ANNOTATION_CHECKSUM_INSERT");

#define VIRTUALIZE_BEGIN(region) asm volatile ("##_ANNOTATION_VIRTUALIZE-REGION_BEGIN_" region);
#define VIRTUALIZE_END(region) asm volatile ("##_ANNOTATION_VIRTUALIZE-REGION_END_" region);

#define FLATTEN_BEGIN(region) asm volatile ("##_ANNOTATION_FLATTEN-REGION_BEGIN_" region);
#define FLATTEN_END(region) asm volatile ("##_ANNOTATION_FLATTEN-REGION_END_" region);

#define ENCODE_INTEGER_BEGIN(region) asm volatile ("##_ANNOTATION_ENCODEINTEGER-REGION_BEGIN_" region);
#define ENCODE_INTEGER_END(region) asm volatile ("##_ANNOTATION_ENCODEINTEGER-REGION_END_" region);

#define ENTROPY_VARIABLE(v) asm volatile (TIGRESS_IDENT("##_ANNOTATION_ENTROPY-VARIABLE_")TIGRESS_STR(v));
#define ENTROPY_UNINIT asm volatile ("##_ANNOTATION_ENTROPY-UNINIT");
