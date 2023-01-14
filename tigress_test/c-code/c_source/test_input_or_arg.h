#ifndef __TEST_INPUT_OR_ARG_
#define __TEST_INPUT_OR_ARG_
typedef enum {INPUT, ARG} en_in_choice_t;

int test_input_or_arg(en_in_choice_t choice, char expect, int argc, char** argv); 
#endif
