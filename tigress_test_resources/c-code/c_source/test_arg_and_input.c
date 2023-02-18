#include "test_input_or_arg.h"

int main(int argc, char **argv) {
  int ret1 = test_input_or_arg(ARG, 'd', argc, argv);
  if (0 != ret1)
    return ret1;
  return test_input_or_arg(INPUT, 'c', 0, 0);
}
