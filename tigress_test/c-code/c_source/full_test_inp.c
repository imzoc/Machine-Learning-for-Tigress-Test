#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <errno.h>
#include <limits.h>
#include "test_input_or_arg.h"

int test_input_or_arg(en_in_choice_t choice, char expect, int argc, char** argv) {
  struct timeval tv;
  int ret = gettimeofday(&tv, NULL);
  int saved_errno = errno;
  if (0 != ret) {
    perror("Error: gettimeofday retuned a non-zero return code.\n");
    fprintf(stderr, "Error: gettimeofday returned: %d;"
                    "errno is: %d.\n", ret, saved_errno);
    fprintf(stderr, "%s", "\n");
    return 1;
  }
  char input = (expect + 10) % CHAR_MAX;
  switch (choice) {
    case INPUT:
      input = getchar();
      break;
    case ARG:
      if (2 != argc) {
        fprintf(stderr, "%s", "Error: wrong number of arguments; expected 1!\n");
        return 10;
      }
      else {
        if (argv[1] && strlen(argv[1]) > 0)
          input = argv[1][0];
        else {
          fprintf(stderr, "%s", "Error: argument was NULL or empty\n");
          return 11;
        }
      }
      break;
  }
  if (expect != input) {
    fprintf(stderr, "%s", "Error: wrong input\n");
    printf("Time since the epoch at program start was: %lu seconds;"
           " and %lu microseconds.\n", tv.tv_sec, tv.tv_usec);
    return 2;
  }
  else {
    printf("%s", "OK\n");
  }
  return 0;
}
#include "test_input_or_arg.h"

int main() {
  return test_input_or_arg(INPUT, 'a', 0, 0);
}
