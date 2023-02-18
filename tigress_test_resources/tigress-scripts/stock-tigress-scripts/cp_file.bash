#!/bin/bash

# Take Tigress arguments and ignore all of them except the name of the
# input file and the name of the output file.  Copy the input to the
# output.

options=(`getopt -l out: -o "" -q -- $@`)
if [ "${options[0]}" != "--out" ]; then
  echo "Error: missing required option, --out=<output-filename>." 1>&2
  exit 1
fi

output_filename=${options[1]}
echo ${output_filename}

if [ "${options[2]}" != "--" ]; then
  echo "Error: apparently missing source filename." 1>&2
  exit 2
fi

input_filename=${options[3]}
echo ${input_filename}

echo $input_filename $output_filename| xargs /bin/cp

