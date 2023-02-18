# Introduction

Welcome to Tigress_test. This is a testing program for Dr. Christian Collberg's [Tigress](https://tigress.wtf) project for obfuscating C code.

## Main ideas and jargon

_Transformations_ are what tigress *actually does* to the code. There are ~32 kinds of transformations, outlined on [Tigress.wtf](https://tigress.wtf/transformations.html).

We must always specify *input and output .c files* (the output one, of course, will be the obfuscated/transformed version of the input one).

_Functions_ represent the functions *in the .c program(s) we want to modify* that we would like to apply the transformation(s) to.
* For example, for the .c file `$TIGRESS_TEST_HOME/database/c_simple_source/arith/unArith1_1.c` (which uses the template `$TIGRESS_TEST_HOME/database/c_simple_source/arith/unArith1.in`), the function that we want to run is `tigress_obf`.

## Directory structure on barbara

In $TIGRESS_TEST_HOME (again, ~/tigress_test), there are 3 folders: database, src, and test_output.
- `database/` contains:
	- .c files to obfuscate for testing.
- `src/` contains:
	- The source code for Tigress_test.
	- The executable for Tigress_test, located at `_build/default/tigress_test2/tigress_test2.exe`.
	- Scripts for Tigress_test, located in tigress_test2/
- `test_output/` contains:
	- The results.json file(s) generated by Tigress_test.

Most filenames can be made relative to \$TIGRESS_TEST_HOME (which is, again, ~/tigress_test). `.tigress_test_config` can exist in multiple directories, however, so be careful. 

There are some files to note outside of \$TIGRESS_TEST_HOME, as well. `/var/tigress_test/saved_test_results/` contains some results files that may be useful. The ones in `2022-08-14/` were produced using a script like `$TIGRESS_TEST_HOME/src/tigress_test2/script_db_simple_pairs.json`, but with the `“pipeline_length”:` parameter omitted. When running in parallel with 48 processes, Waleed believes it takes about a couple of hours to produce those results.

---
# Using Tigress_test

## Configuration

- Set $TIGRESS_HOME: `export TIGRESS_HOME=~/tigress/3.3.3`
- Set $TIGRESS_TEST_HOME with `export TIGRESS_TEST_HOME=~/tigress_test`
- Add the Tigress executable to path: `export PATH=~/tigress/3.3.3:$PATH`

## An explanation of parameters

Every tigress command execution must include lots of information. It gets this information from a variety of places. This section specifies how information is retrieved. This is important because the differences between scripts should be small and you need to focus on the right parameters.

### `.tigress_test_config`

When running a script, Tigress_test retreives information from a .tigress_test_config file. It contains lots of relevant information that you should be aware of. This is an example `.tigress_test_config` file:
```
{
  "format_version": [ 1 ],
  "path_to_program": "tigress",
  "program_default_options": "--Environment=x86_64:Linux:Gcc:4.6 -D _Float128=double --Transform=InitEntropy --Functions=main --InitEntropyKinds=vars --Transform=InitOpaque --Functions=main --InitOpaqueStructs=list,array --Transform=InitImplicitFlow --Functions=main --InitImplicitFlowKinds=counter_int,bitcopy_loop ",
  "compiler_options": "-lm -lpthread -Wpedantic",
  "database_dir": "database",
  "path_to_c_compiler": "/usr/bin/gcc",
  "default_script": "src/tigress_test2/script_01.json",
  "output_dir": "test_output/c_simple_test/"
  ,"compatible_platforms": ["Linux_x86"]
  ,"remote_results_sync_interval_seconds": 5
}
```
Most of these parameters contain environment information and can be ignored. Others, however, contain information quite relevant to running scripts. Here are ones of note: such as: `"compiler_options":`, `"database_dir":`, and `"output_dir":`. 

* `"compiler_options":` provides parameters to the c compiler. In my `.tigress_test_config` file, I added -Wpedantic to generate more verbosity in the results.json file's "output from compiling original/obfuscated source": fields.
* `"database_dir":` provides a path to prepend to any filenames specified in the `"from_database":` parameter in Tigress_test scripts (see "Running Tigress_test>Running Tigress_test from a script>Using option 2").
* `"output_dir":` specifies where Tigress_test should output the results.json file. This can be wherever you want, just make sure you can find it!

Tigress_test looks for `.tigress_test_config` in the working directory first; then it looks in `~/` (\$HOME). Personally, I run tigress_test.exe from `~/` (\$HOME) so there is no conflict between config files. Do with that what you will.

### `$TIGRESS_TEST_HOME/database/*_description.json`

These files are needed for using the `"from_database":` parameter method (see "Running Tigress_test>Running Tigress_test from a script>Using parameter method 2").

## Running Tigress_test

Tigress_test can be run in two main ways:
- "Ad hoc", where no script is used and, instead, options are configured directly on the command line with --flags.
- From a script, where options are pulled from a pre-configured script.json file.

### Running Tigress_test “ad hoc”

Running Tigress_test "ad hoc" from `~/` ($HOME) will look something like this:

`tigress_test/src/_build/default/tigress_test2/tigress_test.exe adhoc {--flags} --source-file=tigress_test/database/c_source/c_file_*.c`.

The first parameter is the executable file for Tigress_test; the second parameter is “adhoc”, which specifies that Tigress_test will receive information about the execution entirely from the command line (and the .tigress_test_config file).

Flags are configurable and some are necessary. The following lists necessary flags and what they do:
- --transform={transformation}
- --seed={seed}
- --transform-option=”Functions,{functions in the .c file you provide}”

Some extra flags of note include:
- --transform-option=”{transformation option name},{transformation option}”
	- This is for extra transformation options, such as VirtualizeDispatch=direct for the virtualize transformation.
		- This could look like --transform-option=”VirtualizeDispatch,direct” if the transformation used was Virtualize; see Tigress.wtf for more information.
- --save={saved_script.json}
	- This can save all of the options specified in the adhoc execution to a script, which can be rerun and reproduced using `tigress_test.exe run {saved_script.json}`.

Use the command, “../build/default/tigress_test2/tigress_test2.exe help adhoc”, to see the detailed usage information.

### Running Tigress_test from a script

Running Tigress_test from a script from `~/` ($HOME) will look something like this:

`tigress_test/src/_build/default/tigress_test2/tigress_test.exe run script.json

To do this, you need a script. Scripts all contain certain parameters. These are:
* TODO

There are two main ways to write scripts from here:
1. By specifying `"source_files" :` and `"transformations" :`
	* This is useful for running highly specific tests only with transformation options that you explicitly write in the script.
2. By specifying `"from_database":`
	* This is useful for running more tests with an array of transformations and transformation options.

Note: if both field types appear in a script, Tigress_test will ignore the second one that appears in the script.

Note: if the script file name is left blank, it will check the configuration file for a default script, and if one is configured, it will use it (otherwise, it will give an error).

#### Using parameter method 1 (`"source_files" :` and `"transformations" :`)

In the first style, we specify source files manually with the `"source_files":` parameter. Here is an example of a full `"source_files":` parameter (this one is taken from `script_01.json`):
```
"source_files": [
    {
      "filename": "database/c_source/test?.c",
      "checksum": ""
    }
  ],
```

It's pretty self explanatory; `"filename":` and `"checksum":` are the only sub-parameters.

Next, we specify the transformations and transformation options with the `"transformations":` parameter. Here is an example of a full `"transformations":` parameter (this one is also taken from `script_01.json`):
```
"transformations": [
    {
      "pipeline": [
        {
          "name": "Virtualize",
          "options": [
            { "name": "Functions",
              "value" : "fib,fac"
            },
            { "name": "VirtualizeDispatch",
              "value" : "direct"
            }
          ]
        }
      ]
    },
    {
      "pipeline": [
        {
          "name": "Split",
          "options": [
            { "name": "Functions",
              "value" : "fib,fac"
            }
          ]
        }
      ]
    }
```

As you can see, there is a lot more going on here. Inside `"transformations":` we have two "pipelines", each specifying transformations and transformation options within them. Transformations can be applied on top of each other by putting them in the same pipeline; putting them in separate pipelines, as is done here, applies them independently in separate tests. This gives you lots of control over the transformations, the transformation options, and how they are applied (i.e. in parallel or in series). 

Transformations should always be in a pipeline, even if you only have 1 pipeline and one transformation. You can also have any number of pipelines you want.

#### Using parameter method 2 (`"from_database":`)

In the second style, we specify everything in the `"from_database":` parameter. Here is an example of a full `"from_database":` parameter (this one is taken from `script_db_simple_pairs.json`):
```
"from_database": {
    "pipeline_length": "Pairs",
    "source_file_patterns": [ "c_simple_source/**.c" ],
    "transformations": ["Exclude", ["AddOpaque", "AntiTaintAnalysis", "InitPlugins", "RandomFuns", "RndArgs", "Jit", "JitDynamic", "CleanUp"]]
   }
```
* `"pipeline_length":` will only take "Pairs".
	* Include this parameter if you want to test, in pipelines, all possible pairs of all included transformations. This will generate pairs in both directions: 1-->2 AND 2-->1. That can end up being **a lot**. Waleed has estimated that the `"from_database":` excerpt above, if run will actually take about a month to complete when it is run in parallel mode with 48 processes.
* `"source_file_patterns":` expects a path, which can include wildcards.
* `"transformations":` expects either "Include" or "Exclude", followed by a list of whichever transformations.
	* If you specify "Include", Tigress_test will include all the transformations you subsequently specified when it generates run options.
	* If you specify "Exclude", Tigress_test will include all transformations (specified in and read from `$TIGRESS_TEST_HOME/database/transformation_description.json`) *except for* the transformations you subsequently specified.

You may be asking: "where do we specify the functions?" Good question. The `$TIGRESS_TEST_HOME/database/transformation_description.json` file contains this information for all .c code in `$TIGRESS_TEST_HOME/database/`. 

**The second style is dependent on these two `$TIGRESS_TEST_HOME/database/*_description.json` files and wont work on .c code that isn't specified in those files.**

In many scripts using the `"from_database":` parameter method, there will also be empty `"source_files":` and `"transformations":` parameters. As mentioned in "Running Tigress_test>Running Tigress_test from a script", Tigress_test will ignore the second parameter method that appears in the script.

---

# Tigress Error Message Types:

If you get any of the following errors when running tigress_test2, it is likely a bug in tigress_test2, but it could also indicate a problem with the Tigress documentation (saying that it can do something that it cannot):
* ERR-NOT-IMPLEMENTED (** This feature has not been implemented. *)
* ERR-NOT-POSSIBLE (** For example, it's not possible to apply this transformation to this function. *)
* ERR-BAD-REQUEST (** User asked for something that does not make sense. *)
* ERR-ILLEGAL-ARG (** A command line argument does not make sense. *)
* ERR-ARG-PARSE (** Parse error during command line parsing. *)

You should not get the following error message because code annotations have not been implemented yet. However, if you do it might be a bug in Tigress:
* ERR-BAD-ANNOTATION-REQUEST (** User had a bad annotation in the code, such as for function regions. *)

The "ShouldNeverHappen exception" is an indication of a programming error in Tigress. This error doesn’t have specific text associated with it, except that it concatenates "\<\<Tigress\>\>", the name of the transformation and then a colon (":").