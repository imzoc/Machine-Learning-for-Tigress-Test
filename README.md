# Setting up Tigress_test

## Configuration

- Set $TIGRESS_HOME: `export TIGRESS_HOME=~/tigress/3.3.3`
- Set $TIGRESS_TEST_HOME with `export TIGRESS_TEST_HOME=~/tigress_test`
- Add the Tigress executable to path: `export PATH=~/tigress/3.3.3:$PATH`

---
## Directory structure

In $TIGRESS_TEST_HOME (again, ~/tigress_test), there are 3 folders: database, src, and test_output.
- database contains:
	- .c files to obfuscate for testing.
- src contains:
	- The source code for Tigress_test
	- The executable for Tigress_test, located in _build/default/tigress_test2/tigress_test2.exe)
	- Scripts for Tigress_test, located in tigress_test2/
- test_output contains:
	- The obfuscated .c code outputted by the test by default. (.tigress-test-config)).

Note that most filenames can be made relative to $TIGRESS_TEST_HOME (which is, again, ~/tigress_test).

---
--- 
# Instructions for running Tigress_test

## Main ideas and lingo

_Transformations_ are what tigress *actually does* to the code. There are ~32 kinds of transformations, outlined on (Tigress.wtf)[[https://tigress.wtf/transformations.html](https://tigress.wtf/transformations.html)].

We must always specify *input and output .c files* (the output one, of course, will be the obfuscated/transformed version of the input one).

_Functions_ represent the functions *in the .c program(s) we want to modify* that we would like to apply the transformation(s) to.
* For example, for the .c file `$TIGRESS_TEST_HOME/database/c_simple_source/arith/unArith1_1.c` (which uses the template `$TIGRESS_TEST_HOME/database/c_simple_source/arith/unArith1.in`), the function that we want to run is `tigress_obf`.

---

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

---
#### `from_database` option automation.

See "Running Tigress_test>Running Tigress_test from a script>Using option 2".

What files does Tigress_test pull information from?

---
---
## Running Tigress_test

Tigress_test can be run in two main ways:
- `./tigress_test.exe adhoc {options}`
	- All options are configured directly on the command line with CLI flags.
- `./tigress_test.exe run script.json {options}`
	- Most options are pulled from a pre-configured .json script file.

### Running Tigress_test “ad hoc”

If you run Tigress_test from `~/` ($HOME), the basic format will look like this:
- `tigress_test/src/_build/default/tigress_test2/tigress_test.exe adhoc {--flags} --source-file=tigress_test/database/c_source/c_file_*.c`.

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

---
### Running Tigress_test from a script

If you run Tigress_test from `~/` ($HOME), the basic format will look like this:
- `tigress_test/src/_build/default/tigress_test2/tigress_test.exe run script.json

To do this, you need a script. There are two major ways to format scripts:
1. By specifying `"source_files" :` and `"transformations" :`
	* This is useful for running highly specific tests only with transformation options that you explicitly write in the script.
2. By specifying `"from_database":`
	* This is useful for running more tests with an array of transformations and transformation options.

If both feild types appear in a script, Tigress_test will ignore the second one that appears in the script.

If the script file name is left blank, it will check the configuration file for a default script, and if one is configured, it will use it (otherwise, it will give an error).

#### Using parameter method 1

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

---
#### Using parameter method 2

In the second style, we let the Tigress_test program automate some of the option generation process. This automation is helpful when you want to run tests with different combinations of transformations and transformation options.

In the second style, we specify everything in the `"from_database":` parameter. Here is an example of a full `"from_database":` parameter (this one is taken from `script_db_simple_pairs.json`):
```
"from_database": {
    "pipeline_length": "Pairs",
    "source_file_patterns": [ "c_simple_source/**.c" ],
    "transformations": ["Exclude", ["AddOpaque", "AntiTaintAnalysis", "InitPlugins", "RandomFuns", "RndArgs", "Jit", "JitDynamic", "CleanUp"]]
   }
```
* `"pipeline_length":` Can be pairs.
* `"source_file_patterns":` expects a path, which can include wildcards.
* `"transformations":` expects either "Include" or "Exclude", followed by a list of whichever transformations.
	* If you specify "Include", Tigress_test will include all the transformations you subsequently specified when it generates run options.
	* If you specify "Exclude", Tigress_test will include all of the transformations it is capable of performing *except for* the transformations you subsequently specified.

In many scripts using the `"from_database":` parameter method, there will also be empty `"source_files":` and `"transformations":` parameters. As mentioned in "Running Tigress_test>Running Tigress_test from a script", Tigress_test will ignore the second parameter method that appears in the script.

---

An example of the second style of writing scripts is in the file “script_db_simple.json”.  Below is a version of that script (I am not sure that it is the same version as the one installed in your home directory. I think a script like this one is called “script_db_simple_pairs.json in your installation).
```
{
  "file_type": "script",
  "file_format_version": [ 1 ],
  "seeds": [ 1 ],
  "compiler_spec": {
    "filename": "gcc",
    "version": "5.4",
    "version_command": "gcc --version"
  },
  "compiler_options": "",
  "tigress_spec": {
    "filename": "tigress",
    "version": "3.2",
    "version_command": ""
  },
  "tigress_options": "--Environment=x86_64:Linux:Gcc:4.6",
  "from_database": {
    "pipeline_length": "Pairs",
    "source_file_patterns": [ "c_simple_source/**.c" ],
    "transformations": ["Exclude", ["AddOpaque", "AntiTaintAnalysis", "InitPlugins", "RandomFuns", "RndArgs", "Jit", "JitDynamic", "CleanUp"]]
   },
  "source_files" : [],
  "transformations" : []
}
```
Notice that in this script, “source_files” and “transformations” fields are empty arrays.  Instead, there is a new field, “from_database”.  The presence of that (“from_database”) field causes those other two fields to be ignored.  In the “from_database” section, you can tell it which transformations to include or exclude.  If you give it “transformations”: [“Exclude”, []] it will create a test for every option of every transformation.  It knows what those transformations and options are by reading them from the file “tigress_test/database/transformation_description.json”.  In the field, “source_file_patterns”, the pair of asterisks (“**”) tells it to descend into directories to find files (or put another way “**” matches any character, even a “/”, whereas, “*” (just one asterisk), doesn’t match “/” and, therefore, doesn’t descend into directories).  So it will find c_simple_source/arith/*.c and c_simple_source/call/*.c, etc., and create tests for them. It knows which functions, arguments, and inputs to provide to those source files by reading the file “tigress_test/database/source_description.json”.

  

“Pipleline_length”: “Pairs” means: test all combinations of pairs of transformations.  In this context, “pair” means having the output of one transformation passed to a second transformation to apply that second transformation on top of the first.  Omit that field (“pipeline_length”) to produce tests for pipelines containing only a single transformation (i.e., with the field omitted, it defaults to pipelines of length one).  You should not run this script.  I have calculated that it will take about a month to complete when it is run in parallel mode with 48 processes.  Eventually we will run a script like this, but not right now.  You can run scripts like this with the “pipeline_length” field omitted.  For example, if you put [“Include”, [“Virtualize”]] in the “transformations field” and [ "c_simple_source/**.c" ] in the “source_files” field, it will thoroughly test the Virtualize transformation using the c_simple_source files.  Or if you put [“gcc.c-torture/**.c”] in the “source_files” field, it will test with the GCC Torture Test source files.

The results files in the /var/tigress_test/saved_test_results/2022-08-14 directory were produced using a script like above, but with “pipeline_length” omitted.  I believe it takes about a couple of hours to produce those results when running in parallel with 48 processes.

---

## Tigress Error Message Types:

* ERR-NOT-IMPLEMENTED (** This feature has not been implemented. *)
* ERR-NOT-POSSIBLE (** For example, it's not possible to apply this transformation to this function. *)
* ERR-BAD-REQUEST (** User asked for something that does not make sense. *)
* ERR-ILLEGAL-ARG (** A command line argument does not make sense. *)
* ERR-ARG-PARSE (** Parse error during command line parsing. *)

If you get any of the above errors when running tigress_test2, it is likely a bug in tigress_test2, but it could also indicate a problem with the Tigress documentation (saying that it can do something that it cannot).

* ERR-BAD-ANNOTATION-REQUEST (** User had a bad annotation in the code, such as for function regions. *)

You should not get the above error message because code annotations have not been implemented, yet.  If you got that error message, it might be a bug in Tigress.

ShouldNeverHappen exception:  This error doesn’t have specific text associated with it, except that it starts with <<Tigress>> then the name of the transformation and then a colon (:).  This kind of exception is an indication of a programming error in Tigress.
