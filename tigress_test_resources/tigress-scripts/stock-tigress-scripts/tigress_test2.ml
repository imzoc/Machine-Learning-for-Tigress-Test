(* framework for testing Tigress obfuscation tool
   Waleed Mebane <mebanew@arizona.edu>
   12 December 2021
   in development (not complete)
   2nd development version
*)
open Lib
open Util
open SourceDescription
module TestScript_t = TestScriptv1_1.TestScript_t
module TestScript_j = TestScriptv1_1.TestScript_j


let transformation_descriptions_basename = "transformation_description.json"
let source_descriptions_basename = "source_description.json"

let kDEFAULT_SYNC_INTERVAL_SECONDS = 60
let kDEFAULT_FILE_POLL_INTERVAL_SECONDS = 5
(*let kDEFAULT_PIDFILE_UPDATE_INTERVAL_SECONDS = 10*)
let kPIDFILE_FILENAME = "tigress_test.pidfile"
let kTOO_LONG_SECONDS = 300.0

let kDEFAULT_TEST_ORDER = (`Transformation (`Seed `Source_code))

module Database = struct
  (* define a struct type to hold the database *)
  type t = {
    (* TBD *)
    placeholder: int
  }
  (* define a function that takes a database directory as input
     and outputs a database struct *)
end

(*
let run_tigress args =
  Locations.tigress_exe ^ " " ^ args
    |> Sys.command
*)

let gen_tigress_args
    (output_filename: string)
    (db: Database.t)
    (default_options: string)
    (seed: int)
    (transformation_pipeline: TestCase.OcamlT.Transformation.t list)
    (source_file_path: string)
  =
  (* Returns a string of arguments to pass to Tigress *)
  (* note: "seed" is not optional because we need to be able to reproduce
           tests.  The seed may be randomly generated, but we should
           know what it is. *)
  (* TODO: lookup transformation name and options in the database,
           in order to determine which Tigress options the names
           correspond to (since Tigress options can change from version
           to version); for now, just use the strings as presented *)
  "--Seed=" ^ string_of_int (seed + db.placeholder - db.placeholder)
  ^ " " ^ default_options
  ^ (List.fold_right 
       (fun 
         {TestCase.OcamlT.Transformation.name = x; options = y} 
         acc -> 
         " --Transform=" ^ x ^ " "
         ^ (List.fold_right 
              (fun 
                { TestCase.OcamlT.TransformationOption.name = n; 
                  value = v; 
                } 
                t -> 
                let v =
                  match v with
                  | None -> ""
                  | Some x -> "=" ^ x
                in
                "--" ^ n ^ v ^ " " ^ t
              )
              y 
              ""
           )
         ^ " " ^ acc
       ) transformation_pipeline ""
    )
  ^ " --out=" ^ output_filename
  ^ " " ^ source_file_path


let files_equal (file1: string) (file2: string): bool =
  (* Compare two files byte by byte.  Return false if they differ;
     true otherwise. *)
  let chan1 = open_in file1 in
  let chan2 = open_in file2 in
  if in_channel_length chan1 <> in_channel_length chan2 then
    false
  else
    try
      while input_byte chan1 = input_byte chan2 do
        ()
      done;
      false
    with
      End_of_file -> true


let run_tigress
    ?(timeout_seconds: int option = None)
    ~(tigress_file_path: string)
    ~(source_file_path: string)
    ~(output_filename: string)
    ~(temp_dir: string)
    ~(default_options: string)
    (seed: int)
    (transformation_pipeline: TestCase.OcamlT.Transformation.t list)
  : string (* stdout *) * string (* stderr *) * Unix.process_status * string (* command_line *)
  =
  let tigress_args =
    gen_tigress_args
      output_filename
      (*	obf_c_filename *)
      {placeholder = 0}
      default_options
      (*	(Defaults.program_default_options_str defaults) *)
      seed
      transformation_pipeline
      source_file_path
  in
  let starting_directory = Unix.getcwd () in
  Fun.protect (* ensure that we change back to the starting directory *)
    ~finally:(fun () -> Unix.chdir starting_directory) 
    begin fun () ->
      Unix.chdir temp_dir;
      Logs.info (fun m -> m "Running Tigress");
      let tigress_command_line = tigress_file_path ^ " " ^ tigress_args in
      Logs.info (fun m -> m "Tigress command line: %s" tigress_command_line); (* for debugging *)
    let stdout_chan, stdin_chan, stderr_chan =
      let tigress_args = 
          String.split_on_char ' ' tigress_args
      in
      Unix.open_process_args_full
        tigress_file_path
        (List.fold_left
          (fun acc x -> Array.append acc [|x|])
          [|Filename.basename tigress_file_path;
          |]
          tigress_args
        )
        (Unix.environment ())
    in
    let pid = Unix.process_full_pid (stdout_chan, stdin_chan, stderr_chan) in
 
    let timeout_seconds =
      match timeout_seconds with
      | None -> -1.0
      | Some x -> float_of_int x
    in
    let output_str, error_str, timed_out, _ = 
      Subprocess.do_subprocess_io
        stdin_chan
        stdout_chan
        stderr_chan
        ""
        timeout_seconds
    in
    (if timed_out then
       Unix.kill pid Sys.sigkill);
    let process_status = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan)
    in
    (output_str, error_str, process_status, tigress_command_line)
(*
      let tigress_standard_output_file_redirect = (Filename.concat temp_dir ("tigress_" ^ (Filename.basename output_filename) ^ "_standard_output")) in
      let tigress_standard_error_file_redirect = (Filename.concat temp_dir ("tigress_" ^ (Filename.basename output_filename) ^ "_standard_error")) in
      let process_status: Unix.process_status = 
        Unix.system (tigress_command_line ^ "> " ^ tigress_standard_output_file_redirect ^ " 2> " ^ tigress_standard_error_file_redirect)
      in
      (Common.read_all_from_file tigress_standard_output_file_redirect,
       Common.read_all_from_file tigress_standard_error_file_redirect,
       process_status
      )
*)
    end (* Fun.protect *)

module Halstead_result: sig
  type halstead_result =
    | Halstead_not_available
    | Halstead_different
    | Halstead_no_difference

  type t
  val create: output:string -> error:string -> string list -> Unix.process_status -> t
  val output: t -> string
  val error: t -> string
  val metric_lines: t -> string option list

  module Function_length: sig
    type t
    type difference
    val create: string -> int option -> t
    val compare: t -> t -> (int option * difference) option
    val function_name: t -> string
    val length: t -> int option
    val difference_function_name: difference -> string
    val difference_value: difference -> int option
    val greatest_difference:
      difference list ->
      int
  end

  val compare: obf:t -> orig:t -> halstead_result * Function_length.difference list
  val function_lengths: t -> Function_length.t list
  val process_status: t -> Unix.process_status

end = struct
  type halstead_result =
    | Halstead_not_available
    | Halstead_different
    | Halstead_no_difference

  module Function_length = struct
    type t = {
      function_name: string;
      length: int option;
    }

    type difference = {
      function_name: string;
      difference: int option;
    }

    let function_name (x: t): string = x.function_name
    let length (x: t): int option = x.length
    let difference_function_name (x: difference): string = x.function_name
    let difference_value (x: difference): int option = x.difference

    let create
        (function_name: string)
        (length: int option)
      : t
      = { function_name; length }

    let compare
        (a: t)
        (b: t)
      : (int option * difference) option
      =
      if a.function_name <> b.function_name then
        None
      else begin
        match a.length with
        | None -> None
        | Some a_length ->
          (match b.length with
           | None -> None
           | Some b_length ->
             let difference = a_length - b_length in
             Some (Some difference, {function_name = a.function_name; 
                                     difference = Some difference; })
          )
      end

    let greatest_difference
        (l: difference list)
      : int
      =
      List.fold_left
        (fun (acc: int) (x: difference): int ->
           match x with
           | { function_name = _; difference = None } -> acc
           | { function_name = _; difference = Some x } ->
             let x = Int.abs x in
             if x > acc then x else acc
        )
        0
        l

  end

  type t = {
    output: string;
    error: string;
    metric_lines: string option list;
    function_lengths: Function_length.t list;
    process_status: Unix.process_status;
  }

  let output (x: t): string = x.output
  let error (x: t): string = x.error
  let metric_lines (x: t): string option list = x.metric_lines
  let function_lengths (x: t): Function_length.t list = x.function_lengths
  let process_status (x: t): Unix.process_status = x.process_status

  let create
      ~(output: string)
      ~(error: string)
      (*    (metric_liines: string list)
            (function_lengths: function_length list)
      *)
      (function_names: string list)
      (process_status: Unix.process_status)
    : t = begin
    let fun_lengths: (string * string option * int option) list =
      List.map
        (fun x ->
           let function_regex = "^halstead\\." ^ x ^ "\\.length = ([0-9]*)" in
           let re = Re.compile (Re.Posix.re ~opts:[`Newline] function_regex) in
           let (hal_line, hal_len): string option * int option = 
             match Re.exec_opt re output with
             | None -> (None, None)
             | Some group -> 
               let hal_line =
                 Re.Group.get_opt group 0
               in
               (match hal_line with
                | None -> Logs.debug (fun m -> m "No match for function, %s, in software metrics output." x )
                | Some s -> Logs.debug (fun m -> m "Match for function, %s, in software metrics output: %s" x s)
               );
               let hal_len =
                 Re.Group.get_opt group 1
               in
               let hal_len =
                 (match hal_len with
                  | None -> None
                  | Some s -> Some (int_of_string s)
                 )
               in
               (hal_line, hal_len)
           in
           (x, hal_line, hal_len)
        )
        function_names
    in
    let metric_lines =
      List.map
        (fun (_, line, _) -> line)
        fun_lengths
    in
    let function_lengths =
      List.map
        (fun (function_name, _, length) -> 
           Function_length.create function_name length
        )
        fun_lengths
    in
    {  output;
       error;
       metric_lines;
       function_lengths;
       process_status;
    }
  end


  let compare ~(obf: t) ~(orig: t): halstead_result * Function_length.difference list =
    let fun_len_diffs =  
      let orig_fun_lengths = 
        List.map
          (fun (x: Function_length.t): int option -> x.length)
          orig.function_lengths
      in
      let obf_fun_lengths =
        List.map
          (fun (x: Function_length.t): int option -> x.length)
          obf.function_lengths
      in
      List.map2
        (fun a b -> 
           match a with
           | None -> None
           | Some a ->
             (match b with
              | None -> None
              | Some b ->
                Logs.debug (fun m -> m "Halstead length of an original function = %d and length of corresponding obfuscated function = %d." a b);
                Some (a - b)
             )
        )
        orig_fun_lengths
        obf_fun_lengths      
    in
    let halstead_result =
      List.fold_left
        (* false if any of the lengths are different *)
        (fun acc diff: bool ->
           match diff with
           | None -> acc
           | Some diff ->
             Logs.debug (fun m -> m "Intermediate Halstead result = (diff = 0) && acc = %B; (diff = 0) = %B; diff = %d; acc = %B." ((diff = 0) && acc) (diff = 0) diff acc);
             (diff = 0) && acc
        )
        true
        fun_len_diffs 
    in
    let fun_len_diffs =
      List.map2
        (fun (functions: Function_length.t) (difference: int option): 
          Function_length.difference ->
          { function_name = functions.function_name; difference }
        )
        obf.function_lengths
        fun_len_diffs
    in
    let halstead_result = 
      if halstead_result then Halstead_no_difference else Halstead_different
    in
    (halstead_result, fun_len_diffs)
end

let run_tigress_for_halstead_metrics
    ?(timeout_seconds: int option = None)
    ~(tigress_file_path: string)
    ~(output_filename: string)
    ~(default_options: string)
    ~(source_file_path: string)
    ~(temp_dir: string)
    (functions_option: TestCase.OcamlT.TransformationOption.t)
    (functions: string list)
  : Halstead_result.t
  = begin
    let halstead_transformation: TestCase.OcamlT.Transformation.t =
      { name = "SoftwareMetrics";
        options = 
          { name = "SoftwareMetricsKind"; 
            value = Some "halstead";
          } :: [ functions_option ];
      }
    in
    let tigress_output, tigress_error, process_status, _ =
      run_tigress
        ~timeout_seconds
        ~tigress_file_path
        ~output_filename
        ~default_options
        ~source_file_path
        ~temp_dir
        0 (* arbitrary value *)
        [halstead_transformation] 
    in
    Halstead_result.create
      ~output: tigress_output
      ~error: tigress_error
      functions
      process_status
  end

module Program_results : sig
  type t
  val output: t -> string
  val error: t -> string
  val process_status: t -> Unix.process_status
  val timed_out: t -> bool
  val elapsed_time: t -> float
  val test_results: 
    obf:t -> 
    orig:t -> 
    tigress_output: string ->
    tigress_error: string ->
    tigress_command_line: string ->
    halstead_before: string ->
    halstead_after: string ->
    halstead_difference: int ->
    halstead_difference_delta_percent: TestResults_t.delta_percent ->
    previous_result: TestResults_t.result_t option ->
    TestResults.t
  val create: 
    output: string -> 
    error: string -> 
    compiler_output: string ->
    compiler_error: string ->
    compiler_command_line: string ->
    Unix.process_status -> 
    bool -> 
    float -> 
    TestCase.OcamlT.t ->
    t

end = struct
  type t = {
    output: string;
    error: string;
    compiler_output: string;
    compiler_error: string;
    compiler_command_line: string;
    process_status: Unix.process_status;
    timed_out: bool;
    elapsed_time: float;
    test: TestCase.OcamlT.t;
  }
  let create
      ~(output: string)
      ~(error: string)
      ~(compiler_output: string)
      ~(compiler_error: string)
      ~(compiler_command_line: string)
      (process_status: Unix.process_status)
      (timed_out: bool)
      (elapsed_time: float)
      (test: TestCase.OcamlT.t)
    : t
    = {
      output;
      error;
      compiler_output;
      compiler_error;
      compiler_command_line;
      process_status;
      timed_out;
      elapsed_time;
      test;
    }

  let output (x: t): string = x.output
  let error (x: t): string = x.error
  let process_status (x: t): Unix.process_status = x.process_status
  let timed_out (x: t): bool = x.timed_out
  let elapsed_time (x: t): float = x.elapsed_time

  let test_results 
      ~(obf: t) 
      ~(orig: t)
      ~(tigress_output: string)
      ~(tigress_error: string)
      ~(tigress_command_line: string)
      ~(halstead_before: string)
      ~(halstead_after: string)
      ~(halstead_difference: int)
      ~(halstead_difference_delta_percent: TestResults_t.delta_percent)
      ~(previous_result: TestResults_t.result_t option)
    : TestResults.t =
    let timed_out =
      obf.timed_out || orig.timed_out 
    in
    let run_status: TestResults.run_status =
      if timed_out then
        TimedOut
      else
        Completed
    in
    Logs.info (fun m -> m "Comparing outputs from stdin and stderr, and process status (exit codes and whether or not signalled) between the run of the original and that of the obfuscated binaries.");
    Logs.debug (fun m -> m "timed_out=%s" (string_of_bool timed_out));
    Logs.debug (fun m -> m "obf_out=orig_out?:%s" (string_of_bool (obf.output = orig.output)));
    Logs.debug (fun m -> m "obf_out=:%s" obf.output);
    Logs.debug (fun m -> m "orig_out=:%s" orig.output);
    Logs.debug (fun m -> m "obf_err=orig_err?:%s" (string_of_bool (obf.error = orig.error)));
    Logs.debug (fun m -> m "obf_err=:%s" obf.error);
    Logs.debug (fun m -> m "orig_err=:%s" orig.error);
    let elapsed_time_delta_percent: TestResults_t.delta_percent =
      (match previous_result with
       | None -> `MISSING_VALUE
       | Some x ->
         (match x.elapsed_time with
          | None -> `MISSING_VALUE
          | Some elapsed_time ->
            try `DELTA_PERCENT ((obf.elapsed_time -. elapsed_time) /. elapsed_time *. 100.) with
            | Division_by_zero -> `DIVIDE_BY_ZERO
         )
      )
    in
    let rslt = 
      if (not timed_out) &&
         obf.output = orig.output &&
         obf.error = orig.error && 
         obf.process_status = orig.process_status then
        (* TODO: indicate which of the 3 was the
           	    source of the failure if there is not
           	    a match *)
        TestResults.create 
          run_status
          PASS
          ~elapsed_time_secs:obf.elapsed_time
          ~elapsed_time_orig_secs:orig.elapsed_time
          ~elapsed_time_delta_percent
          ~tigress_output
          ~tigress_error
          ~tigress_command_line
          ~compiler_output:obf.compiler_output
          ~compiler_error: obf.compiler_error
          ~compiler_output_orig: orig.compiler_output
          ~compiler_error_orig: orig.compiler_error
          ~compiler_command_line_for_obf: obf.compiler_command_line
          ~compiler_command_line_for_orig: orig.compiler_command_line
          ~halstead_before
          ~halstead_after
          ~halstead_difference
          ~halstead_difference_delta_percent
          ~previous_result
          obf.test
      else
        TestResults.create 
          run_status
          FAIL
          ~elapsed_time_secs:obf.elapsed_time
          ~elapsed_time_orig_secs:orig.elapsed_time
          ~elapsed_time_delta_percent
          ~tigress_output
          ~tigress_error
          ~tigress_command_line
          ~compiler_output:obf.compiler_output
          ~compiler_error: obf.compiler_error
          ~compiler_output_orig: orig.compiler_output
          ~compiler_error_orig: orig.compiler_error
          ~compiler_command_line_for_obf: obf.compiler_command_line
          ~compiler_command_line_for_orig: orig.compiler_command_line
          ~halstead_before
          ~halstead_after
          ~halstead_difference
          ~halstead_difference_delta_percent
          ~previous_result
          obf.test
    in
    rslt
end

let run_program
    ~(program_filename: string)
    ~(program_input: string)
    ~(compiler_output: string)
    ~(compiler_error: string)
    ~(compiler_command_line: string)
    (program_args: string list)
    (max_run_seconds: int option)
    (test: TestCase.OcamlT.t)
  : Program_results.t
  = begin
    let (stdout_chan, stdin_chan, stderr_chan) = Unix.open_process_args_full program_filename (Array.of_list ( program_filename :: program_args )) (Unix.environment ()) in
    let pid = Unix.process_full_pid (stdout_chan, stdin_chan, stderr_chan) in
    let timeout = match max_run_seconds with
      | None -> (-1.0)
      | Some x -> float_of_int x in
    let (out_str, err_str, timed_out, elapsed_time) = Subprocess.do_subprocess_io stdin_chan stdout_chan stderr_chan program_input timeout in
    (if timed_out then
       Unix.kill pid Sys.sigkill);
    let exit_code = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan)
    in
    Program_results.create
      ~output: out_str
      ~error: err_str
      ~compiler_output
      ~compiler_error
      ~compiler_command_line
      exit_code
      timed_out
      elapsed_time 
      test
  end

let process_success (x: Unix.process_status): bool = begin
  match x with
  | WSIGNALED n -> 
    if n = Sys.sigint then 
      exit 100
    else false
  | WEXITED n ->
    if n = 0 then
      true
    else
      false
  | WSTOPPED _ -> false
end

let run_compiler
    ?(timeout_seconds: int option = None)
    ~(path_to_c_compiler: string)
    ~(output_filename: string)
    ~(compiler_options: string)
    ~(source_file_path: string)
    ()
(*    ~(temp_dir: string) *)
  : Unix.process_status * string (* stdout *) * string (* stderr *) * string (* command_line *)
  = begin
    let compiler_options = String.trim compiler_options in
    (* TODO: make sure output flag (e.g. "-o" is portable across platforms *)
    let cc_command_line = 
      Printf.sprintf "%s -o %s %s %s"
        path_to_c_compiler 
        output_filename 
        source_file_path
        compiler_options
    in
(*
    let cc_standard_output_file_redirect = (Filename.concat temp_dir ("cc_" ^ (Filename.basename output_filename) ^ "_standard_output")) in
    let cc_standard_error_file_redirect = (Filename.concat temp_dir ("cc_" ^ (Filename.basename output_filename) ^ "_standard_error")) in
*)
    Logs.info (fun m -> m "Compiler command line: %s" cc_command_line);
(*    let process_status: Unix.process_status =
      Unix.system (cc_command_line ^ "> " ^ cc_standard_output_file_redirect ^ " 2> " ^ cc_standard_error_file_redirect) 
    in
    let output = Common.read_all_from_file cc_standard_output_file_redirect
    in
    let error = Common.read_all_from_file cc_standard_error_file_redirect
    in
    (process_status, output, error)
  *)
    let stdout_chan, stdin_chan, stderr_chan =
      Unix.open_process_args_full
        path_to_c_compiler
        (List.fold_left
          (fun acc x -> Array.append acc [|x|])
          [|Filename.basename path_to_c_compiler;
            "-o";
            output_filename;
            source_file_path
          |]
          (match String.index_opt compiler_options ' ' with
          | None -> 
            if String.length compiler_options > 0 then
              [compiler_options]
            else
              []
          | Some _ ->
            String.split_on_char ' ' compiler_options
          )
        )
        (Unix.environment ())
    in
    let pid = Unix.process_full_pid (stdout_chan, stdin_chan, stderr_chan) in
 
    let timeout_seconds =
      match timeout_seconds with
      | None -> -1.0
      | Some x -> float_of_int x
    in
    let output_str, error_str, timed_out, _ = 
      Subprocess.do_subprocess_io
        stdin_chan
        stdout_chan
        stderr_chan
        ""
        timeout_seconds
    in
    (if timed_out then
       Unix.kill pid Sys.sigkill);
    let process_status = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan)
    in
    (process_status, output_str, error_str, cc_command_line)
  end

(* For each test case in the list, run the test:
     run tigress to do the obfuscation;
     run the compiler to compile the original and obfuscated C programs;
     run the original and obfuscated compiled programs with the same
       parameters and inputs;
     compare the outputs byte-for-byte.
   If the outputs are not the same, the test FAILs otherwise it succeeds.
   Output all of the information needed to reproduce the test to the
     results.json file, together with "PASS" or "FAIL" status.
   Output "FAIL" statuses and/or an indication of the testing progress
     to the standard out.
   Done. *)

let do_test 
    (defaults: Defaults.config_base)
    (testresult_file: TestResults.File.t)
    (temp_dir: string)
    (test: TestCase.OcamlT.t)
    (previous_result: TestResults_t.result_t option)
  : TestResults.t option 
  = 
  Logs.info (fun m -> m "test number: %d" test.test_number);
  (match previous_result with
   | None -> Logs.info (fun m -> m "no previous result found")
   | Some previous_result ->
     Logs.info 
       (fun m -> 
          m "previous result:\n %s" (TestResults_j.string_of_result_t previous_result)
       )
  );
  let rslt =
    let path_to_c_compiler = test.compiler.filename in
    let obf_c_filename = Filename.concat temp_dir "out.c" in
    (* apply Halstead measure to check lengths of functions before
       and after obfuscations in order to determine whether the
       transformation did anything *)
    let functions_option: TestCase.OcamlT.TransformationOption.t option =
      match test.transformations with
      | [] -> None
      (* For now, I'll assume that the first transformation in the pipeline
         	 contains a Function option if there is one. TODO: try the other
         	 transformations. *)
      | first_transformation::_ ->
        List.find_opt
          (fun (x: TestCase.OcamlT.TransformationOption.t) -> x.name = "Functions")
          first_transformation.options
    in
    let function_names =
      match functions_option with
      | None ->
        Logs.warn (fun m -> m "No \"Functions\" option could be found. Unable to apply Halstead measure.");
        None
      | Some functions_option ->
        (match functions_option.value with
         |None ->
           Logs.warn (fun m -> m "No values for \"Functions\" option! Unable to apply Halstead measure.");
           None
         | Some functions_str ->
           let function_names_list: string list =
             String.split_on_char ',' functions_str
           in
           Some function_names_list
        )
    in
    let tigress_file_path = 
      match defaults#path_to_program with
      | None -> begin
        let filename = test.tigress.filename in
        match defaults#tigress_home with
        | None -> filename
        | Some home_dir ->
          Defaults.prefix_relative_paths_with_home_dir 
            ~home_dir 
            filename
      end
      | Some x -> x
    in
    let source_file_name =
      let filename = test.source_file.filename in
      match defaults#tigress_test_home with
      | None -> filename
      | Some home_dir ->
        Defaults.prefix_relative_paths_with_home_dir 
          ~home_dir 
          filename
    in
    let halstead_before: Halstead_result.t option = 
      match function_names with
      | None -> None
      | Some functions ->
        (match functions_option with
         | None -> None
         | Some function_options ->
           Some (
             run_tigress_for_halstead_metrics
               ~timeout_seconds: defaults#program_timeout_seconds
               ~tigress_file_path
               ~output_filename: "halstead_before"
               ~default_options: (match defaults#program_default_options with None -> "" | Some x -> x)
               ~source_file_path: source_file_name
               ~temp_dir
               function_options
               functions
           )
        )
    in
    let tigress_output, tigress_error, tigress_process_status, tigress_command_line =
      run_tigress
        ~timeout_seconds: defaults#program_timeout_seconds
        ~tigress_file_path
        ~output_filename: obf_c_filename
        ~default_options: (match defaults#program_default_options with None -> "" | Some x -> x)
        ~source_file_path: source_file_name
        ~temp_dir
        test.seed
        test.transformations
    in
    if not (process_success tigress_process_status) then (
      Logs.info (fun m -> m "Error running Tigress."); 
      TestResults.create
        TigressError
        FAIL
        ~elapsed_time_secs:0.
        ~elapsed_time_orig_secs:0.
        ~elapsed_time_delta_percent: `MISSING_VALUE
        ~tigress_output
        ~tigress_error
        ~tigress_command_line
        ~compiler_output: ""
        ~compiler_error: ""
        ~compiler_output_orig: ""
        ~compiler_error_orig: ""
        ~compiler_command_line_for_obf: ""
        ~compiler_command_line_for_orig: ""
        ~halstead_before: (match halstead_before with None -> ""| Some halstead_before -> (String.concat "; " (List.filter_map (fun x -> x) (Halstead_result.metric_lines halstead_before) )))
        ~halstead_after: ""
        ~halstead_difference: 0
        ~halstead_difference_delta_percent: `MISSING_VALUE
        ~previous_result
        test
    )
    else begin
      if (Str.string_match (Str.regexp "ERR-NOT-POSSIBLE") tigress_error 0)
        || (Str.string_match (Str.regexp "ERR-NOT-IMPLEMENTED") tigress_output 0) then begin
        Logs.info (fun m -> m "Transformation not implemented or not possible."); 
        TestResults.create
          TigressError
          PASS
          ~elapsed_time_secs:0.
          ~elapsed_time_orig_secs:0.
          ~elapsed_time_delta_percent: `MISSING_VALUE
          ~tigress_output
          ~tigress_error
          ~tigress_command_line
          ~compiler_output: ""
          ~compiler_error: ""
          ~compiler_output_orig: ""
          ~compiler_error_orig: ""
          ~compiler_command_line_for_obf: ""
          ~compiler_command_line_for_orig: ""
          ~halstead_before: (match halstead_before with None -> ""| Some halstead_before -> (String.concat "; " (List.filter_map (fun x -> x) (Halstead_result.metric_lines halstead_before) )))
          ~halstead_after: ""
          ~halstead_difference: 0
          ~halstead_difference_delta_percent: `MISSING_VALUE
          ~previous_result
          test
      end
      else begin
      let halstead_after: Halstead_result.t option = 
        match function_names with
        | None -> None
        | Some functions ->
          (match functions_option with
           | None -> None
           | Some function_options ->
             Some (
               run_tigress_for_halstead_metrics
                 ~timeout_seconds: defaults#program_timeout_seconds
                 ~tigress_file_path
                 ~output_filename: "halstead_after"
                 ~default_options: (match defaults#program_default_options with None -> "" | Some x -> x)
                 ~source_file_path: obf_c_filename
                 ~temp_dir
                 function_options
                 functions
             )
          )
      in

      Logs.info (fun m -> m "Running C compiler to compile obfuscated C source file.");
      let obf_exe_filename = Filename.concat temp_dir "obf" in
      let cc_obf_process_status, cc_obf_output, cc_obf_error, cc_obf_command_line = 
        run_compiler
          ~timeout_seconds: defaults#compiler_timeout_seconds
          ~path_to_c_compiler
          ~output_filename: obf_exe_filename
          ~compiler_options: test.compiler_options
          ~source_file_path: obf_c_filename
          ()
      in
      let halstead_difference: int = 
        match halstead_before with
        | None -> 0
        | Some halstead_before ->
          (match halstead_after with
           | None -> 0
           | Some halstead_after ->
             let (_, difference) =
               Halstead_result.compare
                 ~obf: halstead_after
                 ~orig: halstead_before
             in
             Halstead_result.Function_length.greatest_difference 
               difference
          )
      in
      let halstead_difference_delta_percent: TestResults_t.delta_percent =
        (match previous_result with
         | None -> `MISSING_VALUE
         | Some x ->
           (match x.halstead_function_length_greatest_difference with
            | None -> `MISSING_VALUE
            | Some previous_halstead_difference ->
              try `DELTA_PERCENT ((float_of_int (halstead_difference - previous_halstead_difference)) /. (float_of_int previous_halstead_difference) *. 100.) with
              | Division_by_zero -> `DIVIDE_BY_ZERO
           )
        )
      in
      let halstead_before = (match halstead_before with None -> ""| Some halstead_before -> (String.concat "; " (List.filter_map (fun x -> x) (Halstead_result.metric_lines halstead_before) ))) in
      let halstead_after = (match halstead_after with None -> ""| Some halstead_after -> (String.concat "; " (List.filter_map (fun x -> x) (Halstead_result.metric_lines halstead_after) ))) in
      if not (process_success cc_obf_process_status) then (
        Logs.info (fun m -> m "Error compiling obfuscated C source file."); 
        TestResults.create
          CompilerError
          FAIL
          ~elapsed_time_secs:0.
          ~elapsed_time_orig_secs:0.
          ~elapsed_time_delta_percent: `MISSING_VALUE
          ~tigress_output
          ~tigress_error
          ~tigress_command_line
          ~compiler_output: cc_obf_output
          ~compiler_error: cc_obf_error
          ~compiler_output_orig: ""
          ~compiler_error_orig: ""
          ~compiler_command_line_for_obf: cc_obf_command_line
          ~compiler_command_line_for_orig: ""
          ~halstead_before
          ~halstead_after
          ~halstead_difference
          ~halstead_difference_delta_percent
          ~previous_result
          test
      )
      else begin
        Logs.info (fun m -> m "Running C compiler to compile original C source file.");
        let orig_exe_filename = Filename.concat temp_dir "orig" in
        let cc_orig_process_status, cc_orig_output, cc_orig_error, cc_orig_command_line =
          run_compiler
            ~timeout_seconds: defaults#compiler_timeout_seconds
            ~path_to_c_compiler
            ~output_filename: orig_exe_filename
            ~compiler_options: test.compiler_options
            ~source_file_path: source_file_name
            ()
        in
        if not (process_success cc_orig_process_status) then (
          Logs.info (fun m -> m "Error compiling original C source file."); 
          TestResults.create
            CompilerError
            FAIL
            ~elapsed_time_secs:0.
            ~elapsed_time_orig_secs:0.
            ~elapsed_time_delta_percent: `MISSING_VALUE
            ~tigress_output
            ~tigress_error
            ~tigress_command_line
            ~compiler_output: cc_obf_output
            ~compiler_error: cc_obf_error
            ~compiler_output_orig: cc_orig_output
            ~compiler_error_orig: cc_orig_error
            ~compiler_command_line_for_obf: cc_obf_command_line
            ~compiler_command_line_for_orig: cc_orig_command_line
            ~halstead_before
            ~halstead_after
            ~halstead_difference 
            ~halstead_difference_delta_percent
            ~previous_result
            test
        )
        else begin

          let rslt =

            Logs.info (fun m -> m "Running binary executable produced from obfuscated C source file.");
            let obf_run_result =
              run_program
                ~program_filename: obf_exe_filename
                ~program_input: test.source_file.input
                ~compiler_output: cc_obf_output
                ~compiler_error: cc_obf_error
                ~compiler_command_line: cc_obf_command_line
                test.source_file.args
                test.max_run_seconds
                test
            in
            Logs.info (fun m -> m "Running binary executable produced from original C source file.");
            let orig_run_result =
              run_program
                ~program_filename: orig_exe_filename
                ~program_input: test.source_file.input
                ~compiler_output: cc_orig_output
                ~compiler_error: cc_orig_error
                ~compiler_command_line: cc_orig_command_line
                test.source_file.args
                test.max_run_seconds
                test
            in
            Program_results.test_results
              ~obf: obf_run_result
              ~orig: orig_run_result
              ~tigress_output
              ~tigress_error
              ~tigress_command_line
              ~halstead_before
              ~halstead_after
              ~halstead_difference 
              ~halstead_difference_delta_percent
              ~previous_result
          in
          rslt
        end
        end
      end
    end
  in
  (* print_endline (TestResults.to_result_string rslt); *)
  (* This currently doesn't do anything with the
     		   TestResults.File.state_t object that is
     		   returned.
     		   The state object currently tracks the count of
     		   results written to disk so far, if it is passed
     		   in to TestResults.write_result each time.
     		   That seems cumbersome.  I would have to pass
     		   that state into do_test each time.
     		   Since I'm not doing that, the result number in
     		   the output file will always be "1" for every
     		   result entry.  (But the test number from the
     		   test cases file is still recorded.)
     		   TODO: Get this working.  I'm thinking of
     		    using mutable state for this.
     		*)
  let write_result_exn_partial =
    (TestResults.write_result_exn testresult_file)
  in
  ignore (
    KeyboardInterrupt.critical_section_do
      write_result_exn_partial
      rslt
  );
  Some rslt

let parse_test_cases_until_error_helper (cases_list: TestCase.t list): (TestCase.OcamlT.t ReverseList.t, TestCase.content_error) result =
  Common.find_fold_left
    (fun acc x ->
       match acc with
       | Error x -> (Error x, true)
       | Ok l -> 
         (match TestCase.to_ocaml_t x with
          | Ok x -> ( Ok (ReverseList.append l x), false )
          | Error x -> ( Error x, true)
         )
    )
    (Ok (ReverseList.of_list []), false)
    cases_list  

let parse_test_cases_until_error (cases_list: TestCase.t list): (TestCase.OcamlT.t list, TestCase.content_error) result =
  match parse_test_cases_until_error_helper cases_list with
  | Ok x -> Ok (ReverseList.to_list x)
  | Error x -> Error x

let filenames_by_modification_time_descending
    ?(dir=Filename.current_dir_name)
    ?(pattern="*")
    ?(no_more_recent_than: float option = None)
    ()
  : string list
  = begin
    let _, results_files =
      let dir = Unix.opendir dir in
      Fun.protect
        ~finally:(fun () -> Unix.closedir dir)
        (fun () ->
           Common.filepattern_to_filenames
             dir
             pattern
        )
    in
    let results_files =
      List.map (Filename.concat dir) results_files
    in
    let results_files_by_modtime_descending =
      let time_filter_pred =
        match no_more_recent_than with
        | Some no_more_recent_than -> (fun (_, mtime) -> no_more_recent_than > mtime)
        | None -> (fun _ -> true)
      in
      List.rev
        (List.filter (time_filter_pred)
           (List.sort (fun (_, mtime1) (_, mtime2) -> compare mtime1 mtime2)
              (List.map (fun x -> (x, (Unix.stat x).st_mtime)) results_files) 
           )
        )
    in
    List.map
      (fun (fn, _) -> fn)
      results_files_by_modtime_descending
  end

let rec results_from_previous_run_helper
    ?(testcases_filename:string option=None)
    ?(exclusive_test_numbers:int NonemptyList.t option=None)
    ?(no_later_than: float option=None)
    ~(results_filenames: string list)
    ~(acc: TestResults_t.result_with_previous_result_t list)
    (checksum: Sha256.t option)
  : TestResults_t.result_with_previous_result_t list
  = begin
    match results_filenames with
    | [] -> acc
    | hd :: tl ->
      (*        Logs.info (fun m -> m "Reading previous results from results file."); *)
      Logs.debug (fun m -> m "Checking file, %s, for previous results" hd);
      let result_doc : TestResults_t.result_doc_t option = try Some (TestResults.read_results_from_file hd) with
        | (_: exn) -> 
          Logs.debug (fun m -> m "Exception reading results file, %s" hd);
          None
      in
      (match result_doc with
       | None -> 
         results_from_previous_run_helper
           ~testcases_filename ~exclusive_test_numbers ~results_filenames:tl ~acc checksum
       | Some result_doc ->
         let creation_time_matches: bool =
           match no_later_than with
           | None -> true
           | Some latest_time ->
             if result_doc.creation_time > latest_time then
               false
             else
               true
         in
         let filename_matches: bool =
           match testcases_filename with
           | None -> true
           | Some testcases_filename ->
             Logs.debug (fun m -> m "In results_from_previous_run_helper: testcases_filename = %s" testcases_filename);
             Logs.debug (fun m -> m "Test cases filename in results file is %s" result_doc.test_cases_file.filename);
             if result_doc.test_cases_file.filename = testcases_filename then
               true
             else
               false
         in 
         let file_matches: bool =
           if creation_time_matches then
             begin
               match checksum with
               | None -> filename_matches
               | Some checksum ->
                 let checksum = Sha256.to_hex checksum in
                 Logs.debug (fun m -> m "In results_from_previous_run_helper: checksum = %s" checksum);
                 Logs.debug (fun m -> m "Checksum in results file is %s" result_doc.test_cases_file.checksum);
                 if result_doc.test_cases_file.checksum = checksum then
                   true
                 else
                   false
             end
           else
             false
         in
         if not file_matches then
           results_from_previous_run_helper
             ~testcases_filename ~exclusive_test_numbers ~results_filenames:tl ~acc checksum
         else begin

           let test_numbers: int list option =
             match exclusive_test_numbers with
             | None -> (* try to determine the test numbers from the test cases file *)      
               (*prerr_endline (TestResults_j.string_of_result_doc_t result_doc);*)
               let test_cases_filename: string = 
                 result_doc.test_cases_file.filename 
               in
               (*        Logs.info (fun m -> m "Reading corresponding test cases file, containing the test cases which produced these results."); *)
               let testcases_doc = TestCase.from_json_file test_cases_filename in
               (match testcases_doc with
                | Error _ ->
                  Logs.err (fun m -> m "Parse error when reading test cases file: %s" test_cases_filename); 
                  None
                | Ok testcases_doc ->
                  Some (
                    List.map
                      (fun case -> TestCase.test_number case)
                      (TestCase.test_cases_exn testcases_doc)
                  )
               )
             | Some x -> Some (NonemptyList.list_of x)
           in
           (* get results for each of the test numbers;
              		if test_numbers is None, then get all of
              		the results in the file *)
           match test_numbers with
           | None ->
             List.fold_left
               (fun acc x: TestResults_t.result_with_previous_result_t ReverseList.t -> 
                  match x with
                  | None -> acc
                  | Some x -> ReverseList.append acc x
               )
               (ReverseList.of_list acc)
               result_doc.results
             |> ReverseList.to_list
           | Some test_numbers ->
             (* get the test results in the current file and
                get the test numbers of results missing from
                the current file *)
             let acc, missing, _ =
               let results =
                 List.filter_map
                   (fun x -> x)
                   result_doc.results
               in
               List.fold_left 
                 (fun (acc, missing, remaining) (x: TestResults_t.result_with_previous_result_t) ->
                    match remaining with
                    | [] -> (acc, missing, remaining)
                    | hd :: tl -> 
                      (* assume the test results appear in order of
                         test_number ascending and that the
                         test_numbers list is in that order as well *)
                      if x.test_case.tc_test_number < hd then
                        (acc, missing, remaining)
                      else if x.test_case.tc_test_number > hd then
                        (acc, ReverseList.append missing hd, tl)
                      else (* found a test number *)
                        (ReverseList.append acc x, missing, tl)
                 )
                 (ReverseList.of_list ([]:TestResults_t.result_with_previous_result_t list),
                  ReverseList.of_list ([]:int list),
                  test_numbers
                 )
                 results
             in
             (* if there are missing test_results, try a previous
                results file based on the same test cases file
                since some previous runs might only have
                been of some of the test cases *)
             let results_filenames = tl in
             let acc = ReverseList.to_list acc in
             let missing = ReverseList.to_list missing in
             (match missing with
              | [] -> acc
              | hd :: tl ->
                results_from_previous_run_helper
                  ~testcases_filename
                  ~exclusive_test_numbers: (Some (NonemptyList.create hd tl))   
                  ~results_filenames
                  ~acc
                  checksum
             )
         end
      )
  end

let results_from_previous_run
    ?(testcases_filename:string option=None)
    ?(filename_pattern:string=("results_testcases_*.json"))
    ?(no_later_than: float option = None)
    ?(exclusive_test_numbers:int NonemptyList.t option=None)
    ?(search_dir:string=Filename.current_dir_name)
    (checksum: Sha256.t option)
  : TestResults_t.result_with_previous_result_t list
  =
  let testcases_filename =
    match testcases_filename with
    | None -> None
    | Some x ->
      Some (
        x
        |> Filename.basename
        |> Filename.remove_extension
      )
  in
  Logs.debug (fun m -> m "In results_from_previous_run, filename_pattern is %s; search_dir is %s" filename_pattern search_dir);
  let results_filenames =
    filenames_by_modification_time_descending
      ~dir:search_dir
      ~pattern:filename_pattern
      ~no_more_recent_than:no_later_than
      ()
  in
  (List.iter
     (fun (x: string) -> 
        Logs.debug (fun m -> m "Results filename to check: %s" x)
     )
     results_filenames
  );
  results_from_previous_run_helper
    ~testcases_filename
    ~exclusive_test_numbers
    ~no_later_than
    ~results_filenames
    ~acc:[]
    checksum


let filename_of_results_to_rerun 
    (options: Options.Rerun.config)
    (output_dir: string) 
  :string option =
  match options#results_filename_opt with 
  | Some filename -> 
    Logs.info (fun m -> m "Name of results file to rerun or continue from given on command line: %s" filename);
    if Sys.file_exists filename then (
      Logs.info (fun m -> m "File found.");
      Some filename
    )
    else (
      Logs.info (fun m -> m "File not found.");
      None
    )
  |None -> (* search for most recent result file in current directory *)
    Logs.info (fun m -> m "Searching for the most recent results file in the current directory.");
    let results_files_by_modtime_descending =
      filenames_by_modification_time_descending
        ~dir:output_dir
        ~pattern:"results_*.json"
        ()
    in
    (
      match results_files_by_modtime_descending with
      | fn::_ -> 
        Logs.info (fun m -> m "Found recent results file: %s" fn);
        Some fn
      | [] -> 
        Logs.info (fun m -> m "No recent results file found.");
        None
    )

(** Use regexes to find other parts that correspond with one found results
 file.  For example, if the results filename is something like
  "results_testcases_..._part1_<timestamp>.json", then this function finds other
  files with the same name but with "..._part2....json", etc. instead of
  "...part1....json".  It returns the found filenames in a list.
*)
let filenames_of_other_results_parts
  ~(results_part_filename: string)
  ~(output_dir: string) 
  : string list
  = begin
  let results_filename_parts_pattern = 
    let re = Re.compile (Re.Posix.re "_part([0-9]*)_[0-9]*T.*[.]json") in 
    Re.replace_string
      re
      ~by:"*"
      results_part_filename
  in
  let _, results_files =
    let dir = Unix.opendir output_dir in
    let pattern = results_filename_parts_pattern in
    Fun.protect
      ~finally:(fun () -> Unix.closedir dir)
      (fun () ->
          Common.filepattern_to_filenames
            dir
            pattern
      )
  in
  List.fold_left
    (fun acc x -> 
      if x = results_part_filename then
        acc
      else
        x :: acc
    )
    []
    results_files
  end

let gen_test_cases_from_script_elements
  ?(test_order=None)
  ~(test_counter_start:int)
  ~(home_dir: string)
  compiler
  compiler_options
  tigress
  tigress_options
  (max_run_seconds: int option)
  (seeds: int NonemptyList.t)
  (source_files: TestScript_t.source_code_spec_t list)
  (transformations: EnrichedTestScript.TransformationPipelinesSet.t)
  = begin
    let module TestCaseComp: TestScript.TestCaseComparable = struct
      type t = TestScript.test_case_with_platforms_exclude
      let compare = (TestScript.compare_by_test_order
                       (match test_order with
                        | Some x -> x
                        | None -> kDEFAULT_TEST_ORDER
                       )
                    )
    end
    in
    let module TestCases = 
      TestScript.Make_TestCaseOfScript(TestCaseComp)
    in
    let test_cases =
      TestCases.gen_test_cases_from_script_helper
        ~test_counter_start
        ~home_dir
        compiler
        compiler_options
        tigress
        tigress_options
        max_run_seconds
        seeds
        source_files
        transformations
      |> TestCases.to_seq
    in
    test_cases
  end

let gen_test_cases_from_script
    config (*: script_with_config*)
  = begin
    let module TestCaseComp: TestScript.TestCaseComparable = struct
      type t = TestScript.test_case_with_platforms_exclude
      let compare = (TestScript.compare_by_test_order
                       (match config#test_order with
                        | Some x -> x
                        | None -> kDEFAULT_TEST_ORDER
                       )
                    )
    end
    in
    let module TestCases = 
      TestScript.Make_TestCaseOfScript(TestCaseComp)
    in
    let test_cases =
      TestCases.gen_test_cases_from_script
        config
      |> TestCases.to_seq
    in
    test_cases
  end
  
let rec calc_next_test_case_index
  ~(last_tpos: int)
  ~(last_opos: int)
  ~(last_vpos: int)
  ~(t_len: int)
  ~(step_remain:int)
  (test_case_gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec)
  : [`Index of int * int * int | `End ]
  = begin
    if last_tpos >= t_len then (* t overflow *)
      `End
    else begin
      let o_len = 
        Array.length test_case_gen_spec.transformations.(last_tpos).options 
      in
      if last_opos >= o_len then begin (* o overflow *)
        calc_next_test_case_index
          ~last_tpos:(last_tpos + 1)
          ~last_opos:0
          ~last_vpos:0
          ~t_len
          ~step_remain
          test_case_gen_spec
      end
      else begin
        let v_len =
          Array.length 
            test_case_gen_spec.transformations.(last_tpos).options.(last_opos).values
        in
        if last_vpos >= v_len then begin (* v overflow *)
          (* add back the amount by which we overshot the end of v *)
          let step_remain =
            step_remain + (last_vpos - (v_len - 1))
          in
          calc_next_test_case_index
            ~last_tpos
            ~last_opos:(last_opos + 1)
            ~last_vpos:0
            ~t_len
            ~step_remain
            test_case_gen_spec
        end
        else begin
          if step_remain > 0 then begin
          calc_next_test_case_index
            ~last_tpos
            ~last_opos
            ~last_vpos:(last_vpos + step_remain)
            ~t_len
            ~step_remain:0
            test_case_gen_spec
          end
          else begin
            `Index (last_tpos, last_opos, last_vpos)
          end
        end
      end
    end
  end

let rec calc_next_test_case_index_seq
  ~(last_tpos: int)
  ~(last_opos: int)
  ~(last_vpos: int)
  ~(run_remain: int)
  ~(t_len: int)
  (*~(parallel_processes_offset:int) *)
  ~(parallel_processes_run_length:int)
  (test_case_gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec)
  ()
  : (int * int * int) Seq.node
  = begin
  let rslt = calc_next_test_case_index
    ~last_tpos
    ~last_opos
    ~last_vpos
    ~t_len
    ~step_remain:0
    test_case_gen_spec
  in
    match rslt with
    | `End -> Seq.Nil
    | `Index (last_tpos, last_opos, last_vpos) ->
      let run_remain, next_v = 
        if run_remain <= 0 then begin
          (* skip the rest of the run begin processed by other parallel processes *)
          let next_v =
            let run_skip =
              (test_case_gen_spec.run_length - parallel_processes_run_length + 1) + test_case_gen_spec.step
            in
              last_vpos + run_skip
          in
            parallel_processes_run_length, next_v
        end
        else
          run_remain - 1, last_vpos + 1
      in
        Seq.Cons ( 
          (last_tpos, last_opos, last_vpos)
          ,calc_next_test_case_index_seq
            ~last_tpos
            ~last_opos
            ~last_vpos:next_v
            ~t_len
            ~run_remain
            (*~parallel_processes_offset*)
            ~parallel_processes_run_length
            test_case_gen_spec
        )
  end

(*
let test_cases_generator_transformation_to_test_case_ocamlt_transformation
  ~(tpos: int)
  ~(opos: int)
  ~(vpos: int)
  (test_case_gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec)
  : TestCase.OcamlT.Transformation.t
  = {
    name = test_case_gen_spec.transformations.(tpos).name;
    options = [{
      name = test_case_gen_spec.transformations.(tpos).options.(opos).name;
      value = test_case_gen_spec.transformations.(tpos).options.(opos).values.(vpos);
    }];
  }
*)

let test_cases_generator_transformation_to_enriched_test_script_transformation
  ~(tpos: int)
  ~(opos: int)
  ~(vpos: int)
  (test_case_gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec)
  : EnrichedTestScript.transformation_with_platforms
  = begin
  let options =
    let options: TestCaseGenerator.Option_t.transform_option = test_case_gen_spec.transformations.(tpos).options.(opos) in
    let {name=name;description=description;values=_;id=_}: TestCaseGenerator.Option_t.transform_option =
      options
    in
    let value = options.values.(vpos) in
    let value: EnrichedTestScript.Value.value_with_platforms option = 
      match value with
      | None -> None
      | Some value -> Some {
        value;
        platforms_exclude=EnrichedTestScript.PlatformSet.empty; (* dummy value *)
      }
    in
    EnrichedTestScript.OptionSet.singleton {
      name;
      description;
      value;
      ordering=None;
      platforms_exclude=EnrichedTestScript.PlatformSet.empty; (* dummy value *)
    }
  in
  let {
    id=_; 
    name=name; 
    description=description; 
    output_functions=output_functions;
    input_functions=input_functions;
    options=_;
  }: TestCaseGenerator.Doc_t.transformation = test_case_gen_spec.transformations.(tpos)
  in
  {
    name;
    description;
    output_functions;
    input_functions;
    options;
    ordering=None;
    platforms_exclude=EnrichedTestScript.PlatformSet.empty; (* dummy value *)
  }
end

let test_cases_transformation_seq
  (test_case_gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec)
  : EnrichedTestScript.transformation_with_platforms Seq.t
  = begin
  let parallel_processes_offset =
    match test_case_gen_spec.parallel_processes_offset with
    | None -> 0
    | Some x -> x
  in
  let parallel_processes_run_length =
    match test_case_gen_spec.parallel_processes_run_length with
    | None -> test_case_gen_spec.run_length
    | Some x -> x 
  in
  Seq.map
    (fun (tpos, opos, vpos) ->
      test_cases_generator_transformation_to_enriched_test_script_transformation
        ~tpos
        ~opos
        ~vpos
        test_case_gen_spec
    )  
    (calc_next_test_case_index_seq
      ~last_tpos:0
      ~last_opos:0
      ~last_vpos:(parallel_processes_offset+(test_case_gen_spec.offset*(test_case_gen_spec.run_length+test_case_gen_spec.step)))
      ~t_len:(Array.length test_case_gen_spec.transformations)
      ~run_remain:parallel_processes_run_length
      (*~parallel_processes_offset*)
      ~parallel_processes_run_length
      test_case_gen_spec
    )
  end

type test_case_seq_indexes = {
  transformation_i: EnrichedTestScript.transformation_with_platforms Seq.t;
  additional_pipeline: (EnrichedTestScript.transformation_with_platforms Seq.t) array;
  seed_i: int;
  source_code_i: int; 
  test_counter: int;
}

let rec test_cases_seq
  (*~(tigress_test_home: string)*)
  (first_indexes: test_case_seq_indexes)
  (last_indexes: test_case_seq_indexes)
  (test_case_gen_spec: TestCaseGenerator.Doc_t.contents)
  ()
  : TestCase.OcamlT.t Seq.node
  = begin
    let rec incr_additional_pipeline
      (*(previous_pipeline: EnrichedTestScript.transformation_with_platforms Seq.t list)*)
      (pipeline_i: int)
      (last_indexes: test_case_seq_indexes)
      : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
      = begin
        let additional_pipeline_length = Array.length last_indexes.additional_pipeline in
        if additional_pipeline_length <= 0 then
          `Indexes last_indexes
        else begin
          let incr
            (last_indexes: test_case_seq_indexes)
            = begin
            let transformations = last_indexes.additional_pipeline.(pipeline_i) in
            match transformations () with
            | Seq.Nil -> 
              Array.set 
                last_indexes.additional_pipeline
                pipeline_i 
                first_indexes.additional_pipeline.(pipeline_i);
              `Overflow last_indexes
            | Seq.Cons (_, next) ->
              Array.set
                last_indexes.additional_pipeline
                pipeline_i
                next;
              `Indexes {last_indexes with test_counter = last_indexes.test_counter + 1}
          end
          in
          if pipeline_i = (additional_pipeline_length - 1) then
            incr last_indexes
          else begin
            match
              incr_additional_pipeline
                (pipeline_i + 1)
                last_indexes
            with
            | `Overflow last_indexes ->
              incr last_indexes
            | x -> x
          end
        end
(*
        match last_indexes.additional_pipeline with
        | [] -> `Indexes {last_indexes with additional_pipeline = previous_pipeline}
        | hd :: tl -> begin
          match hd () with
          | Seq.Nil ->
            `Overflow {
              last_indexes with
              List.append previous_pipeline Lfirst_indexes.additional_pipeline
            }
          | Seq.Cons (_, next) ->
*)
      end
    in
    let incr_transformations
      (last_indexes: test_case_seq_indexes)
      : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
      = begin
          let incr
            (last_indexes: test_case_seq_indexes)
            = begin
            match last_indexes.transformation_i () with
            | Seq.Nil -> 
              `Overflow {
                last_indexes with
                transformation_i = first_indexes.transformation_i;
              }
            | Seq.Cons (_, next) -> 
              `Indexes {
                last_indexes with 
                transformation_i = next;
                test_counter = last_indexes.test_counter + 1
              }
            end
          in
            if Array.length last_indexes.additional_pipeline > 0 then begin
              match incr_additional_pipeline 0 last_indexes with
              | `Overflow last_indexes -> incr last_indexes
              | x -> x
            end
            else
              incr last_indexes
      end
    in
    let incr_seeds
      (last_indexes: test_case_seq_indexes)
      : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
      = begin
            let seed_i = last_indexes.seed_i + 1 in
            if seed_i >= Array.length test_case_gen_spec.seeds then
              `Overflow {
                last_indexes with
                seed_i = first_indexes.seed_i;
              }
            else
              `Indexes {
                last_indexes with
                seed_i;
                test_counter = last_indexes.test_counter + 1
              }
      end
    in
    let incr_source_files
      (last_indexes: test_case_seq_indexes)
      : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
      = begin
            let source_code_i = last_indexes.source_code_i + 1 in
            if source_code_i >= Array.length test_case_gen_spec.source_files then
              `Overflow {
                last_indexes with
                source_code_i = first_indexes.source_code_i;
              }
            else
              `Indexes {
                last_indexes with
                source_code_i;
                test_counter = last_indexes.test_counter + 1
              }
      end
    in
    let incr_next
      (*(last_indexes: test_case_seq_indexes)*)
      (incr: [< 
            `Transformation of [< `Seed | `Source_code ] 
          | `Seed of [< `Transformation | `Source_code ]
          | `Source_code of [< `Transformation | `Seed ]
          ])
      : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
      = begin
        let incr_next
          (incr: [<
                `Transformation
                | `Seed
                | `Source_code
                ])
          : [`Overflow of test_case_seq_indexes | `Indexes of test_case_seq_indexes]
          = begin
          match incr with
          | `Transformation -> begin
            incr_transformations last_indexes
          end
          | `Seed -> begin
            incr_seeds last_indexes
          end
          | `Source_code -> begin
            incr_source_files last_indexes
          end
        end (* inner incr_next function *)
        in
          match incr with
          | `Transformation next -> begin
            match incr_next next with
            | `Overflow last_indexes -> begin
              incr_transformations last_indexes
            end
            | x -> x
          end
          | `Seed next -> begin
            match incr_next next with
            | `Overflow last_indexes ->
              incr_seeds last_indexes
            | x -> x
          end
          | `Source_code next -> begin
            match incr_next next with
            | `Overflow last_indexes ->
              incr_source_files last_indexes
            | x -> x
          end
      end (* outer incr_next function *)
      in
      let rslt =
        match test_case_gen_spec.ordering with
        | `Transformation next -> begin
          match incr_next next with
          | `Overflow last_indexes ->
            incr_transformations last_indexes
          | x -> x
        end
        | `Seed next -> begin
          match incr_next next with
          | `Overflow last_indexes ->
            incr_seeds last_indexes
          | x -> x
        end
        | `Source_code next -> begin
          match incr_next next with
          | `Overflow last_indexes ->
            incr_source_files last_indexes
          | x -> x
        end
      in
        match rslt with
        | `Overflow _ -> Seq.Nil
        | `Indexes current_indexes -> begin
          match current_indexes.transformation_i () with
          | Seq.Nil -> Seq.Nil
          | Seq.Cons (current_transformation, _) -> begin
          let transformation_pipeline = 
            Array.fold_left
              (fun acc x ->
                match x () with
                | Seq.Nil -> acc
                | Seq.Cons (current_transformation, _) ->
                  ReverseList.append
                    acc
                    current_transformation
              )
              (ReverseList.of_list [current_transformation])
              current_indexes.additional_pipeline
            |> ReverseList.to_list
          in
          let transformation_pipeline: EnrichedTestScript.transformation_pipeline_with_platforms = {
            pipeline = transformation_pipeline;
          }
          in
          let transformation_pipeline =
            EnrichedTestScript.TransformationPipelinesSet.singleton
              transformation_pipeline
          in
          let test_case = 
            gen_test_cases_from_script_elements
              ~test_counter_start:(current_indexes.test_counter)
              ~home_dir:""
              test_case_gen_spec.compiler
              test_case_gen_spec.compiler_options
              test_case_gen_spec.tigress
              test_case_gen_spec.tigress_options
              test_case_gen_spec.max_run_seconds
              (NonemptyList.create 
                test_case_gen_spec.seeds.(current_indexes.seed_i)
                []
              )
              [test_case_gen_spec.source_files.(current_indexes.source_code_i)]
              transformation_pipeline
          in
            match test_case () with
            | Seq.Nil -> Seq.Nil
            | Seq.Cons (test_case, _) -> begin
              let {test_case=test_case; platforms_exclude=_}: TestScript.test_case_with_platforms_exclude = test_case in
                Seq.Cons (
                  TestCase.OcamlT.of_test_cases_t
                    test_case
                  ,test_cases_seq
                    first_indexes
                    current_indexes
                    test_case_gen_spec
              ) 
            end
        end
      end
  end


let pipeline_length_to_positive_integer
  (pipeline_length: TestScriptv1_1.Doc_t.pipeline_length option)
  : [`One | `GreaterThanOne of int]
  = begin
          match pipeline_length with
          | None 
          | Some `Number 0 
          | Some `Number 1 -> `One
          | Some `Pairs -> `GreaterThanOne 2
          | Some `Triples -> `GreaterThanOne 3
          | Some `Number x -> `GreaterThanOne x
  end

let generate_test_cases_sequences
  (*~(tigress_test_home: string)*)
  ?(gen_spec_index: int option = None)
  (testcases: TestCaseGenerator.Doc_t.contents)
  : (TestCase.OcamlT.t Seq.t) list
  = begin
    let gen_spec_list =
      match gen_spec_index with
      | None -> testcases.gen_spec
      | Some i -> begin
        let list_of_ith_element =
          try [List.nth testcases.gen_spec i] with
          | _ -> []
        in
          list_of_ith_element
      end
    in
    List.map
      (fun gen_spec ->
      let transformation_seq =
        test_cases_transformation_seq
          gen_spec
      in
      let additional_pipeline () =
        let pipeline_length = testcases.pipeline_length in
        let pipeline_length = 
          pipeline_length_to_positive_integer pipeline_length
        in
        let gen_spec = {
          gen_spec with
          offset = 0;
          run_length = 1;
          step = 1
        }
        in
        let transformation_seq =
          test_cases_transformation_seq
            gen_spec
        in
        match pipeline_length with
        | `One -> [||]
        | `GreaterThanOne pipeline_length -> begin
          Array.fold_left
            (fun acc _ ->
              Array.append acc [|transformation_seq|]
            )
            [||]
            (Array.make (pipeline_length - 1) ())
          end
      in
      let initial_indexes = {
        transformation_i = transformation_seq;
        additional_pipeline = additional_pipeline ();
        seed_i = 0;
        source_code_i = 0;
        test_counter = 0;
      }
      in
      let initial_indexes_copy = {
        initial_indexes with
        additional_pipeline = additional_pipeline ();
      }
      in
        test_cases_seq
          (*~tigress_test_home*)
          initial_indexes
          initial_indexes_copy
          testcases
      )
      gen_spec_list
  end
 
let count_generator_transformations
  ?(gen_spec_index: int option = None)
  (testcases: TestCaseGenerator.Doc_t.contents)
  : int list
  = begin
    let pipeline_length = 
      match 
        pipeline_length_to_positive_integer testcases.pipeline_length
      with
      | `One -> 1
      | `GreaterThanOne x -> x
    in
    let gen_spec_list =
      match gen_spec_index with
      | None -> testcases.gen_spec
      | Some i -> begin
        let list_of_ith_element =
          try [List.nth testcases.gen_spec i] with
          | _ -> []
        in
          list_of_ith_element
      end
    in
    List.map
      (fun (gen_spec: TestCaseGenerator.Doc_t.transformations_gen_spec) ->
        let num_transformations =
          Array.fold_left
            (fun acc (t: TestCaseGenerator.Doc_t.transformation) ->
              acc + (
                Array.fold_left
                (fun acc (o: TestCaseGenerator.Option_t.transform_option) ->
                  acc + (Array.length o.values)
                )
                0
                t.options
              )
            )
            0
            gen_spec.transformations
        in
        let num_transformations =
          int_of_float (
            (float_of_int pipeline_length) ** (float_of_int num_transformations)
          )
        in
        num_transformations 
        * (Array.length testcases.seeds)
        * (Array.length testcases.source_files)
      )
      gen_spec_list
  end

 
let gen_test_cases_for_run_from_script
    (script_info: Defaults.script_spec)
    (config: Options.Run.config)
    (*  (output_dir: string) *)
    (database_dir: string option) 
    (transformation_descriptions: TransformationDescription.Doc_t.t option)
    (source_descriptions: SourceDesc_t.doc option)
    (*  : (TestCaseSet.t, string) result *)
    : [ `Gen of (TestScript.script_with_config * TestScript.TestCaseTransformationSet.t TestScript.PlatformsMap.t * TestScriptv1_1.Doc_t.pipeline_length option)
       | `Full of (TestScript.script_with_config * TestScript.test_case_with_platforms_exclude Seq.t) ]
  = begin
    let script_filename = script_info.filename in
    Logs.info (fun m -> m "Script filename provided: %s" script_filename);
    let test_scriptv1_1_json_string = 
      Common.read_all_from_file script_filename 
    in
      let test_scriptv1_1 = 
        TestScriptv1_1.Doc_j.t_of_string test_scriptv1_1_json_string 
      in
      let tigress_test_home =
        match config#tigress_test_home with
        | None -> Filename.current_dir_name
        | Some x -> x 
      in

      (* For the first step of generating TestCaseGenerators,
         we need pipeline_length = 1; later we need to know
         the real pipeline_length; so, save it to a variable.*)
      let pipeline_length =
        match test_scriptv1_1.from_database with
        | None -> None
        | Some x -> 
          x.pipeline_length
      in
      (* If pipeline length is > 1 make test-cases-generator data; 
         otherwise, make test-cases data *)
      match pipeline_length with
      | None
      | Some `Number 0
      | Some `Number 1 -> begin (* create test-cases data *)
        let test_script = TestScript.script_t_of_v1_1_script_t
          ~tigress_test_home
          ~database_dir 
          test_scriptv1_1
          transformation_descriptions
          source_descriptions
        in
        let config = 
          new TestScript.script_with_config config test_script test_scriptv1_1 
        in
        let test_cases =
          gen_test_cases_from_script
            config
        in
        `Full (config, test_cases)
      end
      | Some `Pairs
      | Some `Triples
      | Some `Number _ -> begin (* create test-cases-generator data *)
        (* For the first step of generating TestCaseGenerators,
          we need pipeline_length = 1; later we need to know
          the real pipeline_length; so, save it to a variable.*)
        let test_scriptv1_1, pipeline_length =
          match test_scriptv1_1.from_database with
          | None -> test_scriptv1_1, None
          | Some x -> 
            {test_scriptv1_1 with from_database = Some {x with pipeline_length = None}}
            , x.pipeline_length
        in
        let test_script = TestScript.script_t_of_v1_1_script_t
            ~tigress_test_home
            ~database_dir
            test_scriptv1_1
            transformation_descriptions
            source_descriptions
        in
        let config = 
          new TestScript.script_with_config config test_script test_scriptv1_1 
        in
        (*gen_test_cases_from_script config*)
        let transformations = test_script.transformations in
        let transformations = 
          EnrichedTestScript.TransformationPipelinesSet.fold
            (fun x acc -> 
              ReverseList.append acc (List.hd x.pipeline)
            )
            transformations
            ReverseList.empty
          |> ReverseList.to_list
        in
        let transformations_by_platform =
          TestScript.partition_transformations_by_platforms
            transformations
        in
      `Gen (config, transformations_by_platform, pipeline_length)
    end
  end        
let update_generator_source_file_paths
  (test_cases_generator_contents: TestCaseGenerator.Doc_t.contents)
  (database_dir: string option)
  = begin
  let source_files: TestScript_t.source_code_spec_t array =
    let source_files = test_cases_generator_contents.source_files in
    match database_dir with
    | None -> source_files
    | Some database_dir -> begin
      Array.map
        (fun (source_file_spec: TestScript_t.source_code_spec_t) ->
          let source_filename = 
            let filename = source_file_spec.source_filename in
            if Filename.is_implicit filename then
              Filename.concat database_dir filename
            else
              filename
          in
          {source_file_spec with source_filename;}
        )
        source_files
    end
  in
  {test_cases_generator_contents with source_files;}
end

let test_case_sequence_and_num_cases_at
  (index: int)
  ((sequence, num_test_cases): TestCase.OcamlT.t Seq.t * int)
  : TestCase.OcamlT.t Seq.t * int
  = begin
           let rec consume_sequence_until
            (remaining_to_consume: int)
            (sequence: TestCase.OcamlT.t Seq.t)
            : TestCase.OcamlT.t Seq.t
            = begin
              if remaining_to_consume <= 0 then
                sequence
              else begin
                match sequence () with
                | Seq.Nil -> (fun () -> Seq.Nil)
                | Seq.Cons (_, x) ->
                  consume_sequence_until
                    (remaining_to_consume - 1)
                    x
              end
            end
          in
          let sequence, num_test_cases =
            if index > num_test_cases then
              (fun () -> Seq.Nil), 0
            else begin
          let sequence =
              consume_sequence_until 
                index
                sequence
          in
          let num_test_cases =
              num_test_cases - index
          in
          sequence, num_test_cases
        end
      in
      sequence, num_test_cases
    end
 
let gen_test_cases_for_run_from_test_cases
    ({filename=test_cases_filename;test_number_start=test_cases_start;find_all_parts=_; gen_spec_index;}: Defaults.test_cases_run_spec)
    (database_dir: string option)
  (*  (config: Options.Run.config) 
      (output_dir: string)
      (database_dir: string option)
      (transformation_descriptions: TransformationDescription.Doc_t.t option)
      (source_descriptions: SourceDesc_t.doc option) *)
  : (
    [`List of (TestCase.OcamlT.t list * TestCase.Metadata.t * Unix.file_descr)
    | `Seq of ((TestCase.OcamlT.t Seq.t) list * int list * TestCase.Metadata.t * Unix.file_descr) ]
    , string) result
  = begin
    Logs.info (fun m -> m "Test cases filename provided: %s" test_cases_filename);
    let test_cases_fd = Unix.openfile test_cases_filename [O_RDONLY] 0 in
    try
    begin
    let work () = begin
      let test_cases_string =
        let chan = Unix.in_channel_of_descr test_cases_fd in
        really_input_string chan (in_channel_length chan)
      in
      Logs.info (fun m -> m "Reading test cases from test cases file.");
      match TestCase.from_json_string test_cases_string with
      | Error Parser_error e -> 
        (Logs.err (fun m -> m "Parser error reading test cases file.");
         raise e)
      | Error Wrong_file_type _ -> begin (* try interpreting the file as a test-cases-generator file *)
        let test_cases_generator = TestCaseGenerator.Doc_j.t_of_string test_cases_string in
        let metadata = 
          TestCase.Metadata.create
            test_cases_filename
            0.0
            (Sha256 (Sha256.string test_cases_string))
        in
        let test_cases_generator_contents: TestCaseGenerator.Doc_t.contents =
          let {
            file_type=_;
            file_format_version=_;
            tigress_version=_;
            tigress_options;
            tigress;
            compiler_options;
            compiler;
            max_run_seconds;
            pipeline_length;
            seeds;
            source_files;
            ordering;
            gen_spec;
          }: TestCaseGenerator.Doc_t.t = test_cases_generator in
          {
            tigress_options;
            tigress;
            compiler_options;
            compiler;
            max_run_seconds;
            pipeline_length;
            seeds;
            source_files;
            ordering;
            gen_spec; 
          } 
        in
        (* Assume relative directories refer to files in the database *)
        let test_cases_generator_contents = 
          update_generator_source_file_paths
            test_cases_generator_contents
            database_dir
       in
        let gen_spec_index =
          match gen_spec_index with
          | None -> None
          | Some x -> begin
            Some (NonemptyList.list_of x |> NonegIntRange.t_list_to_int_list)
          end
        in
        let sequences =
          List.concat (
            match gen_spec_index with
            | None ->
              [generate_test_cases_sequences
                ~gen_spec_index:None
                test_cases_generator_contents 
              ]
            | Some x ->
              List.map
                (fun gen_spec_index ->
                  let gen_spec_index = Some gen_spec_index in
                  generate_test_cases_sequences
                    ~gen_spec_index
                    test_cases_generator_contents 
                )
                x
          )
        in
        let num_test_cases = 
          List.concat (
            match gen_spec_index with
            | None ->
              [count_generator_transformations 
                ~gen_spec_index:None
                test_cases_generator_contents 
              ]
            | Some x ->
              List.map
                (fun gen_spec_index ->
                  let gen_spec_index = Some gen_spec_index in
                  count_generator_transformations 
                    ~gen_spec_index
                    test_cases_generator_contents 
                )
                x
          )
        in
          let sequences, num_test_cases =
            match test_cases_start with
            | None -> sequences, num_test_cases
            | Some test_cases_start -> begin
              if test_cases_start <= 0 then
                sequences, num_test_cases
              else begin
                List.fold_left2
                  (fun 
                    ((acc_seq, acc_num): TestCase.OcamlT.t Seq.t list * int list) 
                    (a: TestCase.OcamlT.t Seq.t) 
                    (b: int) ->
                    let sequence, num_test_cases =
                      test_case_sequence_and_num_cases_at
                        test_cases_start
                        (a, b)
                    in
                    (sequence :: acc_seq, num_test_cases :: acc_num)
                  )
                  ([], [])
                  sequences
                  num_test_cases
              end
            end
      in
        Ok (`Seq (
          sequences,
          num_test_cases,
          metadata,
          test_cases_fd
        ))
      end
      | Error Unsupported_version s ->
        failwith s
      | Ok test_cases_intermediate_repr -> 	  
        match TestCase.test_cases test_cases_intermediate_repr with
        | None -> failwith "Error: could not find required field, \"cases\", in test cases file."
        | Some test_case_irepr_list ->
          match test_cases_start with 
          | None -> begin
              let test_cases_rslt = (parse_test_cases_until_error test_case_irepr_list) in
              match test_cases_rslt with
              | Error e -> Error e
              | Ok test_cases_rslt -> 
                Ok (`List (
                  test_cases_rslt 
                  ,TestCase.Metadata.create 
                    test_cases_filename 
                    0.0 
                    (Sha256 (Sha256.string test_cases_string))
                  ,test_cases_fd
                ))
            end 
          | Some next_test_number ->
            let test_cases_rslt = (parse_test_cases_until_error test_case_irepr_list) in
            Logs.info (fun m -> m "Generating list of remaining test cases to run.");
            (
              match test_cases_rslt with
              | Error e -> Error e
              | Ok test_cases ->
                (Ok (`List
                   ((List.rev
                       (List.fold_left 
                          (fun acc (x: TestCase.OcamlT.t) -> 
                             (*prerr_endline 
                             				  ("test_number =" ^ (string_of_int x.test_number));*)
                             if x.test_number >= next_test_number then
                               x :: acc
                             else
                               acc
                          ) [] test_cases)
                    )
                   , TestCase.Metadata.create 
                      test_cases_filename 
                      0.0 
                      (Sha256 (Sha256.string test_cases_string))
                    ,test_cases_fd
                   )

                ))
            )
    end (* work function*)
    in
    match work () with
    | Ok x -> Ok x
    | Error x -> Unix.close test_cases_fd; Error x
    end (* try *)
    with
    | x -> Unix.close test_cases_fd; raise x
  end

let gen_test_cases_filename
    ?(prefix="testcases_")
    (script_info: Defaults.test_run_file(*Defaults.script_spec*))
  : string
  = begin
    match script_info with
    | TestCases {filename=filename; test_number_start=_; find_all_parts=_; gen_spec_index=_;} -> filename
    | Script {filename=script_filename} ->
      let current_time = Common.human_readable_current_localtime_string () in
      let output_filename = 
        (
          prefix
          ^ (
            script_filename
            |> Filename.basename
            |> Filename.remove_extension
          )
          ^ "_" ^ current_time
          ^ ".json" 
        ) (* |> Filename.concat output_dir *)
      in
      output_filename
  end

let gen_test_cases_doc_string
    (test_cases: [`TCSet of TestScript.test_case_with_platforms_exclude Seq.t | `TCList of TestCase.OcamlT.t list])
  : string = begin
  match test_cases with
  | `TCSet test_cases ->
    let test_cases =
      (
        Seq.fold_left
          (fun 
            acc 
            ({test_case=x; platforms_exclude=_}: TestScript.test_case_with_platforms_exclude) ->
            x :: acc
          )
          []
          test_cases
      )
      |> List.rev
    in
    let test_cases_doc =
      TestScript.test_cases_doc_from_test_cases test_cases
    in
    TestScript_j.string_of_test_cases_doc_t test_cases_doc 
  | `TCList test_cases ->
    TestCase.doc_from_test_cases test_cases
    |> Yojson.pretty_to_string
end

let write_test_cases_to_file
    ~(filename: string)
    ~(output_dir: string)
    ~(test_cases_doc_string: string)
    ()
  : unit = begin
  let filename =
    Filename.concat output_dir filename
  in
  Logs.info (fun m -> m "Generating test cases file from script file. Test cases file will be named: %s." filename);
  (Common.write_string_to_file
     ~filename
     test_cases_doc_string
  )
end
(*
let gen_test_cases_for_run 
  (config: Options.Run.config)
(*  (options: Options.Run.t) *)
  (output_dir: string)
  (database_dir: string option)
  (transformation_descriptions: TransformationDescription.Doc_t.t option)
  (source_descriptions: SourceDesc_t.doc option)
(*
  : (
     (TestCase.OCamlT.t list * TestCase.Metadata.t) list * HostCasesMap.t, 
      string
    ) result
*)
(*  : (TestCase.OcamlT.t list list, string) result * TestCase.Metadata.t  *)
  : ([`TCSet of TestScript.test_case_with_platforms_exclude Seq.t | `TCList of TestCase.OcamlT.t list],
     string) result
  = begin
(*  let test_cases_data_opt: (int option * string * TestCase.Metadata.t) option =
*)
  match (
       config#script_file
       ) (* TODO: automatically discover whether the given file is a script
	    file or a test cases file, based on the file_type field of
	    the json doc contained in the file *)
    with
    | Some Script script_info -> 
      let test_cases =
        gen_test_cases_for_run_from_script
          script_info
          config
(*          output_dir *)
          database_dir
          transformation_descriptions
          source_descriptions
      in begin
        match test_cases with
        | Ok test_cases -> Ok (`TCSet test_cases)
        | Error e -> Error e
      end
    | Some TestCases test_cases_info ->
      let test_cases =
        gen_test_cases_for_run_from_test_cases
          test_cases_info
          config
          output_dir
          database_dir
          transformation_descriptions
          source_descriptions
      in begin
        match test_cases with
        | Ok (test_cases, _) -> Ok (`TCList test_cases)
        | Error e -> Error e 
      end
    | None ->
	 failwith "Fatal error: either a script filename or a test cases filename must be provided."
end
*)

let gen_test_cases_for_continue
    (*(defaults: Defaults.t)*)
    (options: Options.Rerun.config) 
    (output_dir: string)
    (database_dir: string option)
  : [ `Single of ([
    `List of (TestCase.OcamlT.t list * TestCase.Metadata.t * Unix.file_descr)
    | `Seq of ((TestCase.OcamlT.t Seq.t) list * int list * TestCase.Metadata.t * Unix.file_descr)
    ], string) result
     | `Multiple of string * string list
    ]
  =
  let results_filename = filename_of_results_to_rerun options output_dir in
  match results_filename with
  | None -> begin
    (* no results file found; try interpreting the file as a test_cases file *)
    match options#results_filename_opt with
    | None ->
      `Single
      (Error "Could not find a results file to continue running test cases from." 
      (*,TestCase.Metadata.create "" 0.0 Missing *)
      )
    | Some filename -> begin
      if Option.is_some options#test_number_end
        || Option.is_some options#test_numbers then
        `Single (Error "Could not find a results file to continue running test cases from, and not trying to interpret filename as a test cases file because of incompatible options.") 
      else begin
        Logs.warn (fun m -> m "Could not find results file to continue running test cases from.  Trying to find a test cases file, instead.");
        `Single (gen_test_cases_for_run_from_test_cases {
          filename;
          test_number_start=options#test_number_start;
          find_all_parts=false;
          gen_spec_index=None;
        }
        database_dir
        )
      end
    end
  end
  | Some filename -> begin
    let other_parts = 
      filenames_of_other_results_parts 
      ~results_part_filename:filename
      ~output_dir
    in
    match other_parts with
    | [] -> begin
      Logs.info (fun m -> m "Reading previous results from results file.");
      let result_doc = TestResults.read_results_from_file filename in
      (*prerr_endline (TestResults_j.string_of_result_doc_t result_doc);*)
      let test_cases_filename: string = 
        result_doc.test_cases_file.filename 
      in
      Logs.info (fun m -> m "Reading corresponding test cases file, containing the test cases which produced these results.");
      let test_cases_fd = Unix.openfile test_cases_filename [O_RDONLY] 0 in
      try
      begin
      let work () = begin
        let test_cases_doc_string = 
          let chan = Unix.in_channel_of_descr test_cases_fd in
          really_input_string chan (in_channel_length chan)
        in
        let testcases_doc = TestCase.from_json_string test_cases_doc_string in
        match testcases_doc with
        | Error Wrong_file_type _ -> begin
          (* check whether this is a test-cases-generator file *)
        let test_cases_generator = TestCaseGenerator.Doc_j.t_of_string test_cases_doc_string in
        let metadata = 
          TestCase.Metadata.create
            test_cases_filename
            0.0
            (Sha256 (Sha256.string test_cases_doc_string))
        in
        let test_cases_generator_contents: TestCaseGenerator.Doc_t.contents =
          let {
            file_type=_;
            file_format_version=_;
            tigress_version=_;
            tigress_options;
            tigress;
            compiler_options;
            compiler;
            max_run_seconds;
            pipeline_length;
            seeds;
            source_files;
            ordering;
            gen_spec;
          }: TestCaseGenerator.Doc_t.t = test_cases_generator in
          {
            tigress_options;
            tigress;
            compiler_options;
            compiler;
            max_run_seconds;
            pipeline_length;
            seeds;
            source_files;
            ordering;
            gen_spec; 
          } 
        in
         (* Assume relative directories refer to files in the database *)
        let test_cases_generator_contents = 
          update_generator_source_file_paths
            test_cases_generator_contents
            database_dir
       in
        let gen_spec_index = result_doc.test_cases_file.gen_spec_index in 
        let sequences =
          generate_test_cases_sequences
            ~gen_spec_index
            test_cases_generator_contents 
        in
        let num_test_cases = 
          count_generator_transformations 
            ~gen_spec_index
            test_cases_generator_contents 
        in
           (* Read last test number from results file and add all test cases
         	      with test numbers greater than that to the list of test cases
         	      to return, which will then be run. *)
          (* Assumes that test cases appear in monotonically increasing
         	      numeric order *)
          Logs.info (fun m -> m "Reading to the end of the results file to determine the test number of the last test case that was tested in a previous run.");
          let last_result_opt: TestResults_t.result_with_previous_result_t option = 
            try
              List.find
                (Option.is_some)
                (List.rev result_doc.results)
            with
              Not_found -> None
          in
          let test_cases_to_skip =
            (match last_result_opt with
            | Some last_result -> last_result.test_case.tc_test_number + 1
            | None -> 0
            )
          in 
           let sequences, num_test_cases =
              if test_cases_to_skip <= 0 then
                sequences, num_test_cases
              else begin
                List.fold_left2
                  (fun 
                    ((acc_seq, acc_num): TestCase.OcamlT.t Seq.t list * int list) 
                    (a: TestCase.OcamlT.t Seq.t) 
                    (b: int) ->
                    let sequence, num_test_cases =
                      test_case_sequence_and_num_cases_at
                        test_cases_to_skip
                        (a, b)
                    in
                    (sequence :: acc_seq, num_test_cases :: acc_num)
                  )
                  ([], [])
                  sequences
                  num_test_cases
              end
          in
         `Single (Ok (`Seq (sequences, num_test_cases, metadata, test_cases_fd)))
        end
        | _ -> begin
        match testcases_doc with
        | Error e ->
          let error_message =
            (match e with
            | Parser_error e -> Logs.err (fun m -> m "Parse error when reading test cases file: %s" test_cases_filename); raise e
            | Wrong_file_type msg -> msg
            | Unsupported_version msg -> msg
            )
          in
          `Single
          (Error error_message
          (*,TestCase.Metadata.create "" 0.0 Missing *)
          )

        | Ok testcases_doc ->
          (* Read last test number from results file and add all test cases
         	      with test numbers greater than that to the list of test cases
         	      to return, which will then be run. *)
          (* Assumes that test cases appear in monotonically increasing
         	      numeric order *)
          Logs.info (fun m -> m "Reading to the end of the results file to determine the test number of the last test case that was tested in a previous run.");
          let last_result_opt: TestResults_t.result_with_previous_result_t option = 
            try
              List.find
                (Option.is_some)
                (List.rev result_doc.results)
            with
              Not_found -> None
          in
          let next_test_number =
            (match last_result_opt with
            | Some last_result -> last_result.test_case.tc_test_number + 1
            | None -> 0
            )
          in begin
            match TestCase.test_cases testcases_doc with
            | None -> failwith "Error: could not find required field, \"cases\", in test cases file."
            | Some test_case_irepr_list ->
              let test_cases_rslt = (parse_test_cases_until_error test_case_irepr_list) in begin

                let test_cases_info = 
                  TestCase.Metadata.create test_cases_filename 0.0 (Sha256 (Sha256.string test_cases_doc_string))
                in begin

                  Logs.info (fun m -> m "Generating list of remaining test cases to run.");
                  `Single
                  (
                  (
                    match test_cases_rslt with
                    | Error e -> Error e
                    | Ok test_cases ->
                      Ok (`List 
                        (List.rev
                          (List.fold_left 
                              (fun acc (x: TestCase.OcamlT.t) -> 
                                (*prerr_endline 
                               			        ("test_number =" ^ (string_of_int x.test_number));*)
                                if x.test_number >= next_test_number then
                                  x :: acc
                                else
                                  acc
                              ) [] test_cases
                              )
                  ,test_cases_info
                  ,test_cases_fd
                  )
                        )

                  )
                  )
              end
            end
          end
        end (* match ... Error Unsupported_version _ *)
        end (* work function*)
        in
        match work () with
        | `Single (Ok x) -> `Single (Ok x)
        | x -> Unix.close test_cases_fd; x
        end (* try *)   
      with
      | x ->
        Unix.close test_cases_fd;
        raise x
    end
    | _ :: _ as other_parts -> begin
      `Multiple (filename, other_parts)
    end
  end

module ResultsbyTestNumber = struct
  type t = TestResults_t.result_with_previous_result_t
  let compare 
      ({test_case = {tc_test_number = tn1; _}; _}: TestResults_t.result_with_previous_result_t)
      ({test_case = {tc_test_number = tn2; _}; _}: TestResults_t.result_with_previous_result_t)
    =
    Stdlib.compare tn1 tn2
end

module ResultsbyTestNumberSet = Set.Make(ResultsbyTestNumber)

class rerun_reconfig config results_filename = object
  inherit Options.Rerun.config_by_copy config 

  method results_filename_opt = Some results_filename
end

let gen_test_cases_for_rerun
    (*(defaults: Defaults.t)*)
    (options: Options.Rerun.config)
    (output_dir: string) 
  (*: ((TestCase.OcamlT.t list, string) result * TestCase.Metadata.t)
    * ((TestCase.OcamlT.t list, string) result * TestCase.Metadata.t) list
  *)
  : [ `Single of (TestCase.OcamlT.t list * TestCase.Metadata.t, string) result
     | `Multiple of string * string list
    ]
  =
  let results_filename = filename_of_results_to_rerun options output_dir 
  in
  match results_filename with
  | None -> 
    `Single
    (Error "Could not find a results file to rerun test cases from." 
    )
  | Some filename -> begin
    (*let filenames: string NonemptyList.t = *)
      let other_parts = 
        filenames_of_other_results_parts 
        ~results_part_filename:filename
        ~output_dir
      in
      (*NonemptyList.create filename other_parts 
    in *)
    let gen_testcases filename = begin
        Logs.info (fun m -> m "Reading previous results from results file.");
      let rerun_passed = not options#failed
      in
      let test_numbers, candidate_results, test_cases_filename, test_cases_file_checksum =
        let result_doc = TestResults.read_results_from_file filename
        in
        let test_cases_filename: string = 
          result_doc.test_cases_file.filename 
        in
        let test_cases_file_checksum: Sha256.t =
          Sha256.of_hex
            result_doc.test_cases_file.checksum
        in
        let test_numbers =
          (match options#test_numbers with
           | Some x ->
             let test_numbers =
               NonegIntRange.t_list_to_int_list (NonemptyList.list_of x)
             in
             (match test_numbers with
              | [] -> None
              | hd::tl -> Some (NonemptyList.create hd tl)
             )
           | None -> None
          )
        in
        (* If the user has requested specific test numbers and they cannot
           all be found in the specified results file, then get them
           from a previous results file.
           Merge those previous results with the results of the specified
           results file so that if the user also requested to rerun
           failed tests, those will come from among the results from
           the specified results file.
           This should result in this function producing a list of test 
           cases to run that the user would expect in most cases.
        *)
        let candidate_results = 
          let results: TestResults_t.result_with_previous_result_t list = 
            List.filter_map (fun x -> x) result_doc.results
          in
          (
            match test_numbers with
            | None -> results
            | Some test_numbers ->
              let results = 
                ResultsbyTestNumberSet.union
                  (ResultsbyTestNumberSet.of_list results)
                  (ResultsbyTestNumberSet.of_list (
                      results_from_previous_run
                        ~no_later_than: (Some result_doc.creation_time)
                        ~exclusive_test_numbers: (Some test_numbers)
                        ~search_dir: output_dir
                        (Some test_cases_file_checksum)
                    )
                  )
              in
              List.of_seq (ResultsbyTestNumberSet.to_seq results)
          )
        in
        let test_cases_file_checksum: TestCase.Metadata.Checksum.t =
          Sha256 test_cases_file_checksum
        in
        let test_numbers = 
          (match test_numbers with
           | None -> None
           | Some x -> Some (NonemptyList.list_of x)
          )
        in
        (test_numbers, candidate_results, test_cases_filename, test_cases_file_checksum)
      in
      let convert_fn 
          (test_fn: TestResults_t.result_with_previous_result_t -> bool)
          (acc: TestCase.OcamlT.t list)
          (test_result: TestResults_t.result_with_previous_result_t):
        TestCase.OcamlT.t list =
        if (test_fn test_result) then
          (TestCase.OcamlT.of_test_cases_t test_result.test_case) :: acc
        else
          acc
      in
      let convert_fn =
        (
          match (test_numbers, rerun_passed) with
          | (Some test_numbers, true) ->
            let test_fn (x: TestResults_t.result_with_previous_result_t) =
              List.exists ((=) x.test_case.tc_test_number) test_numbers
            in
            (convert_fn test_fn)
          | (None, false) ->
            let test_fn (x: TestResults_t.result_with_previous_result_t) =
              (String.compare "FAIL" x.result) = 0
            in
            (convert_fn test_fn)
          | (Some test_numbers, false) ->
            let test_fn (x: TestResults_t.result_with_previous_result_t) =
              (List.exists ((=) x.test_case.tc_test_number) test_numbers)
              || (String.compare "FAIL" x.result) = 0
            in
            (convert_fn test_fn)
          | (None, true) ->
            (fun 
              (acc: TestCase.OcamlT.t list)
              (test_result: TestResults_t.result_with_previous_result_t):
              TestCase.OcamlT.t list ->
              (TestCase.OcamlT.of_test_cases_t test_result.test_case) :: acc
            )
        )
      in
      Logs.info (fun m -> m "Generating list of test cases to re-run.");
      (Ok 
         (List.rev
            (List.fold_left 
               (fun acc (x: TestResults_t.result_with_previous_result_t) -> 
                  (*match x with
                  		      | None -> acc
                  		      | Some x ->*)
                  (convert_fn acc x)
               ) [] candidate_results)
          ,
          TestCase.Metadata.create 
            test_cases_filename 
            0.0 
            test_cases_file_checksum 
         )
      )
    end
    in
    match other_parts with
    | [] ->
      `Single (gen_testcases filename)
    | _ :: _ as other_parts -> begin
      `Multiple (filename, other_parts)
    end
(*    
      NonemptyList.map
        (gen_testcases)
        filenames
      |> NonemptyList.tuple_of
*)
    end


let gen_test_cases_for_adhoc_run
    (options: Options.Adhoc.config)
    (output_dir: string) 
  : (TestCase.OcamlT.t list, string) result * TestCase.Metadata.t
  =
  match options#path_to_c_compiler with
  | None -> failwith "Fatal error: location of C compiler not given on command line or in configuration file."
  | Some path_to_c_compiler ->
    let tigress_home = Common.val_or_default_option options#tigress_home Locations.tigress_home in
    let tigress_exe = 
      (match options#path_to_program with
       | Some x -> x
       | None ->
         (match tigress_home with
          | None -> failwith "Fatal error: no path to Tigress program in configuration file and no tigress home environment variable or configuration option."
          | Some x -> x ^ "/tigress"
         )
      )
    in
    Logs.debug (fun m -> m "defaults.tigress_home=\"%s\"" 
                   (Defaults.string_option_to_string options#tigress_home) 
               );
    Logs.debug (fun m -> m "Locations.tigress_home=\"%s\"" 
                   (Defaults.string_option_to_string Locations.tigress_home)
               );
    Logs.debug (fun m -> m "tigress_exe=\"%s\"" tigress_exe);
    let source_files_directory =
      (Filename.dirname options#source_file)
    in
    let dir =
      Unix.opendir source_files_directory
    in
    let num_files_found, files_basenames =
      Common.filepattern_to_filenames
        dir
        (Filename.basename options#source_file)
    in
    Unix.closedir dir;
    (if num_files_found = 0 then  
       failwith ("Error: no files found with pattern, "
                 ^ options#source_file
                 ^ ", in directory, "
                 ^ source_files_directory
                 ^ ".")
    );
    let source_files = 
      List.map (Filename.concat source_files_directory) files_basenames
    in
    let seeds = 
      match options#seeds with
      | Some x -> NonegIntRange.t_list_to_int_list (NonemptyList.list_of x)
      | None -> []
    in
    Logs.info (fun m -> m "Constructing a script (in memory) from the command line options.");
    let script: TestScriptv1_1.Doc_t.t = {
      file_type = TestScript.file_type;
      file_format_version = TestScript.current_file_format_version;
      tigress_spec: TestScript_t.program_spec_t option = Some {
          prog_filename = tigress_exe;
          prog_version = None;
          prog_version_command = None;
        };
      tigress_options = 
        options#program_default_options;
      max_run_seconds = options#max_run_seconds;
      compiler_spec: TestScript_t.program_spec_t option = Some {
          prog_filename = path_to_c_compiler;
          prog_version = None;
          prog_version_command = None;
        };
      compiler_options = 
        options#compiler_options;
      from_database = None;
      remote_hosts_filename = None;
      source_files = 
        List.map (fun (filename: string): TestScript_t.source_code_spec_t -> {
              TestScript_t.source_filename = filename;
              source_checksum = None;
              source_args = Some options#c_program_args;
              source_input = Some options#c_program_input;
              source_functions = None;
            }) source_files;
      transformations = 
        [{ TestScript_t.pipeline = options#transformation :: options#additional_transformations; }];
      seeds = seeds;
      test_order = None;
    }
    in
    (match options#save_to_script_filename with
     | Some filename -> 
       Logs.info (fun m -> m "Saving command line options as a script with filename, %s" filename);
       let json = 
         (* commented out prettify because of stack overflow *)
         (* Yojson.Safe.prettify ( *)
         TestScriptv1_1.Doc_j.string_of_t script
         (* ) *)
       in
       let chan = open_out filename in
       output_string chan json;
       close_out chan
     | None -> ()
    );

    let current_time = Common.human_readable_current_localtime_string () in 
    let test_cases_filename = 
      Filename.concat
        output_dir
        ("testcases_adhoc_" ^ current_time ^ ".json")
    in

    Logs.info (fun m -> m "Generating test cases from script and saving to file: %s" test_cases_filename);
    let test_cases = 
      let enriched_script: EnrichedTestScript.t = {
        tigress_spec = script.tigress_spec;
        tigress_options = script.tigress_options;
        max_run_seconds = script.max_run_seconds;
        compiler_spec = script.compiler_spec;
        compiler_options = script.compiler_options;
        source_files = script.source_files;
        transformations = EnrichedTestScript.transformation_pipeline_set_of_test_script_transformation_pipeline_list script.transformations;
        seeds = script.seeds;
      }
      in
(*
          let script: TestScriptv1_1.Doc_t.t = {
            file_type = script.file_type;
            file_format_version = script.file_format_version;
            tigress_spec = Some script.tigress_spec;
          }
          in
*)
      let script = 
        new TestScript.script_with_config 
          (options: Options.Adhoc.config :> Options.Run.config) 
          enriched_script
          script 
      in
      Seq.fold_left
        (fun 
          acc 
          ({test_case=x; platforms_exclude=_;}: TestScript.test_case_with_platforms_exclude) 
          ->
            x :: acc
        )
        []
        (gen_test_cases_from_script script)
      |> List.rev
    in

    let test_cases_checksum
      : TestCase.Metadata.Checksum.t
      = 
      let test_cases_doc = TestScript.test_cases_doc_from_test_cases test_cases in
      let test_cases_doc_string = 
        TestScript_j.string_of_test_cases_doc_t test_cases_doc
      in
      (Common.write_string_to_file
         ~filename: test_cases_filename
         test_cases_doc_string
      );
      Sha256 (Sha256.string test_cases_doc_string)
    in

    (Ok (
        List.fold_left
          (fun acc x -> (TestCase.OcamlT.of_test_cases_t x) :: acc) 
          []
          test_cases
        |> List.rev
      ),
     TestCase.Metadata.create test_cases_filename 0.0 test_cases_checksum
    )

(** Input some random bits to use to create the temporary directory name
    Create the temporary directory
    Return the name of the temporary directory

    The large random number is supposed to make it unlikely
    that it will have the same name as a previously existing directory
    or one being at about the same time by another process in a
    race condition.
*)
let create_temp_dir
    (random_ints: int * int * int * int):
  string =
  match random_ints with
    (_0, _1, _2, _3) ->
    let name = 
      Printf.sprintf "tigress_test_%010d%010d%010d%010d"
        _0 _1 _2 _3
      |> Filename.concat (Filename.get_temp_dir_name ())
    in
    Unix.mkdir name 0o755;
    name

(*
module HostCompare = struct
  type t = RemoteHosts_t.host
  let compare (a: t) (b: t): int =
    String.compare a.host_address b.host_address
end
*)

module HostMap = TestScript.HostMap(*Map.Make(Host)*)
module HostSet = TestScript.HostSet

module HostwithPriorityInfo = struct
  type t = {
    host_info: Host.t;
    processes_to_allocate: int;
    last_allocated_counter: int;
  }
end
module HostbyPriority = struct
  type t = HostwithPriorityInfo.t
  let compare (a: t) (b: t): int = begin
    let rslt = b.processes_to_allocate - a.processes_to_allocate in
    if rslt <> 0 then
      rslt
    else begin
      let rslt =
        b.last_allocated_counter - a.last_allocated_counter
      in
      if rslt <> 0 then
        rslt
      else
        String.compare 
          (Host.host_address a.host_info)
          (Host.host_address b.host_info)
    end
  end
end
module HostbySpecwithPriorityInfo = struct
  type t = HostwithPriorityInfo.t
  let compare (a: t) (b: t): int = begin
    compare a.host_info b.host_info
  end
end

module HostsbyPrioritySet = Set.Make(HostbyPriority)
module HostswithPriorityMap = Map.Make(HostbySpecwithPriorityInfo)

let schedule_one_test
    (tests_scheduled: (TestScript_t.test_cases_t ReverseList.t) HostMap.t)
    (test_to_schedule: TestScript.test_case_with_platforms_exclude)
    (host_priority: HostsbyPrioritySet.t)
  : (TestScript_t.test_cases_t ReverseList.t) HostMap.t * HostsbyPrioritySet.t
  = begin
    let platforms_include =
      EnrichedTestScript.platforms_complement
        test_to_schedule.platforms_exclude
    in
(* commented out (debugging) print statements
    Printf.eprintf "In schedule_one_test: platforms_include = %s\n"
      (EnrichedTestScript.PlatformSet.fold (fun x acc -> TransformationDescription.Platform_t.show_platform x ^ " " ^ acc) platforms_include "");
    (HostMap.iter (fun k v -> 
         Printf.eprintf "Host: %s, Num tests scheduled: %d\n"
           (Host.host_address k)
           (ReverseList.length v)
       )
        tests_scheduled
    );
    (HostsbyPrioritySet.iter
       (fun x ->
          let p0, p = Host.platforms x.host_info in
          let p0 = TransformationDescription.Platform_t.show_platform p0 in
          let p = List.map (TransformationDescription.Platform_t.show_platform) p in
          Printf.eprintf "Host: %s, processes_to_allocate: %d, last_allocated: %d, platforms: %s\n"
            (Host.host_address x.host_info)
            x.processes_to_allocate
            x.last_allocated_counter
            (String.concat ", " (p0 :: p))
       )
       host_priority
    );
*)
    (** For each platform "platform" on which we need to run this test,
       check all hosts "hosts" (should be passed in only those hosts
       which this test has not been allocated to, yet).
       Allocate a test if platform matches, for those platforms
       remaining to be matched.
       @return A host that the test may be allocated to
    *)
    let find_host_for_platform
        (hosts: HostsbyPrioritySet.t)
        (platform: TransformationDescription.Platform_t.platform)
      : HostsbyPrioritySet.elt option
      = begin
        let host_with_priority_opt =
          HostsbyPrioritySet.find_first_opt
            (fun x ->
               let platform_compare0, platform_compare_remain = Host.platforms x.host_info in
               platform = platform_compare0 ||
               List.exists ((=) platform) platform_compare_remain
            )
            hosts
        in
        (* Note: Host may be allocated even if processes_to_allocate = 0
            because if that is the situation, it means there are no
            other hosts running on the needed platform that have
            processes_to_allocate > 0 *)
        host_with_priority_opt
      end
    in
    let hosts_remain, hosts_allocated =
      EnrichedTestScript.PlatformSet.fold
        (fun 
          platform 
          (hosts_remain, hosts_allocated) ->
          let h = 
            find_host_for_platform
              hosts_remain
              platform
          in
          match h with
          | None -> (hosts_remain, hosts_allocated)
          | Some h -> 
            let hosts_remain =
              HostsbyPrioritySet.filter (fun x -> x.host_info <> h.host_info) hosts_remain
            in
            let hosts_allocated = h :: hosts_allocated in
(* commented out (debugging) print statements
            Printf.eprintf "Host(found): %s\n" (Host.host_address h.host_info);
            List.iter (fun (x: HostwithPriorityInfo.t) -> Printf.eprintf "Host(allocated): %s\n" (Host.host_address x.host_info)) hosts_allocated;
            (HostsbyPrioritySet.iter
               (fun x ->
                  Printf.eprintf "Host(remain): %s, processes_to_allocate: %d, last_allocated: %d\n"

                    (Host.host_address x.host_info)
                    x.processes_to_allocate
                    x.last_allocated_counter
               )
               hosts_remain
            );
*)
            (
              (*hosts_remain = *)
              hosts_remain,
              (*hosts_allocated = *)
              hosts_allocated
            )
        )
        platforms_include
        ( (*hosts_remain = *)host_priority,
                             (*hosts_allocated = *)[]
        )
    in
    (
      List.fold_left
        (fun 
          (acc: (TestScript_t.test_cases_t ReverseList.t) HostMap.t) 
          (h: HostswithPriorityMap.key) 
          : (TestScript_t.test_cases_t ReverseList.t) HostMap.t ->
          HostMap.update h.host_info
            (fun x ->
               match x with
               (*| None -> Logs.error (fun m -> m "Host missing from HostsMap: %s!" h.host_address)*)
               | None -> Some (ReverseList.append ReverseList.empty test_to_schedule.test_case)
               | Some x -> Some (ReverseList.append x test_to_schedule.test_case)
            )
            acc
        )
        tests_scheduled
        hosts_allocated
      ,
      (* make a new hosts priority set by adding all hosts from hosts_remain
         keeping priorities the same and incrementing last_allocated_counter,
         then adding all hosts from hosts_allocated, decrementing priority
         and setting last_allocated_counter to zero.
      *)
      let new_host_priority =
        HostsbyPrioritySet.map
          (fun 
            {host_info=host_info; 
             processes_to_allocate=priority; (*priority*) 
             last_allocated_counter=last;
            } -> {
              host_info;
              processes_to_allocate = 
                  priority;
              last_allocated_counter = last + 1;
            }
          )
          hosts_remain
      in
      let new_host_priority =
        List.fold_left
          (fun
            acc
            ({host_info=host_info;
              processes_to_allocate=priority;
              last_allocated_counter=_;
             }: HostsbyPrioritySet.elt) ->
            HostsbyPrioritySet.add
              {host_info;
               processes_to_allocate = (
                 if priority < 1 then
                  0
                 else
                  priority - 1
               );
               last_allocated_counter = 0;
              }
              acc
          )
          new_host_priority
          hosts_allocated
      in
      (* if all hosts have "processes_to_allocated" = 0, 
         then reset them all to "max_processes" *)
        match 
          HostsbyPrioritySet.find_first_opt 
            (fun x -> x.processes_to_allocate > 0)
            new_host_priority
        with
        | None -> 
          HostsbyPrioritySet.map
            (fun x -> { x with processes_to_allocate = Host.max_processes x.host_info })
            new_host_priority
        | Some _ -> new_host_priority
    )
  end

(*
type host_to_test_cases = {
  local: TestScript_t.test_cases_t list option;
  remote: TestScript_t.test_cases_t list HostMap.t;
}
*)
let rerun_arguments_of_rerun_config
  (options: Options.Rerun.config)
  : string array = begin
  Array.append
    (if options#failed then
      [|"--failed"|]
    else
      [||])
    ([|
      String.concat ","
        (
        List.map
          (NonegIntRange.to_string)
          (match options#test_numbers with
          | None -> []
          | Some x -> NonemptyList.list_of x
          )
        )
    |])
  end

module StringSet = Set.Make(String)

let update_work_queue
  work_queue_directory
  being_processed
  file_pattern
  : string list
  = begin
    let _, queue_files =
      let dir = Unix.opendir work_queue_directory in
      Fun.protect
        ~finally:(fun () -> Unix.closedir dir)
        (fun () -> 
          Common.filepattern_to_filenames
            dir
            file_pattern
        )
    in
    Logs.debug (fun m -> m "In update_work_queue, matching files in queue directory:\n");
    List.iter (fun x -> Logs.debug (fun m -> m "\t%s\n" x)) queue_files;
    let queue_files = StringSet.of_list queue_files in
    let being_processed = StringSet.of_list being_processed in
    let new_files = StringSet.diff queue_files being_processed in
    List.of_seq (StringSet.to_seq new_files)
  end

  let update_queued_commands
    (work_queue_directory: string)
    (file_pattern: string)
    (command_template: string array)
    (file_position_in_command: int)
    (queued_commands: string array list)
    = begin
      Logs.debug (fun m -> m "In update_queued_commands\n");
      Logs.debug (fun m -> m "\t file_pattern: %s\n" file_pattern);
      let pos = file_position_in_command in
      let queued_files: string list =
        List.map
          (fun (x: string array) -> 
            let filename = Array.get x pos in
            Logs.debug (fun m -> m "\tqueued file: %s\n" filename);
            filename
          )
          queued_commands
      in
      let new_files = 
        update_work_queue
          work_queue_directory
          queued_files
          file_pattern
      in
      List.map
        (fun (x: string) -> 
          Logs.debug (fun m ->m "File in queue to add to commands to be processed: %s\n" x);
          let arr = 
            Array.copy command_template 
          in
          Array.set arr pos (Filename.concat work_queue_directory x);
          arr
        )
        new_files
    end

let restart_lagging_processes
  work_queue_dir
  processes
  = begin
    let file_position_in_command = 2 in
    let running, killed = 
      List.fold_left
        (fun (running, killed) (command, process_chans) ->
          let status_filename_prefix = 
            let filename = Array.get command file_position_in_command in
            "status_" 
            ^ (filename |> Filename.basename |> Filename.remove_extension)
          in
          let files_in_dir = Sys.readdir work_queue_dir in
          match
            Array.find_opt
              (fun filename ->
                (*if (String.sub filename 0 (String.length status_filename_prefix))
                  = status_filename_prefix then begin*)
                if Str.string_match 
                  (Str.regexp (status_filename_prefix ^ ".*\\.json"))
                  filename
                  0
                then begin
                  let time_since_file_modification =
                    let { st_mtime = last_modification_time; _ }: Unix.stats =
                      Unix.stat (Filename.concat work_queue_dir filename)
                    in
                    ((Unix.time ()) -. last_modification_time)
                  in
                  if time_since_file_modification > kTOO_LONG_SECONDS then
                    false
                  else
                    true
                end
                else
                  false
              )
              files_in_dir
          with
          | None -> (* no recently modified status file found; stop the process *)
            let pid = Unix.process_full_pid process_chans in
            Unix.kill pid Sys.sigkill;
            (running, (command, process_chans) :: killed)
          | Some _ -> ((command, process_chans) :: running, killed)
        )
        ([], [])
        processes
        in
        (List.rev running, List.rev killed)
  end
let gen_test_cases_for_rerun_filename
  test_cases_metadata = begin
  let orig_filename = 
    TestCase.Metadata.filename test_cases_metadata
  in
  let prefix = 
    (Filename.basename orig_filename)
    |> Filename.remove_extension
  in
    prefix 
    ^ "_rerun_" 
    ^ (Common.human_readable_current_localtime_string ())
    ^ ".json"
end


let write_test_cases_generator_for_host
  ~(output_dir: string)
  ~(output_file_prefix: string)
  (host: Host.t)
  (testcases: TestCaseGenerator.Doc_t.contents)
  : ((TestCase.OcamlT.t Seq.t) list) * TestCase.Metadata.t                     
  = begin
      let test_cases_metadata = 
        let test_cases_doc_string =
          let {
            (*pipeline_length=*)pipeline_length; 
            (*seeds=*)seeds; 
            (*source_files=*)source_files; 
            (*ordering=*)ordering; 
            (*gen_spec=*)gen_spec;
            (*compiler=*)compiler;
            (*compiler_options=*)compiler_options;
            (*tigress=*)tigress;
            (*tigress_options=*)tigress_options;
            (*max_run_seconds=*)max_run_seconds;
          }: TestCaseGenerator.Doc_t.contents =
            testcases
          in
          let test_cases_doc: TestCaseGenerator.Doc_t.t =
            {
              file_type = "test_case_generator";
              file_format_version = [ 1 ];
              tigress_version = ""; (* TODO: use a real Tigress version *)
              pipeline_length;
              seeds;
              source_files;
              ordering;
              gen_spec;
              compiler;
              compiler_options;
              tigress;
              tigress_options;
              max_run_seconds;
            }
          in
          TestCaseGenerator.Doc_j.string_of_t test_cases_doc
        in
        let output_filename =
          output_file_prefix 
          ^ "_"
          ^ (Host.host_address host) (* TODO: concatenate with suffix *)
          ^ ".json"
        in
        let output_file_full_path =
          Filename.concat output_dir output_filename
        in
        (Common.write_string_to_file
           ~filename: output_file_full_path
           test_cases_doc_string
        );
        TestCase.Metadata.create output_filename 0.0 (Sha256 (Sha256.string test_cases_doc_string))
      in
      let test_cases_sequences =
        generate_test_cases_sequences
          testcases
    in
      (
        test_cases_sequences
        ,test_cases_metadata
      )
  end


let () =
  (* Sys.set_signal Sys.sigint Signal_default; *)
  Sys.catch_break false;
  (*    Logs.set_level (Some Logs.Debug); *)
  Random.self_init ();
  let config = 
    let defaults = 
      (match Config.config_file_opt with
       | Some config_file -> (*prerr_endline config_file;*) Defaults.from_config_file config_file
       | None -> Defaults.empty)
    in
    (* Printf.eprintf "Defaults=\n%s" (Defaults.to_string defaults);flush stderr; *)
    let options = Options.from_command_line () in
    Logs.debug (fun m -> m "Defaults=\n%s" (Defaults.to_string defaults));
    Options.to_config options defaults
  in
  (* These are default config values from the combination of the config file 
      and the command line (i.e., with the program arguments from the command line
      overriding defaults found in the configuration file);
      whereas "defaults" in the code block above are defaults from the config file,
      if any, before any program arguments from the command line are taken into
      consideration.
    "defaults" differ from "config" because "config" contains configuration specific
    to sub-commands. "defaults" contains the configuration that is common to all
    sub-commands.*)
  let defaults = Options.Config.to_defaults config in
     (*Printf.eprintf 
      "defaults#tigress_home = %s\ndefaults#path_to_program = %s\n"
      (Defaults.string_option_to_string defaults#tigress_home)
      (Defaults.string_option_to_string defaults#path_to_program);
      flush stderr;*)
  let output_directory_name = 
    match defaults#output_dir with
    | Some x -> x
    | None -> Filename.current_dir_name
  in
(* commented out because it is being done elsewhere, instead
  (* obtain file locks *)
  (
    match config with
    | Continue config
    | Rerun config ->
(*    let fd = Unix.openfile "" [O_RDWR; O_CREAT; O_TRUNC] 0o640 in
      Unix.lockf fd F_TLOCK 0;
*) ()
    | TestCasesRun config ->
(*
    let fd = Unix.openfile "" [O_RDWR; O_CREAT; O_TRUNC] 0o640 in
      Unix.lockf fd F_TLOCK 0;
*) ()
    | Run _
    | Stop _
    | Status _
    | ContinueQueue _
    | Adhoc _
    | WorkerRun _ -> ()
   );
*)
                let process_pool_update_queued_commands =
                  let command_template =
                    [| Sys.executable_name;
                      "continue"; 
                      ""
                    |]
                  in
                  match defaults#work_queue_dir with
                  | None -> (fun x -> x)
                  | Some work_queue_dir ->
                    (fun
                      (
                        results_accum, 
                        (orig_commands, remaining_commands, results),
                        new_command_options
                      )
                      ->
                        Logs.debug (fun m -> m "In process_pool_update_queued_commands\n");
                        Logs.debug (fun m -> m "In process_pool_update_queued_commands, work_queue_dir is: %s\n" work_queue_dir);
                      let running_commands (*, _(*stopped_commands*)*) =
                      (*  restart_lagging_processes
                          work_queue_dir  *)
                          (ReverseList.to_list results)
                      in
                      let running_commands =
                        List.map
                          (fun (command, _) -> command)
                          running_commands
                      in
                      (* consider running_commands plus remaining_commands
                         to be already in process;
                         stopped commands may therefore be re-added to the
                         queue.  If the kill signal was sent to the 
                         process running that command, but the process
                         did not stop; the new process should exit when
                         it fails to get the lock on the test-cases file;
                         so there should not be duplicated testing work. *)
                      (
                        results_accum
                        ,(orig_commands, remaining_commands, results)
                        , new_command_options @
                        (update_queued_commands
                          work_queue_dir
                          "{results_,testcases_}*.json"
                          command_template
                          2
                          (running_commands 
                          @ remaining_commands)
                        )
                      )
                    )
                in        
  let temporary_directory_name = 
    create_temp_dir 
      ((Random.bits ()),
       (Random.bits ()),
       (Random.bits ()),
       (Random.bits ()) 
      ) 
  in
  let remote_hosts: RemoteHosts_t.host list =
    let remote_hosts_filename: string option =
      match config with
      | Run config -> config#remote_hosts_filename
      | Stop config -> config#remote_hosts_filename
      | Status config -> config#remote_hosts_filename
      | Rerun config
      | Continue config -> config#remote_hosts_filename
      | ContinueQueue _
      | Adhoc _
      | TestCasesRun _
      | WorkerRun _ -> None
    in
    match remote_hosts_filename with
    | None -> []
    | Some fn -> begin
      let doc =
        let remote_hosts_file_string = Common.read_all_from_file fn in
        RemoteHosts_j.doc_of_string remote_hosts_file_string
      in
      doc.hosts
    end 
  in
  let transformation_descriptions: TransformationDescription.Doc_t.t option =
    match defaults#database_dir with
    | None -> None
    | Some database_dir ->
      let transformation_descriptions_fullpath = 
        Filename.concat database_dir transformation_descriptions_basename
      in
      if Sys.file_exists transformation_descriptions_fullpath then
        let json_string = 
          Common.read_all_from_file transformation_descriptions_fullpath
        in
        Some (TransformationDescription.Doc_j.t_of_string json_string)
      else
        None
  in
  (match transformation_descriptions with
   | Some x -> Logs.debug (fun m -> m "Read %s" x.file_type)
   | None -> Logs.debug (fun m -> m "No transformation descriptions file")
  );
  let source_descriptions: SourceDesc_t.doc option =
    match defaults#database_dir with
    | None -> None
    | Some database_dir ->
      let source_descriptions_fullpath = 
        Filename.concat database_dir source_descriptions_basename
      in
      if Sys.file_exists source_descriptions_fullpath then
        let json_string = 
          Common.read_all_from_file source_descriptions_fullpath
        in
        Some (SourceDesc_j.doc_of_string json_string)
      else
        None
  in 
  (match source_descriptions with
   | Some x -> Logs.debug (fun m -> m "Read %s" x.file_type)
   | None -> Logs.debug (fun m -> m "No source descriptions file")
  );
  (*    let options = Options.from_command_line () in *)
  let verbosity: Logs.level option = 
    match config with
    | Run x -> x#verbosity
    | TestCasesRun x -> x#verbosity
    | WorkerRun (`Run x) -> x#verbosity
    | WorkerRun (`Rerun x) -> x#verbosity
    | Continue x -> x#verbosity
    | ContinueQueue x -> x#verbosity
    | Rerun x -> x#verbosity
    | Adhoc x -> x#verbosity
    | Status x -> x#verbosity
    | Stop x -> x#verbosity
    (*      | TestCasesRun x -> x#verbosity *)
  in
  Logs.set_level verbosity;
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.info (fun m -> m "Logging level set to: %s" (Logs.level_to_string @@ Logs.level ()));
  let (*[@warning "-26"]*) write_pid process_id_dir () = begin
    let pidfile_filename =
      match process_id_dir with
      | None -> kPIDFILE_FILENAME
      | Some dir -> Filename.concat dir kPIDFILE_FILENAME
    in  
    let fd = Unix.openfile pidfile_filename [O_RDWR; O_CREAT; O_TRUNC] 0o640 in
      Unix.lockf fd F_TLOCK 0;
      let pid = Unix.getpid () in
      let pid = string_of_int pid in
        ignore (Unix.single_write_substring fd pid 0 (String.length pid));
    (* retain lock and keep file open? *)
        fd
  end 
  in
   (* write test cases to a file
     output_dir is the directory into which to write the output file
     output_filename is the name of the output file to write

     It returns a list of test cases that were written, and those
      will be used later on to run the test.contents
     The TestCase.Metadata.t part of the returned tuple will be
      used later to find the results from a previous result, if such
      exist. *)
  let write_test_cases_ocamlt
      ~(output_dir: string)
      ~(output_filename: string)
      (test_cases: TestCase.OcamlT.t list)
    : TestCase.Metadata.t
    = begin
      let test_cases_info = 
        let test_cases_doc_string =
          let test_cases_doc = TestCase.doc_from_test_cases test_cases in
          Yojson.pretty_to_string test_cases_doc 
        in
        let output_file_full_path =
          Filename.concat output_dir output_filename
        in
        (Common.write_string_to_file
           ~filename: output_file_full_path
           test_cases_doc_string
        );
        TestCase.Metadata.create output_filename 0.0 (Sha256 (Sha256.string test_cases_doc_string))
      in
        test_cases_info
    end (* function write_test_cases *)
  in
  let write_test_cases 
      ~(output_dir: string)
      ~(output_filename: string)
      (test_cases: (TestScript_t.test_cases_t ReverseList.t))
    : TestCase.OcamlT.t list * TestCase.Metadata.t
    = begin
      let test_cases =
        ReverseList.fold
          (fun acc x -> (TestCase.OcamlT.of_test_cases_t x) :: acc)
          []
          test_cases
      in
      (test_cases
        ,write_test_cases_ocamlt
          ~output_dir
          ~output_filename
          test_cases
      )
    end
  in 
  let rerun_enqueue
    (options: Options.Rerun.config)
    (work_queue_dir: string)
    : [ `Single of TestCase.OcamlT.t list * string |
        `Multiple of (TestCase.OcamlT.t list * string) * ((TestCase.OcamlT.t list * string) list) ]
    = begin
               match gen_test_cases_for_rerun options output_directory_name with
              | `Single x -> begin
                match x with
                | (Error e) -> failwith e
                | Ok (test_cases, test_cases_metadata) ->
                  (* Write test cases to a file in work_queue directory *)
                  let test_cases_filename = 
                    gen_test_cases_for_rerun_filename test_cases_metadata
                  in
                  let _ =
                    write_test_cases_ocamlt
                      ~output_dir:work_queue_dir
                      ~output_filename:test_cases_filename
                      test_cases
                  in
                  `Single (test_cases, test_cases_filename)
              end
              | `Multiple (x0, x_remain) -> begin
                (*Generate testcases*)
                let test_cases = begin
                  List.fold_left
                    (fun acc results_filename ->
                      let options = new rerun_reconfig options results_filename
                      in
                      let options = (options:rerun_reconfig :> Options.Rerun.config)
                      in
                      match 
                        gen_test_cases_for_rerun options output_directory_name
                      with
                      | `Single (Error e) -> Logs.warn (fun m -> m "Attempting to process results file, %s, got error, %s" results_filename e); acc
                      | `Multiple _ -> Logs.warn (fun m -> m "Multiple filenames produced after decomposition of multiple filenames into single filenames."); acc
                      | `Single (Ok test_cases) -> test_cases :: acc
                    )
                    []
                    (x0 :: x_remain)
                  |> List.rev
                end
                in
                let test_cases = begin
                  match test_cases with
                  | [] -> failwith "No test cases to rerun."
                  | _ :: _ as test_cases -> test_cases
                end
                in
                (*Write test cases to files in work_queue directory *)
                let test_cases_and_filenames =
                  List.map
                    (fun (test_cases, test_cases_metadata) ->
                      let test_cases_filename = 
                        gen_test_cases_for_rerun_filename test_cases_metadata
                      in
                      let _ =
                        write_test_cases_ocamlt
                          ~output_dir:work_queue_dir
                          ~output_filename:test_cases_filename
                          test_cases
                      in
                      (test_cases, test_cases_filename)
                    )
                    test_cases
                in
                  match test_cases_and_filenames with
                  | [] -> failwith "No test cases to rerun."
                  | x0 :: x_remain -> `Multiple (x0, x_remain)
              end
            end
            in

  let _ =
  (match config with
    | Stop config -> begin
      let (stop_local, stop_remote_filter): 
        (bool * [ `All | `Some of string list ] option) =
        match config#remotes_to_stop with
        | `None -> (true, None)
        | `All -> (true, Some `All)
        | `Some x -> (false, Some (`Some x))
      in 
      let remotes_to_stop = begin
      match stop_remote_filter with
       | None -> []
       | Some remotes_to_stop ->
         List.fold_left
           (fun acc (x: RemoteHosts_t.host) -> 
             match remotes_to_stop with
             | `All -> x :: acc
             | `Some hostnames -> 
                if List.exists ((=)x.host_address) hostnames then
                  x :: acc
                else
                  acc
           )
           []
           remote_hosts
      end
      in
      (*stop remotes*)
      List.iter
        (fun (h: RemoteHosts_t.host) ->
          let host =
            match h.login_name with
            | None -> h.host_address
            | Some user -> user ^ "@" ^ h.host_address
          in
          let command = 
            h.path_to_tigress_test 
            ^ " stop "
          in
          Logs.info (fun m -> m "Remote host: %s" host);
          Logs.info (fun m -> m "Remote command: %s" command);
          ignore (
            Unix.open_process_args_full "ssh"
              [| "ssh";
                  host; 
                  command;
              |]
              (Unix.environment ())
          )
        )
        remotes_to_stop
      ;
      if stop_local then begin
        (* read pid from pidfile *)
        let pidfile_filename =
          match config#process_id_dir with
          | None -> kPIDFILE_FILENAME
          | Some dir -> Filename.concat dir kPIDFILE_FILENAME
        in  
        let pid = 
          try Some (Common.read_all_from_file pidfile_filename) with 
          | _ -> None
        in
        (match pid with
        | None 
        | Some "" -> 
          Logs.err (fun m -> m "No pidfile found, error reading pidfile, or empty pidfile. Unable to stop process.")
        | Some pid ->
        begin
          Logs.info (fun m -> m "Pid in local pidfile:%s" pid);
          let pid = int_of_string pid in
          try Unix.kill pid Sys.sigkill with
          | Unix.Unix_error (Unix.ESRCH, "kill", _) ->
            Logs.err (fun m -> m "Pid (%d) in pidfile doesn't correspond to a running process. Couldn't kill pid (%d)." pid pid)
        end
        )
      end
      else
        ();
      exit 0
    end
    | Status config -> begin
      (* read pidfile *)
        let pidfile_filename =
          match config#process_id_dir with
          | None -> kPIDFILE_FILENAME
          | Some dir -> Filename.concat dir kPIDFILE_FILENAME
        in  
        let pid = try 
          Some (Common.read_all_from_file pidfile_filename)
          with _ -> None 
        in
        let exit_code =
        begin
        match pid with
        | None -> print_endline "No pidfile found or error reading pidfile on local machine; Tigress-test might not be running locally."
          ; 1
        | Some pid ->
          Printf.printf "Process id is: %s\n" pid;
          (* read pidfile modification time; *)
          let { st_mtime = last_modification_time; _ }: Unix.stats =
            Unix.stat pidfile_filename
          in
          let likely_stalled =
            let time_since_pidfile_modification =
              ((Unix.time ()) -. last_modification_time)
            in
            match config#work_queue_dir with
            | None -> false
            | Some dir ->
              let result_files_in_queue =
                let files_in_queue = Sys.readdir dir in
                let kRESULT_FILE_PREFIX = "status_" in
                  let result_files_in_queue_reverse =
                  Array.fold_left
                    (fun acc s -> 
                      (*if String.sub s 0 (String.length kRESULT_FILE_PREFIX)
                        = kRESULT_FILE_PREFIX then*)
                      if Str.string_match
                        (Str.regexp (kRESULT_FILE_PREFIX ^ ".*\\.json"))
                        s
                        0
                      then
                          s :: acc 
                      else 
                        acc
                    ) 
                    []
                    files_in_queue
                  in
                  List.rev result_files_in_queue_reverse
              in
              let rec recent_file_change
                files_list
                =
                (* check directory list from both ends in case it is in order
                   according to when the file was last added. In that case 
                   either the first or the last is likely to be the most
                   recent. *)
                  let mod_time_was_recent
                    filename
                    =
                    let { st_mtime = last_modification_time; _ }: Unix.stats =
                      Unix.stat (Filename.concat dir filename)
                    in
                    if last_modification_time < kTOO_LONG_SECONDS then
                      true
                    else
                      false
                  in
                  match files_list with
                  | [] -> (* reached end without finding recent file *)
                    false
                  | hd :: tl ->
                    if mod_time_was_recent hd then
                      true
                    else
                      recent_file_change tl
              in
            (* if program has been running long enough AND 
               no results file has changed in the queue recently
               then likely_stalled = true else false.
               time_since_pidfile_modification should be a reliable indicator
               of when the program started because we are using the pid
               stored in the pidfile to check whether the file is running
               if the pid in the file matches the pid of a running process,
               either it is by coincidence (unlikely) or if should have
               been the one to write the pid into the pidfile.
            *)
            if (time_since_pidfile_modification > (kTOO_LONG_SECONDS /. 2.))
              && (
                recent_file_change 
                  result_files_in_queue
              ) then
                false
            else
              true
          in
            Printf.printf 
              "PIDfile last modified: %s\n"
              (Common.tm_to_iso8601_string
                (Unix.localtime
                  last_modification_time
                )
              );
            if Sys.unix || Sys.cygwin then begin
              let exitcode = Sys.command ("kill -s 0 " ^ pid) in
                if exitcode = 0 then begin
                  if likely_stalled then begin
                    Printf.printf "Process is running, but may be stalled.\n"
                    ; 2
                  end
                  else begin
                    Printf.printf "Process is running\n"
                    ; 0
                  end
                end
                else begin
                  Printf.printf "Process is not running.\n"
                  ; 1
                end
            end
            else begin (*win32 case*)
              (*TODO: under win32, check whether process is running*)
              if likely_stalled then 1 else 0
            end
        end
        in
        (match config#work_queue_dir with
        | None -> print_endline "No work_queue_directory configured to check."
        | Some work_queue_dir ->
        (* check whether there are files in queue; *)
          let status_files = 
            DirSearch.dirsearch 
              "status_*.json"
              ~root:work_queue_dir
              ()
          in
          List.iter
            (fun fn ->
              let status = 
                try Some (
                  Status_j.t_of_string
                    (Common.read_all_from_file fn)
                )
                with _ -> None
              in
                match status with
                | None -> Printf.printf "Error reading file: %s\n" fn
                | Some { 
                  file_type = _; 
                  file_format_version= _;
                  status = {
                    total_test_cases = total_test_cases;
                    passed = passed;
                    failed = failed;
                  };
                  test_cases_file=_;} -> 

                    Printf.printf 
                      "Status %s:\n\ttest cases to run: %d\n\tpassed: %d\n\tfailed: %d\n"
                      fn
                      total_test_cases 
                      passed 
                      failed
            )
            status_files
        );
        (* call status for each remote in remote hosts file *)
        (match config#remotes_to_check with
        | `None -> ()
        | `All
        | `Some _ ->
          let pids =
      List.map
        (fun (h: RemoteHosts_t.host) ->
          let host =
            match h.login_name with
            | None -> h.host_address
            | Some user -> user ^ "@" ^ h.host_address
          in
          let command = 
            h.path_to_tigress_test 
            ^ " status --local-only "
          in
          Logs.info (fun m -> m "Remote host: %s" host);
          Logs.info (fun m -> m "Remote command: %s" command);
            Unix.create_process "ssh"
              [| "ssh";
                  host; 
                  command;
              |]
              Unix.stdin
              Unix.stdout
              Unix.stderr
        )
        remote_hosts
        in
          List.iter
            (fun pid -> ignore (Unix.waitpid [] pid))
            pids
        );
      exit exit_code
    end
    | WorkerRun config -> begin
      (* If this is a worker-rerun case then
         write the files to rerun into the work-queue directory. *)
      let config_process_id_dir,
          config_script_file,
          config_output_dir,
          config_work_queue_dir,
          config_max_processes,
          config_continue_queue
        = begin
        match config with
        | `Rerun config -> begin
          match config#work_queue_dir with
          | None -> failwith "No work_queue_dir configured."
          | Some work_queue_dir ->
            let filenames =
              rerun_enqueue
                config
                work_queue_dir
            in
            let config_script_file: Defaults.test_run_file option =
              let filenames = begin
                match filenames with
                | `Single (_, filename) -> filename
                | `Multiple ((_, x0), x_remain) ->
                  let x_remain =
                    List.map
                      (fun (_, x) -> x)
                      x_remain
                  in
                  String.concat "," (x0 :: x_remain)
              end
              in
              Some (TestCases
              {
                filename = filenames;
                test_number_start = config#test_number_start;
                find_all_parts = false;
                gen_spec_index = None;
              })
            in
            config#process_id_dir
            ,config_script_file
            ,config#output_dir
            ,config#work_queue_dir
            ,config#max_processes
            ,false
        end       
        | `Run config -> 
          config#process_id_dir
          ,config#script_file
          ,config#output_dir
          ,config#work_queue_dir
          ,config#max_processes
          ,config#continue_queue
      end  
      in
      (* Printf.eprintf "in WorkerRun case\n"; flush stderr; *)
      Logs.debug (fun m -> m "in WorkerRun case\n");
      (* Printf.eprintf "config_continue_queue is %b\n" config_continue_queue; flush stderr; *)
      Logs.debug (fun m -> m "config_continue_queue is %b\n" config_continue_queue);
      (* test cases files should have already been put into queue
         by the calling process *)
      if Sys.win32 then begin
        (* Printf.eprintf "case Sys.win32 is true\n"; flush stderr; *)
        Logs.debug (fun m -> m "case Sys.win32 is true\n");
        ignore (Subprocess.windows_schedule_task
          ~now_and_later:true
          ~task_name:"Tigress-test worker run"
          ~command:(Sys.executable_name ^ " continue-queue")
          1);

          exit 0 
      end
      else begin (* other OSes besides win32 *)
        (* Printf.eprintf "case Sys.win32 is false\n"; flush stderr; *)
        Logs.debug (fun m -> m "case Sys.win32 is false\n");
        Subprocess.unix_put_process_in_background ();

          let fd = write_pid config_process_id_dir () in
            Fun.protect
              ~finally:(fun () -> Unix.lockf fd F_ULOCK 0; Unix.ftruncate fd 0; Unix.close fd) 
              (fun () ->
                (* start a separate process to work on each file *)
                let filenames = config_script_file in
                let filenames, gen_spec_indexes =
                  match filenames with
                  | None -> 
                    if config_continue_queue then
                     [], [] 
                    else
                      failwith "No filenames provided!"
                  | Some Script _ -> failwith "expected \"TestCases\", got \"Script\"."
                  | Some TestCases x -> (
                    String.split_on_char ',' x.filename
                    , match x.gen_spec_index with
                      | None -> []
                      | Some x -> NonemptyList.list_of x |> NonegIntRange.t_list_to_int_list
                  )
                in
                let output_directory_name =
                    match config_work_queue_dir with
                    | None -> begin
                      match config_output_dir with

                      | None -> ""
                      | Some x -> x
                    end
                    | Some x -> x
                    
                in
                let do_while_waiting = 
                  (fun x ->
                    let result = 
                    process_pool_update_queued_commands
                     x
                    in
                    Thread.delay 
                      (float_of_int kDEFAULT_FILE_POLL_INTERVAL_SECONDS);
                    result
                  )
                in
                let commands =
                  if config_continue_queue then begin
                    let _, _, commands_to_continue_from_queue =
                      Logs.debug (fun m -> m "calling process_pool_update_queued_commands\n");
                      process_pool_update_queued_commands
                        (ReverseList.empty, ([], [], ReverseList.empty), [])
                    in
                      commands_to_continue_from_queue
                  end
                else begin
                  List.concat (
                  List.map
                    (fun filename ->
                      let filepath = 
                        if Filename.is_implicit filename then
                          (Filename.concat 
                            output_directory_name
                            filename)
                        else
                          filename
                      in
                      (*Printf.eprintf 
                        "testcases-run filepath: %s\n"
                        filepath;*)
                      Logs.debug (fun m -> m
                        "testcases-run filepath: %s\n"
                        filepath);
                      flush stderr;
                      match gen_spec_indexes with
                      | [] ->
                        [[|
                          Sys.executable_name;
                          "testcases-run";
                          filepath
                        |]]
                      | _ :: _ ->
                        List.map
                          (fun gen_spec_index ->
                            [|
                              Sys.executable_name;
                              "testcases-run";
                              filepath;
                              "--gen-spec-index";
                              string_of_int gen_spec_index;
                            |]
                          )
                          gen_spec_indexes
                    )
                    filenames
                  )
                end
                in
                let process_chans = 
                  Subprocess.process_pool_do
                    ~do_while_waiting_or_do_after_waiting:`After
                    config_max_processes
                    Sys.executable_name
                    commands
                    do_while_waiting
                    (ReverseList.empty)
                in
                let process_chans =
                  ReverseList.map
                    (fun (_, process_chan) -> process_chan)
                    process_chans
                  |> ReverseList.to_list
                in
(*
                let process_chans = begin
                                 List.map
                                   (fun filename -> 
                                      Unix.open_process_args_full Sys.executable_name
                                        [| Sys.executable_name;
                                           "testcases-run"; 
                                           (Filename.concat 
                                              output_directory_name
                                              filename)
                                        |]
                                        (Unix.environment ())
                                   )
                                   filenames;
                  
                                end
                                in
*)

                (List.iter
                  (fun (in1,out,in2)(*process_chans*) ->
                    (*
                    let pid = Unix.process_full_pid process_chans in
                    ignore (Unix.waitpid [] pid)
                    *)
                    (*ignore (Unix.close_process_full process_chans)*)
                    close_in in1;close_out out;close_in in2 
                  )
                  process_chans);

                exit 0
            )
(*        end *)
      end
    end
    | Run _
    | TestCasesRun _
    | Continue _
    | ContinueQueue _
    | Rerun _
    | Adhoc _ -> begin
      ()
    end
  )
  in

  (* writes the parallel test cases to (potentially) multiple files.

     Output is the list of files for each host, which will be
      used later to run the tests (by passing the filenames to
      a remote instance of the Tigress_test program).
  *)
  let write_parallel_test_cases 
      ~(output_dir: string)
      ~(output_filename_prefix: string)
      (test_cases: (TestScript_t.test_cases_t ReverseList.t) HostMap.t)
    : (Host.t * string list) list 
    = begin
      (* test cases per parallel host *)
      let test_cases_files_by_host: (Host.t * string list) list = begin
        HostMap.fold
          (fun k v acc ->
             (* (Printf.eprintf "In write_parallel_test_cases: host = %s\n" (Host.show k)); *)
             (Logs.debug (fun m -> m "In write_parallel_test_cases: host = %s\n" (Host.show k)));
             let test_cases =
               let test_cases =
                 ReverseList.fold
                   (fun acc x -> (TestCase.OcamlT.of_test_cases_t x) :: acc)
                   []
                   v
               in
               (* acc is an array of lists of test cases (TestCase.OcammlT.t)
                  n is the number of parts into which to divide the test cases for this host
                    (i.e., its number of max processes)
                  i is a counter (nonnegative integer)
               *)
               let parts, _, _ =
                 List.fold_left
                   (fun ((acc, n, i): (TestCase.OcamlT.t list array * int * int)) (case: TestCase.OcamlT.t) ->
                      if i < n then
                        (Array.set acc i (case :: (Array.get acc i)); acc,
                                                                      n,
                                                                      i + 1
                        )
                      else
                        (Array.set acc 0 (case :: (Array.get acc 0)); acc,
                                                                      n,
                                                                      1
                        )
                   )
                   (Array.make 
                      (Host.max_processes k) 
                      [](*([]:TestCase.OcamlT.t)*), 
                    Host.max_processes k,
                    0
                   )
                   test_cases
               in
               parts |> Array.to_seq |> List.of_seq
             in
             (* write test cases to files *)
             let host_address = 
               match k with
               | Local _ -> ""
               | Remote x -> "_remote_" ^ x.host_address
             in
             let filenames: string list = 
               (List.mapi
                  (fun i x ->
                     let output_filename =
                       output_filename_prefix 
                       ^ host_address
                       ^ "_part" 
                       ^ (string_of_int (i + 1))
                       ^ ".json"
                     in
                     let output_file_full_path =
                       Filename.concat output_dir output_filename
                     in
                     let test_cases_doc_string =
                       let test_cases_doc = TestCase.doc_from_test_cases x in
                       Yojson.pretty_to_string test_cases_doc 
                     in
                     (Common.write_string_to_file
                        ~filename: output_file_full_path
                        test_cases_doc_string
                     );
                     output_filename
                  )
                  test_cases
               )
             in
             (k, filenames) :: acc
          ) (* HostMap.fold fun *)
          test_cases
          []
      end 
      in
      test_cases_files_by_host    
    end
  in
         (* Periodically run rsync to synchronize the remote
           results files locally *)
        let rec sync_from_remote_helper 
            (* ?(eof:bool=false) *) (* rename this as it no longer means EOF *) 
            ?(host_filenames_recycle : (RemoteHosts_t.host * string list * bool) list = [])
    (*        ~(stdin_chan: out_channel)
            ~(stdout_chan: in_channel)
            ~(stderr_chan: in_channel) *)
            (host_filenames : (RemoteHosts_t.host * string list  * bool) list)
            (timeout:float): unit = begin
          (*if eof then () else begin *)
            let host_filenames, host_filenames_recycle =
              match host_filenames with
              | [] -> List.rev host_filenames_recycle, []
              | _ :: _ as x -> x, host_filenames_recycle
            in
            (match host_filenames_recycle with
            | [] -> ignore (Unix.select [] [] [] timeout);
            | _ :: _ -> ()
            );
            match host_filenames with
            | [] -> prerr_endline "No files left to sync.";flush stderr;()
            | (h, filenames, last_sync) :: tl ->
           let copy_command =
              let files_full_paths =
                List.map
                  (fun filename ->
                     let result_filename_pattern = 
                       let re = Re.compile (Re.Posix.re "([.]json)") in 
                       let result_filename_prefix = begin
                        Re.replace_string
                           re
                           ~by:""
                           filename
                       end
                       in
                       "results_*"
                       ^ result_filename_prefix
                       ^ "*.json"
                     in
                     let host_login = 
                       match h.login_name with
                       | None -> h.host_address
                       | Some user -> user ^ "@" ^ h.host_address
                     in
                     host_login ^ ":"
                     ^ Filename.concat h.directory result_filename_pattern
                  )
                  filenames
                |> (String.concat " ")
             in
              "rsync --rsh=ssh --compress " 
              ^ files_full_paths
              ^ " "
              ^ output_directory_name
            in
            Logs.info (fun m -> m "copy command: %s\n" copy_command);
            ignore (Unix.system copy_command);
            (* check tigress-test status and if tigress-test has
               finished then stop rsyncing files*)
           let stdout_chan, stdin_chan, stderr_chan =
              let host =
                match h.login_name with
                | None -> h.host_address
                | Some user -> user ^ "@" ^ h.host_address
              in
              let command = 
                h.path_to_tigress_test 
                ^ " status "
              in
            Unix.open_process_args_full
              "ssh"
                [|host;
                  command;
                |]
              (Unix.environment ())
            in
            let process_status = Unix.close_process_full (stdout_chan, stdin_chan, stderr_chan) in
            let host_status = 
              match process_status with
              | WEXITED x -> x
              | WSIGNALED _ -> 0 (*125*)
              | WSTOPPED _ -> 0 (*126*)
            in
            (* check status at remote and exit if the remote
               run has finished. *)
            let host_filenames_recycle =
              Printf.eprintf "In sync_from_remote_helper, host_status=%d, host=%s, filenames=%s, last_sync=%b\n" 
                host_status h.host_address (String.concat "," filenames) last_sync;
              flush stderr;
              if host_status = 0
                 || host_status = 2 then (* still running *)
                (h, filenames, last_sync) :: host_filenames_recycle
              else begin if last_sync then
                host_filenames_recycle
              else (* if remote has just finished then sync one more
                      time to get the last bit of the results *)
                (h, filenames, true) :: host_filenames_recycle
              end
            in
            sync_from_remote_helper
    (*          ~eof:(not timed_out) *)
    (*          ~stdin_chan
              ~stdout_chan
              ~stderr_chan *)
              ~host_filenames_recycle
              tl
              timeout
          (*end*)
        end
        in
        let sync_from_remote
          (host_filenames : (RemoteHosts_t.host * string list) list)
          (timeout:float): unit = begin
            sync_from_remote_helper
              (List.map
                (fun (host, filename) -> (host, filename, false))
                host_filenames
              )
              timeout
          end
        in 
   let rerun_or_continue_remote_queue
    (rerun_or_continue: [`Rerun | `Continue ])
    (remote_hosts_option: [ `All | `Some of string list ])
    (rerun_arguments: string array)
    (remote_results_sync_interval_seconds: int option)
    : unit 
    = begin
      Logs.info (fun m -> m "In rerun_or_continue_queue");
             let remote_hosts = 
            (
              List.fold_left
                (fun acc (x: RemoteHosts_t.host) -> 
                  match remote_hosts_option with
                  | `All -> 
                    Logs.info (fun m -> m "Adding host: %s\n" x.host_address);
                    x :: acc
                  | `Some hostnames -> 
                    if List.exists ((=)x.host_address) hostnames then
                    begin
                      Logs.info (fun m -> m "Adding host: %s\n" x.host_address);
                      x :: acc
                    end
                    else
                      acc
                )
                []
                remote_hosts
              |> List.rev
            )
            in

         let remote_process_chans: (RemoteHosts_t.host * (in_channel * out_channel * in_channel) ) list = begin
          List.map
            (fun (h: RemoteHosts_t.host) ->
                 (* prerr_endline "In section for running parallel tests on remote host."; flush stderr; *)
                 Logs.debug (fun m -> m "In section for running parallel tests on remote host.");

                 (* then run tigress_test on the remote host, using ssh 
                     *)
                 Logs.info (fun m -> m "starting tests running on remote host, %s" h.host_address);
                 let process_chans = begin
                        let host =
                          match h.login_name with
                          | None -> h.host_address
                          | Some user -> user ^ "@" ^ h.host_address
                        in
                        let command = 
                          h.path_to_tigress_test 
                          ^ (match rerun_or_continue with
                          | `Continue ->
                            " worker-run --continue-queue "
                          | `Rerun ->
                            " worker-rerun "
                            ^ (String.concat " " (Array.to_list rerun_arguments))
                          )
                        in
                        Logs.info (fun m -> m "Remote host: %s" host);
                        Logs.info (fun m -> m "Remote command: %s" command);
                        Unix.open_process_args_full "ssh"
                          [| "ssh";
                             host; 
                             command;
                          |]
                          (Unix.environment ())
                 end
                 in
                   (h, process_chans)
            )
            remote_hosts
        end
        in
          let remote_sync_interval =
            match remote_results_sync_interval_seconds with
            | None -> float_of_int kDEFAULT_SYNC_INTERVAL_SECONDS
            | Some x -> float_of_int x
          in 
          let th: Thread.t option = begin
          match remote_process_chans with
          | [] -> None
          | _ :: _ as hosts_list ->
            Some ( Thread.create
              (sync_from_remote
    (*            ~stdin_chan
                ~stdout_chan
                ~stderr_chan *)
                (List.map
                  (fun (h, _) -> (h, [""]))
                  hosts_list
                )
                )
                remote_sync_interval
            )
          end                
          in
          (List.iter
            (fun chans ->
              ignore (Unix.close_process_full chans)
            )
            (
              List.map
                (fun (_, chans) -> chans)
                remote_process_chans
            )
          );
          (match th with
          | None -> ()
          | Some th ->
            Thread.join th
          )
        end
    in 
  let test_cases_rslt: [`Serial of [ `Seq of TestCase.OcamlT.t Seq.t * int * string option | `List of TestCase.OcamlT.t list ] * TestCase.Metadata.t * Unix.file_descr option | `Parallel of [ `ContinueResults of string list * Options.Rerun.config | `RerunResults of string list * Options.Rerun.config | `ContinueRemoteQueue of Options.Run.config | `TestCases of [ `CasesFiles of (Host.t * string list) list | `GenFiles of (Host.t * string * int list) list ] * int option]] =
    let test_cases_rslt : [`SerialSeq of TestCase.OcamlT.t Seq.t * int * string option * TestCase.Metadata.t * Unix.file_descr option | `SerialList of TestCase.OcamlT.t list * TestCase.Metadata.t * Unix.file_descr option | `Parallel of [ `ContinueResults of string list * Options.Rerun.config | `RerunResults of string list * Options.Rerun.config | `ContinueRemoteQueue of Options.Run.config | `TestCases of [ `CasesFiles of (Host.t * string list) list | `GenFiles of (Host.t * string * int list) list ] * int option]] =
      let local_host: Defaults.LocalHostSpec.t = (*config#local_host in*)
        let options = Options.Config.to_defaults config in
        let platforms: (Host.platform * Host.platform list) option =
          match options#compatible_platforms with
          | None -> None
          | Some [] -> None
          | Some (hd :: tl) -> Some (hd, tl)
        in
        let platforms =
          match platforms with
          | None -> begin
              Logs.warn (fun m -> m "Local platform not set (in config file) and could not be determined!");
              (* Act as if local machine can run tests for any platform;
                 It may attempt it and then fail.
                 TODO: consider making platform a required config parameter *)
              match TransformationDescription.Platform_j.all_of_platform with
              | [] -> failwith "No platforms found in list of platforms!"
              | hd :: tl -> (hd, tl)
          end
          | Some x -> x
        in
        {
          max_processes = options#max_processes;
          platforms = platforms;
        }
      in
      match config with
      | Run options -> begin
        Logs.info (fun m -> m "In Tigress_test, \"Run\" case");
        let rslt =
        let host_priority: HostsbyPrioritySet.t =
          List.fold_left
            (fun acc (x: RemoteHosts_t.host): HostsbyPrioritySet.t -> 
              HostsbyPrioritySet.add (
                {host_info = Remote x;
                 processes_to_allocate = x.max_processes;
                 last_allocated_counter = 0;
                }
              ) acc)
            (
            if options#remote_only then
              HostsbyPrioritySet.empty
            else
            (HostsbyPrioritySet.singleton
               { host_info = Local local_host;
                 processes_to_allocate = local_host.max_processes;
                 last_allocated_counter = 0;
               })
            )
            (
              List.fold_left
                (fun acc (x: RemoteHosts_t.host) -> 
                  match options#remote_hosts with
                  | `All -> x :: acc
                  | `Some hostnames -> 
                    if List.exists ((=)x.host_address) hostnames then
                      x :: acc
                    else
                      acc
                )
                []
                remote_hosts
              |> List.rev
            )
        in
          match options#script_file with
          | Some Script script_info -> 
            let script_filename = script_info.filename in
            Logs.info (fun m -> m "Script filename provided: %s" script_filename);
            Logs.info(*.debug*) (fun m -> m "Calling gen_test_cases_for_run");
            (*let script_with_config, transformations_by_platform, pipeline_length =*)
            let test_cases_rslt =
              (* generate test cases according to order specified in script *)
              gen_test_cases_for_run_from_script
                script_info
                options
                options#database_dir_rel
                transformation_descriptions
                source_descriptions
            in begin
              match test_cases_rslt with
              | `Full (_(*script_with_config*), test_cases) -> begin
                  (* allocate tests to hosts for parallel testing *)
                  let test_cases, _ = 
                    Seq.fold_left
                      (fun (test_cases_by_host, host_priority) test ->
                         schedule_one_test
                           test_cases_by_host
                           test
                           host_priority
                      )
                      (HostMap.empty, host_priority)
                      test_cases
                  in
                  let output_filename_prefix =
                    let current_time = 
                      Common.human_readable_current_localtime_string () 
                    in
                    "testcases_"
                    ^ (
                      script_filename
                      |> Filename.basename
                      |> Filename.remove_extension
                    )
                    ^ "_" ^ current_time
                  in
                  match options#parallel_run with
                  | false -> begin
                      match HostMap.find_opt (Local local_host) test_cases with
                      | None -> failwith "Error: no local test cases and parallel testing option not selected.  Nothing to do!"
                      | Some test_cases -> 
                        let output_filename = 
                          output_filename_prefix ^ ".json"
                        in
                        let testcases, testcases_metadata =
                          write_test_cases 
                            ~output_dir:output_directory_name
                            ~output_filename 
                            test_cases
                        in
                        `SerialList (
                          testcases
                          ,testcases_metadata
                          ,None
                        )
                    end
                  | true -> begin
                    `Parallel ( `TestCases ( `CasesFiles (
                      write_parallel_test_cases 
                        ~output_dir:output_directory_name
                        ~output_filename_prefix
                        test_cases)
                    , options#remote_results_sync_interval_seconds))
                  end
             end
              | `Gen (script_with_config, transformations_by_platform, pipeline_length) -> begin
              (* (Printf.eprintf "Number of platforms in transformations_by_platforms: %d\n" (TestScript.PlatformsMap.cardinal transformations_by_platform); *)
              (Logs.debug (fun m -> m "Number of platforms in transformations_by_platforms: %d\n" (TestScript.PlatformsMap.cardinal transformations_by_platform));
              flush stderr;
              ignore (TestScript.PlatformsMap.fold
                (fun _ v i ->
                  (* Printf.eprintf "Number of transformation for platform set %d: %d\n" i (TestScript.TestCaseTransformationSet.cardinal v); flush stderr; *)
                  Logs.debug (fun m -> m "Number of transformation for platform set %d: %d\n" i (TestScript.TestCaseTransformationSet.cardinal v));
                  i + 1
                )
                transformations_by_platform
                0
              )
              );
              let platforms_by_hosts =
                let hosts: HostSet.t =
                  match options#parallel_run with
                  | false -> begin
                    HostSet.singleton (Local local_host)
                  end
                  | true -> begin
                    HostsbyPrioritySet.fold 
                      (fun x acc ->
                        HostSet.add x.host_info acc
                      )
                      host_priority
                      HostSet.empty
                  end
                in
                TestScript.partition_platforms_by_hosts
                  transformations_by_platform
                  hosts
              in
              let test_case_generators_by_host =
                let program_spec_example: TestScript_t.program_spec_t = {
                  prog_filename = "";
                  prog_version = Some "";
                  prog_version_command = Some "";
                }
                in
                TestScript.make_generator_from_partitions
                  ~pipeline_length
                  ~max_run_seconds:script_with_config#max_run_seconds
                  ~tigress_options:script_with_config#program_default_options
                  ~compiler_options:script_with_config#compiler_options
                  ~compiler:({program_spec_example with prog_filename = (match script_with_config#path_to_c_compiler with None -> "cc" | Some x -> x)})
                  ~tigress:({program_spec_example with prog_filename = (
                    match script_with_config#path_to_program with 
       | Some x -> if Filename.is_implicit x then
         (match options#tigress_home with
          | None -> failwith "Fatal error: no path to Tigress program in configuration file and no tigress home environment variable or configuration option."
          | Some tigress_home -> Filename.concat tigress_home x
         )
       else x
       | None ->
         (match options#tigress_home with
          | None -> failwith "Fatal error: no path to Tigress program in configuration file and no tigress home environment variable or configuration option."
          | Some x -> Filename.concat x "tigress"
         )
      )})
                  ~seeds:(Array.of_list script_with_config#seeds)
                  ~source_files:(Array.of_list script_with_config#source_files)
                  ~ordering:(match script_with_config#test_order with
                        | Some x -> x
                        | None -> kDEFAULT_TEST_ORDER
                       )
                  transformations_by_platform
                  platforms_by_hosts
              in
              (* replace relative paths with absolute paths to
                 databasee locations, if the generator is to
                 be used locally.
              *)
              let test_case_generators_by_host =
                HostMap.fold
                  (fun host generator acc -> 
                    match host with
                    | Local x ->
                      let generator =
                        update_generator_source_file_paths
                          generator
                          options#database_dir
                      in
                      HostMap.add (Local x) generator acc
                    | Remote x ->
                      HostMap.add (Remote x) generator acc
                  )
                  test_case_generators_by_host
                  HostMap.empty
              in
                   let output_filename_prefix =
                    let current_time = 
                      Common.human_readable_current_localtime_string () 
                    in
                    "testcases_"
                    ^ (
                      script_filename
                      |> Filename.basename
                      |> Filename.remove_extension
                    )
                    ^ "_" ^ current_time
                  in
              (* write generators to disk *)
              let test_cases_sequences_and_file_metadata_by_host =
              (HostMap.fold
                (fun host testcases acc ->
                  (*let tigress_test_home =
                    match options#tigress_test_home with
                    | None -> Filename.current_dir_name
                    | Some x -> x 
                  in*)
                  (Printf.printf "Host: %s\n" (Host.host_address host);
                  Printf.printf "%s\n" (TestCaseGenerator.Doc_j.string_of_contents testcases));
                  let test_cases_sequences, test_cases_metadata =
                    write_test_cases_generator_for_host
                      ~output_dir:output_directory_name
                      ~output_file_prefix:output_filename_prefix
                      host
                      testcases
                  in
                  let num_transformations =
                    count_generator_transformations
                      testcases
                  in
                  let test_cases_sequences = 
                    List.combine test_cases_sequences num_transformations
                  in
                  HostMap.add
                    host
                    (test_cases_sequences, test_cases_metadata)
                    acc
                )
                test_case_generators_by_host
                HostMap.empty
              )
              in
              if options#parallel_run then begin
                `Parallel (
                  `TestCases ( `GenFiles (
                    HostMap.fold
                      (fun host (l, meta) acc ->
                        (
                          host
                          ,TestCase.Metadata.filename meta
                          ,( (* list of integers between zero and (List.length l) - 1 *)
                            fun x -> 
                              let _, l =
                                List.fold_left
                                  (fun (i_acc, l_acc) _ ->
                                    (i_acc + 1, i_acc :: l_acc)
                                  )
                                  (0, [])
                                  x
                              in
                              List.rev l
                          ) l
                        ) :: acc
                      )
                      test_cases_sequences_and_file_metadata_by_host
                      []
                    )
                    , options#remote_results_sync_interval_seconds
                  )
                )
              end
              else begin

              (* produce Sequence of test cases for localhost from generator data *)
              let tests_for_local_host =
                HostMap.find_opt
                  (Local {
                    max_processes = 0; (* dummy values *)
                    platforms = (`Linux_x86, [])
                    }
                  )
                  test_cases_sequences_and_file_metadata_by_host
              in
              let test_cases_sequences, test_cases_file_metadata = 
                match tests_for_local_host with
                | None -> ([], TestCase.Metadata.create "" 0.0 Missing)
                | Some x -> x
              in
              let test_cases_sequences, num_transformations = 
(*
                match test_cases_sequences with
                | [] -> (Seq.empty, 0)
                | x -> begin
*)
                  let seqs, nums = begin
                    List.fold_left
                      (fun (s_acc, n_acc) (s, n) ->
                        (s :: s_acc, n :: n_acc)
                      )
                      ([], [])
                      test_cases_sequences
                  end
                  in
                  let seqs = List.rev seqs in
                  let nums = List.fold_left (+) 0 nums in
                  (Common.concat_sequences_of_list seqs
                  , nums
                  )
(*                end *)
              in
              let gen_spec_indexes = 
                match options#script_file with
                | None -> None
                | Some x -> begin
                  match x with
                  | Script _ -> None
                  | TestCases x -> begin
                    match x.gen_spec_index with
                    | None -> None
                    | Some x -> begin
                      let x, _ = NonemptyList.tuple_of x in
                      Some (NonegIntRange.to_string x)
                    end
                  end
                end
              in
                `SerialSeq (test_cases_sequences, num_transformations, gen_spec_indexes, test_cases_file_metadata, None)
            end
            end

            end
          | Some TestCases _ ->
            (* Arriving at this case must indicate a programming error *)
            failwith "Fatal error: Incorrect type for file to process with \"run\" command" (* ; expected \"Script\", got \"TestCases\"" *)
          | None ->
            failwith "Fatal error: either a script filename or a test cases filename must be provided."
          in
            if options#generate_only then
              exit 0
            else
              rslt
        end
      | ContinueQueue config -> begin
        if config#remote_only then begin
          match config#remote_hosts with
          | `All 
          | `Some _ -> begin
            `Parallel (
              `ContinueRemoteQueue config
            )
          end
        end
        else 
        `Parallel ( `TestCases (`CasesFiles [(Host.Local local_host, [])]
                               ,config#remote_results_sync_interval_seconds)
                  )
        
      end
      | TestCasesRun config -> begin
          Logs.debug (fun m -> m "Calling gen_test_cases_for_run");
          match config#script_file with
          | Some TestCases test_cases_info ->
            let gen_spec_indexes = begin
                  match test_cases_info.gen_spec_index with
                  | None -> None
                  | Some x -> begin
                    let x, _ = NonemptyList.tuple_of x in
                    Some (NonegIntRange.to_string x)
                  end
              end
            in
            let test_cases =
              gen_test_cases_for_run_from_test_cases
                test_cases_info
                config#database_dir
            in begin
              match test_cases with
              | Error e -> failwith e
              | Ok x -> begin
                match x with
                | `List (test_cases, test_cases_info, fd) ->
                `SerialList (test_cases, test_cases_info, Some fd)
                | `Seq (test_cases, num_test_cases, test_cases_info, fd) -> 
                  `SerialSeq (Common.concat_sequences_of_list test_cases, List.fold_left (+) 0 num_test_cases, gen_spec_indexes, test_cases_info, Some fd)
              end
            end
          | Some Script _ ->
            (* Arriving at this case must indicate a programming error *)
            failwith "Fatal error: Incorrect type for file to process with \"run\" command" (* ; expected \"TestCases\", got \"Script\"" *)
          | None ->
            failwith "Fatal error: either a script filename or a test cases filename must be provided."
        end   
      | Continue options -> begin
          Logs.debug (fun m -> m "Calling gen_test_cases_for_continue");
          match 
            gen_test_cases_for_continue 
              options 
              output_directory_name 
              options#database_dir 
          with
          | `Single x -> begin
            match x with 
            | (Error e) -> failwith e
            | Ok x -> begin
              match x with
              | `List (test_cases, test_cases_info, fd) ->
              `SerialList (test_cases, test_cases_info, Some fd)
              | `Seq (test_cases, num_test_cases, test_cases_info, fd) ->
                `SerialSeq (List.hd test_cases, List.hd num_test_cases, None, test_cases_info, Some fd)
            end
          end
          | `Multiple (x0, x_remain) -> begin
            `Parallel (
              `ContinueResults (x0 :: x_remain, options)
            )
          end
        
      end  
      | Rerun options -> begin
        match options#remote_hosts with
        | `None -> begin
          if options#enqueue then begin
            match options#work_queue_dir with
            | None -> failwith "No work queue directory configured."
            | Some work_queue_dir -> begin
              match rerun_enqueue options work_queue_dir with
              | `Single (_, test_cases_filename) -> begin

                  `Parallel ( `TestCases ( `CasesFiles [(Host.Local local_host, [test_cases_filename])]
                               ,options#remote_results_sync_interval_seconds)
                  )
              end
              | `Multiple ((_, x0), x_remain) -> begin
                let x_remain =
                  List.map
                    (fun (_, x) -> x)
                    x_remain
                in
                let test_cases_filenames = x0 :: x_remain in
                 `Parallel ( `TestCases (`CasesFiles [(Host.Local local_host, test_cases_filenames)]
                               ,options#remote_results_sync_interval_seconds)
                  )
                
              end
            end
          end
          else begin
            Logs.debug (fun m -> m "Calling gen_test_cases_for_rerun");
            match gen_test_cases_for_rerun options output_directory_name with
            | `Single x -> begin
              match x with
              | (Error e) -> failwith e
              | Ok (test_cases, test_cases_info) ->
                `SerialList (test_cases, test_cases_info, None)
            end
            | `Multiple (x0, x_remain) -> begin
              `Parallel (
                `RerunResults (x0 :: x_remain, options)
              )
            end
          end
        end
        | `All
        | `Some _ -> begin
          `Parallel (
            `RerunResults ([], options)
          )
        end
      end
      | Adhoc options -> begin
          Logs.debug (fun m -> m "Calling gen_test_cases_for_adhoc_run");
          match gen_test_cases_for_adhoc_run options output_directory_name with
          | (Error e, _) -> failwith e
          | (Ok test_cases, test_cases_info) ->
            `SerialList (test_cases, test_cases_info, None)
        end
      | WorkerRun _
      | Status _
      | Stop _ -> failwith "Cases previously handled"
    in
      match test_cases_rslt with
      | `SerialList (l, meta, fd) -> `Serial ((`List l), meta, fd)
      | `SerialSeq (s, len, ind, meta, fd) -> `Serial ((`Seq (s, len, ind)), meta, fd)
      | `Parallel x -> `Parallel x
  in
  Fun.protect
    ~finally:(fun () ->  
      match test_cases_rslt with
      | `Serial (_, _, Some fd) ->
        Unix.lockf fd F_ULOCK 0; Unix.close fd
      | `Parallel _
      | `Serial (_, _, None) -> ()
    )
    (fun () ->
      let test_cases_rslt =
        match test_cases_rslt with
        | `Serial (x, y, _) -> `Serial (x, y)
        | `Parallel x -> `Parallel x
      in
      (match defaults#tigress_home with
       | Some tigress_home -> Unix.putenv "TIGRESS_HOME" tigress_home
       | None -> ()
      );
      match test_cases_rslt with
      | `Parallel x -> begin
        match x with 
        | `ContinueResults (filenames, options) -> begin
         let rerun_arguments =
            rerun_arguments_of_rerun_config options
          in
            Logs.info (fun m -> m "Continuing most recent local run.");
            let program_options = 
              List.map
                (fun filename ->
                  Array.append
                    [|
                      Sys.executable_name
                      ; "continue"
                      ; filename
                    |]
                    rerun_arguments
                )
                filenames
            in
            (* run parallel local processes *)
            ignore (
              Subprocess.process_pool_do
                options#max_processes
                Sys.executable_name
                program_options
                (fun x -> x) (* do nothing while waiting for processes to complete *)
                (ReverseList.empty)
              (*|> ReverseList.to_list *)
            )
          end
        | `ContinueRemoteQueue options -> begin
            Logs.info (fun m -> m "Continuing remote queues.");
            rerun_or_continue_remote_queue
              `Continue
              options#remote_hosts
              [||]
              options#remote_results_sync_interval_seconds
        end
        | `RerunResults (filenames, options) -> begin
          let remote_hosts_option:
            [`All | `Some of string list] option = begin
            match options#remote_hosts with
            | `None -> None
            | `Some x -> Some (`Some x)
            | `All -> Some (`All)
          end
          in
          let rerun_arguments =
            rerun_arguments_of_rerun_config options
          in
          match remote_hosts_option with
          | None -> begin
          let program_options = 
            List.map
              (fun filename ->
                Array.append
                  [|
                    Sys.executable_name
                    ; "rerun"
                    ; filename
                  |]
                  rerun_arguments
              )
              filenames
          in
          (* run parallel local processes *)
          ignore (
            Subprocess.process_pool_do
              options#max_processes
              Sys.executable_name
              program_options
              (fun x -> x) (* do nothing while waiting for processes to complete *)
              (ReverseList.empty)
            (*|> ReverseList.to_list *)
          )
          end
          | Some remote_hosts_option -> begin
            rerun_or_continue_remote_queue
              `Rerun
              remote_hosts_option
              rerun_arguments
              options#remote_results_sync_interval_seconds
          end
        end
        | `TestCases (test_cases, remote_results_sync_interval_seconds) -> 
          let local_test_cases, remote_test_cases =
            let local_test_cases, remote_test_cases =
              List.fold_left
                (fun (local_acc, remote_acc) (x: Host.t * string list * int list) ->
                  match x with
                  | (Local h, fns, gen_spec_indexes) -> (ReverseList.append local_acc (h, fns, gen_spec_indexes), remote_acc)
                  | (Remote h, fns, gen_spec_indexes) -> (local_acc, ReverseList.append remote_acc (h, fns, gen_spec_indexes))
                )
                (ReverseList.empty, ReverseList.empty)
                ( 
                  match test_cases with
                  | `CasesFiles x -> List.map (fun (h, l) -> (h, l, [])) x
                  | `GenFiles x -> List.map (fun (h, fn, ind) -> (h, [fn], ind)) x
                )
            in
              (ReverseList.to_list local_test_cases, 
              ReverseList.to_list remote_test_cases)
          in
        let remote_process_chans: (RemoteHosts_t.host * string list * (in_channel * out_channel * in_channel) ) list =
          List.map
            (fun ((h, filenames, indexes): RemoteHosts_t.host * string list * int list) ->
               Logs.info (fun m -> m "Host: %s\nFilenames:%s\n" 
                             (RemoteHosts_j.show_host h)
                             (String.concat ", " filenames)
                         );
                 (* prerr_endline "In section for running parallel tests on remote host."; flush stderr; *)
                 Logs.debug (fun m -> m "In section for running parallel tests on remote host.");
                 Logs.info (fun m -> m "copying test cases to remote host, %s" h.host_address);
                 (* first copy the files to the remote host, using rsync *)
                 let copy_command = 
                   let host_login = 
                     match h.login_name with
                     | None -> h.host_address
                     | Some user -> user ^ "@" ^ h.host_address
                   in
                   let files_full_paths =
                     List.map (Filename.concat output_directory_name) filenames
                     |> String.concat " "
                   in
                   "rsync --rsh=ssh " 
                   ^ files_full_paths
                   ^ " " ^ host_login ^ ":"
                   ^ h.directory 
                 in
                 Logs.info (fun m -> m "copy command: %s\n" copy_command);
                 ignore (Unix.system copy_command);

                 (* then run tigress_test on the remote host, using ssh 
                     *)
                 Logs.info (fun m -> m "starting tests running on remote host, %s" h.host_address);
                 let process_chans = begin
                    match filenames with
                    | [] -> failwith "No test cases files for parallel run."
                    | _ :: _ as filenames  -> begin
                        let filenames =
                          List.map
                            (Filename.concat h.directory)
                            filenames
                        in
                        let filenames = String.concat "," filenames in
                        let gen_spec_indexes = List.map (string_of_int) indexes in
                        let gen_spec_indexes = String.concat "," gen_spec_indexes in
                        let host =
                          match h.login_name with
                          | None -> h.host_address
                          | Some user -> user ^ "@" ^ h.host_address
                        in
                        let command = 
                          h.path_to_tigress_test 
                          ^ " worker-run "
                          ^ filenames
                          ^ (match indexes with
                             | [] -> ""
                             | _ :: _ -> " --gen-spec-index=" ^ gen_spec_indexes
                          )
                        in
                        Logs.info (fun m -> m "Remote host: %s" host);
                        Logs.info (fun m -> m "Remote command: %s" command);
                        Unix.open_process_args_full "ssh"
                          [| "ssh";
                             host; 
                             command;
                          |]
                          (Unix.environment ())
                    end
                 end
                 in
                   (h, filenames, process_chans)
            )
            remote_test_cases
        in
 begin
          let remote_sync_interval =
            match remote_results_sync_interval_seconds with
            | None -> float_of_int kDEFAULT_SYNC_INTERVAL_SECONDS
            | Some x -> float_of_int x
          in 
          let do_while_waiting =
            (fun x ->
              (* Printf.eprintf "In \"do_while_waiting\".\n"; *)
              Logs.debug (fun m -> m "In \"do_while_waiting\".\n");
              (match remote_process_chans with
              | [] -> ()
              | _ :: _ as host_filenames_list ->
                  sync_from_remote
                    (List.map
                      (fun (h, fns, _) -> (h, fns))
                      host_filenames_list
                    )
                    remote_sync_interval
              );
              process_pool_update_queued_commands
                x
            )
          in
          let commands =
            let requested_commands =
            (List.concat
              (
                List.map
                  (fun (_, filenames, gen_spec_indexes) ->
                    List.concat (
                List.map
                  (fun (filename) ->
                    match gen_spec_indexes with
                    | [] ->
                      [[|
                        Sys.executable_name;
                        "testcases-run";
                        filename
                      |]]
                    | _ :: _ ->
                      List.map
                        (fun gen_spec_index ->
                          [|
                            Sys.executable_name;
                            "testcases-run";
                            filename;
                            "--gen-spec-index";
                            string_of_int gen_spec_index
                          |]
                        )
                        gen_spec_indexes
                  )
                  filenames
                    )
                  )
                  local_test_cases
              )
            )
            in
            let _, _, commands_to_continue_from_queue_plus_requested_commands =
              Logs.debug (fun m -> m "calling processs_pool_update_queued_commands\n");
              process_pool_update_queued_commands
                (ReverseList.empty, ([], [], ReverseList.empty), requested_commands)
            in
              commands_to_continue_from_queue_plus_requested_commands
         in
                    let local_process_chans = 
                      Subprocess.process_pool_do
                        ~do_while_waiting_interval_seconds:0.0
                        defaults#max_processes
                        Sys.executable_name
                        commands
                        (do_while_waiting)
                        (ReverseList.empty)
                    in
                    let local_process_chans =
                      ReverseList.map
                        (fun (_, process_chans) -> process_chans)
                        local_process_chans
                      |> ReverseList.to_list 
                    in
                (List.iter
                  (fun (in1,out,in2) ->
                    close_in in1;close_out out;close_in in2 
                  )
                  local_process_chans);
(* Earlier we rsync'd the files from the remote while
   local testing was proceeding. Now continue rsync'ing
   from remote until remote testing concludes. *)
          let th: Thread.t option = begin
          match remote_process_chans with
          | [] -> None
          | _ :: _ as host_filenames_list ->
            Some ( Thread.create
              (sync_from_remote
    (*            ~stdin_chan
                ~stdout_chan
                ~stderr_chan *)
                (List.map
                  (fun (h, fns, _) -> (h, fns))
                  host_filenames_list
                )
                )
                remote_sync_interval
            )
          end                
          in

          (List.iter
            (fun chans ->
(*
            let pid = Unix.process_full_pid chans in
              ignore (Unix.waitpid [] pid);
*)
              ignore (Unix.close_process_full chans)
            )
            (
              List.map
                (fun (_, _, chans) -> chans)
                remote_process_chans
            )
          );
          (match th with
          | None -> ()
          | Some th ->
            Thread.join th
          )
        end
      end
      | `Serial (test_cases, test_cases_info) ->
        let test_cases_seq, num_test_cases, gen_spec_indexes =
        match test_cases with
        | `List test_cases -> begin
        Logs.info (fun m -> m "Checking for previous results for test cases.");

        let previous_results =
          match test_cases with
          | [] -> []
          | hd :: tl ->
            results_from_previous_run
              ~testcases_filename:(Some (TestCase.Metadata.filename test_cases_info))
              ~exclusive_test_numbers:
                (Some
                   (NonemptyList.create 
                      hd.test_number 
                      (List.map 
                         (fun (x: TestCase.OcamlT.t) -> x.test_number) 
                         tl
                      )
                   )
                )
              ~search_dir:output_directory_name
              (TestCase.Metadata.Checksum.to_Sha256_t_option
                 (TestCase.Metadata.checksum test_cases_info)
              )
        in
        let previous_results =
          List.map
            (TestResults.result_with_previous_result_t_to_result_t)
            previous_results
        in
        let test_cases_with_previous_results =
          (List.iter
             (fun (x: TestCase.OcamlT.t) ->
                Logs.debug (fun m -> m "test_case test#%d" x.test_number)
             )
             test_cases
          );
          (List.iter
             (fun (x: TestResults_t.result_t) ->
                Logs.debug (fun m -> m "previous_result test#%d" x.test_case.tc_test_number)
             )
             previous_results
          );
          ReverseList.zip
            ~compare: 
              (fun 
                (case: TestCase.OcamlT.t) 
                (rslt: TestResults_t.result_t) -> 
                case.test_number - rslt.test_case.tc_test_number
              )
            test_cases
            previous_results
        in      
        let test_cases_with_previous_results =
          List.filter_map
            (fun (case, rslt) ->
               match case with
               | None -> None
               | Some x -> Some (x, rslt)
            )
            (ReverseList.to_list test_cases_with_previous_results)
        in
          (
            List.to_seq test_cases_with_previous_results
           ,List.length test_cases
           ,None
          ) 
      end
      | `Seq (test_cases, len, gen_spec_indexes) -> 
        ( 
          Seq.map
            (fun x -> (x, None))
            test_cases
         ,len
         ,gen_spec_indexes
        )
    in
        (*let total_test_cases: int = List.length test_cases in*)
        Logs.app (fun m -> m "Starting to run %d test cases." num_test_cases(*total_test_cases*));
        let test_fn, status_file, results_file =
          let output_directory_name =
            match defaults#work_queue_dir with
            | None -> output_directory_name
            | Some x -> x
          in
          let status_file, _ =
            Status.File.initialize_exn 
              ~part:gen_spec_indexes
              output_directory_name 
              num_test_cases(*total_test_cases*)
              test_cases_info
          in
          let results_file =
              TestResults.File.initialize_exn 
                ~part:gen_spec_indexes
                output_directory_name 
                test_cases_info
          in
          (
            (do_test 
              defaults 
              results_file
              temporary_directory_name
            ),
            status_file,
            results_file
          )
        in
        (*let test_cases_seq = List.to_seq test_cases_with_previous_results in*)
        let test_rslts_seq: (TestCase.OcamlT.t * TestResults.t option) Seq.t = 
          Seq.map
            (fun ((case, rslt): TestCase.OcamlT.t * TestResults_t.result_t option) -> (*print_endline "!!!!";*)(case, test_fn case rslt))
            test_cases_seq
        in
        Logs.debug (fun m -> m "After Seq.map");
        Logs.info (fun m -> m "Running tests.");
        let counter: int ref = ref 0 in
        (Seq.iter
           (fun ((test, test_rslt): (TestCase.OcamlT.t * TestResults.t option)) -> 
              (if !counter mod 5 = 0 then
                 prerr_char '.'
              );
              counter := !counter + 1;
              (
                match test_rslt with
                | None -> Logs.warn (fun m -> m "test_rslt=None")
                | Some test_rslt -> 
                  (match Status.write_status status_file test_rslt with
                  | Ok _ -> ()
                  | Error _ -> 
                    Logs.err (fun m -> m "Could not write to status file!")
                  );
                  (match test_rslt |> TestResults.get_result with
                   | FAIL ->
                     prerr_string "test#";
                     prerr_int test.test_number;
                     prerr_string ":FAIL ";
                     prerr_string (
                       match test_rslt |> TestResults.get_status with
                       | TimedOut -> "(timed out) "
                       | TigressError -> "(Tigress error) "
                       | CompilerError -> "(Compiler error) "
                       | _ -> ""
                     )
                   | _ -> Logs.info (fun m -> m "test#%d:PASS" test.test_number) (* print_endline "test_rslt not FAIL" *)
                  )
              ); flush stderr
           )
           test_rslts_seq
        ); 
        prerr_char '\n';
        (* Move test cases files and status files from 
           work_queue_directory to output directory *)
        (match defaults#work_queue_dir with
        | None -> ()
        | Some work_queue_directory ->
          (* move test cases file*)
          let fn = 
              TestCase.Metadata.filename test_cases_info 
          in
          Unix.rename 
            (Filename.concat 
              work_queue_directory 
              fn)
            (Filename.concat output_directory_name fn);
          (* move results file *)
          try
            TestResults.move_result_file_exn
              ~new_location:output_directory_name
              results_file;
          with _ -> 
            Logs.err (
              fun m -> m "Error while attempting to move results file from work queue directory to output directory"
            );
          (* move status file*)
          try
            Status.move_status_file_exn 
              ~new_location:output_directory_name
              status_file
          with _ -> 
            Logs.err (
              fun m -> m "Error while attempting to move status file from work queue directory to output directory"
            )
        );
  ) (* Fun.protect for releasing file lock *)