(*-----------------------------------------------------*) 
(*|Interpreter Project - Aaron Massey & Brayden Stille|*) 
(*-----------------------------------------------------*) 


(*-----------------------------------------------------*) 
(*|                  Type Definitions                 |*) 
(*-----------------------------------------------------*) 

type stack_value =
  | Int of int
  | Float of float
  | Str of string
  | Name of string
  | Bool of bool
  | Error
  | Unit
  | Closure of stack_value (*fun name*)
               * stack_value (*param name*)
               * environment  (*captured env*)
               * string list  (*function body*)

and environment = (stack_value * stack_value) list

type operation =
  | Add
  | Sub
  | Mult
  | Div
  | Rem

type boolean_op =
  | And
  | Or
  | Not

type stack = stack_value list

type env_stack = (environment * stack) list

(*-----------------------------------------------------*) 
(*|                  Type Validation                  |*) 
(*-----------------------------------------------------*) 


let is_letter c : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_digit c : bool = c >= '0' && c <= '9'

let is_valid_name (s : string) : bool =
  if String.length s = 0 then false
  else
    let first_char = s.[0] in 
    if not (is_letter first_char || first_char = '_') then
      false
    else
      let rec check_rest i = 
        if i >= String.length s then true
        else
          let char = s.[i] in
          if is_letter char || is_digit char || char = '_' then
            check_rest (i + 1)
          else
            false
      in
      check_rest 1

let is_valid_int (s : string) : bool =
  match int_of_string_opt s with
    | Some _ -> true
    | None -> false

let is_valid_float (s : string) : bool =
  match float_of_string_opt s with
    | Some _ -> true
    | None -> false

let is_quoted_string (s : string) : bool =
  String.length s >= 2 &&
  String.get s 0 = '\"' &&
  String.get s (String.length s - 1) = '\"'

let string_of_stack_value (v : stack_value) : string =
  match v with
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Str s -> s
    | Name n -> n
    | Bool b -> if b then ":true:" else ":false:"
    | Error -> ":error:"
    | Unit  -> ":unit:"
    | Closure (_, _, _, _) -> ":closure:"


(*-----------------------------------------------------*) 
(*|                   File Handling                   |*) 
(*-----------------------------------------------------*) 

let read_lines (filename : string) : string list =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in 
      loop (line :: acc)
    with End_of_file -> 
      close_in ic;
      List.rev acc
  in
  loop []


let write_lines (filename : string) (stack : stack) : unit =
  let oc = open_out filename in
  try 
    List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) stack;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let tokenize_command (s : string) : string list =
  let s = String.trim s in
  if String.length s = 0 then []
  else
    match String.index_opt s ' ' with 
      | None -> [s] 
      | Some idx ->
        let cmd = String.sub s 0 idx in
        let arg = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in
        [cmd; arg] 

(*-----------------------------------------------------*) 
(*|             Command Implementations               |*) 
(*-----------------------------------------------------*) 

let pushInt (n : int) (stk : stack) (env : env_stack): stack * env_stack =
  (Int n :: stk, env)

let pushFloat (f : float) (stk : stack) (env : env_stack): stack * env_stack =
  (Float f :: stk, env)

let pushStr (s : string) (stk : stack) (env : env_stack): stack * env_stack =
  (Str s :: stk, env)

let pushName (name : string) (stk : stack) (env : env_stack): stack * env_stack =
  (Name name :: stk, env)

let pushBool (b : bool) (stk : stack) (env : env_stack): stack * env_stack =
  (Bool b :: stk, env)

let pushError (stk : stack) (env : env_stack): stack * env_stack =
  (Error :: stk, env)

let pushUnit (stk : stack) (env : env_stack): stack * env_stack =
  (Unit :: stk, env)

let push (arg : string) (stk : stack) (env : env_stack): stack * env_stack =
  if is_quoted_string arg then
    let s = String.sub arg 1 (String.length arg - 2) in
    pushStr s stk env
  else
    match arg with
    | ":true:" -> pushBool true stk env
    | ":false:" -> pushBool false stk env
    | ":error:" -> pushError stk env
    | ":unit:" -> pushUnit stk env
    | arg when is_valid_name arg -> pushName arg stk env
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk env
    | arg when is_valid_float arg -> pushFloat (float_of_string arg) stk env
    | _ -> pushError stk env
 

let pop (stk: stack) (env : env_stack): stack * env_stack =
  match stk with                
    | [] -> pushError stk env
    | _ :: rest -> (rest, env) 


let arithmetic (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =
  match op with 
    | Add -> ( 
      match stk with
        | Int a :: Int b :: rest ->
          pushInt (b + a) rest env
        | Float a :: Float b :: rest ->
          pushFloat (b +. a) rest env
        | _ -> pushError stk env
          )
    | Sub -> (
      match stk with
      | Int a :: Int b :: rest ->
        pushInt (b - a) rest env
      | Float a :: Float b :: rest ->
        pushFloat (b -. a) rest env
      | _ -> pushError stk env
    ) 
    | Mult -> (
      match stk with 
        | Int a :: Int b :: rest ->
          pushInt (b * a) rest env
        | Float a :: Float b :: rest ->
          pushFloat (b *. a) rest env
        | _ -> pushError stk env
    ) 
    | Div -> (
      match stk with
        | Int a :: Int b :: rest ->
          if a = 0 then
            pushError (Int a :: Int b :: rest) env
          else
            pushInt (b / a) rest env
        | Float a :: Float b :: rest ->
          if a = 0.0 then
            pushError (Float a :: Float b :: rest) env
          else
            pushFloat (b /. a) rest env
        | _ -> pushError stk env
    ) 
    | Rem ->  (
      match stk with
        | Int a :: Int b :: rest ->
          if a = 0 then
            pushError (Int a :: Int b :: rest) env
          else
            pushInt (b mod a) rest env
        | Float a :: Float b :: rest ->
          if a = 0.0 then
            pushError (Float a :: Float b :: rest) env
          else
            pushFloat (mod_float b a) rest env
        | _ -> pushError stk env
    )    

let arithmetic_helper (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =
  match stk with 
    | Int _ :: Int _ :: _ -> arithmetic op stk env
    | Float _ :: Float _ :: _ -> arithmetic op stk env
    | _ -> pushError stk env
  
let sign (stk : stack) (env : env_stack): stack * env_stack =
  match stk with 
    | Int a :: rest -> pushInt (a * -1) rest env
    | Float a :: rest -> pushFloat (a *. -1.0) rest env
    | _ -> pushError stk env

let swap (stk : stack) (env: env_stack): stack * env_stack =
  match stk with
    | a :: b :: rest -> (b :: a :: rest, env)
    | _ -> pushError stk env

let tostring (stk : stack) (env : env_stack): stack * env_stack =
  match stk with
    | [] -> pushError stk env
    | v :: rest -> pushStr (string_of_stack_value v) rest env

let println (out : out_channel) (stk : stack) (env : env_stack) : stack*env_stack =
  match stk with
    | [] -> pushError stk env
    | v :: rest ->
        Printf.fprintf out "%s\n" (string_of_stack_value v);
        (rest, env)


(*-----------------------------------------------------*) 
(*|               Part 2 Functions Code               |*) 
(*-----------------------------------------------------*) 

let rec fetch_from_environment_list (name : string) (env_list : environment): stack_value =
  match env_list with
    | [] -> Error
    | (n, v) :: rest -> 
          if string_of_stack_value n = name then v
          else fetch_from_environment_list name rest

let rec fetch_from_env_stack (name : string) (env : env_stack): stack_value =
  match env with
  | [] -> Error
  | (current_scope, _) :: outer_scopes ->
      begin match fetch_from_environment_list name current_scope with
      | Error -> fetch_from_env_stack name outer_scopes
      | value -> value
      end

let rec check_environment_list (name : string) (env_list : environment): bool =
  match env_list with
    | [] -> false
    | (n, _) :: rest ->
        if string_of_stack_value n = name then true
        else check_environment_list name rest 

let add_to_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment =
  (name, value) :: env_list

let remove_from_environment_list (name : stack_value) (env_list : environment): environment =
  let key = string_of_stack_value name in
  List.filter (fun (n,_) -> string_of_stack_value n <> key) env_list

let replace_in_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment =
  let env_without_name = remove_from_environment_list name env_list in
  add_to_environment_list name value env_without_name

let rec resolve_names stk env =
  List.map (function 
    | Name n ->
        let v = fetch_from_env_stack n env in
        if v = Error then Name n else v
    | other -> other 
  ) stk

let boolean_logic (op : boolean_op) (stk : stack) (env : env_stack): stack*env_stack =
  match op with
    | And -> (
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b && a) rest env
        | _ -> pushError stk env
    )
    | Or -> (
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b || a) rest env
        | _ -> pushError stk env
    )
    | Not -> (
      match stk with
        | Bool a :: rest -> pushBool (not a) rest env
        | _ -> pushError stk env
    )

let cat (stk : stack) (env : env_stack): stack * env_stack =
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest env
    | _ -> pushError stk env

let equal_ (stk : stack) (env : env_stack): stack * env_stack =
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest env
    | Float a :: Float b :: rest -> pushBool (a = b) rest env
    | _ -> pushError stk env

let lessThan_ (stk: stack) (env : env_stack): stack * env_stack =
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest env
    | Float a :: Float b :: rest -> pushBool (b < a) rest env
    | _ -> pushError stk env

let assign (stk : stack) (env : env_stack) : stack * env_stack =
  match env with 
  | [] -> pushError stk []
  | (current_env, old_stack) :: outer_envs ->
      (match stk with 
        | Int i :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Int i) current_env
            else
              add_to_environment_list name_sv (Int i) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Float f :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Float f) current_env
            else
              add_to_environment_list name_sv (Float f) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Bool b :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Bool b) current_env
            else
              add_to_environment_list name_sv (Bool b) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Str s :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Str s) current_env
            else
              add_to_environment_list name_sv (Str s) current_env
          in 
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Unit  :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv Unit current_env
            else
              add_to_environment_list name_sv Unit current_env
          in 
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Name a :: Name n :: rest ->
          let name_sv = Name n in
          let value = fetch_from_env_stack a env in
          if value = Error then
            pushError stk env
          else
            let new_current_env =
              if check_environment_list n current_env then
                replace_in_environment_list name_sv value current_env
              else
                add_to_environment_list name_sv value current_env
            in 
            (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | _ -> pushError stk env
      )

let if_ (stk: stack) (env : env_stack): stack*env_stack =
  match stk with 
    | trueVal :: falseVal :: Bool condition :: rest ->
      if condition then
        (trueVal :: rest, env)
      else
        (falseVal :: rest, env)
    | _ -> pushError stk env

let let_ (stk: stack) (env: env_stack) : stack * env_stack =
  (stk, ([], stk) :: env)

let end_ (stk: stack) (env: env_stack) : stack * env_stack =
  match env with
  | [] -> pushError stk []
  | (current_env, stack_before_let) :: outer_env ->
      (match stk with
        | [] -> (Error :: stack_before_let, outer_env)
        | top_val :: _ -> (top_val :: stack_before_let, outer_env)
      )


(*-----------------------------------------------------*) 
(*|               Part 3 Functions Code               |*) 
(*-----------------------------------------------------*)

let recording : bool ref = ref false
let currentFunName : stack_value ref = ref (Name "undefined")
let currentParamName : stack_value ref = ref (Name "undefined")
let currentFunEnv : environment ref = ref ([] : environment)
let currentBody : string list ref = ref []

let funcNameValid (name : string) : bool =
  is_valid_name name &&
  name <> "true" &&
  name <> "false" &&
  name <> "error" &&
  name <> "unit"

let functionStart (stk : stack) (env : env_stack) : stack * env_stack =
  match stk with
  | Name funName :: Name param :: rest ->
      if funcNameValid funName && funcNameValid param then begin
        recording := true;
        currentFunName := Name funName;
        currentParamName := Name param;

        (match env with
         | (current_env, _) :: _ -> currentFunEnv := current_env
         | _ -> currentFunEnv := ([] : environment));

        currentBody := [];
        (Unit :: rest, env)
      end
      else
        pushError stk env
  | _ -> pushError stk env

let functionEnd (stk : stack) (env : env_stack) : stack * env_stack =
  if not !recording then
    pushError stk env
  else begin
    recording := false;

    let closure =
      Closure(
        !currentFunName,
        !currentParamName,
        !currentFunEnv,
        !currentBody
      )
    in

    match env with
    | (current_env, old_stack) :: outer ->
        let new_env =
          replace_in_environment_list !currentFunName closure current_env
        in
        (Unit :: stk, (new_env, old_stack) :: outer)

    | _ -> pushError stk env
  end

let returnFunction (stk : stack) (env : env_stack) : stack * env_stack =
  match stk with
  | v :: _ -> ([v], env)
  | _ -> ([Error], env)

let rec callFunction (stk : stack) (env : env_stack) : stack * env_stack =
  match stk with
  | arg :: Name funName :: rest ->
      let closure_value = fetch_from_env_stack funName env in

      begin match closure_value with
      | Closure(Name fn, Name param, captured_env, body) ->

          let actualValue =
            match arg with
            | Name n ->
                let v = fetch_from_env_stack n env in
                if v = Error then Error else v
            | Error -> Error
            | _ -> arg
          in

          if actualValue = Error then
            pushError stk env
          else
            let saved_env = env in
            let saved_stack = rest in

            let call_env =
              (add_to_environment_list (Name param) actualValue captured_env,
               [])
            in

            let rec run cmds fstk fenv =
              match cmds with
              | [] -> fstk
              | cmd :: more ->
                  let t = tokenize_command (String.trim cmd) in
                  let (nstk, nenv) =
                    match t with
                    | ["return"] -> returnFunction fstk fenv
                    | ["push"; a] -> push a fstk fenv
                    | ["pop"] -> pop fstk fenv
                    | ["add"] -> arithmetic_helper Add fstk fenv
                    | ["sub"] -> arithmetic_helper Sub fstk fenv
                    | ["mult"] -> arithmetic_helper Mult fstk fenv
                    | ["div"] -> arithmetic_helper Div fstk fenv
                    | ["rem"] -> arithmetic_helper Rem fstk fenv
                    | ["sign"] -> sign fstk fenv
                    | ["swap"] -> swap fstk fenv
                    | ["toString"] -> tostring fstk fenv
                    | ["cat"] -> cat fstk fenv
                    | ["and"] -> boolean_logic And fstk fenv
                    | ["or"] -> boolean_logic Or fstk fenv
                    | ["not"] -> boolean_logic Not fstk fenv
                    | ["equal"] -> equal_ fstk fenv
                    | ["lessThan"] -> lessThan_ fstk fenv
                    | ["assign"] -> assign fstk fenv
                    | ["if"] -> if_ fstk fenv
                    | ["let"] -> let_ fstk fenv
                    | ["end"] -> end_ fstk fenv
                    | ["fun"] -> functionStart fstk fenv
                    | ["funEnd"] -> functionEnd fstk fenv
                    | ["call"] -> callFunction fstk fenv
                    | _ -> pushError fstk fenv
                  in
                  run more nstk nenv
            in

            let result_stack = run body [] [call_env] in

            let ret =
              match result_stack with
              | v :: _ -> v
              | _ -> Error
            in

            (ret :: saved_stack, saved_env)

      | _ ->
          pushError stk env
      end

  | _ -> pushError stk env


(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 

let interpreter ((input : string), (output : string)) : unit =
  let lines = read_lines input in
  let oc = open_out output in  

  let exec_cmd ?(resolve=true) f (stk : stack) (env : env_stack) : (stack * env_stack) =
    let adjusted_stk = if resolve then resolve_names stk env else stk in
    f adjusted_stk env
  in

  let rec execute (commands : string list) (stk : stack) (env : env_stack): stack =
    match commands with
    | [] -> stk
    | cmd :: rest ->
      let trimmed_cmd = String.trim cmd in
      let tokens = tokenize_command trimmed_cmd in
      if tokens = ["quit"] then stk
      else
        let (new_stk, new_env) =
          match tokens with
          | ["push"; arg] -> exec_cmd (push arg) stk env
          | ["pop"] -> exec_cmd pop stk env
          | ["add"] -> exec_cmd (arithmetic_helper Add) stk env
          | ["sub"] -> exec_cmd (arithmetic_helper Sub) stk env
          | ["mult"] -> exec_cmd (arithmetic_helper Mult) stk env
          | ["div"] -> exec_cmd (arithmetic_helper Div) stk env
          | ["rem"] -> exec_cmd (arithmetic_helper Rem) stk env
          | ["sign"] -> exec_cmd sign stk env
          | ["swap"] -> exec_cmd swap stk env
          | ["toString"] -> exec_cmd tostring stk env
          | ["println"] -> exec_cmd (println oc) stk env
          | ["cat"] -> exec_cmd cat stk env
          | ["and"] -> exec_cmd (boolean_logic And) stk env
          | ["or"] -> exec_cmd (boolean_logic Or) stk env
          | ["not"] -> exec_cmd (boolean_logic Not) stk env
          | ["equal"] -> exec_cmd equal_ stk env
          | ["lessThan"] -> exec_cmd lessThan_ stk env
          | ["assign"] -> exec_cmd ~resolve:false assign stk env
          | ["if"] -> exec_cmd if_ stk env
          | ["let"] -> exec_cmd ~resolve:false let_ stk env
          | ["end"] -> exec_cmd ~resolve:false end_ stk env
          | ["fun"] -> exec_cmd functionStart stk env
          | ["funEnd"] -> exec_cmd functionEnd stk env
          | ["return"] -> exec_cmd returnFunction stk env
          | ["call"] -> exec_cmd callFunction stk env
          | _ -> exec_cmd pushError stk env
        in
        execute rest new_stk new_env
  in

  let final_stack = execute lines [] [ ([], []) ] in 
  List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) final_stack;
  close_out oc

(*-----------------------------------------------------*) 
(*|        Used directories to test program           |*)
(*-----------------------------------------------------*)

let () =
  let directories = ["Part_1_Tests" ; "Part_2_Tests" ; "Part_3_Tests"] in
  let filenames =
    [ "input1.txt"; "input2.txt"; "input3.txt"; "input4.txt"; "input5.txt";
      "input6.txt"; "input7.txt"; "input8.txt"; "input9.txt"; "input10.txt" ]
  in
  List.iter
    (fun dir ->
       List.iter
         (fun file ->
            let input_path = dir ^ "/In/" ^ file in
            let output_path =
              dir ^ "/Out/" ^
              String.sub file 0 (String.length file - 4) ^ "_output.txt"
            in
            if Sys.file_exists input_path then
              interpreter (input_path, output_path)
            else
              Printf.printf "Warning: Skipping missing file %s\n" input_path)
         filenames)
    directories
