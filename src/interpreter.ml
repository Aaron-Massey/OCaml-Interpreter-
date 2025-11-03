(*-----------------------------------------------------*) 
(*|Interpreter Project - Aaron Massey & Brayden Stille|*) 
(*-----------------------------------------------------*) 


(*-----------------------------------------------------*) 
(*|                  Type Definitions                 |*) 
(*-----------------------------------------------------*) 


type stack_value = (*Aaron Massey*)                                        
  | Int of int
  | Float of float 
  | Str of string                                           
  | Name of string                                          
  | Bool of bool                                            
  | Error                                                   
  | Unit

type operation = (*Int ops*)
  | Add
  | Sub 
  | Mult 
  | Div   
  | Rem

type boolean_op = (*Boolean ops*)
  | And 
  | Or 
  | Not 
                                                           
type stack = stack_value list  (*Aaron Massey*)                             
type var = (stack_value * stack_value)

type environment = var list  (*Aaron Massey*)

type env_stack = (environment * stack) list 

type enviroment = (stack_value * stack_value) list  (*Aaron Massey*)

(*-----------------------------------------------------*) 
(*|                  Type Validation                  |*) 
(*-----------------------------------------------------*) 


let is_letter c : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') (*Checks if a character is a letter*)

let is_digit c : bool = c >= '0' && c <= '9' (*Checks if a character is a digit*)

let is_valid_name (s : string) : bool = (*Aaron Massey*)
  if String.length s = 0 then false (*If the string is empty, it is not a valid name*)
  else
    let first_char = s.[0] in 
    if not (is_letter first_char || first_char = '_') then (* The first character must be a letter or '_'*)
      false
    else
      let rec check_rest i = 
        if i >= String.length s then true
        else
          let char = s.[i] in
          if is_letter char || is_digit char || char = '_' then (*Every element must be either a digit, letter, or '_'*)
          check_rest (i + 1)
          else
            false
    in
    check_rest 1

let is_valid_int (s : string) : bool = (*Brayden Stille*)
  match int_of_string_opt s with (*Tries to convert the string to an int*)
    | Some _ -> true (*If string can be converted to an int it will return true*)
    | None -> false (*If string cannot be converted to an int it will return false*)

let is_valid_float (s : string) : bool = 
  match float_of_string_opt s with (*Tries to convert the string to a float*)
    | Some _ -> true (*If string can be converted to a float it will return true*)
    | None -> false (*If string cannot be converted to a float it will return false*)
 

let is_quoted_string (s : string) : bool = (*Brayden Stille*)
  String.length s >= 2 && String.get s 0 = '\"' && String.get s (String.length s - 1) = '\"' 
  (*Checks if the string is at least 2 characters long and if the first and last characters are quotes*)


let string_of_stack_value (v : stack_value) : string =  (*Aaron Massey*)
  match v with                                                
    | Int i -> string_of_int i (*Converts an Int to a string*)
    | Float f -> string_of_float f (*Converts a Float to a string*)
    | Str s -> s (*Returns the string*)                   
    | Name n -> n (*Returns the name*)                   
    | Bool b -> if b then ":true:" else ":false:" (*Converts a Bool to a string*)       
    | Error -> ":error:" (*Returns the error as a string*)     
    | Unit  -> ":unit:" (*Returns the unit as a string*)                                      


(*-----------------------------------------------------*) 
(*|                   File Handling                   |*) 
(*-----------------------------------------------------*) 


(* Read File into FIFO String List (Stack)*)
let read_lines (filename : string) : string list = (*Aaron Massey*)
 
  let ic = open_in filename in (* Open the file *) 
  let rec loop acc = (* Recursive function that adds lines to list*)
    try
      let line = input_line ic in 
      loop (line :: acc) (* Add the line to to list *)
    with End_of_file -> 
      close_in ic; (* Close the file*)
      List.rev acc (* Reverse the list so it maintains FIFO order*)
  in
  loop []


let write_lines (filename : string) (stack : stack) : unit =  (*Aaron Massey*)

  let oc = open_out filename in (*Open the output file*)
  try 
    List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) stack; (*Write each line to the output file*)

    close_out oc (*Close the output file*)
  with e ->
    close_out_noerr oc; (*Close the output file without raising an error*)
    raise e (*raises the error*)

let tokenize_command (s : string) : string list = (*Aaron Massey*)
  let s = String.trim s in (*Trims whitespace from the string*)
  if String.length s = 0 then [] (*If the string is empty, return an empty list*)
  else
    match String.index_opt s ' ' with 
      | None -> [s] 
      | Some idx ->
        let cmd = String.sub s 0 idx in
        let arg = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in [cmd; arg] 

(*-----------------------------------------------------*) 
(*|             Command Implementations               |*) 
(*-----------------------------------------------------*) 


let pushInt (n : int) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Int n :: stk, env) (*Takes an Int (n) and pushes it onto the stack*)

let pushFloat (f : float) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Float f :: stk, env) (*Takes a Float (f) and pushes it onto the stack*)

let pushStr (s : string) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Str s :: stk, env) (*Takes a String (s) and pushes it onto the stack*)

let pushName (name : string) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Name name :: stk, env) (*Takes a Name (name) and pushes it onto the stack*)

let pushBool (b : bool) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Bool b :: stk, env) (*Takes a Bool (b) and pushes it onto the stack*)

let pushError (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Error :: stk, env) (*Takes the stack and pushes an Error onto it*)

let pushUnit (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Unit :: stk, env)

let push (arg : string) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  if is_quoted_string arg then (*Checks if the argument is wrapped in quotes*)
    let s = String.sub arg 1 (String.length arg - 2) in (*Removes the quotes from the argument*)
      pushStr s stk env (*Calls the pushStr function with s as the string and the stack as stk*)
  else
  match arg with (*matches the push function with the argument*)
    | ":true:" -> pushBool true stk env (*Calls the pushBool function with true and the stack as stk*)
    | ":false:" -> pushBool false stk env (*Calls the pushBool function with false and the stack as stk*)
    | ":error:" -> pushError stk env (*Calls the pushError function with the stack as stk*)
    | ":unit:" -> pushUnit stk env (*Calls the pushUnit function with the stack as stk*)
    | arg when is_valid_name arg -> pushName arg stk env(*Calls the pushName function with arg as the name and the stack as stk*)
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk env (*Calls the pushInt function with arg converted to an int and the stack as stk*)
    | arg when is_valid_float arg -> pushFloat (float_of_string arg) stk env (*Calls the pushFloat function with arg converted to a float and the stack as stk*)
    | _ -> pushError stk env (*If the argument is not valid, calls the pushError function with the stack as stk*)
 

let pop (stk: stack) (env : env_stack): stack * env_stack=  (*Aaron Massey*)
  match stk with                
    | [] -> pushError stk env
    | _ :: rest -> (rest, env) 


let int_arithmetic (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =  (*Aaron Massey*)
  match op with 
    | Add -> ( 
      match stk with
        | Int a :: Int b :: rest -> (*If there are two ints, add them*)
          pushInt (b + a) rest env 
        | _ -> pushError stk env  (*Otherwise return the original stack with an error*)
          )
    | Sub -> (
      match stk with
      | Int a :: Int b :: rest ->
        pushInt (b - a) rest env (*If there are two Ints, subtract them *)
      | _ -> pushError stk env (*Otherwise return the original stack with an error*)
    ) 
    | Mult -> (
      match stk with 
        | Int a :: Int b :: rest ->
          pushInt (b * a) rest env  (*If there are two Ints, multiply them *)
        | _ -> pushError stk env (*Otherwise return the original stack with an error*)
    ) 
    | Div -> (
      match stk with
        | Int a :: Int b :: rest -> (*If there are two Ints, divide them *)
          if a = 0 then (*If the denominator is 0, push the Ints back onto the stack with an error*)
            pushError (Int a :: Int b :: rest) env
          else
            pushInt (b / a) rest env(*If the Ints are valid, divide them*)
        | _ -> pushError stk env  (*If there are no Ints, push an error to the stack*)
    ) 
    | Rem ->  (
      match stk with
        | Int a :: Int b :: rest -> (*If there are two Ints, get the modulo*)
          if a = 0 then
            pushError (Int a :: Int b :: rest) env (*If the denominator is 0, return the Ints to the stack and push an error*)
          else
            pushInt (b mod a) rest env (* Otherwise push the modulo (remainder) of the Ints*)
        | _ -> pushError stk env (*If there are no Ints push an error to the stack*)
    ) 

  let float_arithmetic (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =  (*Aaron Massey*)
    match op with 
      | Add -> ( 
        match stk with
          | Float a :: Float b :: rest -> (*If there are two floats, add them*)
            pushFloat (b +. a) rest env 
          | _ -> pushError stk env  (*Otherwise return the original stack with an error*)
            )
      | Sub -> (
        match stk with
        | Float a :: Float b :: rest ->
          pushFloat (b -. a) rest env (*If there are two floats, subtract them *)
        | _ -> pushError stk env (*Otherwise return the original stack with an error*)
      ) 
      | Mult -> (
        match stk with 
          | Float a :: Float b :: rest ->
            pushFloat (b *. a) rest env  (*If there are two floats, multiply them *)
          | _ -> pushError stk env (*Otherwise return the original stack with an error*)
      ) 
      | Div -> (
        match stk with
          | Float a :: Float b :: rest -> (*If there are two floats, divide them *)
            if a = 0.0 then (*If the denominator is 0, push the floats back onto the stack with an error*)
              pushError (Float a :: Float b :: rest) env
            else
              pushFloat (b /. a) rest env(*If the floats are valid, divide them*)
          | _ -> pushError stk env  (*If there are no floats, push an error to the stack*)
      ) 
      | Rem ->  (
        match stk with
          | Float a :: Float b :: rest -> (*If there are two floats, get the modulo*)
            if a = 0.0 then
              pushError (Float a :: Float b :: rest) env (*If the denominator is 0, return the floats to the stack and push an error*)
            else
              pushFloat (mod_float b a) rest env (* Otherwise push the modulo (remainder) of the floats*)
          | _ -> pushError stk env (*If there are no floats push an error to the stack*)
      )

let arithmetic_helper (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =  (*Aaron Massey*)
  match stk with 
    | Int a :: Int b :: rest -> int_arithmetic op stk env (*If there are two Ints, call the int_arithmetic function*)
    | Float a :: Float b :: rest -> float_arithmetic op stk env 
    | _ -> pushError stk env (*If there are no Ints, push an error to the stack*)
  
let sign (stk : stack) (env : env_stack): stack * env_stack = (*Aaron Massey*)
  match stk with 
    | Int a :: rest -> pushInt (a * -1) rest env (*If there is an Int, multiply it by -1*)
    | Float a :: rest -> pushFloat (a *. -1.0) rest env (*If there is a Float, multiply it by -1.0*)
    | _ -> pushError stk env (*If there is no Int, push an error to the stack*)

let swap (stk : stack) (env: env_stack): stack * env_stack = (*Aaron Massey*)
  match stk with
    | a :: b :: rest -> ((b :: a :: rest), env) (*Swaps the top two elements of the stack*)
    | _ -> pushError stk env (*If there are not enough elements to swap, push an error onto the stack*)

let tostring (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | [] -> pushError stk env (*If the stack is empty, push an error onto the stack*)
    | v :: rest -> pushStr (string_of_stack_value v) rest env (*Calls the pushStr function with the string representation of the top stack value*)

let println (out : out_channel) (stk : stack) (env : env_stack) : stack*env_stack = (*Aaron Massey*)
  match stk with
    | [] -> pushError stk env (*If the stack is empty, push an error onto the stack*)
    | v :: rest -> Printf.fprintf out "%s\n" (string_of_stack_value v); (rest, env) (*Calls the Printf.fprintf function to print the top stack value to the output channel*)



(*-----------------------------------------------------*) 
(*|               Part 2 Functions Code               |*) 
(*-----------------------------------------------------*) 


let rec fetch_from_environment_list (name : string) (env_list : environment): stack_value = (*Aaron Massey*)
  match env_list with
    | [] -> Error (*If the name is not found push an error*)
    | (n, v) :: rest -> 
          if string_of_stack_value n = name then v (*If the name is found, return the value*)
          else fetch_from_environment_list name rest (*Otherwise, continue searching the rest of the environment list*)


let rec fetch_from_env_stack (name : string) (env : env_stack): stack_value = (*Aaron Massey*)
  match env with
  | [] -> Error (* Searched all environments, not found *)
  | (current_scope, _) :: outer_scopes ->
      match fetch_from_environment_list name current_scope with
      | Error -> fetch_from_env_stack name outer_scopes (* Not in this scope, check outer *)
      | value -> value (* Found! *)


let rec check_environment_list (name : string) (env_list : environment): bool = (*Aaron Massey*)
  match env_list with
    | [] -> false (*If the name is not found, return false*)
    (*If the name is found, return true; otherwise, continue searching the rest of the environment list*)
    | (n, v) :: rest -> if string_of_stack_value n = name then true else check_environment_list name rest 

let add_to_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment = (*Aaron Massey*)
  (name, value) :: env_list (*Adds a name-value pair to the environment list*)


let remove_from_environment_list (name : stack_value) (env_list : environment): environment = (*Aaron Massey*)
  let key = string_of_stack_value name in (*Converts the name to a string*)
  List.filter(fun(n,_) -> string_of_stack_value n <> key) env_list (*Removes the name-value pair from the environment list*)


let replace_in_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment = (*Aaron Massey*)
  let env_without_name = remove_from_environment_list name env_list in (*Removes the name-value pair from the environment list*)
  add_to_environment_list name value env_without_name (*Adds the new name-value pair to the environment list*)


let rec resolve_names stk env =  (*Aaron Massey*)
  List.map(function 
    | Name n -> (*If the name is found, resolve it; otherwise, keep it as a Name*)
        let v = fetch_from_env_stack n env in
        if v = Error then Name n else v (* If not bound, keep it as a Name *)
    | other -> other 
  ) stk

let boolean_logic (op : boolean_op) (stk : stack) (env : env_stack): stack*env_stack = (*Brayden Stille*)
  match op with (*Matches boolean operations*)
    | And -> ( (*If op is And*)
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b && a) rest env (*if both are true return true else false*)
        | _ -> pushError stk env (*if not enough elements push error to the stack*)
    )
    | Or -> ( (*If op is Or*)
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b || a) rest env (*if either are true return true if neither are true return false*)
        | _ -> pushError stk env (*if not enough elements push error to the stack*)
    )
    | Not -> ( (*If op is Not*)
      match stk with
        | Bool a :: rest -> pushBool (not a) rest env  (*if true return false, if false return true*)
        | _ -> pushError stk env (*if wrong element push error to the stack*)
    )

let cat (stk : stack) (env : env_stack): stack * env_stack= (*Brayden Stille*)
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest env (*Concatenates two strings together*)
    | _ -> pushError stk env (*if not enough elements push error to the stack*)

let equal_ (stk : stack) (env : env_stack): stack * env_stack= (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest env (*if int a and b are the same return true*)
    | Float a :: Float b :: rest -> pushBool (a = b) rest env (*if float a and b are the same return true*)
    | _ -> pushError stk env (*if not enough elements push error to the stack*)

let lessThan_ (stk: stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest env (*if int b is less than int a return true*)
    | Float a :: Float b :: rest -> pushBool (b < a) rest env (*if float b is less than float a return true*)
    | _ -> pushError stk env (*if not enough elements push error to the stack*)

let assign (stk : stack) (env : env_stack) : stack * env_stack = (*Brayden Stille*)
  match env with 
  | [] -> (pushError stk []) (* Should not happen if we initialize with a global env *)
  | (current_env, old_stack) :: outer_envs -> (* Get the top environment *)
      (match stk with 
        | Int i :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Int i) current_env (*Assigns int to var*)
            else
              add_to_environment_list name_sv (Int i) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs) (* Push updated top env back *)
        | Float f :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Float f) current_env (*Assigns float to var*)
            else
              add_to_environment_list name_sv (Float f) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Bool b :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Bool b) current_env (*Assigns bool to var*)
            else
              add_to_environment_list name_sv (Bool b) current_env
          in
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Str s :: Name n :: rest -> 
          let name_sv = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_sv (Str s) current_env (*Assigns string to var*)
            else
              add_to_environment_list name_sv (Str s) current_env
          in 
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Unit  :: Name n :: rest -> 
          let name_vs = Name n in
          let new_current_env =
            if check_environment_list n current_env then
              replace_in_environment_list name_vs (Unit) current_env (*Assigns unit to var*)
            else
              add_to_environment_list name_vs (Unit) current_env
          in 
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        | Name a :: Name n :: rest ->
          let name_sv = Name n in
          let value = fetch_from_env_stack a env in
          if value = Error then
            (pushError stk env) (* Error: binding to unbound name  *)
          else
            let new_current_env =
              if check_environment_list n current_env then
                replace_in_environment_list name_sv value current_env (*Assigns value to var*)
              else
                add_to_environment_list name_sv value current_env
          in 
          (Unit::rest, (new_current_env, old_stack) :: outer_envs)
        
        | _ -> (pushError stk env) (* Pushes error, returns original state *)
      )

let if_ (stk: stack) (env : env_stack): stack*env_stack = (*Brayden Stille*)
  match stk with 
    | trueVal :: falseVal :: Bool condition :: rest ->
      if condition then
        (trueVal :: rest, env) (*If condition is true, push trueVal to the stack*)
      else
        (falseVal :: rest, env) (*If condition is false, push falseVal to the stack*)
    | _ -> (pushError stk env)

let let_ (stk: stack) (env: env_stack) : stack * env_stack = (*Brayden Stille*)
  (stk, ([], stk) :: env) (*Pushes a new environment onto the stack*)

let end_ (stk: stack) (env: env_stack) : stack * env_stack = (*Brayden Stille*)
  match env with
  | [] -> (pushError stk []) (* 'end' without matching 'let' *)
  | (current_env, stack_before_let) :: outer_env ->
      (match stk with
        | [] -> (Error :: stack_before_let, outer_env) (*Pushes error, returns original state*)
        | top_val :: _ -> (top_val :: stack_before_let, outer_env) (*Pushes the top value of the current stack to the main stack*)
      )

(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = (*Aaron Massey and Brayden Stille*)
  let lines = read_lines input in
  let oc = open_out output in  

  let exec_cmd ?(resolve=true) f (stk : stack) (env : env_stack) : (stack * env_stack) =
    let adjusted_stk = if resolve then resolve_names stk env else stk in
    f adjusted_stk env
  in

  let rec execute (commands : string list) (stk : stack) (env : env_stack): stack =
    match commands with
    | [] -> stk (*If there are no commands left return the stack*)
    | cmd :: rest -> (*If there is a command left, turn it to a string then match it with the function*)
      let trimmed_cmd = String.trim cmd in (*Trims the whitespace from the command*)
      let tokens = tokenize_command trimmed_cmd in (*Tokenizes the command into a list of strings*)
      (* Handle quit specially *)
      if tokens = ["quit"] then stk
      else
        let (new_stk, new_env) =
      
    match tokens with
          | ["push"; arg] -> exec_cmd (push arg) stk env (*Calls the push function*)
          | ["pop"] -> exec_cmd (pop) stk env (*Calls the pop function*)
          | ["add"] -> exec_cmd (arithmetic_helper Add) stk env (*Calls the arithmetic_helper with the Add operation*)
          | ["sub"] -> exec_cmd (arithmetic_helper Sub) stk env (*Calls the arithmetic_helper with the Sub operation*)
          | ["mult"] -> exec_cmd (arithmetic_helper Mult) stk env (*Calls the arithmetic_helper with the Mult operation*)
          | ["div"] -> exec_cmd (arithmetic_helper Div) stk env (*Calls the arithmetic_helper with the Div operation*)
          | ["rem"] -> exec_cmd (arithmetic_helper Rem) stk env (*Calls the arithmetic_helper with the Rem operation*)
          | ["sign"] -> exec_cmd (sign) stk env (*Calls the sign function*)
          | ["swap"] -> exec_cmd (swap) stk env (*Calls the swap function*)
          | ["toString"] -> exec_cmd (tostring) stk env (*Calls the tostring function*)
          | ["println"] -> exec_cmd (println oc) stk env  (*Calls the println function*)
          | ["cat"] -> exec_cmd (cat) stk env (*Calls the cat function*)
          | ["and"] -> exec_cmd (boolean_logic And) stk env (*Calls the boolean_logic function with And operation*)
          | ["or"] -> exec_cmd (boolean_logic Or) stk env (*Calls the boolean_logic function with Or operation*)
          | ["not"] -> exec_cmd (boolean_logic Not) stk env (*Calls the boolean_logic function with Not operation*)
          | ["equal"] -> exec_cmd (equal_) stk env (*Calls the equal_ function*)
          | ["lessThan"] -> exec_cmd (lessThan_) stk env (*Calls the lessThan_ function*)
          | ["assign"] -> exec_cmd ~resolve:false (assign) stk env (*Calls the assign function*)
          | ["if"] -> exec_cmd (if_) stk env (*Calls the if_ function*)
          | ["let"] -> exec_cmd ~resolve:false (let_) stk env (*Calls the let_ function*)
          | ["end"] -> exec_cmd ~resolve:false (end_) stk env (*Calls the end_ function*)
          | _ -> exec_cmd (pushError) stk env  (*If command is not recognized; pushError function is called*)
        in
        execute rest new_stk new_env (*Continue executing the rest of the commands*)
  in

  let final_stack = execute lines [] [ ([], []) ] in 

  List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) final_stack;
  close_out oc (*close output channel*)

(*-----------------------------------------------------*) 
(*|        Used directories to test program           |*)
(*-----------------------------------------------------*)
(*

let () =
  let directories = ["Part_1_Tests" ; "Part_2_Tests"] in
  let filenames = ["input1.txt";"input2.txt";"input3.txt";"input4.txt";"input5.txt";
                 "input6.txt";"input7.txt";"input8.txt";"input9.txt";"input10.txt"] in 
  List.iter (fun dir ->
    List.iter (fun file ->
      let input_path = dir ^ "/In/" ^ file in
      let output_path = dir ^ "/Out/" ^ String.sub file 0 (String.length file - 4) ^ "_output.txt" in
      if Sys.file_exists input_path then
        interpreter (input_path, output_path)
       else
        Printf.printf "Warning: Skipping missing file %s\n" input_path
    ) filenames
  ) directories;
  
  *)
