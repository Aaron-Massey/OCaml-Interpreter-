
(*-----------------------------------------------------*) 
(*|Interpreter Project - Aaron Massey & Brayden Stille|*) 
(*-----------------------------------------------------*) 


(*-----------------------------------------------------*) 
(*|                  Type Definitions                 |*) 
(*-----------------------------------------------------*) 


type stack_value = (*Aaron Massey*)                                        
  | Int of int                                              
  | Str of string                                           
  | Name of string                                          
  | Bool of bool                                            
  | Error                                                   
  | Unit

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
                                                           
type stack = stack_value list  (*Aaron Massey*)                             
type var = (stack_value * stack_value)
type enviroment = var list  (*Aaron Massey*)

(*-----------------------------------------------------*) 
(*|                  Type Validation                  |*) 
(*-----------------------------------------------------*) 


let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') (*Checks if a character is a letter*)

let is_digit c = c >= '0' && c <= '9' (*Checks if a character is a digit*)

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

let is_quoted_string (s : string) : bool = (*Brayden Stille*)
  String.length s >= 2 && String.get s 0 = '\"' && String.get s (String.length s - 1) = '\"' 
  (*Checks if the string is at least 2 characters long and if the first and last characters are quotes*)


let string_of_stack_value (v : stack_value) : string =  (*Aaron Massey*)
  match v with                                              
    | Int i -> string_of_int i (*Converts an Int to a string*)                      
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


let pushInt (n : int) (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Int n :: stk, env) (*Takes an Int (n) and pushes it onto the stack*)

let pushStr (s : string) (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Str s :: stk, env) (*Takes a String (s) and pushes it onto the stack*)

let pushName (name : string) (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Name name :: stk, env) (*Takes a Name (name) and pushes it onto the stack*)

let pushBool (b : bool) (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Bool b :: stk, env) (*Takes a Bool (b) and pushes it onto the stack*)

let pushError (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Error :: stk, env) (*Takes the stack and pushes an Error onto it*)

let pushUnit (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  (Unit :: stk, env)

let push (arg : string) (stk : stack) (env : enviroment): stack * enviroment = (*Brayden Stille*)
  if is_quoted_string arg then (*Checks if the argument is wrapped in quotes*)
    let s = String.sub arg 1 (String.length arg - 2) in (*Removes the quotes from the argument*)
      pushStr s stk env (*Calls the pushStr function with s as the string and the stack as stk*)
  else
  match arg with (*matches the push function with the argument*)
    | ":true:" -> pushBool true stk env(*Calls the pushBool function with true and the stack as stk*)
    | ":false:" -> pushBool false stk env(*Calls the pushBool function with false and the stack as stk*)
    | ":error:" -> pushError stk env(*Calls the pushError function with the stack as stk*)
    | ":unit:" -> pushUnit stk env(*Calls the pushUnit function with the stack as stk*)
    | arg when is_valid_name arg -> pushName arg stk env(*Calls the pushName function with arg as the name and the stack as stk*)
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk env (*Calls the pushInt function with arg converted to an int and the stack as stk*)
    | _ -> pushError stk env (*If the argument is not valid, calls the pushError function with the stack as stk*)
 

let pop (stk: stack) (env : enviroment): stack * enviroment=  (*Aaron Massey*) (*Might need to rework*)
  match stk with                
    | [] -> pushError stk env
    | _ :: rest -> (rest, env) 


let int_arithmatic (op : operation) (stk: stack) (env : enviroment) : stack * enviroment = 
  match op with 
    | Add -> ( 
      match stk with
        | Int a :: Int b :: rest -> (*If there are two ints, add them*)
          pushInt (b + a) rest env 
        | _ -> pushError stk env  (*Otherwise return the original stack with an error *)
          )
    | Sub -> (
      match stk with
      | Int a :: Int b :: rest ->
        pushInt (b - a) rest env 
      | _ -> pushError stk env 
    ) 
    | Mult -> (
      match stk with 
        | Int a :: Int b :: rest ->
          pushInt (b * a) rest env 
        | _ -> pushError stk env 
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
let arithmatic_helper (op : operation) (stk: stack) (env : enviroment) : stack * enviroment = 
  match stk with 
    | Int a :: Int b :: rest -> int_arithmatic op stk env
    | _ -> pushError stk env

let sign (stk : stack) (env : enviroment): stack * enviroment = (*Aaron Massey*)
  match stk with 
    | Int a :: rest -> pushInt (a * -1) rest env (*If there is an Int, multiply it by -1*)
    | _ -> pushError stk env (*If there is no Int, push an error to the stack*)

let swap (stk : stack) (env: enviroment): stack * enviroment = (*Aaron Massey*)
  match stk with
    | a :: b :: rest -> ((b :: a :: rest), env)(*Swaps the top two elements of the stack*)
    | _ -> pushError stk env (*If there are not enough elements to swap, push an error onto the stack*)

let tostring (stk : stack) (env: enviroment): stack * enviroment = (*Brayden Stille*)
  match stk with
    | [] -> pushError stk env(*If the stack is empty, push an error onto the stack*)
    | v :: rest -> pushStr (string_of_stack_value v) rest env (*Calls the pushStr function with the string representation of the top stack value*)

let println (out : out_channel) (stk : stack) (env: enviroment) : stack*enviroment = (*Aaron Massey*)
  match stk with
    | [] -> pushError stk env (*If the stack is empty, push an error onto the stack*)
    | v :: rest -> Printf.fprintf out "%s\n" (string_of_stack_value v); (rest, env) (*Calls the Printf.fprintf function to print the top stack value to the output channel*)



(*-----------------------------------------------------*) 
(*|               Part 2 Functions Code               |*) 
(*-----------------------------------------------------*) 

let rec check_environment (name : string) (env : enviroment): bool =
  match env with
    | [] -> false
    | (n, v) :: rest -> if string_of_stack_value n = name then true else check_environment name rest

let add_to_environment (name : stack_value) (value : stack_value) (env : enviroment): enviroment =
  (name, value) :: env 

let remove_from_environment (name : stack_value) (env : enviroment): enviroment =
  let key = string_of_stack_value name in
  List.filter(fun(n,_) -> string_of_stack_value n <> key) env

let rec fetch_from_environment (name : string) (env : enviroment): stack_value =
  match env with
    | [] -> Error 
    | (n, v) :: rest -> 
          if string_of_stack_value n = name then v  
          else fetch_from_environment name rest

let replace_in_environment (name : stack_value) (value : stack_value) (env : enviroment): enviroment =
  let env_without_name = remove_from_environment name env in
  add_to_environment name value env_without_name

let rec resolve_names stk env = 
  List.map(function 
    | Name n ->
        let v = fetch_from_environment n env in
        if v = Error then Name n else v
    | other -> other 
  ) stk

let boolean_logic (op : boolean_op) (stk : stack) (env : enviroment): stack*enviroment =
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

let cat (stk : stack) (env : enviroment): stack * enviroment=
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest env 
    | _ -> pushError stk env 

let equal_ (stk : stack) (env : enviroment): stack * enviroment= 
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest env 
    | _ -> pushError stk env 

let lessThan_ (stk: stack) (env : enviroment): stack * enviroment = 
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest env 
    | _ -> pushError stk env

let assign (stk : stack) (env : enviroment) : stack * enviroment= 
  match stk with 
    | Int i :: Name n :: rest -> 
      let name_sv = Name n in
      let env_new =
        if check_environment n env then
        replace_in_environment name_sv (Int i) env
        else
        add_to_environment name_sv (Int i) env
      in
      (Unit::rest, env_new)
    | Bool b :: Name n :: rest -> 
      let name_sv = Name n in
      let env_new =
        if check_environment n env then
        replace_in_environment name_sv (Bool b) env
        else
        add_to_environment name_sv (Bool b) env
      in
      (Unit::rest, env_new)
    | Str s :: Name n :: rest -> 
      let name_sv = Name n in
      let env_new =
        if check_environment n env then
        replace_in_environment name_sv (Str s) env
        else
        add_to_environment name_sv (Str s) env
      in 
      (Unit::rest, env_new)
    | Unit  :: Name n :: rest -> 
      let name_vs = Name n in
      let env_new =
        if check_environment n env then
        replace_in_environment name_vs (Unit) env
        else
        add_to_environment name_vs (Unit) env
      in 
      (Unit::rest, env_new)
    | Name a :: Name n :: rest ->
      let name_sv = Name n in
      let value = fetch_from_environment a env in
      if value = Error then
        pushError stk env
      else
        let env_new =
          if check_environment n env then
          replace_in_environment name_sv value env
          else
          add_to_environment name_sv value env
        in 
        (Unit::rest, env_new)
    | _ :: rest -> pushError rest env 
    | [] -> pushError stk env

let if_ (stk: stack) (env : enviroment): stack*enviroment = 
  match stk with 
    | trueVal :: falseVal :: Bool condition :: rest ->
      if condition then
        (trueVal :: rest, env) 
      else
        (falseVal :: rest, env) 
    | _ -> pushError stk env 

let let_ (stk: stack) (env : enviroment) : stack * enviroment =
  stk,env (*STUB*)

  
let end_ (stk: stack) (env : enviroment): stack * enviroment=
  stk,env (*STUB*)



(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = (*Aaron Massey and Brayden Stille*)
  let lines = read_lines input in
  let oc = open_out output in  
  
  let rec exec_cmd ?(resolve=true) f rest (stk : stack) (env : enviroment) =
    let adjusted_stk = if resolve then resolve_names stk env else stk in
    let (new_stk, new_env) = f adjusted_stk env in
    execute rest new_stk new_env
  and execute (commands : string list) (stk : stack) (env : enviroment): stack = (*Aaron Massey and Brayden Stille*)
    match commands with
    | [] -> stk (*If there are no commands left return the stack*)
    | cmd :: rest -> (*If there is a command left, turn it to a string then match it with the function*)
      let trimmed_cmd = String.trim cmd in (*Trims the whitespace from the command*)
      let tokens = tokenize_command trimmed_cmd in (*Tokenizes the command into a list of strings*)
      let new_stk = (*executes the command and returns the new stack*)
        match tokens with
        | ["push"; arg] -> exec_cmd (push arg) rest stk env
        | ["pop"] -> exec_cmd pop rest stk env
        | ["add"] -> exec_cmd (arithmatic_helper Add) rest stk env
        | ["sub"] -> exec_cmd (arithmatic_helper Sub) rest stk env
        | ["mult"] -> exec_cmd (arithmatic_helper Mult) rest stk env
        | ["div"] -> exec_cmd (arithmatic_helper Div) rest stk env
        | ["rem"] -> exec_cmd (arithmatic_helper Rem) rest stk env
        | ["sign"] -> exec_cmd sign rest stk env 
        | ["swap"] -> exec_cmd swap rest stk env 
        | ["toString"] -> exec_cmd tostring rest stk env 
        | ["println"] -> exec_cmd (println oc) rest stk env  
        | ["quit"] -> (stk) (*If command is quit; return the stack and stop executing*)
        | ["cat"] -> exec_cmd cat rest stk env 
        | ["and"] -> exec_cmd (boolean_logic And) rest stk env
        | ["or"] -> exec_cmd (boolean_logic Or) rest stk env
        | ["not"] -> exec_cmd (boolean_logic Not) rest stk env
        | ["equal"] -> exec_cmd equal_ rest stk env 
        | ["lessThan"] -> exec_cmd lessThan_ rest stk env 
        | ["assign"] -> exec_cmd ~resolve:false assign rest stk env 
        | ["if"] -> exec_cmd if_ rest stk env 
        | ["let"] -> exec_cmd let_ rest stk env 
        | ["end"] -> exec_cmd end_ rest stk env 
        | _ -> exec_cmd pushError rest stk env  (*If command is not recognized; pushError function is called*)
      in
      if tokens = ["quit"] then (*If command is quit; return the stack and stop executing*)
        new_stk (*Return the new stack*)
      else
        execute rest new_stk env (*Continue executing the rest of the commands*)
    in
let final_stack = execute lines [] [] in (*Start executing the commands with an empty stack*)


  write_lines output final_stack; (*Write the final stack to the output file*)
  close_out oc (*close output channel*)

(*-----------------------------------------------------*) 
(*|        Manually change filenames for now          |*)
(*-----------------------------------------------------*)
let () =
  let directories = ["Part_1_Tests" ; "Part_2_Tests"] in
  let filenames = ["input1.txt";"input2.txt";"input3.txt";"input4.txt";"input5.txt";
                 "input6.txt";"input7.txt";"input8.txt";"input9.txt";"input10.txt"] in 
  List.iter (fun dir ->
    List.iter (fun file ->
      let input_path = dir ^ "/In/" ^ file in
      let output_path = dir ^ "/Out/" ^ String.sub file 0 (String.length file - 4) ^ "_output.txt" in
      interpreter (input_path, output_path)
    ) filenames
  ) directories;
