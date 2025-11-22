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
  | Closure of string * string * string list * (stack_value * stack_value) list list

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
 
type var = (stack_value * stack_value)

type environment = var list

type env_stack = (environment * stack) list 

let invalidName = ["add" ; "sub" ; "pop" ; "push" ; "mult" ; "div" ; "push" ; "fun" ; "funEnd" ; "and" ; "or" ; "not" ;
                  "int" ; "float" ; "str" ; "bool" ; "assign" ; "if" ;
                  "unit" ; "error" ; "rem" ; "name" ; "equal" ; "lessThan" ;
                  "swap" ; "sign" ; "tostring" ; "println" ; "let" ; "end" ;
                  "cat" ; "fun" ; "funend" ; "return" ; "call" ; "inoutfun" 
                  ] (*Aaron Massey*)

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

let is_valid_float (s : string) : bool = (*Brayden Stille*)
  match float_of_string_opt s with (*Tries to convert the string to a float*)
    | Some _ -> true (*If string can be converted to a float it will return true*)
    | None -> false (*If string cannot be converted to a float it will return false*)
 
let is_quoted_string (s : string) : bool = (*Brayden Stille*)
  String.length s >= 2 && String.get s 0 = '\"' && String.get s (String.length s - 1) = '\"' 
  (*Checks if the string is at least 2 characters long and if the first and last characters are quotes*)


let string_of_stack_value (v : stack_value) : string =  (*Aaron Massey*)
  match v with                                             
    | Int i -> string_of_int i (*Convert int to string*)
    | Float f -> let s = string_of_float f in (*Convert float to string*)
                 if String.get s (String.length s - 1) = '.' then s ^ "0" else s (*Ensure float has decimal part*)
    | Str s -> s  (*Convert string to string*)               
    | Name n -> n (*Convert name to string*)
    | Bool b -> if b then ":true:" else ":false:" (*Convert bool to string*)      
    | Error -> ":error:" (*Convert error to string*)    
    | Unit  -> ":unit:" (*Convert unit to string*)  
    | Closure _ -> ":fun:" (*Convert closure to string*)

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
    with 
      End_of_file -> 
      close_in ic; (* Close the file*)
      List.rev acc (* Reverse the list so it maintains FIFO order*)
  in
  loop []

let write_lines (filename : string) (stack : stack) : unit =   (*Aaron Massey*)
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

let split_args (s : string) : string list = (*Brayden Stille*)
  let s = String.trim s in (*Trims whitespace from the string*)
  match String.index_opt s ' ' with 
  | None -> [s] (*If there is no space, return the whole string as a single argument*)
  | Some idx ->
      let a = String.sub s 0 idx in (*Get the first argument*)
      let b = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in (*Get the second argument*)
      [a; b] (*Return the arguments as a list*)

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
  (Error :: stk, env) (*Pushes an Error onto the stack*)

let pushUnit (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  (Unit :: stk, env) (*Pushes a Unit onto the stack*)

let push (arg : string) (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
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
 

let pop (stk: stack) (env : env_stack): stack * env_stack = (*Aaron Massey*)
  match stk with                
    | [] -> pushError stk env (*If the stack is empty, push an error onto the stack*)
    | _ :: rest -> (rest, env) (*Removes the top element from the stack and returns the rest of the stack*)


let arithmetic (op : operation) (stk: stack) (env : env_stack) : stack * env_stack = (*Aaron Massey*)
  match op with 
    | Add -> ( (*Addition Operation*)
      match stk with
        | Int a :: Int b :: rest -> pushInt (b + a) rest env (*Adds two integers and pushes the result onto the stack*)
        | Float a :: Float b :: rest -> pushFloat (b +. a) rest env (*Adds two floats and pushes the result onto the stack*)
       
        | Int a :: Float b :: rest -> pushFloat (b +. float_of_int a) rest env (*Adds an integer and a float, promoting the integer to float, and pushes the result onto the stack*)
        | Float a :: Int b :: rest -> pushFloat ((float_of_int b) +. a) rest env (*Adds a float and an integer, promoting the integer to float, and pushes the result onto the stack*)
        | _ -> pushError stk env (*If the stack does not contain two numbers, push an error onto the stack*)
          )
    | Sub -> ( (*Subtraction Operation*)
      match stk with
      | Int a :: Int b :: rest -> pushInt (b - a) rest env  (*Subtracts two integers and pushes the result onto the stack*)
      | Float a :: Float b :: rest -> pushFloat (b -. a) rest env  (*Subtracts two floats and pushes the result onto the stack*)
      
      | Int a :: Float b :: rest -> pushFloat (b -. float_of_int a) rest env (*Subtracts an integer and a float, promoting the integer to float, and pushes the result onto the stack*)
      | Float a :: Int b :: rest -> pushFloat ((float_of_int b) -. a) rest env (*Subtracts a float and an integer, promoting the integer to float, and pushes the result onto the stack*)
      | _ -> pushError stk env (*If the stack does not contain two numbers, push an error onto the stack*)
    ) 
    | Mult -> ( (*Multiplication Operation*)
      match stk with 
        | Int a :: Int b :: rest -> pushInt (b * a) rest env (*Multiplies two integers and pushes the result onto the stack*)
        | Float a :: Float b :: rest -> pushFloat (b *. a) rest env (*Multiplies two floats and pushes the result onto the stack*)
        
        | Int a :: Float b :: rest -> pushFloat (b *. float_of_int a) rest env (*Multiplies an integer and a float, promoting the integer to float, and pushes the result onto the stack*)
        | Float a :: Int b :: rest -> pushFloat ((float_of_int b) *. a) rest env (*Multiplies a float and an integer, promoting the integer to float, and pushes the result onto the stack*)
        | _ -> pushError stk env (*If the stack does not contain two numbers, push an error onto the stack*)
    ) 
    | Div -> ( (*Division Operation*)
      match stk with
        | Int a :: Int b :: rest -> 
          if a = 0 then pushError (Int a :: Int b :: rest) env
          else pushInt (b / a) rest env (*Divides two integers and pushes the result onto the stack*)
        | Float a :: Float b :: rest -> 
          if a = 0.0 then pushError (Float a :: Float b :: rest) env
          else pushFloat (b /. a) rest env (*Divides two floats and pushes the result onto the stack*)
        
        | Int a :: Float b :: rest -> 
          if a = 0 then pushError (Int a :: Float b :: rest) env (*Checks for division by zero when dividing an integer by a float and pushes an error onto the stack if true*)
          else pushFloat (b /. float_of_int a) rest env (*Divides an integer and a float, promoting the integer to float, and pushes the result onto the stack*)
        | Float a :: Int b :: rest -> 
          if a = 0.0 then pushError (Float a :: Int b :: rest) env (*Checks for division by zero when dividing a float by an integer and pushes an error onto the stack if true*)
          else pushFloat ((float_of_int b) /. a) rest env (*Divides a float and an integer, promoting the integer to float, and pushes the result onto the stack*)
        | _ -> pushError stk env  (*If the stack does not contain two numbers, push an error onto the stack*)
    ) 
    | Rem ->  ( (*Remainder Operation*)
      match stk with
        | Int a :: Int b :: rest -> 
          if a = 0 then pushError (Int a :: Int b :: rest) env (*Checks for division by zero when computing the remainder of two integers and pushes an error onto the stack if true*)
          else pushInt (b mod a) rest env 
        | Float a :: Float b :: rest -> 
          if a = 0.0 then pushError (Float a :: Float b :: rest) env (*Checks for division by zero when computing the remainder of two floats and pushes an error onto the stack if true*)
          else pushFloat (mod_float b a) rest env (*Computes the remainder of two floats and pushes the result onto the stack*)
        
        | Int a :: Float b :: rest -> 
          if a = 0 then pushError (Int a :: Float b :: rest) env (*Checks for division by zero when computing the remainder of an integer and a float and pushes an error onto the stack if true*)
          else pushFloat (mod_float b (float_of_int a)) rest env (*Computes the remainder of an integer and a float, promoting the integer to float, and pushes the result onto the stack*)
        | Float a :: Int b :: rest -> 
          if a = 0.0 then pushError (Float a :: Int b :: rest) env (*Checks for division by zero when computing the remainder of a float and an integer and pushes an error onto the stack if true*)
          else pushFloat (mod_float (float_of_int b) a) rest env (*Computes the remainder of a float and an integer, promoting the integer to float, and pushes the result onto the stack*)
        | _ -> pushError stk env (*If the stack does not contain two numbers, push an error onto the stack*)
    )   

let arithmetic_helper (op : operation) (stk: stack) (env : env_stack) : stack * env_stack = (*Brayden Stille*)
  match stk with 
    | Int a :: Int b :: rest -> arithmetic op stk env (*Both operands are Ints*)
    | Float a :: Float b :: rest -> arithmetic op stk env (*Both operands are Floats*)
    | Int a :: Float b :: rest -> arithmetic op stk env (*Mixed operands: Int and Float*)
    | Float a :: Int b :: rest -> arithmetic op stk env (*Mixed operands: Float and Int*)
    | _ -> pushError stk env (*If the stack does not contain two numbers, push an error onto the stack*)
  
let sign (stk : stack) (env : env_stack): stack * env_stack = (*Aaron Massey*)
  match stk with 
    | Int a :: rest -> pushInt (a * -1) rest env (*If there is an Int, multiply it by -1*)
    | _ -> pushError stk env (*If there is no Int, push an error to the stack*)

let swap (stk : stack) (env: env_stack): stack * env_stack = (*Aaron Massey*)
  match stk with
    | a :: b :: rest -> ((b :: a :: rest), env)(*Swaps the top two elements of the stack*)
    | _ -> pushError stk env (*If there are not enough elements to swap, push an error onto the stack*)

let tostring (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | [] -> pushError stk env(*If the stack is empty, push an error onto the stack*)
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
      | value -> value (* Found in current scope, return value *)


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

let rec resolve_names stk env n =  (*Aaron Massey*)
  if n <= 0 then stk  (*No more names to resolve*)
  else
  match stk with
  | [] -> [] (*If the stack is empty, return an empty list*)
  | hd :: tl -> (*Resolve the head and recurse on the tail*)
      let v = match hd with Name n -> fetch_from_env_stack n env | x -> x in (*Fetch the value from the environment if it's a Name*)
      let v_res = if v = Error then Name (match hd with Name s -> s | _ -> "") else v in (*If the value is Error, keep it as Name otherwise, use the resolved value*)
      let final_v = if v = Error then hd else v_res in (*Decide whether to keep the original Name or use the resolved value*)
      final_v :: (resolve_names tl env (n - 1)) (*Recurse on the tail with decremented n*)

let boolean_logic (op : boolean_op) (stk : stack) (env : env_stack): stack*env_stack = (*Brayden Stille*)
  match op with (*Matches boolean operations*)
    | And -> ( (*If op is And*)
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b && a) rest env (*If both are true return true else false*)
        | _ -> pushError stk env (*If not enough elements push error to the stack*)
    )
    | Or -> ( (*If op is Or*)
      match stk with
        | Bool a :: Bool b :: rest -> pushBool (b || a) rest env (*If either are true return true if neither are true return false*)
        | _ -> pushError stk env (*If not enough elements push error to the stack*)
    )
    | Not -> ( (*If op is Not*)
      match stk with
        | Bool a :: rest -> pushBool (not a) rest env  (*If true return false, if false return true*)
        | _ -> pushError stk env (*If wrong element push error to the stack*)
    )

let cat (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest env (*Concatenates two strings together*)
    | _ -> pushError stk env (*If not enough elements push error to the stack*)

let equal_ (stk : stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest env (*Compares two Ints and checks if they are equal to eachother*)
    | Float a :: Float b :: rest -> pushBool (a = b) rest env (*Compares two Floats and checks if they are equal to eachother*)
    | Int a :: Float b :: rest -> pushBool ((float_of_int a) = b) rest env (*Compares an Int and a Float, promoting the Int to Float, and checks if they are equal to eachother*)
    | Float a :: Int b :: rest -> pushBool (a = (float_of_int b)) rest env (*Compares a Float and an Int, promoting the Int to Float, and checks if they are equal to eachother*)
    | _ -> pushError stk env (*If not enough elements push error to the stack*)

let lessThan_ (stk: stack) (env : env_stack): stack * env_stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest env (*Compares two Ints and checks if the first is less than the second*)
    | Float a :: Float b :: rest -> pushBool (b < a) rest env (*Compares two Floats and checks if the first is less than the second*)
    (* Mixed: Promote Int to Float *)
    | Int a :: Float b :: rest -> pushBool (b < (float_of_int a)) rest env  (*Compares an Int and a Float, promoting the Int to Float, and checks if the first is less than the second*)
    | Float a :: Int b :: rest -> pushBool ((float_of_int b) < a) rest env (*Compares a Float and an Int, promoting the Int to Float, and checks if the first is less than the second*)
    | _ -> pushError stk env (*If not enough elements push error to the stack*)




let rec update_env_stack (name : string) (value : stack_value) (env : env_stack) : env_stack =
  match env with
  | [] -> [] 
  | (current_scope, saved_stack) :: outer_scopes ->
      (* 1. Check if the variable is in the current scope *)
      if check_environment_list name current_scope then
        let new_scope = replace_in_environment_list (Name name) value current_scope in
        (new_scope, saved_stack) :: outer_scopes
      else
        (* 2. If not in current, check if it exists in outer scopes *)
        (* We use fetch_from_env_stack to verify existence before recursing *)
        match fetch_from_env_stack name outer_scopes with
        | Error -> 
            (* 3. Not found anywhere? Define it in the CURRENT scope *)
            let new_scope = add_to_environment_list (Name name) value current_scope in
            (new_scope, saved_stack) :: outer_scopes
        | _ -> 
            (* 4. It exists in an outer scope, recurse down to update it there *)
            let updated_outer = update_env_stack name value outer_scopes in
            (current_scope, saved_stack) :: updated_outer


let assign (stk : stack) (env : env_stack) : stack * env_stack = 
  match stk with 
  | Name a :: Name n :: rest ->
     (* Case 1: Assigning from a Variable (e.g., push x; push y; assign) *)
     (* We must resolve 'a' first. *)
     let value = fetch_from_env_stack a env in
     if value = Error then (pushError stk env) 
     else
       let (current_scope, saved_stack) = List.hd env in
       let new_scope = 
         if check_environment_list n current_scope then
           replace_in_environment_list (Name n) value current_scope
         else
           add_to_environment_list (Name n) value current_scope
       in
       let new_env = (new_scope, saved_stack) :: (List.tl env) in
       (Unit::rest, new_env)

  | v :: Name n :: rest -> 
      (* Case 2: Assigning a Literal/Value (e.g., push x; push 5; assign) *)
      (* 'v' captures Int, Float, Bool, Str, Unit, Closure, or Error *)
      let (current_scope, saved_stack) = List.hd env in
      let new_scope = 
        if check_environment_list n current_scope then
          replace_in_environment_list (Name n) v current_scope
        else
          add_to_environment_list (Name n) v current_scope
      in
      let new_env = (new_scope, saved_stack) :: (List.tl env) in
      (Unit::rest, new_env)

  | _ -> (pushError stk env)

(* Helper: resolve only arguments for function calls from the list *)
let resolve_val (v : stack_value) (env : env_stack) : stack_value =
  match v with
  | Name n -> 
      let res = fetch_from_env_stack n env in
      if res = Error then Error else res
  | x -> x

let if_ (stk: stack) (env : env_stack): stack*env_stack = 
  match stk with 
    | trueVal :: falseVal :: condition :: rest ->
      (* Manually resolve ONLY the condition (3rd item), preserving branches *)
      let cond_val = resolve_val condition env in
      (match cond_val with
       | Bool c -> if c then (trueVal :: rest, env) else (falseVal :: rest, env)
       | _ -> pushError stk env)
    | _ -> (pushError stk env)

let let_ (stk: stack) (env: env_stack) : stack * env_stack = 
  (stk, ([], stk) :: env) 

let end_ (stk: stack) (env: env_stack) : stack * env_stack = 
  match env with
  | [] -> (pushError stk []) 
  | (current_env, stack_before_let) :: outer_env ->
      (match stk with
        | [] -> (Error :: stack_before_let, outer_env) 
        | top_val :: _ -> (top_val :: stack_before_let, outer_env) 
      )

(*-----------------------------------------------------*) 
(*|               Part 3 Functions Code               |*) 
(*-----------------------------------------------------*) 

let funcNameValid (name : string) : bool = (*Brayden Stille*)
  let name = String.lowercase_ascii name in (*Convert the name to lowercase for case-insensitive comparison*)
  not (List.mem name invalidName) (*Check if the name is not in the list of invalid names*)

let capture_environment (env : env_stack) : (stack_value * stack_value) list list = (*Brayden Stille*)
  List.map (fun (e, _) -> e) env (*Extracts only the environment part from each scope in the env_stack*)

let reconstruct_environment (captured : (stack_value * stack_value) list list) : env_stack = (*Aaron Massey*)
  List.map (fun e -> (e, [])) captured (*Reconstructs the env_stack from the captured environment lists, initializing each stack as empty*)

let rec extract_body (commands : string list) (acc : string list) (depth : int) : string list * string list = (*Aaron Massey*)
  match commands with
  | [] -> (List.rev acc, []) (*If there are no more commands, return the accumulated body and an empty list*)
  | cmd :: rest ->
      let tokens = tokenize_command cmd in (*Tokenizes the current command*)
      match tokens with
      | ["fun"; _] | ["inOutFun"; _] -> extract_body rest (cmd :: acc) (depth + 1) (*If a nested function is found, increase depth and continue accumulating*)
      | ["funEnd"] -> 
          if depth = 0 then (List.rev acc, rest) (*If the depth is zero, return the accumulated body and the remaining commands*)
          else extract_body rest (cmd :: acc) (depth - 1) (*If inside a nested function, decrease depth and continue accumulating*)
      | _ -> extract_body rest (cmd :: acc) depth (*For other commands, continue accumulating without changing depth*)


(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = (*Aaron Massey and Brayden Stille*) 
  let lines = read_lines input in
  let oc = open_out output in  

  let exec_cmd ?(n_args=0) f (stk : stack) (env : env_stack) : (stack * env_stack) =
    let adjusted_stk = resolve_names stk env n_args in
    f adjusted_stk env
  in

  let rec execute (commands : string list) (stk : stack) (env : env_stack): stack * env_stack =
    match commands with
    | [] -> (stk, env) 
    | cmd :: rest -> 
      let trimmed_cmd = String.trim cmd in 
      let tokens = tokenize_command trimmed_cmd in 
      
      if tokens = ["quit"] then (stk, env)
      else if tokens = ["return"] then (stk, env)
      else if tokens = ["funEnd"] then (pushError stk env)
      else
        
        match tokens with
        | ["fun"; args] | ["inOutFun"; args] -> 
             let parts = split_args args in
             (match parts with
              | [funName; argName] ->
                  begin
                    if funcNameValid funName && funcNameValid argName then
                      let (body, remaining) = extract_body rest [] 0 in
                      let funType = if List.hd tokens = "inOutFun" then "inOutFun" else "fun" in
                      let captured_env_data = capture_environment env in 
                      let closure = Closure(funType, argName, body, captured_env_data) in
                      
                      let (current_scope, old_s) = List.hd env in
                      let new_scope = 
                        if check_environment_list funName current_scope then
                          replace_in_environment_list (Name funName) closure current_scope
                        else
                          add_to_environment_list (Name funName) closure current_scope
                      in
                      let new_env = (new_scope, old_s) :: (List.tl env) in
                      
                      let (s, e) = pushUnit stk new_env in
                      execute remaining s e
                    else
                      let (s, e) = pushError stk env in 
                      execute rest s e
                  end
              | _ -> 
                  let (s, e) = pushError stk env in 
                  execute rest s e
             )
        | ["call"] -> 
             let (call_stk, call_env) = exec_cmd ~n_args:0 (fun s e -> (s, e)) stk env in
             (match call_stk with
              | arg_item :: func_item :: stack_rest ->
                  let closure_val = resolve_val func_item call_env in
                  let arg_val = resolve_val arg_item call_env in

                 (match closure_val with
                   | Closure(ftype, paramName, body, saved_env_data) ->
                       if arg_val = Error then match pushError stk env with (s, e) -> execute rest s e
                       else
                         let base_env = reconstruct_environment saved_env_data in
                         
                         let (top_scope, top_stack_ignore) = List.hd base_env in
                         let new_scope = add_to_environment_list (Name paramName) arg_val top_scope in
                         
                         let exec_env = (new_scope, []) :: (List.tl base_env) in
                         
                         let (res_stack, res_env) = execute body [] exec_env in

                         let ret_val = if res_stack = [] then Error else List.hd res_stack in
                         let restored_stack = ret_val :: stack_rest in
                         
                         let final_env = 
                           if ftype = "inOutFun" then
                             match arg_item with
                             | Name actual_name_str ->
                                 let final_param_val = fetch_from_env_stack paramName res_env in
                                 if final_param_val = Error then call_env
                                 else update_env_stack actual_name_str final_param_val call_env
                             | _ -> call_env
                           else
                             call_env
                         in
                         execute rest restored_stack final_env

                   | _ -> match pushError stk env with (s, e) -> execute rest s e
                  )
              | _ -> match pushError stk env with (s, e) -> execute rest s e
             )

        | _ -> 
          let (new_stk, new_env) =
            match tokens with
            | ["push"; arg] -> exec_cmd ~n_args:0 (push arg) stk env 
            | ["pop"] -> exec_cmd (pop) stk env 
            | ["add"] -> exec_cmd ~n_args:2 (arithmetic_helper Add) stk env 
            | ["sub"] -> exec_cmd ~n_args:2 (arithmetic_helper Sub) stk env 
            | ["mult"] -> exec_cmd ~n_args:2 (arithmetic_helper Mult) stk env 
            | ["div"] -> exec_cmd ~n_args:2 (arithmetic_helper Div) stk env 
            | ["rem"] -> exec_cmd ~n_args:2 (arithmetic_helper Rem) stk env 
            | ["sign"] -> exec_cmd ~n_args:1 (sign) stk env 
            | ["swap"] -> exec_cmd (swap) stk env 
            | ["toString"] -> exec_cmd ~n_args:0 (tostring) stk env 
            | ["println"] -> exec_cmd ~n_args:0 (println oc) stk env  
            | ["cat"] -> exec_cmd ~n_args:2 (cat) stk env 
            | ["and"] -> exec_cmd ~n_args:2 (boolean_logic And) stk env 
            | ["or"] -> exec_cmd ~n_args:2 (boolean_logic Or) stk env 
            | ["not"] -> exec_cmd ~n_args:1 (boolean_logic Not) stk env 
            | ["equal"] -> exec_cmd ~n_args:2 (equal_) stk env 
            | ["lessThan"] -> exec_cmd ~n_args:2 (lessThan_) stk env 
            | ["assign"] -> exec_cmd ~n_args:0 (assign) stk env 
            | ["if"] -> exec_cmd ~n_args:0 (if_) stk env
            | ["let"] -> exec_cmd ~n_args:0 (let_) stk env 
            | ["end"] -> exec_cmd ~n_args:0 (end_) stk env 
            | _ -> exec_cmd (pushError) stk env  
          in
          execute rest new_stk new_env 
  in

  let _ = execute lines [] [ ([], []) ] in
  close_out oc
(*-----------------------------------------------------*) 
(*|        Used directories to test program           |*)
(*-----------------------------------------------------*)

(*
let () = (*Used to test outputs*)
  let directories = ["Part_1_Tests" ; "Part_2_Tests" ; "Part_3_Tests"] in
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
