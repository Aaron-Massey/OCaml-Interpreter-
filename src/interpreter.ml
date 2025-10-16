
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
                                                            
                                                           
type stack = stack_value list  (*Aaron Massey*)                             


(*-----------------------------------------------------*) 
(*|                  Type Validation                  |*) 
(*-----------------------------------------------------*) 


let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_digit c = c >= '0' && c <= '9'

let is_valid_name (s : string) : bool = (*Aaron Massey*)
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



let string_of_stack_value (v : stack_value) : string =  (*Aaron Massey*)
  match v with                                              
    | Int i -> string_of_int i                              
    | Str s -> s                                            
    | Name n -> n                                           
    | Bool b -> if b then ":true:" else ":false:"           
    | Error -> ":error:"                                    
    | Unit -> ":unit:"                                      

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

  let oc = open_out filename in 
  try
    List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) stack;

    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


(*-----------------------------------------------------*) 
(*|             Command Implementations               |*) 
(*-----------------------------------------------------*) 


let pushInt (n : int) (stk : stack) : stack = (*Brayden Stille*)
  Int n :: stk (*Takes an Int (n) and pushes it onto the stack*)

let pushStr (s : string) (stk : stack) : stack = (*Brayden Stille*)
  Str s :: stk (*Takes a String (s) and pushes it onto the stack*)

let pushName (name : string) (stk : stack) : stack = (*Brayden Stille*)
  Name name :: stk (*Takes a Name (name) and pushes it onto the stack*)

let pushBool (b : bool) (stk : stack) : stack = (*Brayden Stille*)
  Bool b :: stk (*Takes a Bool (b) and pushes it onto the stack*)

let pushError (stk : stack) : stack = (*Brayden Stille*)
  Error :: stk (*Takes the stack and pushes an Error onto it*)

let pushUnit (stk : stack) : stack = (*Brayden Stille*)
  Unit :: stk (*Takes the stack and pushes a Unit onto it*)

let push (arg : string) (stk : stack) : stack = 
  if String.starts_with ~prefix:"\"" arg && String.ends_with ~suffix:"\"" arg then 
    let s = String.sub arg 1 (String.length arg - 2) in
      pushStr s stk
  else 
  match arg with 
    | ":true:" -> pushBool true stk
    | ":false:" -> pushBool false stk
    | ":error:" -> pushError stk
    | ":unit:" -> pushUnit stk
    | arg when is_valid_name arg -> pushName arg stk
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk
    | _ -> pushError stk
 

let pop (stk: stack) : stack =  (*Aaron Massey*)
  match stk with                
    | [] -> pushError stk       
    | _ :: rest -> rest         

let add (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | Int a :: Int b :: rest ->
      pushInt (a + b) rest
    | Int a :: rest ->
      pushError (Int a :: rest)
    | _ -> pushError stk

let sub (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | Int a :: Int b :: rest ->
      pushInt (b - a) rest
    | Int a :: rest ->
      pushError (Int a :: rest)
    | _ -> pushError stk

let mult (stk : stack) : stack = 
  match stk with
    | Int a :: Int b :: rest ->
      pushInt (a * b) rest
    | Int a :: rest ->
      pushError (Int a :: rest)
    | _ -> pushError stk

let div (stk : stack) : stack = 
  match stk with
    | Int a :: Int b :: rest ->
      if a = 0 then
        pushError (Int a :: Int b :: rest)
      else
        pushInt (b / a) rest
    | Int a :: rest ->
      pushError (Int a :: rest)
    | _ -> pushError stk

let rem (stk : stack) : stack = 
  match stk with
    | Int a :: Int b :: rest ->
      if a = 0 then
        pushError (Int a :: Int b :: rest)
      else
        pushInt (b mod a) rest
    | Int a :: rest ->
      pushError (Int a :: rest)
    | _ -> pushError stk

let sign (stk : stack) : stack = 
  match stk with
    | Int a :: rest -> pushInt (a * -1) stk
    | _ -> pushError stk

let swap (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | a :: b :: rest -> b :: a :: rest
    | _ -> pushError stk

let tostring (stk : stack) : stack = 
  match stk with
    | [] -> pushError stk
    | v :: rest -> pushStr (string_of_stack_value v) rest

let println (stk : stack) (out : out_channel): stack = (*Aaron Massey*)
  match stk with
    | [] -> pushError stk
    | v :: rest -> Printf.fprintf out "%s\n" (string_of_stack_value v); rest


(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = (*Aaron Massey and Brayden Stille*)
  let lines = read_lines input in
  let oc = open_out output in 

  
  let rec execute (commands : string list) (stk : stack) : stack = (*Aaron Massey and Brayden Stille*)
    match commands with
    | [] -> stk
    | cmd :: rest ->
      let trimmed_cmd = String.trim cmd in
      let tokens = String.split_on_char ' ' trimmed_cmd in 
      let new_stk = 
        match tokens with
        | ["push"; arg] -> push arg stk
        | ["pop"] -> pop stk
        | ["add"] -> add stk
        | ["sub"] -> sub stk
        | ["mult"] -> mult stk 
        | ["div"] -> div stk
        | ["rem"] -> rem stk
        | ["sign"] -> sign stk
        | ["swap"] -> swap stk
        | ["toString"] -> tostring stk
        | ["println"] -> println stk oc
        | ["quit"] -> stk
        | _ -> pushError stk
      in
      if tokens = ["quit"] then 
        new_stk
      else 
        execute rest new_stk
    in
  let final_stack = execute lines [] in


  write_lines output final_stack; 
  close_out oc

(*-----------------------------------------------------*)
(*|        Manually change filenames for now          |*)
(*-----------------------------------------------------*)
let () =
  interpreter ("input10-1.txt", "output.txt")
