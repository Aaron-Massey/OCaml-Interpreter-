
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
    | Unit -> ":unit:" (*Returns the unit as a string*)                                      

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

let push (arg : string) (stk : stack) : stack = (*Brayden Stille*)
  if is_quoted_string arg then (*Checks if the argument is wrapped in quotes*)
    let s = String.sub arg 1 (String.length arg - 2) in (*Removes the quotes from the argument*)
      pushStr s stk (*Calls the pushStr function with s as the string and the stack as stk*)
  else
  match arg with (*matches the push function with the argument*)
    | ":true:" -> pushBool true stk (*Calls the pushBool function with true and the stack as stk*)
    | ":false:" -> pushBool false stk (*Calls the pushBool function with false and the stack as stk*)
    | ":error:" -> pushError stk (*Calls the pushError function with the stack as stk*)
    | ":unit:" -> pushUnit stk (*Calls the pushUnit function with the stack as stk*)
    | arg when is_valid_name arg -> pushName arg stk (*Calls the pushName function with arg as the name and the stack as stk*)
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk (*Calls the pushInt function with arg converted to an int and the stack as stk*)
    | _ -> pushError stk (*If the argument is not valid, calls the pushError function with the stack as stk*)
 

let pop (stk: stack) : stack =  (*Aaron Massey*) (*Might need to rework*)
  match stk with                
    | [] -> pushError stk
    | _ :: rest -> rest

let add (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | Int a :: Int b :: rest -> (*If there are two Ints then add them*)
      pushInt (a + b) rest
    | Int a :: rest -> (*If there is only one Int, push it back onto the stack with an error*)
      pushError (Int a :: rest)
    | _ -> pushError stk (*If there is no Int, push an error*)

let sub (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | Int a :: Int b :: rest -> (*If there are two Ints, subtract them*)
      pushInt (b - a) rest
    | Int a :: rest -> (*If there is only one Int, push it back onto the stack with an error*)
      pushError (Int a :: rest)
    | _ -> pushError stk (*If there are no Ints, push an error onto the stack*)

let mult (stk : stack) : stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest ->(*If there are two Ints, multiply them*)
      pushInt (a * b) rest
    | Int a :: rest -> (*If there is only one Int, push it back onto the stack with an error*)
      pushError (Int a :: rest)
    | _ -> pushError stk (*If there are no Ints. push an error onto the stack*)

let div (stk : stack) : stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> (*If there are two Ints, divide them *)
      if a = 0 then (*If the denominator is 0, push the Ints back onto the stack with an error*)
        pushError (Int a :: Int b :: rest)
      else
        pushInt (b / a) rest (*If the Ints are valid, divide them*)
    | Int a :: rest ->
      pushError (Int a :: rest) (*If there is only one Int, push it back onto the stack with an error*)
    | _ -> pushError stk (*If there are no Ints, push an error to the stack*)

let rem (stk : stack) : stack = (*Brayden Stille*)
  match stk with
    | Int a :: Int b :: rest -> (*If there are two Ints, get the modulo*)
      if a = 0 then
        pushError (Int a :: Int b :: rest) (*If the denominator is 0, return the Ints to the stack and push an error*)
      else
        pushInt (b mod a) rest (* Otherwise push the modulo (remainder) of the Ints*)
    | Int a :: rest ->
      pushError (Int a :: rest) (*If there is only one Int push it to the stack with an error*)
    | _ -> pushError stk (*If there are no Ints push an error to the stack*)

let sign (stk : stack) : stack = (*Brayden Stille*)
  match stk with
    | Int a :: rest -> pushInt (a * -1) rest (*Multiplies the top element of the stack by -1 and pushes it back onto the stack*)
    | _ -> pushError stk (*If the top element is not an integer, push an error onto the stack*)

let swap (stk : stack) : stack = (*Aaron Massey*)
  match stk with
    | a :: b :: rest -> b :: a :: rest (*Swaps the top two elements of the stack*)
    | _ -> pushError stk (*If there are not enough elements to swap, push an error onto the stack*)

let tostring (stk : stack) : stack = (*Brayden Stille*)
  match stk with
    | [] -> pushError stk (*If the stack is empty, push an error onto the stack*)
    | v :: rest -> pushStr (string_of_stack_value v) rest (*Calls the pushStr function with the string representation of the top stack value*)

let println (stk : stack) (out : out_channel): stack = (*Aaron Massey*)
  match stk with
    | [] -> pushError stk (*If the stack is empty, push an error onto the stack*)
    | v :: rest -> Printf.fprintf out "%s\n" (string_of_stack_value v); rest (*Calls the Printf.fprintf function to print the top stack value to the output channel*)



(*-----------------------------------------------------*) 
(*|               Part 2 Functions Code               |*) 
(*-----------------------------------------------------*) 

let cat (stk : stack) : stack =
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest
    | _ -> pushError stk

let and_ (stk : stack) : stack = 
  match stk with
    | Bool a :: Bool b :: rest -> pushBool (b && a) rest
    | _ -> pushError stk

let or_ (stk: stack) : stack = 
  match stk with
    | Bool a :: Bool b :: rest -> pushBool (b || a) rest
    | _ -> pushError stk

let not_ (stk: stack) : stack = 
  match stk with
    | Bool a :: rest -> pushBool (not a) rest
    | _ -> pushError stk

let equal_ (stk: stack) : stack = 
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest
    | _ -> pushError stk

let lessThan_ (stk: stack) : stack = 
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest
    | _ -> pushError stk

let assign (stk: stack) : stack = 
  stk (*STUB*)

let if_ (stk: stack) : stack = 
  match stk with 
    | trueVal :: falseVal :: Bool condition :: rest ->
      if condition then
        trueVal :: rest
      else
        falseVal :: rest
    | _ -> pushError stk

let let_ (stk: stack) : stack =
  stk (*STUB*)

  
let end_ (stk: stack) : stack =
  stk (*STUB*)



(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = (*Aaron Massey and Brayden Stille*)
  let lines = read_lines input in
  let oc = open_out output in 

  
  let rec execute (commands : string list) (stk : stack) : stack = (*Aaron Massey and Brayden Stille*)
    match commands with
    | [] -> stk (*If there are no commands left return the stack*)
    | cmd :: rest -> (*If there is a command left, turn it to a string then match it with the function*)
      let trimmed_cmd = String.trim cmd in (*Trims the whitespace from the command*)
      let tokens = tokenize_command trimmed_cmd in (*Tokenizes the command into a list of strings*)
      let new_stk = (*executes the command and returns the new stack*)
        match tokens with
        | ["push"; arg] -> push arg stk (*If command is push; push function is called*)
        | ["pop"] -> pop stk (*If command is pop; pop function is called*)
        | ["add"] -> add stk (*If command is add; add function is called*)
        | ["sub"] -> sub stk (*If command is sub; sub function is called*)
        | ["mult"] -> mult stk (*If command is mult; mult function is called*)
        | ["div"] -> div stk (*If command is div; div function is called*)
        | ["rem"] -> rem stk (*If command is rem; rem function is called*)
        | ["sign"] -> sign stk (*If command is sign; sign function is called*)
        | ["swap"] -> swap stk (*If command is swap; swap function is called*)
        | ["toString"] -> tostring stk (*If command is toString; tostring function is called*)
        | ["println"] -> println stk oc (*If command is println; println function is called*)
        | ["quit"] -> stk (*If command is quit; return the stack and stop executing*)
        | ["cat"] -> cat stk (*If command is cat; cat function is called*)
        | ["and"] -> and_ stk
        | ["or"] -> or_ stk
        | ["not"] -> not_ stk
        | ["equal"] -> equal_ stk
        | ["lessThan"] -> lessThan_ stk
        | ["assign"] -> assign stk
        | ["if"] -> if_ stk
        | ["let"] -> let_ stk
        | ["end"] -> end_ stk
       | _ -> pushError stk (*If command is not recognized; pushError function is called*)
      in
      if tokens = ["quit"] then (*If command is quit; return the stack and stop executing*)
        new_stk (*Return the new stack*)
      else
        execute rest new_stk (*Continue executing the rest of the commands*)
    in
  let final_stack = execute lines [] in (*Start executing the commands with an empty stack*)


  write_lines output final_stack; (*Write the final stack to the output file*)
  close_out oc (*close output channel*)

(*-----------------------------------------------------*)
(*|        Manually change filenames for now          |*)
(*-----------------------------------------------------*)
let () =
  interpreter ("input1-1.txt", "output1.txt");
  interpreter ("input2-1.txt", "output2.txt");
  interpreter ("input3-1.txt", "output3.txt");
  interpreter ("input4-1.txt", "output4.txt");
  interpreter ("input5-1.txt", "output5.txt");
  interpreter ("input6-1.txt", "output6.txt");
  interpreter ("input7-1.txt", "output7.txt");
  interpreter ("input8-1.txt", "output8.txt");
  interpreter ("input9-1.txt", "output9.txt");
  interpreter ("input10-1.txt", "output10.txt");

