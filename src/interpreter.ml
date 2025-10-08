(* Read File into FIFO String List*)
let read_lines (filename : string) : string list = 
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

  (* Define types for stack values*)
type stack_value =
  | Int of int
  | Str of string
  | Name of string
  | Bool of bool
  | Error
  | Unit

let rec to_string (v : stack_value) : string = 
  match v with
    | Int i -> string_of_int i
    | Str s -> s
    | Name n -> n
    | Bool b -> if b then ":true:" else ":false:"
    | Error -> ":error:"
    | Unit -> ":unit:"

type stack = stack_value list

let pushInt (n : int) (stk : stack) : stack = 
  Int n :: stk

let pushStr (s : string) (stk : stack) : stack = 
  Str s :: stk

let pushName (name : string) (stk : stack) : stack =
  Name name :: stk

let pushBool (b : bool) (stk : stack) : stack = 
  Bool b :: stk

let pushError (stk : stack) : stack =
  Error :: stk

let pushUnit (stk : stack) : stack =
  Unit :: stk


let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_digit c = c >= '0' && c <= '9'

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


let write_lines (filename : string) (stack : stack_value list ) : unit =
  let oc = open_out filename in 
  try
    List.iter (fun line -> output_string oc (to_string line ^ "\n")) stack;

    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

(* Main Method*)
let interpreter ( (input : string ), (output : string)) : unit = 
  let lines = read_lines input in
  let oc = open_out output in 

  
  let rec execute (commands : string list) (stk : stack) : stack = 
    match commands with
    | [] -> stk
    | cmd :: rest ->
      let trimmed_cmd = String.trim cmd in
      let tokens = String.split_on_char ' ' trimmed_cmd in 
      let new_stk = 
        match tokens with
        | ["push"; arg] ->
            if String.starts_with ~prefix:"\"" arg && String.ends_with ~suffix:"\"" arg then 
              let s = String.sub arg 1 (String.length arg - 2) in
                pushStr s stk

            else if arg = ":true:" then 
              pushBool true stk
            else if arg = ":false:" then
              pushBool false stk
            else if arg = ":error:" then 
              pushError stk
            else if arg = ":unit:" then
              pushUnit stk

            else if (match int_of_string_opt arg with Some _ -> true | None -> false)then 
              let i = int_of_string arg in
                pushInt i stk

            else if is_valid_name arg then 
              pushName arg stk

            else
              pushError stk  
        | ["pop"] -> 
            (match stk with
              | [] -> pushError []
              | _ :: rest -> rest
            )
        | ["add"] -> stk (*stub*)
        | ["sub"] -> stk (*STUB*)
        | ["mult"] -> stk (*STUB*)
        | ["div"] -> stk (*SUB*) 
        | ["rem"] -> stk (*STUB*)
        | ["sign"] -> stk (*STUB*)
        | ["swap"] -> stk (*STUB*)
        | ["toString"] ->
            (match stk with
              |[] -> pushError []
              | v :: rest -> pushStr (to_string v) rest
            )
        | ["println"] -> 
            (match stk with
            | [] -> pushError [] 
            | v :: rest -> Printf.fprintf oc "%s\n" (to_string v); rest
          ) 
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




let () =
  interpreter ("inputa-1.txt", "output.txt")
