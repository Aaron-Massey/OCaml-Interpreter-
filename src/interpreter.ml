(*-----------------------------------------------------*) 
(*|Interpreter Project - Aaron Massey & Brayden Stille|*) 
(*-----------------------------------------------------*) 

(*-----------------------------------------------------*) 
(*|                  Type Definitions                 |*) 
(*-----------------------------------------------------*) 

type stack_value =
  | Int of int
  | Str of string
  | Name of string
  | Bool of bool
  | Error
  | Unit 

type stack = stack_value list
type enviroment = (string * stack_value) list

(* Special scope marker (not user-creatable name) *)
let scope_marker_str = "__::scope::__"
let scope_marker : stack_value = Name scope_marker_str

(*-----------------------------------------------------*) 
(*|                  Type Validation                  |*) 
(*-----------------------------------------------------*) 

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'

let is_valid_name (s : string) : bool =
  if String.length s = 0 then false else
  let first_char = s.[0] in
  if not (is_letter first_char || first_char = '_') then false else
  let rec check_rest i =
    if i >= String.length s then true
    else
      let ch = s.[i] in
      if is_letter ch || is_digit ch || ch = '_' then check_rest (i+1)
      else false
  in
  check_rest 1

let is_valid_int (s : string) : bool =
  match int_of_string_opt s with
  | Some _ -> true
  | None -> false

let is_quoted_string (s : string) : bool =
  String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'

let string_of_stack_value (v : stack_value) : string =
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
  else match String.index_opt s ' ' with
       | None -> [s]
       | Some idx ->
         let cmd = String.sub s 0 idx in
         let arg = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in
         [cmd; arg]

(*-----------------------------------------------------*) 
(*|             Stack Helpers & Pushers               |*) 
(*-----------------------------------------------------*) 

let pushInt (n : int) (stk : stack) : stack = Int n :: stk
let pushStr (s : string) (stk : stack) : stack = Str s :: stk
let pushName (name : string) (stk : stack) : stack = Name name :: stk
let pushBool (b : bool) (stk : stack) : stack = Bool b :: stk
let pushError (stk : stack) : stack = Error :: stk
let pushUnit (u : stack_value option) (stk : stack) : stack = Unit :: stk  

let push (arg : string) (stk : stack) : stack =
  if is_quoted_string arg then
    let s = String.sub arg 1 (String.length arg - 2) in
    pushStr s stk
  else
    match arg with
    | ":true:" -> pushBool true stk
    | ":false:" -> pushBool false stk
    | ":error:" -> pushError stk
    | ":unit:" -> pushUnit None stk
    | a when is_valid_name a -> pushName a stk
    | a when is_valid_int a -> pushInt (int_of_string a) stk
    | _ -> pushError stk

let pop (stk: stack) : stack =
  match stk with
  | [] -> pushError stk
  | _ :: rest -> rest

let tostring (stk : stack) : stack =
  match stk with
  | [] -> pushError stk
  | v :: rest -> pushStr (string_of_stack_value v) rest

let println (stk : stack) (out : out_channel): stack =
  match stk with
  | [] -> pushError stk
  | v :: rest ->
      Printf.fprintf out "%s\n" (string_of_stack_value v);
      rest

(*-----------------------------------------------------*) 
(*|               Environment (Part 2)                |*) 
(*-----------------------------------------------------*) 

let add_to_environment (name : string) (value : stack_value) (env : enviroment) : enviroment =
  (name, value) :: env

let rec fetch_from_environment (name : string) (env : enviroment) : stack_value option =
  match env with
  | [] -> None
  | (n, v) :: rest -> if n = name then Some v else fetch_from_environment name rest

(* Name dereference only for ops that require concrete values *)
let rec value_for_op (env : enviroment) (v : stack_value) : stack_value =
  match v with
  | Name n ->
      (match fetch_from_environment n env with
       | None -> Error
       | Some (Name _ as nv) -> value_for_op env nv
       | Some other -> other)
  | other -> other

(*-----------------------------------------------------*) 
(*|          Arithmetic / Boolean / String Ops        |*) 
(*-----------------------------------------------------*) 

let add (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      let va = value_for_op env a and vb = value_for_op env b in
      (match va, vb with
       | Int x, Int y -> (pushInt (y + x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let sub (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      let va = value_for_op env a and vb = value_for_op env b in
      (match va, vb with
       | Int x, Int y -> (pushInt (y - x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let mult (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      let va = value_for_op env a and vb = value_for_op env b in
      (match va, vb with
       | Int x, Int y -> (pushInt (y * x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let div (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      let va = value_for_op env a and vb = value_for_op env b in
      (match va, vb with
       | Int x, Int y ->
           if x = 0 then (pushError (a :: b :: rest), env)
           else (pushInt (y / x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let rem (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      let va = value_for_op env a and vb = value_for_op env b in
      (match va, vb with
       | Int x, Int y ->
           if x = 0 then (pushError (a :: b :: rest), env)
           else (pushInt (y mod x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let sign (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: rest ->
      (match value_for_op env a with
       | Int x -> (pushInt (-x) rest, env)
       | _ -> (pushError (a :: rest), env))
  | _ -> (pushError stk, env)

let swap (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest -> (b :: a :: rest, env)
  | _ -> (pushError stk, env)

let cat (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Str x, Str y -> (pushStr (y ^ x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let and_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Bool x, Bool y -> (pushBool (y && x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let or_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Bool x, Bool y -> (pushBool (y || x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let not_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: rest ->
      (match value_for_op env a with
       | Bool x -> (pushBool (not x) rest, env)
       | _ -> (pushError (a :: rest), env))
  | _ -> (pushError stk, env)

let equal_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Int x, Int y -> (pushBool (x = y) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let lessThan_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Int x, Int y -> (pushBool (y < x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

let greaterThan_ (env: enviroment) (stk : stack) : stack * enviroment =
  match stk with
  | a :: b :: rest ->
      (match value_for_op env a, value_for_op env b with
       | Int x, Int y -> (pushBool (y > x) rest, env)
       | _ -> (pushError (a :: b :: rest), env))
  | _ -> (pushError stk, env)

(*-----------------------------------------------------*) 
(*|               assign / if / let / end             |*) 
(*-----------------------------------------------------*) 

let assign (env : enviroment) (stk: stack) : stack * enviroment =
  match stk with
  | v :: Name n :: rest ->
      let rv = value_for_op env v in
      (match rv with
       | Int _ | Str _ | Bool _ | Unit _ ->
           let env2 = add_to_environment n rv env in
           (pushUnit None rest, env2)
       | _ -> (pushError (v :: Name n :: rest), env))
  | _ -> (pushError stk, env)

let if_ (env: enviroment) (stk: stack) : stack * enviroment =
  match stk with
  | x :: y :: z :: rest ->
      (match value_for_op env z with
       | Bool true -> (x :: rest, env)
       | Bool false -> (y :: rest, env)
       | _ -> (pushError (x :: y :: z :: rest), env))
  | _ -> (pushError stk, env)

let let_ (env : enviroment) (stk: stack) : stack * enviroment =
  let env2 = add_to_environment scope_marker_str (Unit) env in
  (scope_marker :: stk, env2)

(* end without a variable named carry; also no primes in names *)
let end_ (env : enviroment) (stk: stack) : stack * enviroment =
  let rec scan_until_marker s top_opt =
    match s with
    | [] -> None
    | x :: xs ->
        if x = scope_marker then Some (top_opt, xs)
        else
          let next_top =
            match top_opt with
            | None -> Some x
            | Some v -> Some v
          in
          scan_until_marker xs next_top
  in
  match scan_until_marker stk None with
  | None -> (pushError stk, env)
  | Some (maybe_top, rest_after_marker) ->
      let rec drop_env_until_marker e =
        match e with
        | [] -> []
        | (k, _) :: xs ->
            if k = scope_marker_str then xs
            else drop_env_until_marker xs
      in
      let env2 = drop_env_until_marker env in
      let result = match maybe_top with Some v -> v | None -> Unit in
      (result :: rest_after_marker, env2)

(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 

let interpreter ((input : string), (output : string)) : unit =
  let lines = read_lines input in
  let oc = open_out output in

  let rec execute (commands : string list) (stk : stack) (env : enviroment) : stack * enviroment =
    match commands with
    | [] -> (stk, env)
    | cmd :: rest ->
        let trimmed_cmd = String.trim cmd in
        let tokens = tokenize_command trimmed_cmd in
        let (new_stk, new_env) =
          match tokens with
          | ["push"; arg] -> (push arg stk, env)
          | ["pop"] -> (pop stk, env)
          | ["add"] -> add env stk
          | ["sub"] -> sub env stk
          | ["mult"] -> mult env stk
          | ["div"] -> div env stk
          | ["rem"] -> rem env stk
          | ["sign"] -> sign env stk
          | ["swap"] -> swap env stk
          | ["toString"] -> (tostring stk, env)
          | ["println"] -> (println stk oc, env)
          | ["cat"] -> cat env stk
          | ["and"] -> and_ env stk
          | ["or"] -> or_ env stk
          | ["not"] -> not_ env stk
          | ["equal"] -> equal_ env stk
          | ["lessThan"] -> lessThan_ env stk
          | ["greaterThan"] -> greaterThan_ env stk
          | ["assign"] -> assign env stk
          | ["if"] -> if_ env stk
          | ["let"] -> let_ env stk
          | ["end"] -> end_ env stk
          | ["quit"] -> (stk, env)
          | _ -> (pushError stk, env)
        in
        if tokens = ["quit"] then (new_stk, new_env) else execute rest new_stk new_env
  in

  let (final_stack, _) = execute lines [] [] in
  write_lines output final_stack;
  close_out oc

(*-----------------------------------------------------*)
(*|        Manually change filenames for now          |*)
(*-----------------------------------------------------*)
let () =
  interpreter ("input1.txt", "output1.txt");
  interpreter ("input2.txt", "output2.txt");
  interpreter ("input3.txt", "output3.txt");
  interpreter ("input4.txt", "output4.txt");
  interpreter ("input5.txt", "output5.txt");
  interpreter ("input6.txt", "output6.txt");
  interpreter ("input7.txt", "output7.txt");
  interpreter ("input8.txt", "output8.txt");
  interpreter ("input9.txt", "output9.txt");
  interpreter ("input10.txt", "output10.txt");
