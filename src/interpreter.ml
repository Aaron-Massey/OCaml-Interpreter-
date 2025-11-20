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
  (* Closure: Type (fun/inOut), ArgName, BodyLines, ClosureEnvironment *)
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

let invalidName = ["add" ; "sub" ; "pop" ; "push" ; "mult" ; "div" ;
                  "push" ; "fun" ; "funEnd" ; "and" ; "or" ; "not" ;
                  "int" ; "float" ; "str" ; "bool" ; "assign" ; "if" ;
                  "unit" ; "error" ; "rem" ; "name" ; "equal" ; "lessThan" ;
                  "swap" ; "sign" ; "tostring" ; "println" ; "let" ; "end" ;
                  "cat" ; "fun" ; "funend" ; "return" ; "call" ; "inoutfun"
                  ]

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
  String.length s >= 2 && String.get s 0 = '\"' && String.get s (String.length s - 1) = '\"' 


let string_of_stack_value (v : stack_value) : string =  
  match v with                                             
    | Int i -> string_of_int i 
    | Float f -> let s = string_of_float f in
                 if String.get s (String.length s - 1) = '.' then s ^ "0" else s
    | Str s -> s                 
    | Name n -> n                 
    | Bool b -> if b then ":true:" else ":false:"       
    | Error -> ":error:"     
    | Unit  -> ":unit:"   
    | Closure _ -> ":fun:"

(*-----------------------------------------------------*) 
(*|                   File Handling                   |*) 
(*-----------------------------------------------------*) 

let read_lines (filename : string) : string list = 
  let ic = open_in filename in 
  let rec loop acc = 
    try
      let line = input_line ic in 
      loop (line :: acc) 
    with 
      End_of_file -> 
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
        let arg = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in [cmd; arg] 

(* Helper to split arguments further (e.g., "funName argName") *)
let split_args (s : string) : string list =
  let s = String.trim s in
  match String.index_opt s ' ' with
  | None -> [s]
  | Some idx ->
      let a = String.sub s 0 idx in
      let b = String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim in
      [a; b]

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
    | arg when is_valid_int arg -> pushInt (int_of_string arg) stk env 
    | arg when is_valid_float arg -> pushFloat (float_of_string arg) stk env 
    | arg when is_valid_name arg -> pushName arg stk env
    | _ -> pushError stk env 
 

let pop (stk: stack) (env : env_stack): stack * env_stack=  
  match stk with                
    | [] -> pushError stk env
    | _ :: rest -> (rest, env) 


let arithmetic (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =  
  match op with 
    | Add -> ( 
      match stk with
        | Int a :: Int b :: rest -> pushInt (b + a) rest env
        | Float a :: Float b :: rest -> pushFloat (b +. a) rest env
        (* Mixed: Promote Int to Float *)
        | Int a :: Float b :: rest -> pushFloat (b +. float_of_int a) rest env
        | Float a :: Int b :: rest -> pushFloat ((float_of_int b) +. a) rest env
        | _ -> pushError stk env 
          )
    | Sub -> (
      match stk with
      | Int a :: Int b :: rest -> pushInt (b - a) rest env 
      | Float a :: Float b :: rest -> pushFloat (b -. a) rest env 
      (* Mixed: Promote Int to Float *)
      | Int a :: Float b :: rest -> pushFloat (b -. float_of_int a) rest env
      | Float a :: Int b :: rest -> pushFloat ((float_of_int b) -. a) rest env
      | _ -> pushError stk env 
    ) 
    | Mult -> (
      match stk with 
        | Int a :: Int b :: rest -> pushInt (b * a) rest env  
        | Float a :: Float b :: rest -> pushFloat (b *. a) rest env  
        (* Mixed: Promote Int to Float *)
        | Int a :: Float b :: rest -> pushFloat (b *. float_of_int a) rest env
        | Float a :: Int b :: rest -> pushFloat ((float_of_int b) *. a) rest env
        | _ -> pushError stk env 
    ) 
    | Div -> (
      match stk with
        | Int a :: Int b :: rest -> 
          if a = 0 then pushError (Int a :: Int b :: rest) env
          else pushInt (b / a) rest env
        | Float a :: Float b :: rest -> 
          if a = 0.0 then pushError (Float a :: Float b :: rest) env
          else pushFloat (b /. a) rest env
        (* Mixed: Promote Int to Float *)
        | Int a :: Float b :: rest -> 
          if a = 0 then pushError (Int a :: Float b :: rest) env
          else pushFloat (b /. float_of_int a) rest env
        | Float a :: Int b :: rest -> 
          if a = 0.0 then pushError (Float a :: Int b :: rest) env
          else pushFloat ((float_of_int b) /. a) rest env
        | _ -> pushError stk env  
    ) 
    | Rem ->  (
      match stk with
        | Int a :: Int b :: rest -> 
          if a = 0 then pushError (Int a :: Int b :: rest) env 
          else pushInt (b mod a) rest env 
        | Float a :: Float b :: rest -> 
          if a = 0.0 then pushError (Float a :: Float b :: rest) env 
          else pushFloat (mod_float b a) rest env 
        (* Mixed: Promote Int to Float *)
        | Int a :: Float b :: rest -> 
          if a = 0 then pushError (Int a :: Float b :: rest) env
          else pushFloat (mod_float b (float_of_int a)) rest env
        | Float a :: Int b :: rest -> 
          if a = 0.0 then pushError (Float a :: Int b :: rest) env
          else pushFloat (mod_float (float_of_int b) a) rest env
        | _ -> pushError stk env 
    )   

let arithmetic_helper (op : operation) (stk: stack) (env : env_stack) : stack * env_stack =  
  match stk with 
    | Int a :: Int b :: rest -> arithmetic op stk env 
    | Float a :: Float b :: rest -> arithmetic op stk env 
    (* Add Mixed cases to helper dispatch *)
    | Int a :: Float b :: rest -> arithmetic op stk env
    | Float a :: Int b :: rest -> arithmetic op stk env
    | _ -> pushError stk env 
  
let sign (stk : stack) (env : env_stack): stack * env_stack = 
  match stk with 
    | Int a :: rest -> pushInt (a * -1) rest env 
    | Float a :: rest -> pushFloat (a *. -1.0) rest env 
    | _ -> pushError stk env 

let swap (stk : stack) (env: env_stack): stack * env_stack = 
  match stk with
    | a :: b :: rest -> ((b :: a :: rest), env) 
    | _ -> pushError stk env 

let tostring (stk : stack) (env : env_stack): stack * env_stack = 
  match stk with
    | [] -> pushError stk env 
    | v :: rest -> pushStr (string_of_stack_value v) rest env 

let println (out : out_channel) (stk : stack) (env : env_stack) : stack*env_stack = 
  match stk with
    | [] -> pushError stk env 
    | v :: rest -> Printf.fprintf out "%s\n" (string_of_stack_value v);
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
      match fetch_from_environment_list name current_scope with
      | Error -> fetch_from_env_stack name outer_scopes 
      | value -> value 

let rec check_environment_list (name : string) (env_list : environment): bool = 
  match env_list with
    | [] -> false 
    | (n, v) :: rest -> if string_of_stack_value n = name then true else check_environment_list name rest 

let add_to_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment = 
  (name, value) :: env_list 

let remove_from_environment_list (name : stack_value) (env_list : environment): environment = 
  let key = string_of_stack_value name in 
  List.filter(fun(n,_) -> string_of_stack_value n <> key) env_list 

let replace_in_environment_list (name : stack_value) (value : stack_value) (env_list : environment): environment = 
  let env_without_name = remove_from_environment_list name env_list in 
  add_to_environment_list name value env_without_name 

let rec resolve_names stk env =  
  List.map(function 
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

let cat (stk : stack) (env : env_stack): stack * env_stack= 
  match stk with
    | Str a :: Str b :: rest -> pushStr (b ^ a) rest env 
    | _ -> pushError stk env 

let equal_ (stk : stack) (env : env_stack): stack * env_stack= 
  match stk with
    | Int a :: Int b :: rest -> pushBool (a = b) rest env 
    | Float a :: Float b :: rest -> pushBool (a = b) rest env 
    (* Mixed: Promote Int to Float *)
    | Int a :: Float b :: rest -> pushBool ((float_of_int a) = b) rest env
    | Float a :: Int b :: rest -> pushBool (a = (float_of_int b)) rest env
    | _ -> pushError stk env 

let lessThan_ (stk: stack) (env : env_stack): stack * env_stack = 
  match stk with
    | Int a :: Int b :: rest -> pushBool (b < a) rest env 
    | Float a :: Float b :: rest -> pushBool (b < a) rest env 
    (* Mixed: Promote Int to Float *)
    | Int a :: Float b :: rest -> pushBool (b < (float_of_int a)) rest env
    | Float a :: Int b :: rest -> pushBool ((float_of_int b) < a) rest env
    | _ -> pushError stk env 

(* Recursive function to update a variable in the nearest scope it exists in *)
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
  | Int i :: Name n :: rest -> 
      let new_env = update_env_stack n (Int i) env in
      (Unit::rest, new_env)
  | Float f :: Name n :: rest -> 
      let new_env = update_env_stack n (Float f) env in
      (Unit::rest, new_env)
  | Bool b :: Name n :: rest -> 
      let new_env = update_env_stack n (Bool b) env in
      (Unit::rest, new_env)
  | Str s :: Name n :: rest -> 
      let new_env = update_env_stack n (Str s) env in
      (Unit::rest, new_env)
  | Unit  :: Name n :: rest -> 
      let new_env = update_env_stack n (Unit) env in
      (Unit::rest, new_env)
  | Closure(t, a, b, e) :: Name n :: rest -> 
       let new_env = update_env_stack n (Closure(t,a,b,e)) env in
       (Unit::rest, new_env)
  | Name a :: Name n :: rest ->
     (* Resolve the value of 'a' first *)
     let value = fetch_from_env_stack a env in
     if value = Error then (pushError stk env) 
     else
       let new_env = update_env_stack n value env in
       (Unit::rest, new_env)
  | _ -> (pushError stk env)

let if_ (stk: stack) (env : env_stack): stack*env_stack = 
  match stk with 
    | trueVal :: falseVal :: Bool condition :: rest ->
      if condition then (trueVal :: rest, env) 
      else (falseVal :: rest, env) 
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

let funcNameValid (name : string) : bool =
  let name = String.lowercase_ascii name in
  not (List.mem name invalidName)

(* Extract the current environment to store in a closure *)
let capture_environment (env : env_stack) : (stack_value * stack_value) list list =
  List.map (fun (e, _) -> e) env

(* Reconstruct env_stack from captured environment list, assuming empty stacks for frames *)
let reconstruct_environment (captured : (stack_value * stack_value) list list) : env_stack =
  List.map (fun e -> (e, [])) captured

(* Helper to extract function body from the list of commands *)
let rec extract_body (commands : string list) (acc : string list) (depth : int) : string list * string list =
  match commands with
  | [] -> (List.rev acc, []) 
  | cmd :: rest ->
      let tokens = tokenize_command cmd in
      match tokens with
      | ["fun"; _] | ["inOutFun"; _] -> extract_body rest (cmd :: acc) (depth + 1) 
      | ["funEnd"] -> 
          if depth = 0 then (List.rev acc, rest)
          else extract_body rest (cmd :: acc) (depth - 1)
      | _ -> extract_body rest (cmd :: acc) depth

(* Resolving logic for arguments on stack that might be names *)
let resolve_val (v : stack_value) (env : env_stack) : stack_value =
  match v with
  | Name n -> 
      let res = fetch_from_env_stack n env in
      if res = Error then Error else res
  | x -> x


(*-----------------------------------------------------*) 
(*|               Main Interpreter Code               |*) 
(*-----------------------------------------------------*) 


let interpreter ( (input : string ), (output : string)) : unit = 
  let lines = read_lines input in
  let oc = open_out output in  

  let exec_cmd ?(resolve=true) f (stk : stack) (env : env_stack) : (stack * env_stack) =
    let adjusted_stk = if resolve then resolve_names stk env else stk in
    f adjusted_stk env
  in

  let rec execute (commands : string list) (stk : stack) (env : env_stack): stack * env_stack =
    match commands with
    | [] -> (stk, env) 
    | cmd :: rest -> 
      let trimmed_cmd = String.trim cmd in 
      let tokens = tokenize_command trimmed_cmd in 
      
      if tokens = ["quit"] then (stk, env)
      else if tokens = ["return"] then (stk, env) (* Stop execution immediately *)
      else if tokens = ["funEnd"] then (pushError stk env) (* Should not be encountered outside parsing *)
      else
        
        (* Handle Function Definitions *)
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
                      
                      (* Bind closure to function name in CURRENT environment *)
                      let (current_scope, old_s) = List.hd env in
                      let new_scope = 
                        if check_environment_list funName current_scope then
                          replace_in_environment_list (Name funName) closure current_scope
                        else
                          add_to_environment_list (Name funName) closure current_scope
                      in
                      let new_env = (new_scope, old_s) :: (List.tl env) in
                      
                      (* Push Unit and continue with REMAINING commands *)
                      (* FIXED: match syntax error by using let binding *)
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

        (* Handle Function Call *)
        | ["call"] -> 
             (* Call pops funName and arg. We need resolve=false to see names. *)
             let (call_stk, call_env) = exec_cmd ~resolve:false (fun s e -> (s, e)) stk env in
             (match call_stk with
              | arg_item :: func_item :: stack_rest ->
                  (* Resolve function to closure *)
                  let closure_val = resolve_val func_item call_env in
                  (* Resolve argument to value *)
                  let arg_val = resolve_val arg_item call_env in

                 (match closure_val with
                   | Closure(ftype, paramName, body, saved_env_data) ->
                       if arg_val = Error then match pushError stk env with (s, e) -> execute rest s e
                       else
                         (* Restore saved environment *)
                         let base_env = reconstruct_environment saved_env_data in
                         
                         (* Add binding for formal parameter *)
                         let (top_scope, top_stack_ignore) = List.hd base_env in
                         let new_scope = add_to_environment_list (Name paramName) arg_val top_scope in
                         
                         (* Create new execution environment with empty stack *)
                         let exec_env = (new_scope, []) :: (List.tl base_env) in
                         
                         let (res_stack, res_env) = execute body [] exec_env in

                         (* Restore environment and stack. Push result (top of res_stack). *)
                         let ret_val = if res_stack = [] then Error else List.hd res_stack in
                         let restored_stack = ret_val :: stack_rest in
                         
                         (* Handle InOutFun Update *)
                         let final_env = 
                           if ftype = "inOutFun" then
                             match arg_item with
                             | Name actual_name_str -> 
                                 (* Lookup formal param in the FUNCTION'S final environment *)
                                 let final_param_val = fetch_from_env_stack paramName res_env in
                                 if final_param_val = Error then call_env
                                 else update_env_stack actual_name_str final_param_val call_env
                             | _ -> call_env (* If arg was not a name, no update *)
                           else
                             call_env
                         in
                         execute rest restored_stack final_env

                   | _ -> match pushError stk env with (s, e) -> execute rest s e (* Not a closure *)
                  )
              | _ -> match pushError stk env with (s, e) -> execute rest s e
             )

        | _ -> 
          let (new_stk, new_env) =
            match tokens with
            (* PUSH, ASSIGN, LET, TOSTRING, PRINTLN MUST NOT RESOLVE NAMES *)
            | ["push"; arg] -> exec_cmd ~resolve:false (push arg) stk env 
            | ["pop"] -> exec_cmd (pop) stk env 
            | ["add"] -> exec_cmd (arithmetic_helper Add) stk env 
            | ["sub"] -> exec_cmd (arithmetic_helper Sub) stk env 
            | ["mult"] -> exec_cmd (arithmetic_helper Mult) stk env 
            | ["div"] -> exec_cmd (arithmetic_helper Div) stk env 
            | ["rem"] -> exec_cmd (arithmetic_helper Rem) stk env 
            | ["sign"] -> exec_cmd (sign) stk env 
            | ["swap"] -> exec_cmd (swap) stk env 
            | ["toString"] -> exec_cmd ~resolve:false (tostring) stk env 
            | ["println"] -> exec_cmd ~resolve:false (println oc) stk env  
            | ["cat"] -> exec_cmd (cat) stk env 
            | ["and"] -> exec_cmd (boolean_logic And) stk env 
            | ["or"] -> exec_cmd (boolean_logic Or) stk env 
            | ["not"] -> exec_cmd (boolean_logic Not) stk env 
            | ["equal"] -> exec_cmd (equal_) stk env 
            | ["lessThan"] -> exec_cmd (lessThan_) stk env 
            | ["assign"] -> exec_cmd ~resolve:false (assign) stk env 
            | ["if"] -> exec_cmd (if_) stk env 
            | ["let"] -> exec_cmd ~resolve:false (let_) stk env 
            | ["end"] -> exec_cmd ~resolve:false (end_) stk env 
            | _ -> exec_cmd (pushError) stk env  
          in
          execute rest new_stk new_env 
  in

  let (final_stack, _) = execute lines [] [ ([], []) ] in 

  List.iter (fun line -> output_string oc (string_of_stack_value line ^ "\n")) final_stack;
  close_out oc
(*-----------------------------------------------------*) 
(*|        Used directories to test program           |*)
(*-----------------------------------------------------*)


let () =
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
