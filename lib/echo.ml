(*This stores 2 pieces of global state: the list of function names to stubify and the
    flag that determines if printing is one or off. It also contains functions
    to change the state*)

(*TODO: use trie?*)
module StringSet = Set.Make(String)

let func_names = ref StringSet.empty

let make_names (l: string list) : unit =
  func_names := StringSet.of_list l

(*let addName (s: string) : unit = 
  funcNames := StringSet.add s !funcNames*)

let has_name (s: string) : bool =
  StringSet.mem s !func_names

(*Current name (we need to do this because we need to parse the name,
  then the arguemnts/curly brace, then unset printing)*)
let curr_name : string ref = ref ""

(*Global state to set printing*)
let print_on = ref true

let set_name (s: string) : unit =
  curr_name := s

let set_printing () : unit =
  print_on := not (has_name !curr_name)

(* let set_printing (s: string) : unit =
  print_on := not (has_name s) *)
(*TODO: add yes/no functions*)

let reset_printing () : unit =
  print_on := true

(*TODO: remove*)
let get_state () : bool =
  !print_on

let get_name () : string =
  !curr_name

(*n consecutive whitespace characters*)
let gen_whitespace (n: int) : string =
  String.make n ' '

let print_if (s: string) : unit =
  if !print_on then
    print_string s
  else 
    print_string(gen_whitespace(String.length s))

let print_char_if (c: char) : unit =
  if !print_on then
    print_char c
  else print_char ' '

let print_always (s: string) : unit =
  print_string s