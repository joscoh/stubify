(*This stores 2 pieces of global state: the list of function names to stubify and the
    flag that determines if printing is one or off. It also contains functions
    to change the state*)

(*TODO: use trie?*)
module StringSet = Set.Make(String)

let func_names = ref StringSet.empty

let make_names (l: string list) : unit =
  func_names := StringSet.of_list l

let has_name (s: string) : bool =
  StringSet.mem s !func_names

(*Current name (we need to do this because we need to parse the name,
  then the arguemnts/curly brace, then unset printing)*)
let curr_name : string ref = ref ""

(*Global state to set printing*)
let print_on = ref true

(*We will always print one string behind because the lexer looks ahead, and
   occasionally, we need to fix something*)
let buffer : string ref = ref ""
(*let buffer : (string list) ref = ref []*)

let set_name (s: string) : unit =
  curr_name := s

(*n consecutive whitespace characters*)
let gen_whitespace (n: int) : string =
  String.make n ' '

let set_printing () : unit =
  print_on := has_name !curr_name;
  (*If printing is disabled, we should not print the last token, so we replace
     it with the appropriate number of whitespace characters*)
  if not !print_on then
    let old = !buffer in 
    buffer := gen_whitespace (String.length old)

(*TODO: add yes/no functions*)

let reset_printing () : unit =
  (*If print_on was false, need to add a closing curly brace and remove the last buffer elt (which is whitespace)*)
  if not !print_on then buffer := "extern void stub_error(char *); stub_error(\"" ^ !curr_name ^ "\"); while(1);}";
  print_on := true

(*The printing functions are a bit of a misnomer. They print the previous
  element and add the new string to the buffer (as the literal string or the
  appropriate amount of whitespace, depending on the flag)*)

let print_if (s: string) : unit =
  print_string !buffer;
  buffer := (if !print_on then s else gen_whitespace(String.length s))

let print_char_if (c: char) : unit =
  print_string !buffer;
  buffer := (if !print_on then String.make 1 c else " ")

let print_always (s: string) : unit =
  print_string !buffer;
  buffer := s

let print_buffer() : unit =
  print_string !buffer