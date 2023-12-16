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

(*Mantain buffer of strings, because we need to go back and fix*)
let buffer : (string list) ref = ref []

let set_name (s: string) : unit =
  curr_name := s

(*n consecutive whitespace characters*)
let gen_whitespace (n: int) : string =
  String.make n ' '

let set_printing () : unit =
  print_on := not (has_name !curr_name);
  (*If printing is disabled, we must remove the last token from the
     buffer and replace it with the appropriate number of whitespace characters*)
  if not !print_on then 
    match !buffer with
    | [] -> ()
    | last :: tl -> buffer := gen_whitespace (String.length last) :: tl


(* let set_printing (s: string) : unit =
  print_on := not (has_name s) *)
(*TODO: add yes/no functions*)

let reset_printing () : unit =
  (*If print_on was false, need to add a closing curly brace and remove the last buffer elt*)
  if not !print_on then
    begin match !buffer with
    | [] -> ()
    | _ :: tl -> buffer := "}" :: tl
    end;
  print_on := true

let get_state () : bool =
  !print_on

(*TODO:remove*)
let get_name () : string =
  !curr_name



let print_if (s: string) : unit =
  buffer := (if !print_on then s else gen_whitespace(String.length s)) :: !buffer;
  ignore(Printf.eprintf "%s" s)


  (*if !print_on then
    buffer := s :: !buffer
    (*print_string s*)
  else 
    buffer := gen_whitespace(String.length s) :: !buffer
    (*print_string(gen_whitespace(String.length s))*)*)

let print_char_if (c: char) : unit =
  buffer := (if !print_on then String.make 1 c else " ") :: !buffer;
  ignore(Printf.eprintf "%c" c)
  (*if !print_on then
    buffer := String.make 1 c :: !buffer
    (*print_char c*)
  else 
    buffer := 
    print_char ' '*)

let print_always (s: string) : unit =
  buffer := s :: !buffer;
  ignore(Printf.eprintf "%s" s)
  (*print_string s*)

let print_buffer() : unit =
  List.iter print_string (List.rev !buffer)