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

(*Global state to set printing*)
let print_on = ref true

let set_printing (s: string) : unit =
  if has_name s then 
    print_on := false

let reset_printing : unit =
  print_on := true

(*n consecutive whitespace characters*)
let gen_whitespace (n: int) : string =
  String.make n ' '

let print_if (s: string) : unit =
  if !print_on then
    print_string s
  else 
    print_string(gen_whitespace(String.length s))

let print_always (s: string) : unit =
  print_string s