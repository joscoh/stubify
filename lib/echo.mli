
(*Create the list of names of function names to stubify*)
val make_names : string list -> unit

(*Set the function name whose body we are in*)
val set_name : string -> unit

(*Disable echoing if the stored function name is in the list*)
val set_printing : unit -> unit

(*Reset echoing to its default state (echo all)*)
val reset_printing : unit -> unit

val get_state: unit -> bool

val get_name : unit -> string

(*Print/echo the given string/char according to the current state of the flag*)
val print_if: string -> unit
val print_char_if: char -> unit

(*Print/echo the string no matter the current state of the flag (used for preserving preprocessing info)*)
val print_always: string -> unit