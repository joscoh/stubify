
(*Create the list of names of function names to stubify*)
val make_names : string list -> unit

(*Disable echoing if the given function name is in the list*)
val set_printing : string -> unit

(*Reset echoing to its default state (echo all)*)
val reset_printing : unit

(*Print/echo the given string according to the current state of the flag*)
val print_if: string -> unit

(*Print/echo the string no matter the current state of the flag (used for preserving preprocessing info)*)
val print_always: string -> unit