(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            *)
(*             Xavier Leroy, Collège de France and Inria               *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)

{
open Lexing
open Parser
open Pre_parser_aux

module SSet = Set.Make(String)

let lexicon : (string, token) Hashtbl.t = Hashtbl.create 17
let ignored_keywords : SSet.t ref = ref SSet.empty

let () =
  List.iter (fun (key, builder) -> Hashtbl.add lexicon key builder)
    [ 
      ("_Alignas", ALIGNAS);
      ("_Alignof", ALIGNOF);
      ("_Bool",  UNDERSCORE_BOOL);
      ("_Generic",  GENERIC);
      ("_Complex", COMPLEX  (*reserved_keyword "_Complex"*));
      ("_Imaginary",  IMAGINARY (*reserved_keyword "_Imaginary"*));
      ("_Static_assert",  STATIC_ASSERT);
      ("__alignof",  ALIGNOF);
      ("__alignof__",  ALIGNOF);
      ("__asm",  ASM);
      ("__asm__",  ASM);
      ("__attribute",  ATTRIBUTE);
      ("__attribute__",  ATTRIBUTE);
      ("__builtin_va_arg",  BUILTIN_VA_ARG);
      ("__builtin_offsetof",  BUILTIN_OFFSETOF);
      ("__const",  CONST);
      ("__const__",  CONST);
      ("__inline",  INLINE);
      ("__inline__",  INLINE);
      ("__packed__",  PACKED);
      ("__restrict",  RESTRICT);
      ("__restrict__",  RESTRICT);
      ("__signed",  SIGNED);
      ("__signed__",  SIGNED);
      ("__volatile",  VOLATILE);
      ("__volatile__",  VOLATILE);
      ("asm",  ASM);
      ("auto",  AUTO);
      ("break",  BREAK);
      ("case",  CASE);
      ("char",  CHAR);
      ("const",  CONST);
      ("continue",  CONTINUE);
      ("default",  DEFAULT);
      ("do",  DO);
      ("double",  DOUBLE);
      ("else",  ELSE);
      ("enum",  ENUM);
      ("extern",  EXTERN);
      ("float",  FLOAT);
      ("for",  FOR);
      ("goto",  GOTO);
      ("if",  IF);
      ("inline",  INLINE);
      ("_Noreturn",  NORETURN);
      ("int",  INT);
      ("long",  LONG);
      ("register",  REGISTER);
      ("restrict",  RESTRICT);
      ("return",  RETURN);
      ("short",  SHORT);
      ("signed",  SIGNED);
      ("sizeof",  SIZEOF);
      ("static",  STATIC);
      ("struct",  STRUCT);
      ("switch",  SWITCH );
      ("typedef",  TYPEDEF);
      ("union",  UNION);
      ("unsigned",  UNSIGNED);
      ("void",  VOID);
      ("volatile",  VOLATILE);
      ("while",  WHILE)];
      (*JOSH - for now, assume not diab*)
  (*if Configuration.system <> "diab" then*)
    (* We can ignore the __extension__ GCC keyword. *)
    ignored_keywords := SSet.add "__extension__" !ignored_keywords

(*JOSH - taken from x86 folder in CompCert - not best way to do it *)
let builtin_typedefs = [
    "__builtin_va_list";
  ]

let init_ctx : SSet.t = SSet.of_list builtin_typedefs

let types_context : SSet.t ref = ref init_ctx

let _ =
  (* See comments in pre_parser_aux.ml *)
  save_context := begin fun () ->
    let save = !types_context in
    fun () -> types_context := save
  end;

  declare_varname := begin fun id ->
    types_context := SSet.remove id !types_context
  end;

  declare_typename := begin fun id ->
    types_context := SSet.add id !types_context
  end

let init filename channel : Lexing.lexbuf =
  let lb = Lexing.from_channel channel in
  lb.lex_curr_p <- {lb.lex_curr_p with pos_fname = filename; pos_lnum = 1};
  lb
}

(* Identifiers *)
let digit = ['0'-'9']
let hexadecimal_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let nondigit = ['_' 'a'-'z' 'A'-'Z']

let hex_quad = hexadecimal_digit hexadecimal_digit
                 hexadecimal_digit hexadecimal_digit
let universal_character_name =
    "\\u" (hex_quad)
  | "\\U" (hex_quad hex_quad)

let identifier_nondigit =
    nondigit
(*| universal_character_name*)
  | '$'

let identifier = identifier_nondigit (identifier_nondigit|digit)*

(* Whitespaces *)
let whitespace_char_no_newline = [' ' '\t'  '\011' '\012' '\r']

(* Integer constants *)
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*

let octal_digit = ['0'-'7']
let octal_constant = '0' octal_digit*

let hexadecimal_prefix = "0x" | "0X"
let hexadecimal_constant =
  hexadecimal_prefix hexadecimal_digit+

let unsigned_suffix = ['u' 'U']
let long_suffix = ['l' 'L']
let long_long_suffix = "ll" | "LL"
let integer_suffix =
    unsigned_suffix long_suffix?
  | unsigned_suffix long_long_suffix
  | long_suffix unsigned_suffix?
  | long_long_suffix unsigned_suffix?

let integer_constant =
    decimal_constant integer_suffix?
  | octal_constant integer_suffix?
  | hexadecimal_constant integer_suffix?

(* Floating constants *)
let sign = ['-' '+']
let digit_sequence = digit+
let floating_suffix = ['f' 'l' 'F' 'L']

let fractional_constant =
    (digit_sequence)? '.' (digit_sequence)
  | (digit_sequence) '.'
let exponent_part =
    'e' ((sign? digit_sequence))
  | 'E' ((sign? digit_sequence))
let decimal_floating_constant =
    fractional_constant exponent_part? floating_suffix?
  | (digit_sequence) exponent_part floating_suffix?

let hexadecimal_digit_sequence = hexadecimal_digit+
let hexadecimal_fractional_constant =
    (hexadecimal_digit_sequence)? '.' (hexadecimal_digit_sequence)
  | (hexadecimal_digit_sequence) '.'
let binary_exponent_part =
    'p' ((sign? digit_sequence))
  | 'P' ((sign? digit_sequence))
let hexadecimal_floating_constant =
    hexadecimal_prefix hexadecimal_fractional_constant
        binary_exponent_part floating_suffix?
  | hexadecimal_prefix (hexadecimal_digit_sequence)
        binary_exponent_part floating_suffix?

(* Preprocessing numbers *)
let preprocessing_number =
  '.'? ['0'-'9']
  (['0'-'9' 'A'-'Z' 'a'-'z' '_' '.'] | ['e' 'E' 'p' 'P']['+' '-'])*

(* Character and string constants *)
let simple_escape_sequence =
  '\\' ( ['\''  '\"'  '?'  '\\'  'a'  'b'  'e'  'f'  'n'  'r'  't'  'v'] as c)
let octal_escape_sequence =
  '\\' ((octal_digit
         | octal_digit octal_digit
         | octal_digit octal_digit octal_digit))
let hexadecimal_escape_sequence = "\\x" (hexadecimal_digit+)

rule initial = parse
  | '\n'                          { Echo.print_always "\n"; new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline + as c  { Echo.print_if c; initial lexbuf }
  | "/*" as c                          { Echo.print_if c; multiline_comment lexbuf; initial lexbuf }
  | "//" as c                          { Echo.print_if c; singleline_comment lexbuf; initial lexbuf }
  | integer_constant as c         { Echo.print_if c; CONSTANT }
  | decimal_floating_constant as c     { Echo.print_if c; CONSTANT }
  | hexadecimal_floating_constant as c { Echo.print_if c; CONSTANT }
  | preprocessing_number     { failwith "These characters form a preprocessor number, but not a constant" }
  | (""|"L"|"u"|"U") "'" as c     { Echo.print_if c; char lexbuf; char_literal_end lexbuf; CONSTANT }
  | (""|"L"|"u"|"U"|"u8") "\"" as c
                                  { Echo.print_if c; string_literal lexbuf; STRING_LITERAL }
  | "..." as c                         { Echo.print_if c; ELLIPSIS }
  | "+=" as c                         { Echo.print_if c; ADD_ASSIGN }
  | "-=" as c                         { Echo.print_if c; SUB_ASSIGN }
  | "*=" as c                           { Echo.print_if c; MUL_ASSIGN }
  | "/=" as c                           { Echo.print_if c; DIV_ASSIGN }
  | "%=" as c                           { Echo.print_if c; MOD_ASSIGN }
  | "|=" as c                           { Echo.print_if c; OR_ASSIGN }
  | "&=" as c                           { Echo.print_if c; AND_ASSIGN }
  | "^=" as c                           { Echo.print_if c; XOR_ASSIGN }
  | "<<=" as c                        { Echo.print_if c; LEFT_ASSIGN }
  | ">>=" as c                        { Echo.print_if c; RIGHT_ASSIGN }
  | "<<" as c                         { Echo.print_if c; LEFT }
  | ">>" as c                         { Echo.print_if c; RIGHT }
  | "==" as c                         { Echo.print_if c; EQEQ }
  | "!=" as c                         { Echo.print_if c; NEQ }
  | "<=" as c                         { Echo.print_if c; LEQ }
  | ">=" as c                         { Echo.print_if c; GEQ }
  | "=" as c                          { Echo.print_char_if c; EQ }
  | "<" as c                          { Echo.print_char_if c; LT }
  | ">" as c                          { Echo.print_char_if c; GT }
  | "++" as c                         { Echo.print_if c; INC }
  | "--" as c                         { Echo.print_if c; DEC }
  | "->" as c                         { Echo.print_if c; PTR }
  | "+" as c                          { Echo.print_char_if c; PLUS }
  | "-" as c                          { Echo.print_char_if c; MINUS }
  | "*" as c                          { Echo.print_char_if c; STAR }
  | "/" as c                          { Echo.print_char_if c; SLASH }
  | "%" as c                          { Echo.print_char_if c; PERCENT }
  | "!" as c                          { Echo.print_char_if c; BANG }
  | "&&" as c                         { Echo.print_if c; ANDAND }
  | "||" as c                         { Echo.print_if c; BARBAR }
  | "&" as c                          { Echo.print_char_if c; AND }
  | "|" as c                          { Echo.print_char_if c; BAR }
  | "^" as c                          { Echo.print_char_if c; HAT }
  | "?" as c                          { Echo.print_char_if c; QUESTION }
  | ":" as c                          { Echo.print_char_if c; COLON }
  | "~" as c                          { Echo.print_char_if c; TILDE }
  | "{"|"<%" as c                     { Echo.print_if c; LBRACE }
  | "}"|"%>" as c                     { Echo.print_if c; RBRACE }
  | "["|"<:" as c                     { Echo.print_if c; LBRACK }
  | "]"|":>" as c                     { Echo.print_if c; RBRACK }
  | "(" as c                          { Echo.print_char_if c; LPAREN }
  | ")" as c                          { Echo.print_char_if c; RPAREN }
  | ";" as c                          { Echo.print_char_if c; SEMICOLON }
  | "," as c                          { Echo.print_char_if c; COMMA }
  | "." as c                          { Echo.print_char_if c; DOT }
  | identifier as id              {
    Echo.print_if id; 
    if SSet.mem id !ignored_keywords then
      initial lexbuf
    else
      try Hashtbl.find lexicon id 
      with Not_found -> PRE_NAME id }
  | eof                           { EOF }
  | _                       { failwith "Lexer error" }

and initial_linebegin = parse
  | '\n'                          { Echo.print_always "\n"; new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline as c   { Echo.print_char_if c; initial_linebegin lexbuf }
  | '#'                          { Echo.print_always "#"; hash lexbuf }
  | ""                           { initial lexbuf }

and char = parse
  | simple_escape_sequence as c        { Echo.print_if c }
  | octal_escape_sequence as c         { Echo.print_if c }
  | hexadecimal_escape_sequence as c   { Echo.print_if c }
  | universal_character_name as c      { Echo.print_if c }
  | '\\' _                        { failwith "incorrect escape sequence" }
  | _ as c                             { Echo.print_char_if c }

and char_literal_end = parse
  | '\'' as c       { Echo.print_char_if c}
  | '\n' | eof { failwith "missing terminating \"'\" character" }
  | ""         { char lexbuf; char_literal_end lexbuf }

and string_literal = parse
  | '\"' as c      { Echo.print_char_if c }
  | '\n' | eof { failwith "missing terminating '\"' character" }
  | ""         { char lexbuf; string_literal lexbuf }

(* We assume gcc -E syntax but try to tolerate variations. *)
and hash = parse
  | whitespace_char_no_newline +
    (digit +)
    whitespace_char_no_newline *
    "\"" ([^ '\n' '\"']*) "\""
    [^ '\n']* '\n' as c
      { Echo.print_always c; (*JOSH - need? not there new_line lexbuf;*) initial_linebegin lexbuf }
  | whitespace_char_no_newline *
    "pragma"
    whitespace_char_no_newline +
    ([^ '\n']* as s) '\n' as c
      { Echo.print_always c; new_line lexbuf; PRAGMA s }
  | [^ '\n']* '\n' as c
      { Echo.print_always c; new_line lexbuf; initial_linebegin lexbuf }
  | [^ '\n']* eof
      { failwith "unexpected end of file" }
  | _
      { failwith "Lexer error" }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/" as c   { Echo.print_if c; () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { Echo.print_always "\n"; new_line lexbuf; multiline_comment lexbuf }
  | _ as c      { Echo.print_char_if c; multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { Echo.print_always "\n"; new_line lexbuf }
  | eof    { () }
  | _ as c      { Echo.print_char_if c; singleline_comment lexbuf }

{

  (* This lexer chooses between [inital] or [initial_linebegin],
     depending on whether we are at the beginning of the line or
     not. *)

  let lexer : lexbuf -> token =
    fun lexbuf ->
      if lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol then
        initial_linebegin lexbuf
      else
        initial lexbuf

  let lexer_aux tokens : lexbuf -> Parser.token =
    let curr_id = ref None in
    types_context := init_ctx;
    fun lexbuf ->
      match !curr_id with
      | Some id ->
        curr_id := None;
        let token =
          if SSet.mem id !types_context then Parser.TYPEDEF_NAME (id, ref TypedefId)
          else Parser.VAR_NAME (id, ref VarId)
        in
        Queue.push token tokens;
        token
      | None ->
        let token = lexer lexbuf in
        begin match token with
        | PRE_NAME id -> curr_id := Some id
        | _ -> Queue.push token tokens
        end;
        token

    let lexer : lexbuf -> Parser.token = lexer_aux (Queue.create ())

}
