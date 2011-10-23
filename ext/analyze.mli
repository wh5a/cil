type key = string * int                 (* Filename, linenum *)
type value = string                     (* Varname *)

val lookup_define : key -> value list
val lookup_use : key -> value list

val is_glob_critical: string -> bool
