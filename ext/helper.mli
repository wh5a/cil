open Cil

val initProtection : file -> unit

val check_stacks : varinfo ref
val check_obj : varinfo ref
val munprotect : varinfo ref
val mreprotect : varinfo ref
val write_stacks : varinfo ref
val write_obj : varinfo ref
val write_long : varinfo ref
(* long double > double > float. I'm betting no one uses long double
   http://stackoverflow.com/questions/399114/why-are-c-c-floating-point-types-so-oddly-named *)
val write_double : varinfo ref

val is_vararg : fundec -> bool

val tag_of_type : typ -> exp
