(*pp ocaml+twt*)

open Cil
open Formatcil
module L = List
module I = Int64

(* Assume that main is defined in only one file. Stop searching for others. *)
let lookingForMain = ref true

let dummyVarinfo = makeGlobalVar "dummy" voidType 

let check_stacks = ref dummyVarinfo
let check_obj = ref dummyVarinfo
let munprotect = ref dummyVarinfo
let mreprotect = ref dummyVarinfo
let write_stacks = ref dummyVarinfo
let write_obj = ref dummyVarinfo
let write_long = ref dummyVarinfo
let write_double = ref dummyVarinfo

let initProtection (f:file) =
  (* Declare all globals we'll need in other modules *)
  check_stacks := findOrCreateFunc f "check_stacks" (cType "void ()(void *, unsigned int)" [])
  check_obj := findOrCreateFunc f "check_obj" (cType "void ()(void *, unsigned int)" [])
  munprotect := findOrCreateFunc f "munprotect" (cType "void ()(void *, unsigned int)" [])
  mreprotect := findOrCreateFunc f "mreprotect" (cType "void ()(void *, unsigned int)" [])
  write_stacks := findOrCreateFunc f "update_stacks" (cType "void ()(void *, unsigned int)" [])
  write_obj := findOrCreateFunc f "write_obj" (cType "void ()(void *, void *, unsigned int)" [])
  write_long := findOrCreateFunc f "write_long" (cType "void ()(void *, long long, int)" [])
  write_double := findOrCreateFunc f "write_double" (cType "void ()(void *, double, int)" [])

  if !lookingForMain then
    List.iter
      function
        | GFun(main_fun, _) when main_fun.svar.vname = "main" ->
            lookingForMain := false
            (* Inject a call in main *)
            let protInit_func = findOrCreateFunc f "protInit" (cType "void ()(void)" [])
            let call_protInit =
              cStmt "protInit();"
                (fun n t -> makeTempVar main_fun ~name:n t)
                locUnknown
                [("protInit", Fv protInit_func)]
            main_fun.sbody.bstmts <- call_protInit :: main_fun.sbody.bstmts
        | _ -> ()
      f.globals

let is_vararg f = match f.svar.vtype with
  | TFun (_, _, isvararg, _) -> isvararg

(* See protect.h for the magic numbers  *)
let tag_of_type t =
  let tag = match unrollType t with
    | TInt(IChar,_) | TInt(ISChar,_) | TInt(IUChar,_) -> 0
    | TInt(IInt,_) | TInt(IUInt,_) -> 1
    | TInt(IShort,_) | TInt(IUShort,_) -> 2
    | TInt(ILong,_) | TInt(IULong,_) -> 3
    | TInt(ILongLong,_) | TInt(IULongLong,_) -> 4
    | TPtr _ -> 3

    | TFloat(FFloat,_) -> 0
    | TFloat(FDouble,_) -> 1
    
  Const(CInt64(I.of_int tag, IInt, None))
