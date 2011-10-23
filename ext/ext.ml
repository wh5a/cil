(*pp ocaml+twt*)

(* CIL libs *)
open Cil
open Formatcil
module P = Pretty
module E = Errormsg
module U = Util
let log = E.log
let warn = E.warn
let list_map = U.list_map

(* Just an example showing how to use CIL's trace module for debugging *)
Trace.traceAddSys "blah"
let trace = Trace.trace "blah"

(* Standard libs http://caml.inria.fr/pub/docs/manual-ocaml/manual034.html *)
open Printf
module L = List

(* ExtLib http://ocaml-extlib.googlecode.com/svn/doc/apiref/ deprecated
   We use the counterpart of the Std module in batteries
   http://thelema.github.com/batteries-included/hdoc/
*)
(* module S = BatStd *)

(* My own modules *)
open Analyze
open Helper

let be_verbose = ref false
let prot_all = ref false

(* let d_stmt () s = printStmt defaultCilPrinter () s *)
let d_plainstmt = printStmt plainCilPrinter
let d_plaininstr = printInstr plainCilPrinter

let currentFun () = match !currentGlobal with
  | GFun (f, _) -> f

(* Protect reads in expressions on a particular line. *)
let rec protect_exp (queueInstr: instr list -> unit) (e:exp) = match e with
  | Lval l -> protect_addr queueInstr l; protect_obj queueInstr l
  (* TODO: for AddrOf and StartOf, their value cannot be corrupted, but
     their referent must be protected if crossing library function calls *)
  | UnOp (_, e1, _) -> protect_exp queueInstr e1
  | BinOp (_, e1, e2, _) -> protect_exp queueInstr e1; protect_exp queueInstr e2
  | CastE (_, e1) -> protect_exp queueInstr e1
  | _ -> ()

and protect_addr queueInstr (lhost,offset) =
  match lhost with
    | Var _ -> ()
    | Mem e -> protect_exp queueInstr e
  protect_offset queueInstr offset

and protect_obj queueInstr ((lhost,_) as lval) =
  (* TODO: Globals may or may not be write protected, depending on our strategy
     TODO: Care should be taken for arrays/pointers if they're being passed to library functions!! *)
  match lhost with
    | Var v -> if not v.vglob then
        v.vaddrof <- true
        let call_check = [cInstr "check_stacks(& %l:lval, sizeof(%l:lval));" locUnknown
                          [("check_stacks", Fv !check_stacks)
                          ;("lval", Fl lval)]
                         ]
        queueInstr call_check
    | _ ->
        let call_check = [cInstr "check_obj(& %l:lval, sizeof(%l:lval));" locUnknown
                          [("check_obj", Fv !check_obj)
                          ;("lval", Fl lval)]
                         ]
        queueInstr call_check

and protect_offset queueInstr offset =
  match offset with
    | NoOffset -> ()
    | Field (_, off) -> protect_offset queueInstr off
    | Index (e, off) -> protect_exp queueInstr e; protect_offset queueInstr off

(* Authorize writes in assignments on a particular line. *)
let write_lval ((lhost,_) as lval) expr instr =
  match lhost with
    | Var v -> (* Direct writes are easier to handle *)
        v.vaddrof <- true
        if v.vglob then
          let call_munprotect = cInstr "munprotect(& %l:lval, sizeof(%l:lval));" locUnknown
                                 [("munprotect", Fv !munprotect)
                                 ;("lval", Fl lval)]
          and call_mreprotect = cInstr "mreprotect(& %l:lval, sizeof(%l:lval));" locUnknown
                                 [("mreprotect", Fv !mreprotect)
                                 ;("lval", Fl lval)]
          [call_munprotect; instr; call_mreprotect]
        else
          let call_write = cInstr "write_stacks(& %l:lval, sizeof(%l:lval));" locUnknown
                             [("write_stacks", Fv !write_stacks)
                             ;("lval", Fl lval)]
          [instr; call_write]
    | Mem ptr -> (* Indirect writes:  *ptr = expr *)
        let fdec = currentFun ()
        let myType = typeOfLval lval
        if isScalarType myType then
          let writer = match myType with
            | TFloat _ -> !write_double
            (* TODO: We currently use long long for greatest compatibility. But long long is 8 bytes on
                     32-bit machines which is inefficient *)
            | _ -> !write_long
          let type_tag = tag_of_type myType
          let call_write = cInstr "writer(%e:ptr, %e:expr, %e:type_tag);" locUnknown
                            [("writer", Fv writer)
                            ;("ptr", Fe ptr)
                            ;("expr", Fe expr)
                            ;("type_tag", Fe type_tag)]
          [call_write]
        else
         match expr with
          | Lval l ->
              let call_write = cInstr "write_obj(%e:ptr, & %l:l, sizeof(* %e:ptr));" locUnknown
                                [("write_obj", Fv !write_obj)
                                ;("ptr", Fe ptr)
                                ;("l", Fl l)]
              [call_write]
          | _ ->
              let v = makeTempVar fdec ~name:"cil_copy" myType
              let copy = cInstr "v = %e:expr;" locUnknown
                          [("v", Fv v)
                          ;("expr", Fe expr)]
              v.vaddrof <- true
              let call_write = cInstr "write_obj(%e:ptr, &v, sizeof(* %e:ptr));" locUnknown
                                [("write_obj", Fv !write_obj)
                                ;("ptr", Fe ptr)
                                ;("v", Fv v)]
              [copy; call_write]

(* Authorize writes in return value of function calls on a particular line. *)
let write_ret ((lhost,_) as lval) func args =
  match lhost with
    | Var v ->
        let instr = Call (Some lval, func, args, locUnknown)
        v.vaddrof <- true
        if v.vglob then
          let call_munprotect = cInstr "munprotect(& %l:lval, sizeof(%l:lval));" locUnknown
                                 [("munprotect", Fv !munprotect)
                                 ;("lval", Fl lval)]
          and call_mreprotect = cInstr "mreprotect(& %l:lval, sizeof(%l:lval));" locUnknown
                                 [("mreprotect", Fv !mreprotect)
                                 ;("lval", Fl lval)]
          [call_munprotect; instr; call_mreprotect]
        else
          let call_write = cInstr "write_stacks(& %l:lval, sizeof(%l:lval));" locUnknown
                             [("write_stacks", Fv !write_stacks)
                             ;("lval", Fl lval)]
          [instr; call_write]
    | Mem ptr ->
        let fdec = currentFun ()
        let myType = typeOfLval lval
        let v = makeTempVar fdec ~name:"cil_copy" myType
        let copy = Call (Some(Var v,NoOffset), func, args, locUnknown)
        if isScalarType myType then
          let writer = match myType with
            | TFloat _ -> !write_double
            (* TODO: We currently use long long for greatest compatibility. But long long is 8 bytes on
                     32-bit machines which is inefficient *)
            | _ -> !write_long
          let type_tag = tag_of_type myType
          let call_write = cInstr "writer(%e:ptr, v, %e:type_tag);" locUnknown
                            [("writer", Fv writer)
                            ;("ptr", Fe ptr)
                            ;("v", Fv v)
                            ;("type_tag", Fe type_tag)]
          [copy; call_write]
        else
          v.vaddrof <- true
          let call_write = cInstr "write_obj(%e:ptr, &v, sizeof(* %e:ptr));" locUnknown
                            [("write_obj", Fv !write_obj)
                            ;("ptr", Fe ptr)
                            ;("v", Fv v)]
          [copy; call_write]

(* The entry point of our instrumentor, which dispatches on statements and instructions. *)
class entryVisitor = object(self)
  inherit nopCilVisitor

  method vglob g = match g with
    | GVar (v, initinfo, loc) ->
        (* These globals are no longer required by GCC to be explicitly initialized
           http://gcc.gnu.org/onlinedocs/gcc-4.5.1/gcc/Variable-Attributes.html

        match initinfo.init with
          | None -> initinfo.init <- makeZeroInit ...
          | _ -> ()
        *)
        if !prot_all || is_glob_critical v.vname then
          v.vattr <- Attr("section", [AStr ".protected"]) :: v.vattr
        else if !be_verbose then printf "Global %s skipped\n" v.vname
        SkipChildren
    (* TODO: Only shadow copy critical args.
       TODO: Combine the multiple updates into a single one.
       TODO: support varargs. *)
    | GFun (fundec, _) ->
        if is_vararg fundec then
           warn "%s is a vararg function.\n" fundec.svar.vname
        let protect_arg arg =
          let lval = var arg
          cStmt "write_stacks(& %l:lval, sizeof(%l:lval));"
            (fun n t -> makeTempVar fundec ~name:n t)
            locUnknown
            [("write_stacks", Fv !write_stacks)
            ;("lval", Fl lval)]
        let protect_args = L.map protect_arg fundec.sformals
        fundec.sbody.bstmts <- protect_args @ fundec.sbody.bstmts
        DoChildren
    | _ -> DoChildren

  method vstmt s = match s.skind with 
    | Instr _ -> DoChildren
    | _ ->
        let l = !currentLoc
        if !be_verbose then
          printf "=== In %s at line %n, " l.file l.line
          log "  %a\n\n\n" d_plainstmt s

        (* TODO: For now we don't use the lists directly because CSurf dereferences pointer aliases.
           We naively protect/check everything if this line is involved. Probably redundant, or even
           wrong when we check an unprotected variable.
        *)
        let critical_used = lookup_use (l.file,l.line)
        and critical_defined = lookup_define (l.file,l.line)

        (* Sanity check *)
        match critical_defined with
          | [] -> ()
          | _ -> warn "The statement cannot possibly define a critical variable.\n"

        (* Skip if critical_used is empty *)
        if (not !prot_all) && (critical_used = []) then
          if !be_verbose then printf "Line %n skipped\n" l.line
        else (* Protect critical variables being read *)
              let queueInstr = self#queueInstr
              match s.skind with
                | Return (Some e, _) -> protect_exp queueInstr e
                | If (e, _, _, _) -> protect_exp queueInstr e
                | Switch (e, _, _, _) -> protect_exp queueInstr e
                | _ -> ()
        
        DoChildren

  (***********************************)
  method vinst i =
    (* TODO: Care should be taken for arrays/pointers if they're being passed to library functions!! *)
    let l = !currentLoc
    if !be_verbose then
      printf "=== In %s at line %n, " l.file l.line
      log "  %a\n\n\n" d_plaininstr i

    let protect_use = lookup_use (l.file,l.line) <> []
    and protect_def = lookup_define (l.file,l.line) <> []

    match (!prot_all, protect_use, protect_def) with
      | (false, false, false) ->
        if !be_verbose then printf "Line %n skipped\n" l.line
        DoChildren

      | _ ->
        let queueInstr = self#queueInstr
    
        match i with
          | Set (l, e, _) ->
              (* TODO: Make sure pointers writes do not go beyond boundaries *)
              (* protect_addr queueInstr l *)
              if protect_use || !prot_all then protect_exp queueInstr e
              if protect_def || !prot_all then ChangeTo (write_lval l e i) else DoChildren

          (* TODO: varargs *)
          | Call (retMaybe, (Lval l as func), args, _) ->
              (* TODO: Similarly make sure writes do not go beyond bounds *)
              if protect_use || !prot_all then
                protect_addr queueInstr l
                L.iter (protect_exp queueInstr) args
              (* TODO: ChangeTo write_lval queueInstr retMaybe *)
              match (retMaybe, protect_def || !prot_all) with
                | (Some ret, true) -> ChangeTo (write_ret ret func args)
                | _ -> DoChildren

          | Asm _ ->
              warn "Assembly unsupported in %s at line %d!\n" l.file l.line
              SkipChildren

let handle_file file =
  log "%s : parsing\n\n" file
  let cil_ast = Frontc.parse file ()
  (* We could have merged the files before any real work *)
  Rmtmps.removeUnusedTemps cil_ast
  initProtection cil_ast
  visitCilFile (new entryVisitor) cil_ast
  let cname = file ^ ".cil.c"
  and cilname = file ^ ".cil"
  let chan = open_out cname
  and chan' = open_out cilname
  log "Dumping to %s and %s\n" cname cilname
  dumpFile defaultCilPrinter chan cname cil_ast
  close_out chan
  dumpFile plainCilPrinter chan' cilname cil_ast
  close_out chan'

let main () =
  let argDesc = [ 
    "--verbose", Arg.Set be_verbose, "\temit verbose logs" ;
    "--protall", Arg.Set prot_all, "\tprotect everything regardless of the analysis" ;
  ]
  initCIL ()
  (* suppress printing line numbers *)
  lineDirectiveStyle := None
  (* Depending on whether cil or gcc is the consumer,
     set Cil.print_cil_input to true or false. This may be tricky.
  *)
  E.colorFlag := true
  E.logChannel := stdout
  Arg.parse argDesc handle_file "Usage:\n"
  trace (P.dprintf "useless trace message\n")

main ()
