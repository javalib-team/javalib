(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine Turpin <first.last@irisa.fr>
 *  Laurent Hubert <first.last@irisa.fr>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open JBasics
open JClass
open JProgram

module PP = struct
  type t = {cl:interface_or_class;
	    meth:concrete_method;
	    pc:int;}

  exception NoCode of (class_name * method_signature)

  let to_string (pp:t) : string =
    let s = pp.meth.cm_signature in
    JDumpBasics.class_name (get_name pp.cl)
      ^ "."^ s.ms_name ^"("
      ^ (String.concat ", "
	    (List.map (JDumpBasics.value_signature "") s.ms_parameters))
      ^ "): " ^ string_of_int pp.pc

  let get_class (pp:t) : interface_or_class =
    pp.cl
    
  let get_meth (pp:t) : concrete_method =
    pp.meth

  let get_pc (pp:t) : int = pp.pc

  let get_pp cl' meth' pc' : t = {cl=cl';meth=meth';pc=pc';}

  (** @raise NoCode if the method is abstract or not found. *)
  let get_first_pp prog cn ms : t =
    match get_interface_or_class prog cn with
      | `Interface {i_initializer = Some m} as c when m.cm_signature = ms ->
	  {cl=c;meth=m;pc=0;}
      | `Class c as cl' when MethodMap.mem ms c.c_methods ->
	  begin
	    match MethodMap.find ms c.c_methods with
	      | ConcreteMethod m->
		  {cl=cl';
		   meth=m;
		   pc=0;}
	      | _ -> raise (NoCode (cn,ms))
	  end
      | _ -> raise (NoCode (cn,ms))

  let get_first_pp_wp c ms : t =
    match get_method c ms with
      | ConcreteMethod m -> {cl=c;meth=m;pc=0;}
      | AbstractMethod _ -> raise (NoCode (get_name c,ms))

  let goto_absolute pp i : t = {pp with pc=i;}

  let goto_relative pp jmp : t ={pp with pc=pp.pc+jmp;}

end

open PP
type pp = PP.t

let get_code (pp:pp): opcodes =
  match pp.meth.cm_implementation with
    | Java c -> c.c_code
    | Native -> raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

let get_opcode (pp:pp) :opcode = (get_code pp).(pp.pc)

let next_instruction pp =
  let opcodes = get_code pp
  and i = ref (succ pp.pc)
  in
    while opcodes.(!i) = OpInvalid do
      incr i;
    done;
    goto_absolute pp !i

let normal_successors pp =
  match get_opcode pp with
    | OpIf (_,l)
    | OpIfCmp (_,l) ->
	[next_instruction pp; goto_relative pp l]
    | OpJsr l
    | OpGoto l -> [goto_relative pp l]
    | OpRet _ -> (* all instruction following a jsr are returned. *)
	let code = get_code pp in
	let i = ref 0 in
	let l = ref [] in
	  while !i < Array.length code do
	    begin
	      match code.(!i) with
		| OpJsr _ ->
		    l := next_instruction (goto_absolute pp !i)::!l
		| _ -> ()
	    end;
	    incr i;
	  done;
	  !l
    | OpTableSwitch (default,_,_,others) ->
	Array.fold_left
	  (fun ppl jmp -> goto_relative pp jmp::ppl)
	  [goto_relative pp default]
	  others
    | OpLookupSwitch (default,others) ->
	List.fold_left
	  (fun ppl (_,jmp) -> goto_relative pp jmp::ppl)
	  [goto_relative pp default]
	  others
    | OpThrow
    | OpReturn _ -> []
    | OpInvalid
    | OpBreakpoint -> assert false
    | _ -> [next_instruction pp]

let static_lookup_static prog cn ms =
  let c =
    match resolve_class prog cn with
      | `Class c -> resolve_method ms c
      | `Interface c -> resolve_interface_method ms c
  in
    match c with
      | `Class c' -> c'::overridden_by_methods ms c
      | `Interface _ -> overridden_by_methods ms c

let static_lookup_interface prog cn ms =
  let c =
    match resolve_class prog cn with
      | `Interface i -> resolve_interface_method ms i
      | `Class _ -> raise IncompatibleClassChangeError
  in
    match c with
      | `Class c' -> c'::overridden_by_methods ms c
      | `Interface _ -> overridden_by_methods ms c

let static_lookup_special prog pp cn ms =
  try
    match resolve_class prog cn with
      | `Interface _ -> raise IncompatibleClassChangeError
      | `Class c ->
	  let c' = resolve_method ms c in
	    match pp.cl,c' with
	      | `Class c1, `Class c2 when
		    (ms.ms_name = "<init>"
			|| not (extends_class c1 c2)) ->
		  [c2]
	      | _ ->
		  match super_class (`Class c) with
		    | None -> raise AbstractMethodError
		    | Some c -> [lookup_virtual_method ms c]
  with
    | NoClassDefFoundError | NoSuchMethodError
    | AbstractMethodError -> []

let static_lookup_virtual prog obj ms =
  match obj with
    | TArray _ ->
	raise (Failure ("invokevirutal on arrays are not supported yet."))
    | TClass cn ->
	let c =
	  match resolve_class prog cn with
	    | `Class c -> resolve_method ms c
	    | `Interface _ -> raise IncompatibleClassChangeError
	in
	  match c with
	    | `Class c' -> c'::overridden_by_methods ms c
	    | `Interface _ -> overridden_by_methods ms c


let handlers pp =
  match pp.meth.cm_implementation with
    | Java code ->
	List.filter (fun e -> e.e_start <= pp.pc && pp.pc < e.e_end) code.c_exc_tbl
    | Native ->
	raise (NoCode (get_name pp.cl,pp.meth.cm_signature))

let exceptional_successors pp =
  List.map (fun e -> goto_absolute pp e.e_handler) (handlers pp)
