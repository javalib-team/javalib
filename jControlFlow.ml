(*
 *  This file is part of JavaLib
 *  Copyright (c)2007 Université de Rennes 1 / CNRS
 *  Tiphaine.Turpin@irisa.fr
 *  Laurent.Hubert@irisa.fr
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

open JClassLow
open JClass
open JProgram

module PP : sig
  type t
  exception NoCode of (class_name * method_signature)
  val to_className : t -> JClassLow.class_name
  val to_ConcreteClass : t -> JProgram.concrete_class
  val to_class : t -> JProgram.class_file
  val to_hardpp : t -> JProgram.class_file * JClass.code * int
  val to_softpp :
    t -> (JClassLow.class_name * JClass.method_signature) * int
  val get_first_pp :
    JProgram.t -> JClassLow.class_name * JClass.method_signature -> t
  val goto_absolute : t -> int -> t
  val goto_relative : t -> int -> t
end =
struct
  type t = {hardpp : class_file * code * int;
       	    softpp : (class_name * method_signature) * int;}

  let to_className (pp:t) : class_name = fst (fst pp.softpp)

  let to_class (pp:t) : class_file = let (c,_,_) = pp.hardpp in c

  let to_ConcreteClass (pp:t) : concrete_class =
    let (c,_,_) = pp.hardpp in
      match c.class_file_type with
	| ConcreteClass nc -> nc
	| _ -> raise (Invalid_argument "the program point is invalid (the class is not concrete)")

  let to_hardpp (pp:t) : class_file * code * int = pp.hardpp
  let to_softpp (pp:t) : (class_name * method_signature) * int = pp.softpp

  let hard2soft (c,m,i) =
    let res = ref None in
      match c.class_file_type with
    	| ConcreteClass cc ->
    	    begin
    	      MethodMap.iter
		(fun ms' m' -> if m==m' then res := Some ms')
		cc.cc_methods;
    	      match !res with
    		| Some ms -> ((c.name,ms),i)
    		| None -> raise (Invalid_argument "the program point is invalid (the method has not been found in the class)")
    	    end
    	| _ -> raise (Invalid_argument "the program point is invalid (the class is not concrete)")


  exception NoCode of (JClassLow.class_name * JClass.method_signature)
  let soft2hard prog ((cn,ms),i) =
    let c = get_class prog cn in
    let m = 
      match get_method c ms with
	| ConcreteMethod m ->
	    begin
	      match m.implementation with
		| Java c -> c
		| Native -> raise (NoCode (cn,ms))
	    end
	| AbstractMethod _ -> raise (NoCode (cn,ms))
    in (c,m,i)

  (** @raise NoCode if the method is abstract or not found. *)
  let get_first_pp prog cms : t =
    let soft = (cms,0) in
      {hardpp = soft2hard prog soft;
       softpp = soft;}

  let goto_absolute pp i : t = 
    { hardpp = (let (c,m,_) = pp.hardpp in (c,m,i));
      softpp = (fst pp.softpp, i);}

  let goto_relative pp jmp : t =
    let (c,m,i) = pp.hardpp in 
      { hardpp = (c,m,i+jmp);
	softpp = (fst pp.softpp, i+jmp);}

end

open PP
type pp = PP.t

let get_opcode (pp:pp) :opcode = let (_,m,i) = PP.to_hardpp pp in m.c_code.(i)

let next_instruction pp = 
  let (c,m,i) = PP.to_hardpp pp in
  let i = ref (succ i) in
    while m.c_code.(!i) = OpInvalid do
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
	let (c,m,i) = PP.to_hardpp pp in
	let i = ref 0 in
	let l = ref [] in
	  while !i < Array.length m.c_code do
	    begin
	      match m.c_code.(!i) with
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

let handlers pp =
  let (_,m,i) = PP.to_hardpp pp in
    List.filter (fun e -> e.e_start <= i && i < e.e_end) m.c_exc_tbl


let exceptional_successors pp = 
  List.map (fun e -> goto_absolute pp e.e_handler) (handlers pp)
