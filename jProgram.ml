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

module ClassMap = Map.Make(struct type t = class_name let compare = compare end)


type abstract_class = {
  ac_super_class : class_file option;
  ac_fields : class_field FieldMap.t;
  ac_methods : abstract_class_method MethodMap.t
}

and concrete_class = {
  cc_final : bool;
  cc_super_class : class_file option;
  cc_fields : class_field FieldMap.t;
  cc_methods : concrete_method MethodMap.t
}

and interface = {
  i_super : class_file; (** must be java.lang.Object. *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t
}

and class_file_type =
    | ConcreteClass of concrete_class
    | AbstractClass of abstract_class
    | Interface of interface

and class_file = {
  name : class_name;
  c_access : [`Public | `Default];
  interfaces : class_file ClassMap.t;
  sourcefile : string option;
  c_deprecated : bool;
  inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  class_file_type : class_file_type
}


type program = class_file ClassMap.t
type t = program

(** [Class_not_found c] is raised when trying to add a class when its
    super class or one of its implemented interfaces is not yet in the
    program structure (i.e. in the map).*)
exception Class_not_found of class_name

let add_classFile c program =
  let imap =
    List.fold_left
      (fun imap iname -> 
	let i = 
	  try ClassMap.find iname program
	  with Not_found -> raise (Class_not_found iname)
	in ClassMap.add iname i imap
      )
      ClassMap.empty
      c.JClass.interfaces
  in let cft =
    match c.JClass.class_file_type with
      | JClass.Interface c ->
	  let super = 
	    try ClassMap.find ["java";"lang";"Object"] program
	    with Not_found -> raise (Class_not_found ["java";"lang";"Object"])
	  in Interface {
	    i_super = super;
	    i_fields = c.JClass.i_fields;
	    i_methods = c.JClass.i_methods;
	  }
      | JClass.AbstractClass c ->
	  let super = 
	    begin
	      match c.JClass.ac_super_class with
		| None -> None
		| Some super ->
		    try Some (ClassMap.find super program)
		    with Not_found -> raise (Class_not_found super)
	    end
	  in AbstractClass {
	    ac_super_class = super;
	    ac_fields = c.JClass.ac_fields;
	    ac_methods = c.JClass.ac_methods;
	  }
      | JClass.ConcreteClass c ->
	  let super = 
	    begin
	      match c.JClass.cc_super_class with
		| None -> None
		| Some super ->
		    try Some (ClassMap.find super program)
		    with Not_found -> raise (Class_not_found super)
	    end
	  in ConcreteClass {
	    cc_final = c.JClass.cc_final;
	    cc_super_class = super;
	    cc_fields = c.JClass.cc_fields;
	    cc_methods = c.JClass.cc_methods;
	  }
  in 
       ClassMap.add 
	 c.JClass.name
	 {name = c.JClass.name;
	  c_access = c.JClass.c_access;
	  interfaces = imap;
	  sourcefile = c.JClass.sourcefile;
	  c_deprecated = c.JClass.c_deprecated;
	  inner_classes = c.JClass.inner_classes;
	  c_other_attributes = c.JClass.c_other_attributes;
	  class_file_type = cft;}
	 program


let parse_program class_path names =
  (* build a map of all the JClass.class_file that are going to be
     translated to build the new hierarchy.*)
  let class_map =
    JFile.read
      class_path
      (fun program c -> ClassMap.add c.JClass.name c program)
      ClassMap.empty
      names
      (* add_rec translate a class from JClass.class_file to
	 class_file and add it in the program structure, recursively
	 adding super classes and implemented interfaces if needed. *)
  in let rec add_rec (c:JClass.class_file) (program:program) : program =
    try
      add_classFile c program
    with 
      | Class_not_found name -> 
	  let missing_class = 
	    try ClassMap.find name class_map
	    with Not_found -> raise (Class_not_found name)
	  in 
	  add_rec c (add_rec (missing_class) program)
  in 
       ClassMap.fold 
	 (fun _ -> add_rec)
	 class_map
	 ClassMap.empty


let store_program filename program : unit =
  let ch = open_out_bin filename
  in 
    Marshal.to_channel ch program [];
    close_out ch

let load_program filename : program =
  let ch = open_in_bin filename
  in let p = (Marshal.from_channel ch : program);
  in close_in ch; p


(* Iterators *)
let fold f s p = ClassMap.fold (fun _ c s -> f s c) p s
let iter f p = ClassMap.iter (fun _ c -> f c) p

type any_method = abstract_class_method
type any_field =
    | InterfaceField of interface_field
    | ClassField of class_field

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception IncompatibleClassChangeError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchMethodError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoSuchFieldError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception NoClassDefFoundError

(** @see <http://java.sun.com/docs/books/jvms/second_edition/html/VMSpecTOC.doc.html> The JVM Specification *)
exception AbstractMethodError


(** this exception is raised to avoid a full unfolding of the
    hierarchy. *)
exception Found_Class of class_file


let defines_method ms c = match c.class_file_type with
  | Interface i -> MethodMap.mem ms i.i_methods
  | AbstractClass ac -> MethodMap.mem ms ac.ac_methods 
  | ConcreteClass nc -> MethodMap.mem ms nc.cc_methods 
let defines_field fs c = match c.class_file_type with
  | Interface i -> FieldMap.mem fs i.i_fields
  | AbstractClass ac -> FieldMap.mem fs ac.ac_fields 
  | ConcreteClass nc -> FieldMap.mem fs nc.cc_fields 
let super c = match c.class_file_type with
  | Interface i -> Some i.i_super
  | AbstractClass ac -> ac.ac_super_class
  | ConcreteClass nc -> nc.cc_super_class



let get_class program cn = ClassMap.find cn program

let resolve_class program cn =
  try get_class program cn with Not_found -> raise NoClassDefFoundError


let get_method c ms = 
  match c.class_file_type with
    | Interface c -> 
	AbstractMethod (MethodMap.find ms c.i_methods)
    | AbstractClass c ->
	MethodMap.find ms c.ac_methods
    | ConcreteClass c ->
	ConcreteMethod (MethodMap.find ms c.cc_methods)

let get_field c fs =
  match c.class_file_type with
    | Interface c -> 
	InterfaceField (FieldMap.find fs c.i_fields)
    | AbstractClass c ->
	ClassField (FieldMap.find fs c.ac_fields)
    | ConcreteClass c ->
	ClassField (FieldMap.find fs c.cc_fields)


let rec resolve_field fs c : class_file =
  if defines_field fs c
  then c
  else
    try
      ClassMap.iter 
	(fun _ i -> 
	  let i = resolve_field fs i
	  in raise (Found_Class i)
	)
	c.interfaces;
      match super c with
	| Some super -> resolve_field fs super
	| None -> raise NoSuchFieldError
    with Found_Class resolved -> resolved

(** [resolve_method' ms c] looks for the method [ms] in [c] and 
    recursively in its super-classes.
    @raise NoSuchMethodError if [ms] has not been found. *)
let rec resolve_method' ms c : class_file =
  if defines_method ms c
  then c
  else
    match super c with
      | Some super -> resolve_method' ms super
      | None -> raise NoSuchMethodError

(** [resolve_interface_method' ms c] looks for the method [ms] in [c]
   and recursively in its interfaces.
   @raise Found_Class if an interface specifying [ms] has been found.
   @raise NoSuchMethodError if [ms] has not been found *)
let rec resolve_interface_method' ms c : unit =
  if defines_method ms c
  then raise (Found_Class c)
  else ClassMap.iter (fun _ i -> resolve_interface_method' ms i) c.interfaces;
  raise NoSuchMethodError

let resolve_method ms c : class_file =
  match c.class_file_type with
    | ConcreteClass _ ->
	begin
	  let c' = resolve_method' ms c
	  in match get_method c' ms with
	    | AbstractMethod _ -> raise AbstractMethodError
	    | ConcreteMethod _ -> c'
	end
    | AbstractClass _ ->
	let rec resolve_abstract_method ms c : class_file =
	  try resolve_method' ms c
	  with NoSuchMethodError -> 
	    try 
	      resolve_interface_method' ms c;
	      failwith "resolve_interface_method' should always finish with an exception"
	    with 
	      | Found_Class resolved -> resolved
	      | NoSuchMethodError -> 
		  match super c with
		    | None -> raise NoSuchMethodError
		    | Some c' -> resolve_abstract_method ms c'
	in resolve_abstract_method ms c
    | Interface i -> 
	raise IncompatibleClassChangeError

let resolve_interface_method ms c : class_file =
  match c.class_file_type with
    | ConcreteClass _
    | AbstractClass _ -> raise IncompatibleClassChangeError
    | Interface i ->
	try 
	  resolve_interface_method' ms c;
	  failwith "resolve_interface_method' should always finish with an exception"
	with
	  | Found_Class resolved -> resolved
	  | NoSuchMethodError ->
	      resolve_method' ms i.i_super (* super = java.lang.object *)

let lookup_virtual_method ms c : class_file =
  let c' = 
    try resolve_method' ms c
    with NoSuchMethodError -> raise AbstractMethodError
  in match get_method c ms with
      | AbstractMethod m -> raise AbstractMethodError
      | ConcreteMethod m -> c'

let lookup_interface_method = lookup_virtual_method


(* {2 Access to the hierarchy} *)

(** The name of a real class, i.e., not an interface or an implicit array class name. *)
type className = class_name

type interfaceName = class_name

let classOrInterfaceName_of_ident p cn =
  let c = ClassMap.find cn p 
  in match c.class_file_type with
    | ConcreteClass _ | AbstractClass _ -> `Class c.name
    | Interface _ -> `Interface c.name

let rec extends_class' c1 c2 : bool =
  match super c1 with
    | None -> false;
    | Some c3 -> 
	if c2==c3 
	then true
	else extends_class' c3 c2

let extends_class p cn1 cn2 : bool =
  if cn1=cn2
  then true
  else extends_class' (ClassMap.find cn1 p) (ClassMap.find cn2 p)

let rec extends_interface' i1 in2 : bool =
  ClassMap.fold
    (fun in3 i3 b -> b || (in2=in3) || (extends_interface' i3 in2))
    i1.interfaces
    false

let extends_interface p in1 in2 : bool =
  if in1=in2
  then true
  else extends_interface' (ClassMap.find in1 p) in2

let rec implements' c1 in2 : bool =
  if extends_interface' c1 in2
  then true
  else match super c1 with
    | None -> false
    | Some c3 -> implements' c3 in2
  
let implements p cn1 in2 : bool = 
  implements' (ClassMap.find cn1 p) in2

let super_class p cn : className =
  let c = ClassMap.find cn p
  in match super c with
    | None -> raise Not_found
    | Some c -> c.name

let rec super_interfaces' i =
  ClassMap.fold 
    (fun iname i ilist -> 
      iname::(List.rev_append (super_interfaces' i) ilist))
    i.interfaces
    []

let super_interfaces p iname =
  super_interfaces' (ClassMap.find iname p)

let rec implemented_interfaces' c = 
  match super c with
    | None -> super_interfaces' c
    | Some c' -> List.rev_append (super_interfaces' c) (implemented_interfaces' c')

let implemented_interfaces p cn =
  let rec rem_dbl = function
    | e::l -> e:: (List.filter ((<>)e) (rem_dbl l))
    | [] -> []
  in rem_dbl (implemented_interfaces' (ClassMap.find cn p))

(* val firstCommonSuperClass : t -> className -> className -> className *)

