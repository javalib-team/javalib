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

open JBasics
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

and class_file_type =
    | ConcreteClass of concrete_class
    | AbstractClass of abstract_class

and class_file = {
  c_name : class_name;
  c_access : [`Public | `Default];
  c_interfaces : interface_file ClassMap.t;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  c_class_file_type : class_file_type;
  mutable c_children : class_file ClassMap.t;
}

and interface_file = {
  i_name : class_name;
  i_access : [`Public | `Default];
  i_interfaces : interface_file ClassMap.t;
  i_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_inner_classes : inner_class list;
  i_other_attributes : (string * string) list;
  i_super : class_file; (** must be java.lang.Object. *)
  i_initializer : concrete_method option; (* should be static/ signature is <clinit>()V; *)
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
  mutable i_children_interface : interface_file ClassMap.t;
  mutable i_children_class : class_file ClassMap.t;
}


type interface_or_class = [
| `Interface of interface_file
| `Class of class_file
]


let get_name = function 
  | `Interface i -> i.i_name
  | `Class c -> c.c_name


type program = interface_or_class ClassMap.t
type t = program

let super = function
  | `Interface i -> Some i.i_super
  | `Class c -> 
      match c.c_class_file_type with
	| AbstractClass ac -> ac.ac_super_class
	| ConcreteClass nc -> nc.cc_super_class


(** [Class_not_found c] is raised when trying to add a class when its
    super class or one of its implemented interfaces is not yet in the
    program structure (i.e. in the map).*)
exception Class_not_found of class_name

let add_classFile c (program:program) =
  let imap =
    List.fold_left
      (fun imap iname -> 
	let i = 
	  try 
	    match ClassMap.find iname program with
	      | `Interface i -> i
	      | `Class c' -> 
		  raise (Invalid_class
			    (JDump.class_name c.JClass.c_name^" is declared to implements "
			      ^JDump.class_name iname^", which is a class and not an interface."))
	  with Not_found -> raise (Class_not_found iname)
	in ClassMap.add iname i imap
      )
      ClassMap.empty
      c.JClass.c_interfaces
  in let cft =
    match c.JClass.c_class_file_type with
      | JClass.AbstractClass ac ->
	  let super = 
	    begin
	      match ac.JClass.ac_super_class with
		| None -> None
		| Some super ->
		    try
		      match ClassMap.find super program with
			| `Class c -> Some c
			| `Interface i -> 
			    raise (Invalid_class
				      (JDump.class_name c.JClass.c_name^" is declared to extends "
					^JDump.class_name super^", which is an interface and not a class."))
		    with Not_found -> raise (Class_not_found super)
	    end
	  in AbstractClass {
	    ac_super_class = super;
	    ac_fields = ac.JClass.ac_fields;
	    ac_methods = ac.JClass.ac_methods;
	  }
      | JClass.ConcreteClass cc ->
	  let super = 
	    begin
	      match cc.JClass.cc_super_class with
		| None -> None
		| Some super ->
		    try
		      match ClassMap.find super program with
			| `Class c -> Some c
			| `Interface i -> 
			    raise (Invalid_class
				      (JDump.class_name c.JClass.c_name^" is declared to extends "
					^JDump.class_name super^", which is an interface and not a class."))
		    with Not_found -> raise (Class_not_found super)
	    end
	  in ConcreteClass {
	    cc_final = cc.JClass.cc_final;
	    cc_super_class = super;
	    cc_fields = cc.JClass.cc_fields;
	    cc_methods = cc.JClass.cc_methods;
	  }
  in 
  let c' =
    {c_name = c.JClass.c_name;
     c_access = c.JClass.c_access;
     c_consts = c.JClass.c_consts;
     c_interfaces = imap;
     c_sourcefile = c.JClass.c_sourcefile;
     c_deprecated = c.JClass.c_deprecated;
     c_inner_classes = c.JClass.c_inner_classes;
     c_other_attributes = c.JClass.c_other_attributes;
     c_class_file_type = cft;
     c_children = ClassMap.empty;}
  in
    begin
      ClassMap.iter
	(fun _ i ->
	  i.i_children_class <- ClassMap.add c'.c_name c' i.i_children_class)
	c'.c_interfaces;
      match super (`Class c') with
	| None -> ();
	| Some parent -> 
	    parent.c_children <- ClassMap.add c'.c_name c' parent.c_children 
    end;
    ClassMap.add 
      c'.c_name
      (`Class c')
      program

let add_interfaceFile c (program:program) =
  let imap =
    List.fold_left
      (fun imap iname -> 
	let i = 
	  try 
	    match ClassMap.find iname program with
	      | `Interface i -> i
	      | `Class c' ->
		  raise (Invalid_class
			    ("Interface "^JDump.class_name c.JClass.i_name^" is declared to extends "
			      ^JDump.class_name c'.c_name^", which is an interface and not a class."))
	  with Not_found -> raise (Class_not_found iname)
	in ClassMap.add iname i imap
      )
      ClassMap.empty
      c.JClass.i_interfaces
  and super = 
    try match ClassMap.find java_lang_object program with
      | `Class c -> c
      | `Interface i -> 
	  raise (Invalid_class"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object)
  in 
  let c' =
    {i_name = c.JClass.i_name;
     i_access = c.JClass.i_access;
     i_consts = c.JClass.i_consts;
     i_interfaces = imap;
     i_sourcefile = c.JClass.i_sourcefile;
     i_deprecated = c.JClass.i_deprecated;
     i_inner_classes = c.JClass.i_inner_classes;
     i_other_attributes = c.JClass.i_other_attributes;
     i_children_interface = ClassMap.empty;
     i_children_class = ClassMap.empty;
     i_super = super;
     i_initializer = c.JClass.i_initializer;
     i_fields = c.JClass.i_fields;
     i_methods = c.JClass.i_methods;
    }
  in
    begin
      ClassMap.iter
	(fun _ i ->
	  i.i_children_interface <- ClassMap.add c'.i_name c' i.i_children_interface)
	c'.i_interfaces
    end;
    ClassMap.add 
      c'.i_name
      (`Interface c')
      program


let add_file f program = match f with
  | `Interface i -> add_interfaceFile i program
  | `Class c -> add_classFile c program

let parse_program class_path names =
  (* build a map of all the JClass.class_file that are going to be
     translated to build the new hierarchy.*)
  let class_map =
    JFile.read
      class_path
      (fun program c -> ClassMap.add (JClass.get_name c) c program)
      ClassMap.empty
      names
      (* add_rec translate a class from JClass.class_file to
	 class_file and add it in the program structure, recursively
	 adding super classes and implemented interfaces if needed. *)
  in let rec add_rec c (program:program) : program =
    try
      add_file c program
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
exception Found_Class of interface_or_class


let defines_method ms = function 
  | `Interface i -> MethodMap.mem ms i.i_methods
  | `Class c -> match c.c_class_file_type with
      | AbstractClass ac -> MethodMap.mem ms ac.ac_methods 
      | ConcreteClass nc -> MethodMap.mem ms nc.cc_methods 
let defines_field fs = function
  | `Interface i -> FieldMap.mem fs i.i_fields
  | `Class c -> match c.c_class_file_type with
      | AbstractClass ac -> FieldMap.mem fs ac.ac_fields 
      | ConcreteClass nc -> FieldMap.mem fs nc.cc_fields 



let get_interface_or_class program cn = ClassMap.find cn program

let resolve_class program cn =
  try get_interface_or_class program cn with Not_found -> raise NoClassDefFoundError


let get_method c ms = match c with
  | `Interface i -> AbstractMethod (MethodMap.find ms i.i_methods)
  | `Class c -> match c.c_class_file_type with
      | AbstractClass c ->
	  MethodMap.find ms c.ac_methods
      | ConcreteClass c ->
	  ConcreteMethod (MethodMap.find ms c.cc_methods)

let get_methods = function
  | `Interface i -> MethodMap.fold (fun ms _ l -> ms::l) i.i_methods []
  | `Class c -> match c.c_class_file_type with
      | AbstractClass c ->
	  MethodMap.fold (fun ms _ l -> ms::l) c.ac_methods []
      | ConcreteClass c ->
	  MethodMap.fold (fun ms _ l -> ms::l) c.cc_methods []

let get_field c fs = match c with
  | `Interface i -> InterfaceField (FieldMap.find fs i.i_fields)
  | `Class c -> match c.c_class_file_type with
      | AbstractClass c ->
	  ClassField (FieldMap.find fs c.ac_fields)
      | ConcreteClass c ->
	  ClassField (FieldMap.find fs c.cc_fields)

let get_fields c =
  let to_list fs f l = fs::l in
    match c with
      | `Interface i -> FieldMap.fold to_list i.i_fields []
      | `Class c -> match c.c_class_file_type with
	  | AbstractClass c -> FieldMap.fold to_list c.ac_fields []
	  | ConcreteClass c -> FieldMap.fold to_list c.cc_fields []


let rec resolve_field fs c : interface_or_class =
  let get_interfaces = function
    | `Interface i -> i.i_interfaces
    | `Class c -> c.c_interfaces
  in
  if defines_field fs c
  then c
  else
    try
      ClassMap.iter 
	(fun _ i -> 
	  let i = resolve_field fs (`Interface i)
	  in raise (Found_Class i)
	)
	(get_interfaces c);
      match super c with
	| Some super -> resolve_field fs (`Class super)
	| None -> raise NoSuchFieldError
    with Found_Class resolved -> resolved

(** [resolve_method' ms c] looks for the method [ms] in [c] and 
    recursively in its super-classes.
    @raise NoSuchMethodError if [ms] has not been found. *)
let rec resolve_method' ms (c:class_file) : class_file =
  if defines_method ms (`Class c)
  then c
  else
    match super (`Class c) with
      | Some super -> resolve_method' ms super
      | None -> raise NoSuchMethodError

(** [resolve_interface_method'' ms i] looks for the methods [ms] in
    [i] and recursively in its interfaces. It returns the list of
    interfaces that defines [ms], starting with [i] if [i] defines
    [ms]. *)
let rec resolve_interface_method'' ms (c:interface_file) : interface_file list =
  ClassMap.fold
    (fun _ i l -> resolve_interface_method'' ms i@l)
    c.i_interfaces
    (if defines_method ms (`Interface c) then [c] else [])

let resolve_method ms (c:class_file) : interface_or_class =
  match c.c_class_file_type with
    | ConcreteClass _ ->
	begin
	  let c' = resolve_method' ms c
	  in match get_method (`Class c') ms with
	    | AbstractMethod _ -> raise AbstractMethodError
	    | ConcreteMethod _ -> `Class c'
	end
    | AbstractClass _ ->
	let rec resolve_abstract_method ms (c:class_file) : interface_or_class =
	  try `Class (resolve_method' ms c)
	  with NoSuchMethodError ->
	    match 
	      ClassMap.fold
		(fun _ i l -> resolve_interface_method'' ms i@l)
		c.c_interfaces
		[]
	    with
	      | resolved::_ -> `Interface resolved
	      | [] -> match super (`Class c) with
		  | None -> raise NoSuchMethodError
		  | Some c' -> resolve_abstract_method ms c'
	in resolve_abstract_method ms c

let resolve_interface_method ms (c:interface_file) : interface_or_class =
  match resolve_interface_method'' ms c with
    | resolved::_ -> `Interface resolved
    | [] -> `Class (resolve_method' ms c.i_super) (* super = java.lang.object *)

let resolve_all_interface_methods ms (c:interface_file) : interface_file list =
  ClassMap.fold
    (fun _ i l -> l@resolve_interface_method'' ms i)
    c.i_interfaces
    []

let lookup_virtual_method ms (c:class_file) : class_file =
  let c' = 
    try resolve_method' ms c
    with NoSuchMethodError -> raise AbstractMethodError
  in match get_method (`Class c) ms with
      | AbstractMethod m -> raise AbstractMethodError
      | ConcreteMethod m -> c'

let lookup_interface_method = lookup_virtual_method


(* {2 Access to the hierarchy} *)

(** The name of a real class, i.e., not an interface or an implicit array class name. *)
type className = class_name

type interfaceName = class_name

let rec extends_class' c1 c2 : bool =
  if c1==c2
  then true
  else
    match super (`Class c1) with
      | None -> false;
      | Some c3 -> extends_class' c3 c2

let extends_class_ref c1 c2 : bool = extends_class' c1 c2

let rec extends_interface' i1 in2 : bool =
  ClassMap.fold
    (fun in3 i3 b -> b || (in2=in3) || (extends_interface' i3 in2))
    i1.i_interfaces
    false

let rec extends_interface_ref (i1:interface_file) (i2:interface_file) : bool =
  i1 == i2 ||
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface_ref i3 i2)
	i1.i_interfaces
	false
    )

let rec implements_ref (c1:class_file) (i2:interface_file) : bool =
  if
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface_ref i3 i2)
	c1.c_interfaces
	false
    )
  then true
  else match super (`Class c1) with
    | None -> false
    | Some c3 -> implements_ref c3 i2

let super_class_ref c : class_file option = super c

let rec super_interfaces_ref i =
  ClassMap.fold 
    (fun _iname i ilist -> 
      i::(List.rev_append (super_interfaces_ref i) ilist))
    i.i_interfaces
    []

let rec implemented_interfaces' (c:class_file) : interface_file list = 
  let directly_implemented_interfaces =
    ClassMap.fold 
      (fun _iname i ilist -> 
	i::(List.rev_append (super_interfaces_ref i) ilist))
      c.c_interfaces
      []
  in
    match super (`Class c) with
      | None -> directly_implemented_interfaces
      | Some c' -> 
	  List.rev_append directly_implemented_interfaces (implemented_interfaces' c')

let implemented_interfaces_ref c =
  let rec rem_dbl = function
    | e::l -> e:: (List.filter ((!=)e) (rem_dbl l))
    | [] -> []
  in (rem_dbl (implemented_interfaces' c))

let rec firstCommonSuperClass_ref (c1:class_file) (c2:class_file) : class_file =
  if extends_class' c1 c2
  then c2
  else match super_class_ref (`Class c2) with
    | Some c3 -> firstCommonSuperClass_ref c1 c3
    | None -> raise (Failure "firstCommonSuperClass_ref: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class.")
