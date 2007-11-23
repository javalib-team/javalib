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


type class_file = {
  c_name : class_name;
  c_access : [`Public | `Default];
  c_final : bool;
  c_abstract : bool;
  c_super_class : class_file option;
  c_fields : class_field FieldMap.t;
  c_interfaces : interface_file ClassMap.t;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  c_methods : jmethod MethodMap.t;
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

let get_interfaces = function
  | `Interface i -> i.i_interfaces
  | `Class c -> c.c_interfaces

let get_consts = function
  | `Interface i -> i.i_consts
  | `Class c -> c.c_consts


type program = interface_or_class ClassMap.t
type t = program

let super = function
  | `Interface i -> Some i.i_super
  | `Class c -> c.c_super_class

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
		  raise (Class_structure_error
			    (JDumpBasics.class_name c.JClass.c_name^" is declared to implements "
			      ^JDumpBasics.class_name iname^", which is a class and not an interface."))
	  with Not_found -> raise (Class_not_found iname)
	in ClassMap.add iname i imap
      )
      ClassMap.empty
      c.JClass.c_interfaces
  in let c_super =
    match c.JClass.c_super_class with
      | None -> None
      | Some super ->
	  try
	    match ClassMap.find super program with
	      | `Class c -> Some c
	      | `Interface i ->
		  raise (Class_structure_error
			    (JDumpBasics.class_name c.JClass.c_name^" is declared to extends "
			      ^JDumpBasics.class_name super^", which is an interface and not a class."))
	  with Not_found -> raise (Class_not_found super)
  in
  let c' =
    {c_name = c.JClass.c_name;
     c_access = c.JClass.c_access;
     c_final = c.JClass.c_final;
     c_abstract = c.JClass.c_abstract;
     c_super_class = c_super;
     c_consts = c.JClass.c_consts;
     c_interfaces = imap;
     c_sourcefile = c.JClass.c_sourcefile;
     c_deprecated = c.JClass.c_deprecated;
     c_inner_classes = c.JClass.c_inner_classes;
     c_other_attributes = c.JClass.c_other_attributes;
     c_fields = c.JClass.c_fields;
     c_methods = c.JClass.c_methods;
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
		  raise (Class_structure_error
			    ("Interface "^JDumpBasics.class_name c.JClass.i_name^" is declared to extends "
			      ^JDumpBasics.class_name c'.c_name^", which is an interface and not a class."))
	  with Not_found -> raise (Class_not_found iname)
	in ClassMap.add iname i imap
      )
      ClassMap.empty
      c.JClass.i_interfaces
  and super =
    try match ClassMap.find java_lang_object program with
      | `Class c -> c
      | `Interface i ->
	  raise (Class_structure_error"java.lang.Object is declared as an interface.")
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


let add_one_file f program = match f with
  | `Interface i -> add_interfaceFile i program
  | `Class c -> add_classFile c program

let get_class class_path class_map name =
  try ClassMap.find name !class_map
  with Not_found ->
    try
      let c = JFile.get_class class_path (JDumpBasics.class_name name)
      in
	assert (JClass.get_name c = name);
	class_map := ClassMap.add name c !class_map;
	c;
    with No_class_found _ -> raise (Class_not_found name)

let add_class_referenced c program to_add =
  Array.iter
    (function
      | ConstMethod (TClass cn,_,_) -> 
	  if not (ClassMap.mem cn program)
	  then to_add := cn::!to_add
      | _ -> ())
    (JClass.get_consts c)


let rec add_file class_path c program =
  let classmap = ref ClassMap.empty in
  let to_add = ref [] in
  let program =
    try
      add_class_referenced c !classmap to_add;
      if not (ClassMap.mem (JClass.get_name c) program)
      then add_one_file c program
      else program
    with Class_not_found cn ->
      let missing_class = get_class class_path classmap cn in
	add_file class_path c (add_file class_path missing_class program)
  in begin
    let program = ref program in
    try while true do
	let cn = List.hd !to_add in
	  to_add := List.tl !to_add;
	  if not (ClassMap.mem cn !program)
	  then 
	    let c = get_class class_path classmap cn
	    in program := add_file class_path c !program
      done;
      !program
    with Failure "hd" -> !program
  end


let parse_program class_path names =
  (* build a map of all the JClass.class_file that are going to be
     translated to build the new hierarchy.*)
  let (jars,others) = List.partition (fun f -> Filename.check_suffix f ".jar") names in
  let class_map =
    JFile.read
      class_path
      (fun program c -> ClassMap.add (JClass.get_name c) c program)
      ClassMap.empty
      jars in
  let class_path = JFile.class_path class_path in
  let class_map = ref
    begin
      List.fold_left
	(fun clmap cn -> 
	  let c= JFile.get_class class_path cn in
	    assert (cn = JDumpBasics.class_name (JClass.get_name c));
	    ClassMap.add (JClass.get_name c) c clmap)
	class_map
	others
    end in
    ClassMap.fold
      (fun _ -> add_file class_path)
      !class_map
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

type any_method = jmethod
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
  | `Class c -> MethodMap.mem ms c.c_methods

let defines_field fs = function
  | `Interface {i_fields=fm;} -> FieldMap.mem fs fm
  | `Class {c_fields=fm;} -> FieldMap.mem fs fm



let get_interface_or_class program cn = ClassMap.find cn program

let resolve_class program cn =
  try get_interface_or_class program cn with Not_found -> raise NoClassDefFoundError


let get_method c ms = match c with
  | `Interface i -> AbstractMethod (MethodMap.find ms i.i_methods)
  | `Class c -> MethodMap.find ms c.c_methods

let get_methods = function
  | `Interface i -> MethodMap.fold (fun ms _ l -> ms::l) i.i_methods []
  | `Class {c_methods = mm;} -> 
      MethodMap.fold (fun ms _ l -> ms::l) mm []

let get_field c fs = match c with
  | `Interface i -> InterfaceField (FieldMap.find fs i.i_fields)
  | `Class c -> ClassField (FieldMap.find fs c.c_fields)

let get_fields c =
  let to_list fs f l = fs::l in
    match c with
      | `Interface i -> FieldMap.fold to_list i.i_fields []
      | `Class c -> FieldMap.fold to_list c.c_fields []

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
