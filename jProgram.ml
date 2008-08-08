(*
 *  This file is part of JavaLib
 *  Copyright (c)2007-2008 Université de Rennes 1 / CNRS
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

module ClassMap = Map.Make(struct type t = class_name let compare = compare end)

type concrete_method = {
  cm_signature : method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access: access;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_implementation : implementation;
  mutable cm_overridden_in : class_file list;
}

and abstract_method = {
  am_signature : method_signature;
  am_access: [`Public | `Protected | `Default];
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  mutable am_overridden_in : interface_file list;
  mutable am_implemented_in : class_file list;
}

and jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of concrete_method

and class_file = {
  c_name : class_name;
  c_access : [`Public | `Default];
  c_final : bool;
  c_abstract : bool;
  c_synthetic: bool;
  c_enum: bool;
  c_other_flags : int list;
  c_super_class : class_file option;
  c_fields : class_field FieldMap.t;
  c_interfaces : interface_file ClassMap.t;
  c_consts : constant array; (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
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
  i_annotation: bool;
  i_other_flags : int list;
  i_interfaces : interface_file ClassMap.t;
  i_consts : constant array; (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_inner_classes : inner_class list;
  i_other_attributes : (string * string) list;
  i_super : class_file; (* must be java.lang.Object. *)
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

let is_static_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_static

let get_method_signature = function
  | AbstractMethod m -> m.am_signature
  | ConcreteMethod m -> m.cm_signature

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

exception Class_not_found of class_name


type any_field =
    | InterfaceField of interface_field
    | ClassField of class_field

exception IncompatibleClassChangeError
exception NoSuchMethodError
exception NoSuchFieldError
exception NoClassDefFoundError
exception AbstractMethodError
exception IllegalAccessError


(* this exception is raised to avoid a full unfolding of the
    hierarchy. *)
exception Found_Class of interface_or_class


let defines_method ms = function
  | `Interface i -> 
      if ms = clinit_signature then i.i_initializer <> None
      else MethodMap.mem ms i.i_methods
  | `Class c -> MethodMap.mem ms c.c_methods
let defines_field fs = function
  | `Interface {i_fields=fm;} -> FieldMap.mem fs fm
  | `Class {c_fields=fm;} -> FieldMap.mem fs fm



let get_interface_or_class program cn = ClassMap.find cn program

let super_class c : class_file option = super c


let get_method c ms = match c with
  | `Interface i ->
      if ms = clinit_signature
      then
	match i.i_initializer with
	  | Some m -> ConcreteMethod m
	  | None -> raise Not_found
      else
	AbstractMethod (MethodMap.find ms i.i_methods)
  | `Class c -> MethodMap.find ms c.c_methods

let get_methods = function
  | `Interface i ->
      let init =
	if i.i_initializer = None then []
	else [clinit_signature]
      in MethodMap.fold (fun ms _ l -> ms::l) i.i_methods init
  | `Class {c_methods = mm;} ->
      MethodMap.fold (fun ms _ l -> ms::l) mm []

let get_field c fs = match c with
  | `Interface i -> InterfaceField (FieldMap.find fs i.i_fields)
  | `Class c -> ClassField (FieldMap.find fs c.c_fields)

let get_fields c =
  let to_list fs _f l = fs::l in
    match c with
      | `Interface i -> FieldMap.fold to_list i.i_fields []
      | `Class c -> FieldMap.fold to_list c.c_fields []

let rec resolve_interface_method' ?(acc=[]) ms (c:interface_or_class) : interface_file list =
  ClassMap.fold
    (fun _ i acc -> 
      if defines_method ms (`Interface i)
      then i::acc
      else resolve_interface_method' ~acc ms (`Interface i))
    (get_interfaces c)
    acc

let rec implements ?(acc=[]) ms (c:class_file) : (class_file option * interface_file list) =
  match c.c_super_class with
    | None -> (None,resolve_interface_method' ~acc ms (`Class c))
    | Some sc ->
	if defines_method ms (`Class sc)
	then (Some sc,resolve_interface_method' ~acc ms (`Class c))
	else implements ~acc:(resolve_interface_method' ~acc ms (`Class c)) ms sc

let rec rem_dbl = function
  | e::(_::_ as l) -> e:: (List.filter ((!=)e) (rem_dbl l))
  | l -> l

let declare_method ioc ms =
  if ms.ms_name = "<init>" || ms.ms_name = "<clinit>" then ()
  else
    let ioc2c = function
      | `Class c -> c
      | `Interface _ -> raise (Invalid_argument "ioc2c")
    in
    let add c' c ms =
      match get_method c ms with
	| ConcreteMethod m -> m.cm_overridden_in <- (ioc2c c')::m.cm_overridden_in
	| AbstractMethod m ->
	    match c' with
	      | `Interface c' -> m.am_overridden_in <- c'::m.am_overridden_in
	      | `Class c' -> m.am_implemented_in <- c'::m.am_implemented_in
    in
      try
	match ioc with
	  | `Interface _ ->
	      List.iter
		(fun i -> add ioc (`Interface i) ms)
		(rem_dbl (resolve_interface_method' ms ioc));
	  | `Class c ->
	      let (super,il) = implements ms c in
		List.iter (fun i -> add ioc (`Interface i) ms) (rem_dbl il);
		match super with
		  | Some c -> add ioc (`Class c) ms
		  | None -> ()
      with Invalid_argument "ioc2c" ->
	raise (Failure "bug in JavaLib: jProgram.add_method")

let ccm2pcm m = {
  cm_signature = m.JClass.cm_signature;
  cm_static = m.JClass.cm_static;
  cm_final = m.JClass.cm_final;
  cm_synchronized = m.JClass.cm_synchronized;
  cm_strict = m.JClass.cm_strict;
  cm_access = m.JClass.cm_access;
  cm_bridge = m.JClass.cm_bridge;
  cm_varargs = m.JClass.cm_varargs;
  cm_synthetic = m.JClass.cm_synthetic;
  cm_other_flags = m.JClass.cm_other_flags;
  cm_exceptions = m.JClass.cm_exceptions;
  cm_attributes = m.JClass.cm_attributes;
  cm_implementation = m.JClass.cm_implementation;
  cm_overridden_in = [];
}

let pcm2ccm m = {
  JClass.cm_signature = m.cm_signature;
  JClass.cm_static = m.cm_static;
  JClass.cm_final = m.cm_final;
  JClass.cm_synchronized = m.cm_synchronized;
  JClass.cm_strict = m.cm_strict;
  JClass.cm_access = m.cm_access;
  JClass.cm_bridge = m.cm_bridge;
  JClass.cm_varargs = m.cm_varargs;
  JClass.cm_synthetic = m.cm_synthetic;
  JClass.cm_other_flags = m.cm_other_flags;
  JClass.cm_exceptions = m.cm_exceptions;
  JClass.cm_attributes = m.cm_attributes;
  JClass.cm_implementation = m.cm_implementation;
}

let cam2pam m = {
  am_signature = m.JClass.am_signature;
  am_access = m.JClass.am_access;
  am_bridge = m.JClass.am_bridge;
  am_varargs = m.JClass.am_varargs;
  am_synthetic = m.JClass.am_synthetic;
  am_other_flags = m.JClass.am_other_flags;
  am_exceptions = m.JClass.am_exceptions;
  am_attributes = m.JClass.am_attributes;
  am_overridden_in = [];
  am_implemented_in = [];
}

let pam2cam m = {
  JClass.am_signature = m.am_signature;
  JClass.am_access = m.am_access;
  JClass.am_bridge = m.am_bridge;
  JClass.am_varargs = m.am_varargs;
  JClass.am_synthetic = m.am_synthetic;
  JClass.am_other_flags = m.am_other_flags;
  JClass.am_exceptions = m.am_exceptions;
  JClass.am_attributes = m.am_attributes;
}

let add_methods mm =
  MethodMap.map
    (fun m ->
      match m with
	| JClass.AbstractMethod m -> AbstractMethod (cam2pam m)
	| JClass.ConcreteMethod m -> ConcreteMethod (ccm2pcm m))
    mm

let add_amethods mm = MethodMap.map (fun m -> cam2pam m) mm

let add_classFile c (program:program) =
  let imap =
    List.fold_left
      (fun imap iname ->
	let i =
	  try
	    match ClassMap.find iname program with
	      | `Interface i -> i
	      | `Class _ ->
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
	      | `Interface _ ->
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
     c_synthetic = c.JClass.c_synthetic;
     c_enum = c.JClass.c_enum;
     c_other_flags = c.JClass.c_other_flags;
     c_super_class = c_super;
     c_consts = c.JClass.c_consts;
     c_interfaces = imap;
     c_sourcefile = c.JClass.c_sourcefile;
     c_deprecated = c.JClass.c_deprecated;
     c_inner_classes = c.JClass.c_inner_classes;
     c_other_attributes = c.JClass.c_other_attributes;
     c_fields = c.JClass.c_fields;
     c_methods = add_methods c.JClass.c_methods;
     c_children = ClassMap.empty;}
  in
    MethodMap.iter
      (fun ms _ -> declare_method (`Class c') ms)
      c'.c_methods;
    ClassMap.iter
      (fun _ i ->
	i.i_children_class <- ClassMap.add c'.c_name c' i.i_children_class)
      c'.c_interfaces;
    begin
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
      | `Interface _ ->
	  raise (Class_structure_error"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object)
  in
  let c' =
    {i_name = c.JClass.i_name;
     i_access = c.JClass.i_access;
     i_consts = c.JClass.i_consts;
     i_annotation = c.JClass.i_annotation;
     i_other_flags = c.JClass.i_other_flags;
     i_interfaces = imap;
     i_sourcefile = c.JClass.i_sourcefile;
     i_deprecated = c.JClass.i_deprecated;
     i_inner_classes = c.JClass.i_inner_classes;
     i_other_attributes = c.JClass.i_other_attributes;
     i_children_interface = ClassMap.empty;
     i_children_class = ClassMap.empty;
     i_super = super;
     i_initializer =
	begin
	  match c.JClass.i_initializer with
	    | None -> None
	    | Some m -> Some (ccm2pcm m)
	end;
     i_fields = c.JClass.i_fields;
     i_methods = add_amethods c.JClass.i_methods;
    }
  in
    MethodMap.iter
      (fun ms _ -> declare_method (`Interface c') ms)
      c'.i_methods;
    ClassMap.iter
      (fun _ i ->
	i.i_children_interface <- ClassMap.add c'.i_name c' i.i_children_interface)
      c'.i_interfaces;
    ClassMap.add
      c'.i_name
      (`Interface c')
      program


let add_one_file f program = match f with
  | `Interface i -> add_interfaceFile i program
  | `Class c -> add_classFile c program


let to_class = function
  | `Interface c -> `Interface
      {JClass.i_name = c.i_name;
       JClass.i_access = c.i_access;
       JClass.i_consts = c.i_consts;
       JClass.i_annotation = c.i_annotation;
       JClass.i_other_flags = c.i_other_flags;
       JClass.i_interfaces =
	  ClassMap.fold (fun cn _ l -> cn::l) c.i_interfaces [];
       JClass.i_sourcefile = c.i_sourcefile;
       JClass.i_deprecated = c.i_deprecated;
       JClass.i_inner_classes = c.i_inner_classes;
       JClass.i_other_attributes = c.i_other_attributes;
       JClass.i_initializer =
	  begin
	    match c.i_initializer with
	      | Some m -> Some (pcm2ccm m)
	      | None -> None
	  end;
       JClass.i_fields = c.i_fields;
       JClass.i_methods = MethodMap.map (fun m -> pam2cam m) c.i_methods;
      }
  | `Class c -> `Class
      {JClass.c_name = c.c_name;
       JClass.c_access = c.c_access;
       JClass.c_final = c.c_final;
       JClass.c_abstract = c.c_abstract;
       JClass.c_enum = c.c_enum ;
       JClass.c_synthetic = c.c_synthetic ;
       JClass.c_other_flags = c.c_other_flags ;
       JClass.c_super_class =
	  (match c.c_super_class with
	    | Some cn -> Some cn.c_name
	    | None -> None);
       JClass.c_consts = c.c_consts;
       JClass.c_interfaces =
	  ClassMap.fold (fun cn _ l -> cn::l) c.c_interfaces [];
       JClass.c_sourcefile = c.c_sourcefile;
       JClass.c_deprecated = c.c_deprecated;
       JClass.c_inner_classes = c.c_inner_classes;
       JClass.c_other_attributes = c.c_other_attributes;
       JClass.c_fields = c.c_fields;
       JClass.c_methods =
	  MethodMap.map
	    (function
	      | AbstractMethod m -> JClass.AbstractMethod (pam2cam m)
	      | ConcreteMethod m -> JClass.ConcreteMethod (pcm2ccm m))
	    c.c_methods;
      }


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
      | ConstMethod (TClass cn,_,_)
      | ConstInterfaceMethod (cn,_,_)
      | ConstField (cn,_,_)
      | ConstValue (ConstClass (TClass cn))
	-> if not (ClassMap.mem cn program) then to_add := cn::!to_add
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

(* Access to the hierarchy *)

let rec extends_class' c1 c2 : bool =
  if c1==c2
  then true
  else
    match super (`Class c1) with
      | None -> false;
      | Some c3 -> extends_class' c3 c2

let extends_class c1 c2 : bool = extends_class' c1 c2

let rec extends_interface' i1 in2 : bool =
  ClassMap.fold
    (fun in3 i3 b -> b || (in2=in3) || (extends_interface' i3 in2))
    i1.i_interfaces
    false

let rec extends_interface (i1:interface_file) (i2:interface_file) : bool =
  i1 == i2 ||
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface i3 i2)
	i1.i_interfaces
	false
    )

let rec implements (c1:class_file) (i2:interface_file) : bool =
  if
    (ClassMap.fold
	(fun _in3 i3 b -> b || extends_interface i3 i2)
	c1.c_interfaces
	false
    )
  then true
  else match super (`Class c1) with
    | None -> false
    | Some c3 -> implements c3 i2

let rec super_interfaces i =
  ClassMap.fold
    (fun _iname i ilist ->
      i::(List.rev_append (super_interfaces i) ilist))
    i.i_interfaces
    []

let rec implemented_interfaces' (c:class_file) : interface_file list =
  let directly_implemented_interfaces =
    ClassMap.fold
      (fun _iname i ilist ->
	i::(List.rev_append (super_interfaces i) ilist))
      c.c_interfaces
      []
  in
    match super (`Class c) with
      | None -> directly_implemented_interfaces
      | Some c' ->
	  List.rev_append directly_implemented_interfaces (implemented_interfaces' c')

let implemented_interfaces c = rem_dbl (implemented_interfaces' c)

let rec firstCommonSuperClass (c1:class_file) (c2:class_file) : class_file =
  if extends_class' c1 c2
  then c2
  else match super_class (`Class c2) with
    | Some c3 -> firstCommonSuperClass c1 c3
    | None -> raise (Failure "firstCommonSuperClass: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class.")

