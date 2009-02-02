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
open JProgram


let add_methods (p:program) mm =
  let imap = ref MethodMap.empty
  and f m =
    match m with
      | JClass.AbstractMethod m -> AbstractMethod (cam2pam p.dictionary m)
      | JClass.ConcreteMethod m -> ConcreteMethod (ccm2pcm p.dictionary m) in
    JClass.MethodMap.iter
      (fun ms m ->
	 let msi = p.dictionary.get_ms_index ms in
	   imap := MethodMap.add msi (f m) !imap) mm;
    !imap

let add_amethods (p:program) mm =
  let imap = ref MethodMap.empty
  and f m = cam2pam p.dictionary m in
    JClass.MethodMap.iter
      (fun ms m ->
	 let msi = p.dictionary.get_ms_index ms in
	   imap := MethodMap.add msi (f m) !imap) mm;
    !imap

let add_classFile c (program:program) =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let iname_index = program.dictionary.get_cn_index iname in
	 let i =
	   try
	     match ClassMap.find iname_index program.classes with
		 | `Interface i -> i
		 | `Class _ ->
		     raise (Class_structure_error
			      (JDumpBasics.class_name c.JClass.c_name^" is declared to implements "
			       ^JDumpBasics.class_name iname^", which is a class and not an interface."))
	   with Not_found -> raise (Class_not_found iname)
	 in ClassMap.add iname_index i imap
      )
      ClassMap.empty
      c.JClass.c_interfaces
  in let c_super =
    match c.JClass.c_super_class with
      | None -> None
      | Some super ->
	  let super_index = program.dictionary.get_cn_index super in
	    try
	      match ClassMap.find super_index program.classes with
		| `Class c -> Some c
		| `Interface _ ->
		    raise (Class_structure_error
			     (JDumpBasics.class_name c.JClass.c_name^" is declared to extends "
			      ^JDumpBasics.class_name super^", which is an interface and not a class."))
	    with Not_found -> raise (Class_not_found super)
  in
  let c' =
    {c_name = c.JClass.c_name;
     c_index = program.dictionary.get_cn_index c.JClass.c_name;
     c_version = c.JClass.c_version;
     c_access = c.JClass.c_access;
     c_generic_signature = c.JClass.c_generic_signature;
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
     c_enclosing_method = c.JClass.c_enclosing_method;
     c_source_debug_extention =c.JClass.c_source_debug_extention;
     c_inner_classes = c.JClass.c_inner_classes;
     c_other_attributes = c.JClass.c_other_attributes;
     c_fields = c.JClass.c_fields;
     c_methods = add_methods program c.JClass.c_methods;
     c_resolve_methods = MethodMap.empty;
     c_may_be_instanciated = true;
     c_children = ClassMap.empty;}
  in
  let c_index' = program.dictionary.get_cn_index c'.c_name in
    MethodMap.iter
      (fun ms _ -> declare_method (`Class c') ms)
      c'.c_methods;
    ClassMap.iter
      (fun _ i ->
	 i.i_children_class <- ClassMap.add c_index' c' i.i_children_class)
      c'.c_interfaces;
    begin
      match super_class (`Class c') with
	| None -> ();
	| Some parent ->
	    parent.c_children <- ClassMap.add c_index' c' parent.c_children
    end;
    { classes =
	ClassMap.add
	  c_index'
	  (`Class c')
	  program.classes;
      static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
      dictionary = program.dictionary }

let add_interfaceFile c (program:program) =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let iname_index = program.dictionary.get_cn_index iname in
	 let i =
	   try
	     match ClassMap.find iname_index program.classes with
	       | `Interface i -> i
	       | `Class c' ->
		   raise (Class_structure_error
			    ("Interface "^JDumpBasics.class_name c.JClass.i_name^" is declared to extends "
			     ^JDumpBasics.class_name c'.c_name^", which is an interface and not a class."))
	   with Not_found -> raise (Class_not_found iname)
	 in ClassMap.add iname_index i imap
      )
      ClassMap.empty
      c.JClass.i_interfaces
  and super =
    try match ClassMap.find java_lang_object_index program.classes with
      | `Class c -> c
      | `Interface _ ->
	  raise (Class_structure_error"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object)
  in
  let c' =
    {i_name = c.JClass.i_name;
     i_index = program.dictionary.get_cn_index c.JClass.i_name;
     i_version = c.JClass.i_version;
     i_access = c.JClass.i_access;
     i_generic_signature = c.JClass.i_generic_signature;
     i_consts = c.JClass.i_consts;
     i_annotation = c.JClass.i_annotation;
     i_other_flags = c.JClass.i_other_flags;
     i_interfaces = imap;
     i_sourcefile = c.JClass.i_sourcefile;
     i_deprecated = c.JClass.i_deprecated;
     i_source_debug_extention = c.JClass.i_source_debug_extention;
     i_inner_classes = c.JClass.i_inner_classes;
     i_other_attributes = c.JClass.i_other_attributes;
     i_children_interface = ClassMap.empty;
     i_children_class = ClassMap.empty;
     i_super = super;
     i_initializer =
	begin
	  match c.JClass.i_initializer with
	    | None -> None
	    | Some m -> Some (ccm2pcm program.dictionary m)
	end;
     i_fields = c.JClass.i_fields;
     i_methods = add_amethods program c.JClass.i_methods;
    }
  in
  let c_index' = program.dictionary.get_cn_index c'.i_name in
    MethodMap.iter
      (fun ms _ -> declare_method (`Interface c') ms)
      c'.i_methods;
    ClassMap.iter
      (fun _ i ->
	i.i_children_interface <- ClassMap.add c_index' c' i.i_children_interface)
      c'.i_interfaces;
    { classes = ClassMap.add
	c_index'
	(`Interface c')
	program.classes;
      static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
      dictionary = program.dictionary }

let add_one_file f program = match f with
  | `Interface i -> add_interfaceFile i program
  | `Class c -> add_classFile c program

let add_class_referenced c dictionary classmap to_add =
  Array.iter
    (function
      | ConstMethod (TClass cn,_,_)
      | ConstInterfaceMethod (cn,_,_)
      | ConstField (cn,_,_)
      | ConstValue (ConstClass (TClass cn))
	-> let cni = (dictionary.get_cn_index cn) in
	  if not (ClassMap.mem cni classmap) then to_add := cn::!to_add
      | _ -> ())
    (JClass.get_consts c)

let get_class class_path dictionary class_map name =
  let name_index = dictionary.get_cn_index name in
    try ClassMap.find name_index !class_map
    with Not_found ->
      try
	let c = JFile.get_class class_path (JDumpBasics.class_name name)
	in
	  class_map := ClassMap.add name_index c !class_map;
	  c;
      with No_class_found _ -> raise (Class_not_found name)

let rec add_file class_path c program =
  let classmap = ref ClassMap.empty in
  let to_add = ref [] in
  let dic = program.dictionary in
  let program =
    try
      let c_index = dic.get_cn_index (JClass.get_name c) in
	(* TODO : the next line might be put after "then" *)
	add_class_referenced c dic !classmap to_add;
	if not (ClassMap.mem c_index program.classes)
	then add_one_file c program
	else program
    with Class_not_found cn ->
      let missing_class = get_class class_path dic classmap cn in
	add_file class_path c (add_file class_path missing_class program)
  in begin
      let p_classes = ref program.classes in
	try while true do
	  let cn = List.hd !to_add in
	    to_add := List.tl !to_add;
	    if not (ClassMap.mem (dic.get_cn_index cn) !p_classes)
	    then
	      let c = get_class class_path dic classmap cn
	      in p_classes := (add_file class_path c
				 { classes = !p_classes;
				   static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
				   dictionary = dic }).classes
	done;
	  { classes = !p_classes;
	    static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
	    dictionary = dic }
	with Failure "hd" -> { classes = !p_classes;
			       static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
			       dictionary = dic }
    end
       
let parse_program class_path names =
  (* build a map of all the JClass.class_file that are going to be
     translated to build the new hierarchy.*)
  let (jars,others) = List.partition (fun f -> Filename.check_suffix f ".jar") names in
  let p_dic = make_dictionary () in
  let class_map =
    JFile.read
      class_path
      (fun cmap c ->
	 let c_index = p_dic.get_cn_index (JClass.get_name c) in
	   ClassMap.add c_index c cmap)
      ClassMap.empty
      jars in
  let class_path = JFile.class_path class_path in
  let class_map = ref
    begin
      List.fold_left
	(fun clmap cn ->
	   let c = JFile.get_class class_path cn in
	   let c_index = p_dic.get_cn_index (JClass.get_name c) in
	     ClassMap.add c_index c clmap)
	class_map
	others
    end in
  let program =
    ClassMap.fold
      (fun _ -> add_file class_path)
      !class_map
      { classes = ClassMap.empty;
	static_lookup = (fun _ _ _ -> failwith "static lookup not Implemented for JCRA");
	dictionary = p_dic }
  in
    JFile.close_class_path class_path;
    program
