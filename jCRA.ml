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

type class_info = { class_data : JProgram.interface_or_class;
		    mutable methods_implementations : ClassSet.t MethodMap.t }

let add_methods dictionary mm =
  let imap = ref MethodMap.empty
  and f m =
    match m with
      | JClass.AbstractMethod m -> AbstractMethod (cam2pam dictionary m)
      | JClass.ConcreteMethod m -> ConcreteMethod (
	  let m = (ccm2pcm dictionary m) in m.cm_has_been_parsed <- true;
	    m) in
    JClass.MethodMap.iter
      (fun ms m ->
	 let msi = dictionary.get_ms_index ms in
	   imap := MethodMap.add msi (f m) !imap) mm;
    !imap

let add_amethods dictionary mm =
  let imap = ref MethodMap.empty
  and f m = cam2pam dictionary m in
    JClass.MethodMap.iter
      (fun ms m ->
	 let msi = dictionary.get_ms_index ms in
	   imap := MethodMap.add msi (f m) !imap) mm;
    !imap

let add_fields dictionary fm =
  let imap = ref FieldMap.empty in
    JClass.FieldMap.iter
      (fun fs cf ->
	 let fsi = dictionary.get_fs_index fs in
	   imap := FieldMap.add fsi cf !imap) fm;
    !imap

let rec new_methods_implementations classes_map ioc =
  match ioc with
    | `Class c ->
	let mmap =
	  (match c.c_super_class with
	     | None -> MethodMap.empty
	     | Some sc ->
		 let sc_methods_implementations =
		   (ClassMap.find sc.c_index
		      classes_map).methods_implementations in
		   MethodMap.map
		     (fun _ -> ClassSet.empty) sc_methods_implementations
	  ) in
	let mmap =
	  (ClassMap.fold
	     (fun _ i mmap ->
		let i_methods_implementations =
		  (ClassMap.find i.i_index
		     classes_map).methods_implementations in
		  MethodMap.fold
		    (fun msi m mmap ->
		       MethodMap.add msi m mmap
		    )
		    i_methods_implementations mmap
	     )
	     c.c_interfaces mmap
	  ) in
	let s = ClassSet.add c.c_index ClassSet.empty in
	  MethodMap.fold
	    (fun msi m (mmap,mset) ->
	       match m with
		 | AbstractMethod _ ->
		     (MethodMap.add msi ClassSet.empty mmap,mset)
		 | ConcreteMethod _ ->
		     (MethodMap.add msi s mmap,
		      MethodSet.add msi mset)
	    )
	    c.c_methods (mmap, MethodSet.empty)
    | `Interface i ->
	let mmap =
	  (ClassMap.fold
	     (fun _ i mmap ->
		let i_methods_implementations =
		  (ClassMap.find i.i_index
		     classes_map).methods_implementations in
		  MethodMap.fold
		    (fun msi m mmap ->
		       MethodMap.add msi m mmap
		    )
		    i_methods_implementations mmap
	     )
	     i.i_interfaces MethodMap.empty
	  ) in
	let mmap =
	  MethodMap.fold
	    (fun msi _ mmap ->
	       MethodMap.add msi ClassSet.empty mmap
	    )
	    i.i_methods mmap in
	  (mmap, MethodSet.empty)
	    
let rec update_methods_implementations classes_map c cmset cni =
  if not ( cmset = MethodSet.empty ) then
    let class_info = ClassMap.find c.c_index classes_map in
    let new_cmset = ref MethodSet.empty in
      MethodSet.iter
	(fun msi ->
	   try
	     let s = MethodMap.find msi class_info.methods_implementations in
	       class_info.methods_implementations <-
		 MethodMap.add msi (ClassSet.add cni s)
		 class_info.methods_implementations;
	       new_cmset := MethodSet.add msi !new_cmset
	   with _ -> ()
	)
	cmset;
      update_super_methods_implementations classes_map c !new_cmset cni

and update_super_methods_implementations classes_map c cmset cni =
  match c.c_super_class with
    | None -> ()
    | Some sc ->
	update_methods_implementations classes_map sc cmset cni
  	      
let rec update_interfaces classes_map ioc interfaces cni =
  match ioc with
    | `Class c ->
	ClassMap.fold
	  (fun i_index i interfaces ->
	     let s =
	       try ClassMap.find i_index interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_index (ClassSet.add cni s) interfaces in
	       update_interfaces classes_map (`Interface i) interfaces cni
	  )
	  c.c_interfaces interfaces
    | `Interface i ->
	ClassMap.fold
	  (fun i_index i interfaces ->
	     let s =
	       try ClassMap.find i_index interfaces
	       with _ -> ClassSet.empty in
	     let interfaces =
	       ClassMap.add i_index (ClassSet.add cni s) interfaces in
	       update_interfaces classes_map (`Interface i) interfaces cni
	  )
	  i.i_interfaces interfaces

let add_classFile c classes_map interfaces dictionary =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let iname_index = dictionary.get_cn_index iname in
	 let i =
	   try
	     match (ClassMap.find iname_index classes_map).class_data with
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
	  let super_index = dictionary.get_cn_index super in
	    try
	      match (ClassMap.find super_index classes_map).class_data with
		| `Class c -> Some c
		| `Interface _ ->
		    raise (Class_structure_error
			     (JDumpBasics.class_name c.JClass.c_name^" is declared to extends "
			      ^JDumpBasics.class_name super^", which is an interface and not a class."))
	    with Not_found -> raise (Class_not_found super)
  in
  let c' =
    {c_name = c.JClass.c_name;
     c_index = dictionary.get_cn_index c.JClass.c_name;
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
     c_fields = add_fields dictionary c.JClass.c_fields;
     c_methods = add_methods dictionary c.JClass.c_methods;
     c_resolve_methods = MethodMap.empty;
     c_may_be_instanciated = true;
     c_children = ClassMap.empty;}
  in
  let c_index' = c'.c_index in
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
    let (methods_implementations,cmset) =
      new_methods_implementations classes_map (`Class c') in
    let c_info = { class_data = (`Class c');
		   methods_implementations = methods_implementations } in
      update_super_methods_implementations classes_map c' cmset c_index';
      interfaces := update_interfaces classes_map (`Class c') !interfaces c_index';
      ClassMap.add c_index' c_info classes_map

let add_interfaceFile c classes_map dictionary =
  let imap =
    List.fold_left
      (fun imap iname ->
	 let iname_index = dictionary.get_cn_index iname in
	 let i =
	   try
	     match (ClassMap.find iname_index classes_map).class_data with
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
    try match (ClassMap.find java_lang_object_index classes_map).class_data with
      | `Class c -> c
      | `Interface _ ->
	  raise (Class_structure_error"java.lang.Object is declared as an interface.")
    with Not_found -> raise (Class_not_found java_lang_object)
  in
  let c' =
    {i_name = c.JClass.i_name;
     i_index = dictionary.get_cn_index c.JClass.i_name;
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
	    | Some m -> Some (ccm2pcm dictionary m)
	end;
     i_fields = add_fields dictionary c.JClass.i_fields;
     i_methods = add_amethods dictionary c.JClass.i_methods;
    }
  in
  let c_index' = c'.i_index in
    MethodMap.iter
      (fun ms _ -> declare_method (`Interface c') ms)
      c'.i_methods;
    ClassMap.iter
      (fun _ i ->
	i.i_children_interface <- ClassMap.add c_index' c' i.i_children_interface)
      c'.i_interfaces;
    (match c'.i_initializer with
       | None -> ()
       | Some cm ->
	   cm.cm_has_been_parsed <- true);
    let (methods_implementations,_) =
      new_methods_implementations classes_map (`Interface c') in
    let i_info = { class_data = (`Interface c');
		   methods_implementations = methods_implementations } in
      ClassMap.add c_index' i_info classes_map

let add_one_file f classes_map interfaces dictionary = match f with
  | `Interface i -> add_interfaceFile i classes_map dictionary
  | `Class c -> add_classFile c classes_map interfaces dictionary

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

let get_class class_path dictionary jclasses_map name =
  let name_index = dictionary.get_cn_index name in
    try ClassMap.find name_index !jclasses_map
    with Not_found ->
      try
	let c = JFile.get_class class_path (JDumpBasics.class_name name)
	in
	  jclasses_map := ClassMap.add name_index c !jclasses_map;
	  c;
      with No_class_found _ -> raise (Class_not_found name)

let rec add_file class_path c classes_map interfaces dictionary =
  let jclasses_map = ref ClassMap.empty in
  let to_add = ref [] in
  let classes_map =
    try
      let c_index = dictionary.get_cn_index (JClass.get_name c) in
	if not (ClassMap.mem c_index classes_map)
	then
	  begin
	    add_class_referenced c dictionary !jclasses_map to_add;
	    add_one_file c classes_map interfaces dictionary
	  end
	else classes_map
    with Class_not_found cn ->
      let missing_class = get_class class_path dictionary jclasses_map cn in
	add_file class_path c
	  (add_file class_path missing_class classes_map interfaces dictionary)
	  interfaces dictionary
  in begin
      let p_classes = ref classes_map in
	try while true do
	  let cn = List.hd !to_add in
	    to_add := List.tl !to_add;
	    if not (ClassMap.mem (dictionary.get_cn_index cn) !p_classes)
	    then
	      let c = get_class class_path dictionary jclasses_map cn
	      in p_classes :=
		   add_file class_path c !p_classes interfaces dictionary
	done;
	  !p_classes
	with Failure "hd" -> !p_classes
    end

let static_virtual_lookup virtual_lookup_map classes_map cni msi =
  try
    ClassMethMap.find (cni,msi) !virtual_lookup_map
  with
    | _ ->
	let cmset = ref ClassMethSet.empty in
	let class_info = ClassMap.find cni classes_map in
	let ioc = class_info.class_data in
	let methods_implementations = class_info.methods_implementations in
	  (match ioc with
	     | `Interface _ ->
		 failwith "Impossible InvokeVirtual"
	     | `Class c ->
		 try
		   let rc = JControlFlow.resolve_method' msi c in
		     cmset := ClassMethSet.add (rc.c_index,msi) !cmset
		 with _ -> ()
	  );
	  (try
	     let cset = MethodMap.find msi methods_implementations in
	       ClassSet.iter
		 (fun ncni ->
		    cmset := ClassMethSet.add (ncni,msi) !cmset
		 ) cset
	   with _ -> ()
	     (* failwith "Impossible InvokeVirtual" *)
	  );
	  virtual_lookup_map :=
	    ClassMethMap.add (cni,msi) !cmset !virtual_lookup_map;
	  !cmset
		  
let static_static_lookup static_lookup_map classes_map cni msi =
  try
    ClassMethMap.find (cni,msi) !static_lookup_map
  with
    | _ ->
	let cmset = ref ClassMethSet.empty in
	let class_info = ClassMap.find cni classes_map in
	let ioc = class_info.class_data in
	  (match ioc with
	     | `Interface _ -> failwith "Impossible InvokeStatic"
	     | `Class c ->
		 let rc = JControlFlow.resolve_method' msi c in
		   cmset := ClassMethSet.add (rc.c_index,msi) !cmset
	  );
	  static_lookup_map :=
	    ClassMethMap.add (cni,msi) !cmset !static_lookup_map;
	  !cmset

let static_interface_lookup interface_lookup_map virtual_lookup_map classes_map
    interfaces cni msi =
  try
    ClassMethMap.find (cni,msi) !interface_lookup_map
  with
    | _ ->
	let equivalent_classes =
	  try ClassMap.find cni interfaces
	  with _ -> ClassSet.empty in
	let cmset =
	  ClassSet.fold
	    (fun cni cmset ->
	       let lookupset =
		 static_virtual_lookup virtual_lookup_map classes_map cni msi in
		 ClassMethSet.union lookupset cmset
	    ) equivalent_classes ClassMethSet.empty in
	  interface_lookup_map :=
	    ClassMethMap.add (cni,msi) cmset !interface_lookup_map;
	  cmset

let static_special_lookup special_lookup_map classes_map cni ccni cmsi =
  try
    ClassMethMap.find (ccni,cmsi) (ClassMap.find cni !special_lookup_map)
  with
    | _ ->
	let update_special_lookup_map cni rcni ccni cmsi =
	  let s = ClassMethSet.add (rcni,cmsi) ClassMethSet.empty in
	  let e =
	    try ClassMap.find cni !special_lookup_map
	    with _ -> ClassMethMap.empty in
	    special_lookup_map :=
	      ClassMap.add cni (ClassMethMap.add (ccni,cmsi) s e)
		!special_lookup_map;
	    s in
	let current_class_info = ClassMap.find cni classes_map in
	let class_info = ClassMap.find ccni classes_map in
	let c =
	  match class_info.class_data with
	    | `Interface _ ->
		failwith "Impossible InvokeSpecial"
	    | `Class c -> c in
	let rc = JControlFlow.resolve_method' cmsi c in
	let rcni = rc.c_index in
	  match current_class_info.class_data with
	    | `Interface _ ->
		update_special_lookup_map cni rcni ccni cmsi
	    | `Class cc ->
		if ( cmsi = init_index
		    || not( (fun c1 c2 ->
			       if ( c1.c_index = c2.c_index ) then false
			       else extends_class c1 c2) cc rc )
		   ) then
		  update_special_lookup_map cni rcni ccni cmsi
		else
		  match cc.c_super_class with
		    | None -> failwith "Impossible InvokeSpecial"
		    | Some sc ->
			let rc = JControlFlow.resolve_method' cmsi sc in
			let rcni = rc.c_index in
			  update_special_lookup_map cni rcni ccni cmsi
	
let static_lookup dic classes_map interfaces cni msi pp =
  let m = get_method (ClassMap.find cni classes_map).class_data msi in
  let virtual_lookup_map = ref ClassMethMap.empty
  and interface_lookup_map = ref ClassMethMap.empty
  and static_lookup_map = ref ClassMethMap.empty
  and special_lookup_map = ref ClassMap.empty in
    match m with
      | AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
      | ConcreteMethod cm ->
	  (match cm.cm_implementation with
	     | Native -> failwith "Can't call static_lookup on Native methods"
	     | Java code ->
		 let c = (Lazy.force code).c_code in
		   try
		     let op = c.(pp) in
		     let (ccni,cmsi) = retrieve_invoke_index dic op in
		       match op with
			 | OpInvoke(`Interface _,_) ->
			     static_interface_lookup interface_lookup_map
			       virtual_lookup_map classes_map interfaces ccni cmsi
			 | OpInvoke (`Virtual _,_) ->
			     static_virtual_lookup virtual_lookup_map
			       classes_map ccni cmsi
			 | OpInvoke (`Static _,_) ->
			     static_static_lookup static_lookup_map
			       classes_map ccni cmsi
			 | OpInvoke (`Special _,_) ->
			     static_special_lookup special_lookup_map
			       classes_map cni ccni cmsi
			 | _ ->
			     failwith "Invalid opcode found at specified program point"
		   with
		     | Not_found -> failwith "Invalid program point"
		     | e -> raise e
	  )

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
  let interfaces = ref ClassMap.empty in
  let p_classes =
    ClassMap.fold
      (fun _ c classes -> add_file class_path c classes interfaces p_dic)
      !class_map ClassMap.empty
  in
    JFile.close_class_path class_path;
    { classes = ClassMap.map (fun ioc_info -> ioc_info.class_data) p_classes;
      static_lookup = static_lookup p_dic p_classes !interfaces;
      dictionary = p_dic }

let parse_program_bench class_path names =
  let time_start = Sys.time() in
    ignore(parse_program class_path names);
    let time_stop = Sys.time() in
      Printf.printf "program parsed in %fs.\n" (time_stop-.time_start)
