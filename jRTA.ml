open JBasics
open JClass
open JProgram

(* TODO: 
   
   - load Main.<clinit> when starting
   - manage visibility
   - remove (class_name*method_signature) in ClassMethSet 
   
*)

let print_method_signature ms =
  JDumpBasics.method_signature ms.ms_name (ms.ms_parameters,ms.ms_return_type)
    
type class_name' = class_name_index*class_name
type method_signature' = method_signature_index*method_signature
    
(*
  module ClassMap = Map.Make(struct type t = class_name' let compare = compare end)
  module MethodMap = Map.Make(struct type t = method_signature' let compare = compare end)
  module ClassMethSet : Set.S with type elt = class_name' * method_signature'
  = Set.Make(struct type t = class_name' * method_signature' 
  let compare = compare end)
  module ClassMethMap : Map.S with type key = class_name' * method_signature'
  = Map.Make(struct type t = class_name' * method_signature'
  let compare = compare end)
*)
    
(* module ClassMap = Map.Make(struct type t = class_name' let compare (x,_) (y,_) = compare x y end)  *)
module ClassMap = Ptmap
module MethodMap = Ptmap
module ClassMethSet = struct
  type t = (class_name*method_signature) Ptmap.t Ptmap.t
  type elt = ((int*class_name)*(int*method_signature))
  let empty : t = Ptmap.empty
  let is_empty : t -> bool = Ptmap.is_empty
  let mem : int*int -> t -> bool = fun (x,y) map ->
    try
      Ptmap.mem y (Ptmap.find x map)
    with Not_found -> false
  let add : elt -> t -> t = fun ((x,x'),(y,y')) map ->
    try
      Ptmap.add x (Ptmap.add y (x',y') (Ptmap.find x map)) map
    with Not_found -> 
      Ptmap.add x (Ptmap.add y (x',y') Ptmap.empty) map
  let remove : int*int -> t -> t = fun (x,y) map ->
    try
      let map_x = Ptmap.remove y (Ptmap.find x map) in
	if Ptmap.is_empty map_x then Ptmap.remove x map
	else Ptmap.add x map_x map
    with Not_found -> map
      
  exception Choose_find
  let choose_map map =
    let res = ref None in
      try
	Ptmap.fold 
	  (fun x a _ -> res := Some (x,a); raise Choose_find)
	  map (); raise Not_found
      with Choose_find ->
	begin match !res with
	    None -> failwith "choose_map"
	  | Some (x,a) -> (x,a)
	end
	  
  let choose : t -> elt = fun map ->
    let (x,map') = choose_map map in
    let (y,(x',y')) = choose_map map' in
      ((x,x'),(y,y'))
end

(* same type class_file and interface_file , except we keep the JClass.jmethod *)
  
let m2m p_dic = function
  | JClass.AbstractMethod m -> AbstractMethod (cam2pam p_dic m)
  | JClass.ConcreteMethod m -> ConcreteMethod (ccm2pcm p_dic m)
      
(* convert a JClass.jclass into a class_file given a program [p], a jclass [c],
   a super class_file [c_super], and a map of interfaces [interfaces].
   Let [c_children] as empty and [c_may_be_instanciated] as false. *)
let jclass2class_file p_dic name c c_super interfaces = 
  {c_index = fst name;
   c_name = snd name;
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
   c_interfaces = interfaces;
   c_sourcefile = c.JClass.c_sourcefile;
   c_deprecated = c.JClass.c_deprecated;
   c_enclosing_method = c.JClass.c_enclosing_method;
   c_source_debug_extention =c.JClass.c_source_debug_extention;
   c_inner_classes = c.JClass.c_inner_classes;
   c_other_attributes = c.JClass.c_other_attributes;
   c_fields = c.JClass.c_fields;
   c_methods = JClass.MethodMap.fold 
      (fun ms m -> MethodMap.add (p_dic.get_ms_index ms) (m2m p_dic m))
      c.JClass.c_methods
      MethodMap.empty;
   c_resolve_methods = MethodMap.empty;
   c_may_be_instanciated = false;
   c_children = ClassMap.empty}
    
(* convert a JClass.interface into a class_file given a program p,
   an interface [i], a super class_file [cl_object], and a map of
   interfaces [imap] 
   Let [i_children_class] and [i_children_interface] as empty. *)
let jinterface2interface_file p_dic i imap cl_object =
  {i_index = p_dic.get_cn_index i.JClass.i_name;
   i_name = i.JClass.i_name;
   i_version = i.JClass.i_version;
   i_access = i.JClass.i_access;
   i_generic_signature = i.JClass.i_generic_signature;
   i_consts = i.JClass.i_consts;
   i_annotation = i.JClass.i_annotation;
   i_other_flags = i.JClass.i_other_flags;
   i_interfaces = imap;
   i_sourcefile = i.JClass.i_sourcefile;
   i_deprecated = i.JClass.i_deprecated;
   i_source_debug_extention = i.JClass.i_source_debug_extention;
   i_inner_classes = i.JClass.i_inner_classes;
   i_other_attributes = i.JClass.i_other_attributes;
   i_children_interface = ClassMap.empty;
   i_children_class = ClassMap.empty;
   i_super = cl_object;
   i_initializer = 
      begin match i.JClass.i_initializer with
	| None -> None | Some m -> Some (ccm2pcm p_dic m)
      end;
   i_fields = i.JClass.i_fields;
   i_methods = JClass.MethodMap.fold 
      (fun ms m -> MethodMap.add (p_dic.get_ms_index ms) (cam2pam p_dic m))
      i.JClass.i_methods
      MethodMap.empty;
  }
    
let c_or_i_name = function
  | `Class c -> c.c_name
  | `Interface i -> i.i_name
      
let signature = function
  | AbstractMethod m -> m.am_signature
  | ConcreteMethod m -> m.cm_signature
      
exception Empty_workset
  
(* choose a method in a worket and remove it. 
   [workset] must be non empty.
   All method signature in [workset] must correspond to concrete methods in [program]. *)
let choose_meth_class_in_workset workset program =
  try
    let (cn,ms) = ClassMethSet.choose !workset in
      workset := ClassMethSet.remove (fst cn,fst ms) !workset; 
      try
	match ClassMap.find (fst cn) program with
	  | `Class cl ->
	      begin
		try
		  match MethodMap.find (fst ms) cl.c_methods with
		    | AbstractMethod _ -> failwith "choose_meth_class_in_workset : abstract method"
		    | ConcreteMethod meth -> (cn,meth)
		with Not_found ->
		  Printf.printf "looking for %d %s in\n"
		    (fst ms) (print_method_signature (snd ms));
		  MethodMap.iter
		    (fun ms meth -> Printf.printf "  %d %s\n" ms (print_method_signature (signature meth)))
		    cl.c_methods;
		  failwith "choose_meth_class_in_workset : method not found"
	      end
	  | `Interface i ->
	      begin
		match i.i_initializer with
		  | Some meth -> (cn,meth)
		  | None -> failwith "choose_meth_class_in_workset: clinit not found in interface"
	      end
      with Not_found ->
	Printf.printf "looking for %d %s in\n"
	  (fst cn) (JDumpBasics.class_name (snd cn));
	ClassMap.iter
	  (fun cn_idx c_or_i -> Printf.printf "  %d %s\n" cn_idx (JDumpBasics.class_name (c_or_i_name c_or_i)))
	  program;
	failwith "choose_meth_class_in_workset : class not found"
  with Not_found -> raise Empty_workset
    
let get_class = function
  | `Interface _ -> failwith "class expected"
  | `Class c -> c
      
let get_interface = function
  | `Interface i -> i
  | `Class _ -> failwith "interface expected"
      
(* load a class or an interface. If the class or the interface has not been loaded
   yet in [program], load it using [class_path] and load recursively (if necessary)
   all its super classes and interfaces. *)
let rec load class_path program_classes program_dictionary name =
  try
    ClassMap.find (fst name) !program_classes
  with Not_found -> 
    begin
      match JFile.get_class class_path (JDumpBasics.class_name (snd name)) with
	| `Interface i ->
	    let super_interfaces = 
	      List.fold_right 
		(fun name -> 
		   let name = (program_dictionary.get_cn_index name,name) in
		     ClassMap.add (fst name) (get_interface(load class_path program_classes program_dictionary name)))
		i.JClass.i_interfaces ClassMap.empty in
	    let i = jinterface2interface_file program_dictionary i super_interfaces
	      (get_class (load class_path program_classes program_dictionary
			    (program_dictionary.get_cn_index java_lang_object, java_lang_object))) in
	      ClassMap.iter
		(fun _ i_super -> i_super.i_children_interface <- ClassMap.add i.i_index i i_super.i_children_interface)
		super_interfaces;
	      (* MethodMap.iter (fun msi _ -> declare_method (`Interface i) msi) i.i_methods; *)
	      program_classes := ClassMap.add (fst name) (`Interface i) !program_classes; `Interface i
	| `Class c -> 
	    let super = match c.JClass.c_super_class with
	      | None -> None
	      | Some cn_super -> Some (get_class (load class_path program_classes program_dictionary
						    (program_dictionary.get_cn_index cn_super, cn_super)))
	    and super_interfaces = 
	      List.fold_right 
		(fun name -> 
		   let name = (program_dictionary.get_cn_index name, name) in
		     ClassMap.add (fst name)
		       (get_interface (load class_path program_classes program_dictionary name)))
		c.JClass.c_interfaces ClassMap.empty in
	    let c = jclass2class_file program_dictionary name c super super_interfaces in
	      program_classes := ClassMap.add (fst name) (`Class c) !program_classes;
	      (* set super.c_children *)
	      (match super with
		 | None -> ()
		 | Some super ->
		     super.c_children <- ClassMap.add c.c_index c super.c_children);
	      (* set i.i_children_class of super interfaces *)
	      ClassMap.iter
		(fun _ i -> i.i_children_class <- ClassMap.add c.c_index c i.i_children_class)
		c.c_interfaces;
	      (* set c_resolve_methods *)
	      (match super with
		 | None -> c.c_resolve_methods <- MethodMap.map (fun meth -> (c,meth)) c.c_methods
		 | Some super ->
		     c.c_resolve_methods <-
		       MethodMap.fold
		       (fun ms m -> MethodMap.add ms (c,m))
		       c.c_methods
		       super.c_resolve_methods);
	      (* MethodMap.iter (fun msi _ -> declare_method (`Class c) msi) c.c_methods; *)
	      `Class c
    end
      
(* resolve method signature [ms] in class [cl]. *)
let resolve_method cl ms =
  try
    MethodMap.find (fst ms) cl.c_resolve_methods
  with
      Not_found ->failwith "resolution failed"
	
(* add an arc [(cn0,ms0) --> (cn,ms)] in the call graph [call_graph]. 
   Does not print call to <clinit>. *)
let add_call_graph (cn0,ms0) (cn,ms) call_graph =
  if ms <> clinit_index then
    let s =
      try ClassMethMap.find (cn0,ms0) !call_graph
      with Not_found -> JProgram.ClassMethSet.empty in
      if (not (JProgram.ClassMethSet.mem (cn,ms) s)) then
  	call_graph :=
  	  ClassMethMap.add (cn0,ms0) (JProgram.ClassMethSet.add (cn,ms) s) !call_graph
	    
let clinit_signature = (clinit_index,JClass.clinit_signature)
  
(* add the <clinit> method of class name [cn] to the workset [workset], if necessary. *)
let rec add_clinits class_path cn0 meth0 cn call_graph workset program_classes program_dictionary =
  match load class_path program_classes program_dictionary cn with
    | `Class cl ->
	begin
	  let (cl,meth) = resolve_method cl clinit_signature in
	    match meth with
	      | AbstractMethod _ -> failwith "add_clinits: abstract method"
	      | ConcreteMethod meth -> 
		  begin
		    if (not (meth.cm_has_been_parsed)) then
		      begin						
			workset := ClassMethSet.add ((cl.c_index,cl.c_name),(meth.cm_index,meth.cm_signature)) !workset;
			match cl.c_super_class with
			  | None -> ()
			  | Some cl_super ->
			      (* the <clinit> super class's must be called too *)
			      add_clinits class_path (cl.c_index,cl.c_name)
				meth (cl_super.c_index,cl_super.c_name) call_graph workset
				program_classes program_dictionary
		      end;
		    add_call_graph (fst cn0, meth0.cm_index) (cl.c_index, meth.cm_index) call_graph
		  end
	end
    | `Interface i ->
	begin
	  match i.i_initializer with
	    | None -> ()
	    | Some meth ->
		if (not meth.cm_has_been_parsed) then
		  (* the <clinit> of super interfaces do not need to be called, according to the official spec *)
		  workset := ClassMethSet.add ((i.i_index,i.i_name),clinit_signature) !workset;
		add_call_graph (fst cn0,meth0.cm_index) (i.i_index,clinit_index) call_graph
	end
	  
(* apply [f] on [cl] and all its super classes and interfaces *)
let rec forall_super_class_and_interfaces f cl =
  f cl;
  match cl with
    | `Class cl ->
	begin
	  match cl.c_super_class with
	    | None -> ()
	    | Some super -> forall_super_class_and_interfaces f (`Class super)
	end;
	ClassMap.iter
	  (fun _ i -> forall_super_class_and_interfaces f (`Interface i))
	  cl.c_interfaces
    | `Interface i ->
	forall_super_class_and_interfaces f (`Class i.i_super);
	ClassMap.iter
	  (fun _ i -> forall_super_class_and_interfaces f (`Interface i))
	  i.i_interfaces
	  
(* apply [f] on [cl] and all its sons *)
let rec forall_class_sons f cl =
  f cl;
  ClassMap.iter (fun _ -> forall_class_sons f) cl.c_children
    
(* apply [f] on all classes that implements [i] *)
let rec forall_interface_sons f i =
  ClassMap.iter (fun _ -> forall_class_sons f) i.i_children_class;
  ClassMap.iter (fun _ -> forall_interface_sons f) i.i_children_interface
    
(* print call graph in [callgraph.dot] and class hierarchy in [hierarchy.dot] *)
(* let print_callgraph callgraph program = *)
(*   let out = open_out "callgraph.dot" in *)
(*     Printf.fprintf out "digraph \"Graph\" {\n"; *)
(*     ClassMethMap.iter *)
(*       (fun (cn1,ms1) succs ->  *)
(* 	 List.iter  *)
(* 	   (fun (cn2,ms2) ->  *)
(* 	      Printf.fprintf out "   \"%s.%s\" -> \"%s.%s\";\n"  *)
(* 		(JDumpBasics.class_name (snd cn1)) *)
(* 		(snd ms1).ms_name *)
(* 		(JDumpBasics.class_name (snd cn2)) *)
(* 		(snd ms2).ms_name *)
(* 	   ) succs *)
(*       ) callgraph; *)
(*     Printf.fprintf out "}"; *)
(*     close_out out; *)
(*     let out = open_out "hierarchy.dot" in *)
(*       Printf.fprintf out "digraph \"Graph\" {\n"; *)
(*       ClassMap.iter *)
(* 	(fun _ c_or_i ->  *)
(* 	   match c_or_i with *)
(* 	     | `Class cl -> *)
(* 		 if cl.c_may_be_instanciated then *)
(* 		   Printf.fprintf out "   \"%s\" [color=blue,style=filled];" (JDumpBasics.class_name cl.c_name); *)
(* 		 begin *)
(* 		   match cl.c_super_class with *)
(* 		     | None -> () *)
(* 		     | Some cl_super -> *)
(* 			 Printf.fprintf out "   \"%s\" -> \"%s\";\n"  *)
(* 			   (JDumpBasics.class_name (c_or_i_name c_or_i)) *)
(* 			   (JDumpBasics.class_name cl_super.c_name) *)
(* 		 end; *)
(* 		 ClassMap.iter *)
(* 		   (fun _ i -> *)
(* 		      Printf.fprintf out "   \"%s\" -> \"%s\" [style=dotted];\n"  *)
(* 			(JDumpBasics.class_name (c_or_i_name c_or_i)) *)
(* 			(JDumpBasics.class_name i.i_name)) *)
(* 		   cl.c_interfaces *)
(* 	     | `Interface i -> *)
(* 		 Printf.fprintf out "   \"%s\" [shape=rectangle];" (JDumpBasics.class_name i.i_name); *)
(* 		 ClassMap.iter *)
(* 		   (fun _ i' -> *)
(* 		      Printf.fprintf out "   \"%s\" -> \"%s\" [style=dotted];\n"  *)
(* 			(JDumpBasics.class_name i.i_name) *)
(* 			(JDumpBasics.class_name i'.i_name)) *)
(* 		   i.i_interfaces; *)
(* 		 Printf.fprintf out "   \"%s\" -> \"%s\";\n"  *)
(* 		   (JDumpBasics.class_name i.i_name) *)
(* 		   (JDumpBasics.class_name i.i_super.c_name) *)
(* 	) *)
(* 	program; *)
(*       Printf.fprintf out "}"; *)
(*       close_out out *)
    
(* subclass_test *)
(* physical equality should be sufficient since we never create two times the same class *)
let rec subclass_test cl1 cl2 =
  if cl1==cl2 then true
  else match cl1.c_super_class with
    | None -> false
    | Some cl1 -> subclass_test cl1 cl2
	
let has_been_parsed = function
  | ConcreteMethod m -> m.cm_has_been_parsed
  | AbstractMethod _ -> true
      
let rta class_path init_workset =
  (* the program structure *)
  let program_classes = ref ClassMap.empty
  and program_dictionary = make_dictionary () in
    (* the workset of pending method to be explored *)
  let workset = ref ClassMethSet.empty
    (* we memorize the virtual invoke instructions (invokevirtual and invokeinterface)
       of the methods that are already parsed *)
  and memorize_virtual_calls = ref ClassMap.empty 
    (* we build the call graph *)
  and call_graph = ref ClassMethMap.empty in
    (* we initialise the workset with all methods in [init_workset] *)
    List.iter
      (fun (cn,ms) ->
	 let cn' = (program_dictionary.get_cn_index cn, cn) in
	 let ms' = (program_dictionary.get_ms_index ms, ms) in
	 let _ = load class_path program_classes program_dictionary cn' in
	   workset := ClassMethSet.add (cn',ms') !workset)
      init_workset;
    (* sub-function to add call [(cn0,ms0) --> (cl,ms)] *)
    let add_call cn ms cl ms' =
      let (cl',meth) = resolve_method cl ms' in
      let cn' = (cl'.c_index,cl'.c_name) in
	if (not (has_been_parsed meth)) then
	  workset := ClassMethSet.add (cn',ms') !workset;
	add_call_graph (cn,ms) (fst cn',fst ms') call_graph
    in
      begin try
	while true do
	  (* we choose a method and remove it from the workset *)
	  let (cn,meth) = choose_meth_class_in_workset workset !program_classes in
	    (* mark [(cn,meth)] as seen *)
	    meth.cm_has_been_parsed <- true;
	    match meth.cm_implementation with 
	      | Native -> ()
	      | Java c ->
		  Array.iter
		    (function
		       | OpGetStatic (cn',_) 
		       | OpPutStatic (cn',_) -> 
			   (* we need to explore [cn'.<clinit>] *)
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			     add_clinits class_path cn meth cn' call_graph workset
			       program_classes program_dictionary
		       | OpNew cn' -> 
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			   let c_or_i = load class_path program_classes program_dictionary cn' in
			     begin
			       (* we need to explore [cn'.<clinit>] *)
			       add_clinits class_path cn meth cn' call_graph workset
				 program_classes program_dictionary;
			       match c_or_i with
				 | `Class c1 ->
				     if not c1.c_may_be_instanciated then
				       (* we have never seen a [OpNew c1] before *)
				       (* we browse all the super classes and interface
					  of [c1] looking for virtual calls that may be ran
					  on an object of class [c1] *)
				       forall_super_class_and_interfaces 
 					 (function
					    | `Class c2 -> 
						begin try
						  List.iter
						    (fun (cn_caller,ms_caller,ms_callee) -> 
						       add_call (fst cn_caller) ms_caller c1 ms_callee)
						    (ClassMap.find c2.c_index !memorize_virtual_calls)
						with Not_found -> ()
						end
					    | `Interface i -> 
						begin try
						  List.iter
						    (fun (cn_caller,ms_caller,ms_callee) -> 
						       add_call (fst cn_caller) ms_caller c1 ms_callee)
						    (ClassMap.find i.i_index !memorize_virtual_calls)
						with Not_found -> () end
					 ) (`Class c1);
				     c1.c_may_be_instanciated <- true
				 | `Interface _ -> failwith "new interface ?"
			     end
		       | OpInvoke (`Virtual t, ms) ->
			   let cn' = match t with 
			     | TClass cn' -> cn' 
			     | TArray _ -> (* should only happen with [clone()] *) java_lang_object
			   in
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			   let ms = (program_dictionary.get_ms_index ms, ms) in
			     begin
			       match load class_path program_classes program_dictionary cn' with
				 | `Interface _ -> failwith "invokevirtual on interface"
				 | `Class cl' -> 
				     forall_class_sons
				       (fun cl -> if cl.c_may_be_instanciated then 
					  add_call (fst cn) meth.cm_index cl ms)
				       cl'
			     end;
			     let calls = 
			       try ClassMap.find (fst cn') !memorize_virtual_calls 
			       with Not_found -> [] in
			       memorize_virtual_calls := ClassMap.add (fst cn') ((cn,meth.cm_index,ms)::calls) !memorize_virtual_calls
		       | OpInvoke (`Interface cn',ms) -> 
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			   let ms = (program_dictionary.get_ms_index ms, ms) in
			     begin
			       match load class_path program_classes program_dictionary cn' with
				 | `Class _ -> failwith "invokestatic on class"
				 | `Interface i -> 
				     forall_interface_sons
				       (fun cl -> if cl.c_may_be_instanciated then
					  add_call (fst cn) meth.cm_index cl ms)
				       i
			     end;
			     let calls = 
			       try ClassMap.find (fst cn') !memorize_virtual_calls 
			       with Not_found -> [] in
			       memorize_virtual_calls := ClassMap.add (fst cn') ((cn,meth.cm_index,ms)::calls) !memorize_virtual_calls
		       | OpInvoke (`Static cn',ms) -> 
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			   let ms = (program_dictionary.get_ms_index ms, ms) in
			     begin
			       match load class_path program_classes program_dictionary cn' with
				 | `Interface _ -> failwith "invokestatic on interface"
				 | `Class cl' -> 
				     add_call (fst cn) meth.cm_index cl' ms
			     end;
			     add_clinits class_path cn meth cn' call_graph workset program_classes program_dictionary
		       | OpInvoke (`Special cn',ms) -> 
			   let cn' = (program_dictionary.get_cn_index cn', cn') in
			   let ms = (program_dictionary.get_ms_index ms, ms) in
			     begin
			       match load class_path program_classes program_dictionary cn' with
				 | `Interface _ -> failwith "invokespecial on interface"
				 | `Class cl' -> 
				     if (snd ms).ms_name = "<init>" then
				       add_call (fst cn) meth.cm_index cl' ms
				     else
				       let (cl',_) = resolve_method cl' ms in
					 match ClassMap.find (fst cn) !program_classes with
					   | `Interface _ -> failwith "invokespecial in an interface"
					   | `Class cl ->
					       begin
						 match cl.c_super_class with
						   | None -> ()
						   | Some cl_super ->
						       if subclass_test cl_super cl' then
							 add_call (fst cn) meth.cm_index cl_super ms
						       else add_call (fst cn) meth.cm_index cl' ms
					       end
			     end					   
		       | _ -> ()
		    ) (Lazy.force c).c_code
	done
      with Empty_workset -> ()
      end;
      ({ classes = !program_classes; dictionary = program_dictionary },!call_graph)
	
let initializeSystemClass : class_name * method_signature = 
  (["java";"lang";"System"],
   {ms_name = "initializeSystemClass";
    ms_parameters = [];
    ms_return_type = None})
    
let test_parsing class_path program_classes =
  ClassMap.iter
    (fun _ c_or_i ->
       match c_or_i with
	 | `Interface i1 ->
	     begin
	       match JFile.get_class class_path (JDumpBasics.class_name i1.i_name) with	   
		 | `Class _ -> assert false
		 | `Interface i2 ->
		     begin
		       match i2.JClass.i_initializer with
			 | None -> ()
			 | Some meth2 ->
			     begin
			       match meth2.JClass.cm_implementation with
				 | Native -> assert false
				 | Java c -> Array.iter (fun _ -> ()) (Lazy.force c).c_code
			     end 
		     end
	     end
	 | `Class c1 ->
	     begin
	       match JFile.get_class class_path (JDumpBasics.class_name c1.c_name) with	   
		 | `Interface _ -> assert false
		 | `Class c2 ->
		     JClass.MethodMap.iter
		       (fun _ meth2 ->
			  begin
			    match meth2 with
			      | JClass.AbstractMethod _ -> ()
			      | JClass.ConcreteMethod meth2 ->
				  begin
				    match meth2.JClass.cm_implementation with
				      | Native -> ()
				      | Java c -> Array.iter (fun _ -> ()) (Lazy.force c).c_code
				  end
			  end)
		       c2.JClass.c_methods
	     end			   
    ) program_classes
    
(** [classname] is the entry class name *)
let start_rta ?(debug = false) class_path classname =
  let time1 = Sys.time () in
  let class_path = JFile.class_path class_path in
  let to_be_explored =
    if ( debug ) then
      [(classname,main_signature)]
    else
      [(classname,main_signature);initializeSystemClass] in
  let (program,_) = rta class_path to_be_explored in
  let time2 = Sys.time () in
    Printf.printf "%d classes parsed\n"
      (ClassMap.fold (fun _ _ n -> n+1) program.classes 0);
    let time3 = Sys.time () in
      test_parsing class_path program.classes;
      let time4 = Sys.time () in
	JFile.close_class_path class_path;
	Printf.printf "RTA parsing: %fs\nSimple parsing (all classes given by RTA and all their methods): %fs\n" 
	  (time2-.time1) (time4-.time3)
	  
let retrieve_invoke_index p op =
  let dic = p.dictionary in
    match op with
      | OpInvoke (`Virtual t, ms) ->
	  let cni = match t with 
	    | TClass cn -> dic.get_cn_index cn
	    | TArray _ -> (* should only happen with [clone()] *)
		java_lang_object_index
	  and msi = dic.get_ms_index ms in (cni,msi)
      | OpInvoke(`Special cn, ms) ->
	  let cni = dic.get_cn_index cn
	  and msi = dic.get_ms_index ms in (cni,msi)
      | OpInvoke(`Static cn, ms) ->
	  let cni = dic.get_cn_index cn
	  and msi = dic.get_ms_index ms in (cni,msi)
      | OpInvoke(`Interface cn, ms) ->
	  let cni = dic.get_cn_index cn
	  and msi = dic.get_ms_index ms in (cni,msi)
      | _ -> failwith "Bad opcode"
	  
let static_lookup p callgraph cni msi pp =
  let ioc = (Ptmap.find cni p.classes) in
  let m =
    match ioc with
      | `Interface _ -> failwith "Can't find a implemented method in an interface"
      | `Class c -> Ptmap.find msi c.c_methods
  in
    match m with
      | AbstractMethod _ -> failwith "Can't call static_lookup on Abstract Methods"
      | ConcreteMethod cm ->
	  (match cm.cm_implementation with
	     | Native -> failwith "Can't call static_lookup on Native methods"
	     | Java code ->
		 let c = (Lazy.force code).c_code in
		   try
		     let op = c.(pp) in
		       match op with
			 | OpInvoke _ ->
			     let x = retrieve_invoke_index p op in
			       (try
				  ClassMethMap.find x callgraph
				with _ ->
				  raise Not_found
			       )
			 | _ ->
			     failwith "Invalid opcode found at specified program point"
		   with _ ->
		     failwith "Invalid program point")
	    
let parse_program ?(debug = false) class_path classname =
  let class_path = JFile.class_path class_path in
  let to_be_explored =
    (* The main class MUST be the first to enter the dictionary
       (after java.lang.Object) *)
    if ( debug ) then
      [(classname,main_signature)]
    else
      [(classname,main_signature);initializeSystemClass] in
  let (program,callgraph) = rta class_path to_be_explored in
    (static_lookup program callgraph, program)
