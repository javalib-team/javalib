open JBasics
open JClass

let print_method_signature ms =
  JDumpBasics.method_signature ms.ms_name (ms.ms_parameters,ms.ms_return_type)

module ClassMap0 = Map.Make(struct type t = class_name let compare = compare end)

type method_signature_index = int 
type method_signature_index_table =
	{ mutable msi_map : method_signature_index MethodMap.t;
	  mutable msi_next : method_signature_index }
type class_name_index = int
type class_name_index_table =
	{ mutable cni_map : class_name_index ClassMap0.t;
	  mutable cni_next : class_name_index }

(* global table *)
let method_signature_index_tab = { msi_map = MethodMap.empty; msi_next = 0 } 
let class_name_index_tab = { cni_map = ClassMap0.empty; cni_next = 0 }

let get_ms_index ms =
  let tab = method_signature_index_tab in
	try
	  (MethodMap.find ms tab.msi_map,ms)
	with Not_found -> 
	  begin
		tab.msi_map <- MethodMap.add ms tab.msi_next tab.msi_map;
		tab.msi_next <- tab.msi_next + 1;
		(tab.msi_next-1,ms)
	  end

(*
let get_ms_index ms =
  let (i,ms) = get_ms_index ms in
	Printf.printf "%s --> %d\n" (print_method_signature ms) i;
	(i,ms)
*)
	
let get_cn_index cn =
  let tab = class_name_index_tab in
	try
	  (ClassMap0.find cn tab.cni_map,cn)
	with Not_found -> 
	  begin
		tab.cni_map <- ClassMap0.add cn tab.cni_next tab.cni_map;
		tab.cni_next <- tab.cni_next + 1;
		(tab.cni_next-1,cn)
	  end

type class_name' = class_name_index*class_name
type method_signature' = method_signature_index*method_signature


module ClassMap = Map.Make(struct type t = class_name' let compare = compare end)
module MethodMap = Map.Make(struct type t = method_signature' let compare = compare end)
module ClassMethSet : Set.S with type elt = class_name' * method_signature'
  = Set.Make(struct type t = class_name' * method_signature' 
					let compare = compare end)
module ClassMethMap : Map.S with type key = class_name' * method_signature'
  = Map.Make(struct type t = class_name' * method_signature'
					let compare = compare end)
(*
 module ClassMap = Map.Make(struct type t = class_name' let compare (x,_) (y,_) = compare x y end) 
   module MethodMap = Map.Make(struct type t = method_signature' let compare (x,_) (y,_) = compare x y end) 
   module ClassMethSet : Set.S with type elt = class_name' * method_signature'
   = Set.Make(struct type t = class_name' * method_signature' 
   let compare ((c1,_),(m1,_)) ((c2,_),(m2,_)) = compare (c1,m1) (c2,m2) end) 
   module ClassMethMap : Map.S with type key = class_name' * method_signature'
   = Map.Make(struct type t = class_name' * method_signature'
   let compare ((c1,_),(m1,_)) ((c2,_),(m2,_)) = compare (c1,m1) (c2,m2) end) 
*)

(* same type class_file and interface_file , except we keep the JClass.jmethod *)

type concrete_method = {
  mutable cm_has_been_parsed : bool;
  cm_index : method_signature_index;
  cm_signature : method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access: access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_implementation : implementation;
}

type abstract_method = {
  am_index : method_signature_index;
  am_signature : method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
}

type jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of concrete_method

type class_file = {
  c_name : class_name;
  c_index : class_name_index;
  c_version : version;
  c_access : [`Public | `Default];
  c_generic_signature : JSignature.classSignature option;
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
  c_enclosing_method : (class_name * method_signature option) option;
  c_source_debug_extention : string option;
  c_inner_classes : inner_class list;
  c_other_attributes : (string * string) list;
  c_methods : jmethod MethodMap.t;
  mutable c_may_be_instanciated : bool;
  mutable c_children : class_file ClassMap.t;
}

and interface_file = {
  i_name : class_name;
  i_index : class_name_index;
  i_version : version;
  i_access : [`Public | `Default];
  i_generic_signature : JSignature.classSignature option;
  i_annotation: bool;
  i_other_flags : int list;
  i_interfaces : interface_file ClassMap.t;
  i_consts : constant array; (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
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

type program = interface_or_class ClassMap.t

let ccm2pcm m = {
  cm_has_been_parsed = false;
  cm_index = fst (get_ms_index m.JClass.cm_signature);
  cm_signature = m.JClass.cm_signature;
  cm_static = m.JClass.cm_static;
  cm_final = m.JClass.cm_final;
  cm_synchronized = m.JClass.cm_synchronized;
  cm_strict = m.JClass.cm_strict;
  cm_access = m.JClass.cm_access;
  cm_generic_signature = m.JClass.cm_generic_signature;
  cm_bridge = m.JClass.cm_bridge;
  cm_varargs = m.JClass.cm_varargs;
  cm_synthetic = m.JClass.cm_synthetic;
  cm_other_flags = m.JClass.cm_other_flags;
  cm_exceptions = m.JClass.cm_exceptions;
  cm_attributes = m.JClass.cm_attributes;
  cm_implementation = m.JClass.cm_implementation
}


let cam2pam m = {
  am_index = fst (get_ms_index m.JClass.am_signature);
  am_signature = m.JClass.am_signature;
  am_access = m.JClass.am_access;
  am_generic_signature = m.JClass.am_generic_signature;
  am_bridge = m.JClass.am_bridge;
  am_varargs = m.JClass.am_varargs;
  am_synthetic = m.JClass.am_synthetic;
  am_other_flags = m.JClass.am_other_flags;
  am_exceptions = m.JClass.am_exceptions;
  am_attributes = m.JClass.am_attributes
}

let m2m = function
  | JClass.AbstractMethod m -> AbstractMethod (cam2pam m)
  | JClass.ConcreteMethod m -> ConcreteMethod (ccm2pcm m)

(* convert a JClass.jclass into a class_file given a jclass [c], a super
   class_file [c_super], and a map of interfaces [interfaces]. 
   Let [c_children] as empty and [c_may_be_instanciated] as false. *)
let jclass2class_file name c c_super interfaces = 
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
	(fun ms m -> MethodMap.add (get_ms_index ms) (m2m m))
	c.JClass.c_methods
	MethodMap.empty;
   c_may_be_instanciated = false;
   c_children = ClassMap.empty}

(* convert a JClass.interface into a class_file given an interface [i], a super
   class_file [cl_object], and a map of interfaces [imap] 
   Let [i_children_class] and [i_children_interface] as empty. *)
let jinterface2interface_file i imap cl_object =
  {i_index = fst (get_cn_index i.JClass.i_name);
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
		| None -> None | Some m -> Some (ccm2pcm m)
	  end;
   i_fields = i.JClass.i_fields;
   i_methods = JClass.MethodMap.fold 
	(fun ms m -> MethodMap.add (get_ms_index ms) (cam2pam m))
	i.JClass.i_methods
	MethodMap.empty;
  }


(* choose a method in a worket and remove it. 
   [workset] must be non empty.
   All method signature in [workset] must correspond to concrete methods in [program]. *)
let choose_meth_class_in_workset workset program =
  try
	let (cn,ms) = ClassMethSet.choose !workset in
	  workset := ClassMethSet.remove (cn,ms) !workset; 
	  try
	  match ClassMap.find cn program with
		| `Class cl ->
			begin
			  try
			  match MethodMap.find ms cl.c_methods with
				| AbstractMethod _ -> failwith "choose_meth_class_in_workset : abstract method"
				| ConcreteMethod meth -> (cn,meth)
			  with Not_found ->
				Printf.printf "looking for %d %s in\n"
				  (fst ms) (print_method_signature (snd ms));
				MethodMap.iter
				  (fun ms _ -> Printf.printf "  %d %s\n" (fst ms) (print_method_signature (snd ms)))
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
		  (fun cn _ -> Printf.printf "  %d %s\n" (fst cn) (JDumpBasics.class_name (snd cn)))
		  program;
		failwith "choose_meth_class_in_workset : class not found"
  with Not_found -> failwith "choose_meth_class_in_workset : workset must be non empty"

let get_class = function
  | `Interface _ -> failwith "class expected"
  | `Class c -> c

let get_interface = function
  | `Interface i -> i
  | `Class _ -> failwith "interface expected"

(* load a class or an interface. If the class or the interface has not been loaded
   yet in [program], load it using [class_path] and load recursively (if necessary)
   all its super classes and interfaces. *)
let rec load class_path program name =
  try
	ClassMap.find name !program 
  with Not_found -> 
	begin
	  match JFile.get_class class_path (JDumpBasics.class_name (snd name)) with
		| `Interface i ->
			let super_interfaces = 
			  List.fold_right 
				(fun name -> 
				   let name = get_cn_index name in
					 ClassMap.add name (get_interface (load class_path program name)))
				i.JClass.i_interfaces ClassMap.empty in
			let i = jinterface2interface_file i super_interfaces (get_class (load class_path program (get_cn_index java_lang_object))) in
			  ClassMap.iter
				(fun _ i_super -> i_super.i_children_interface <- ClassMap.add (i.i_index,i.i_name) i i_super.i_children_interface)
				super_interfaces;
			  program := ClassMap.add name (`Interface i) !program; `Interface i
		| `Class c -> 
			let super = match c.JClass.c_super_class with
			  | None -> None
			  | Some cn_super -> Some (get_class (load class_path program (get_cn_index cn_super)))
			and super_interfaces = 
			  List.fold_right 
				(fun name -> 
				   let name = get_cn_index name in
					 ClassMap.add name (get_interface (load class_path program name)))
				c.JClass.c_interfaces ClassMap.empty in
			let c = jclass2class_file name c super super_interfaces in
			  program := ClassMap.add name (`Class c) !program; 
			  (match super with
				 | None -> ()
				 | Some super ->
					 super.c_children <- ClassMap.add (c.c_index,c.c_name) c super.c_children);
			  ClassMap.iter
				(fun _ i -> i.i_children_class <- ClassMap.add (c.c_index,c.c_name) c i.i_children_class)
				c.c_interfaces;
			  `Class c
	end

(* resolve method signature [ms] in class [cl]. *)
let rec resolve_method cl ms =
  try
	(cl,MethodMap.find ms cl.c_methods)
  with
	  Not_found ->
		match cl.c_super_class with
		  | None -> failwith "resolution failed"
		  | Some cn_super -> resolve_method cn_super ms

(* add an arc [(cn0,ms0) --> (cn,ms)] in the call graph [call_graph]. 
   Does not print call to <clinit>. *)
let add_call_graph (cn0,ms0) (cn,ms) call_graph = ()
(*  if ms<>JClass.clinit_signature then *)
(* 	let l =  *)
(* 	  try ClassMethMap.find (cn0,ms0) !call_graph  *)
(* 	  with Not_found -> [] in *)
(* 	  if (not (List.mem (cn,ms) l)) then *)
(* 		call_graph := *)
(* 		  ClassMethMap.add (cn0,ms0) ((cn,ms)::l) !call_graph *)
		  
let clinit_signature = get_ms_index JClass.clinit_signature

(* add the <clinit> method of class name [cn] to the workset [workset], if necessary. *)
let rec add_clinits class_path cn0 meth0 cn call_graph workset program =
  match load class_path program cn with
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
							  add_clinits class_path (cl.c_index,cl.c_name) meth (cl_super.c_index,cl_super.c_name) call_graph workset program
					  end;
					add_call_graph (cn0,meth0.cm_signature) (cl.c_name,meth.cm_signature) call_graph
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
				add_call_graph (cn0,meth0.cm_signature) (i.i_name,JClass.clinit_signature) call_graph
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

(* reduce the call graph to nodes that are reachable from [nodes],
   with a max diameter [n] if [n] is positive. *)
let rec reduce_call_graph nodes callgraph n =
  let stabilise = ref true in
	if n=0 then nodes
	else
	  let new_nodes =
		ClassMethSet.fold
		  (fun (cn1,ms1) nodes ->
			 try 
			   List.fold_right
				 (fun (cn2,ms2) nodes ->
					if ClassMethSet.mem (cn2,ms2) nodes then nodes
					else (stabilise:=false; ClassMethSet.add (cn2,ms2) nodes))
				 (ClassMethMap.find (cn1,ms1) callgraph)
				 nodes
			 with Not_found -> nodes)
		  nodes nodes in
		if !stabilise then nodes else reduce_call_graph new_nodes callgraph (n-1)

let reduce_call_graph start callgraph max_diameter =
  let nodes = reduce_call_graph (ClassMethSet.add start ClassMethSet.empty) callgraph max_diameter in
	ClassMethMap.fold
	  (fun x1 l callgraph ->
		 if ClassMethSet.mem x1 nodes then
		   ClassMethMap.add x1 (List.filter (fun x2 -> ClassMethSet.mem x2 nodes) l) callgraph
		 else callgraph)
	  callgraph
	  ClassMethMap.empty

(* print call graph in [callgraph.dot] and class hierarchy in [hierarchy.dot] *)
let print_callgraph callgraph program =
  let out = open_out "callgraph.dot" in
  Printf.fprintf out "digraph \"Graph\" {\n";
  ClassMethMap.iter
	(fun (cn1,ms1) succs -> 
	   List.iter 
		 (fun (cn2,ms2) -> 
			Printf.fprintf out "   \"%s.%s\" -> \"%s.%s\";\n" 
			  (JDumpBasics.class_name (snd cn1))
			  (snd ms1).ms_name
			  (JDumpBasics.class_name (snd cn2))
			  (snd ms2).ms_name
		 ) succs
	) callgraph;
  Printf.fprintf out "}";
  close_out out;
  let out = open_out "hierarchy.dot" in
	Printf.fprintf out "digraph \"Graph\" {\n";
	ClassMap.iter
	  (fun cn c_or_i -> 
	   match c_or_i with
		 | `Class cl ->
			 if cl.c_may_be_instanciated then
			   Printf.fprintf out "   \"%s\" [color=blue,style=filled];" (JDumpBasics.class_name cl.c_name);
			 begin
			   match cl.c_super_class with
				 | None -> ()
				 | Some cl_super ->
					 Printf.fprintf out "   \"%s\" -> \"%s\";\n" 
					   (JDumpBasics.class_name (snd cn))
					   (JDumpBasics.class_name cl_super.c_name)
			 end;
			 ClassMap.iter
			   (fun iname _ ->
				  Printf.fprintf out "   \"%s\" -> \"%s\" [style=dotted];\n" 
					(JDumpBasics.class_name (snd cn))
					(JDumpBasics.class_name (snd iname)))
			   cl.c_interfaces
		 | `Interface i ->
			 Printf.fprintf out "   \"%s\" [shape=rectangle];" (JDumpBasics.class_name (snd cn));
			 ClassMap.iter
			   (fun iname _ ->
				  Printf.fprintf out "   \"%s\" -> \"%s\" [style=dotted];\n" 
					(JDumpBasics.class_name i.i_name)
					(JDumpBasics.class_name (snd iname)))
			   i.i_interfaces;
			 Printf.fprintf out "   \"%s\" -> \"%s\";\n" 
			   (JDumpBasics.class_name i.i_name)
			   (JDumpBasics.class_name i.i_super.c_name)
	)
	program;
  Printf.fprintf out "}";
  close_out out

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
  let program = ref ClassMap.empty in
  (* the workset of pending method to be explored *)
  let workset = ref ClassMethSet.empty
  (* we memorize the virtual invoke instructions (invokevirtual and invokeinterface)
	 of the methods that are already parsed *)
  and memorize_virtual_calls = ref ClassMap.empty 
  (* we build the call graph *)
  and call_graph = ref ClassMethMap.empty in
	(* we initialise teh workset with all methods in [init_workset] *)
	List.iter
	  (fun (cn,ms) ->
		 let cn' = get_cn_index cn in
		 let ms' = get_ms_index ms in
		 let _ = load class_path program cn' in
		   workset := ClassMethSet.add (cn',ms') !workset)
	  init_workset;
	(* sub-function to add call [(cn0,ms0) --> (cl,ms)] *)
	let add_call cn0 ms0 cl ms' =
 	  let (cl',meth) = resolve_method cl ms' in
	  let cn' = (cl'.c_index,cl'.c_name) in
		if (not (has_been_parsed meth)) then
		  workset := ClassMethSet.add (cn',ms') !workset;
		(* add_call_graph (cn0,ms0) (cn,ms) call_graph *)
	in
	while (not (ClassMethSet.is_empty !workset)) do
	  (* we choose a method and remove it from the workset *)
	  let (cn,meth) = choose_meth_class_in_workset workset !program in
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
					   let cn' = get_cn_index cn' in
						 add_clinits class_path cn meth cn' call_graph workset program
				   | OpNew cn' -> 
					   let cn' = get_cn_index cn' in
					   let c_or_i = load class_path program cn' in
						 begin
						   (* we need to explore [cn'.<clinit>] *)
						   add_clinits class_path cn meth cn' call_graph workset program;
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
												   add_call cn_caller ms_caller c1 ms_callee)
												(ClassMap.find (c2.c_index,c2.c_name) !memorize_virtual_calls)
											with Not_found -> ()
											end
										| `Interface i -> 
											begin try
											  List.iter
												(fun (cn_caller,ms_caller,ms_callee) -> 
												   add_call cn_caller ms_caller c1 ms_callee)
												(ClassMap.find (i.i_index,i.i_name) !memorize_virtual_calls)
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
					   let cn' = get_cn_index cn' in
					   let ms = get_ms_index ms in
					   begin
						 match load class_path program cn' with
						   | `Interface _ -> failwith "invokevirtual on interface"
						   | `Class cl' -> 
							   forall_class_sons
								 (fun cl -> if cl.c_may_be_instanciated then add_call cn meth.cm_index cl ms)
								 cl'
					   end;
					   let calls = 
						   try ClassMap.find cn' !memorize_virtual_calls 
						   with Not_found -> [] in
						 memorize_virtual_calls := ClassMap.add cn' ((cn,meth.cm_index,ms)::calls) !memorize_virtual_calls
				   | OpInvoke (`Interface cn',ms) -> 
					   let cn' = get_cn_index cn' in
					   let ms = get_ms_index ms in
					   begin
						 match load class_path program cn' with
						   | `Class _ -> failwith "invokestatic on class"
						   | `Interface i -> 
							   forall_interface_sons
								 (fun cl -> if cl.c_may_be_instanciated then add_call cn meth.cm_index cl ms)
								 i
					   end;
					   let calls = 
						   try ClassMap.find cn' !memorize_virtual_calls 
						   with Not_found -> [] in
						 memorize_virtual_calls := ClassMap.add cn' ((cn,meth.cm_index,ms)::calls) !memorize_virtual_calls
				   | OpInvoke (`Static cn',ms) -> 
					   let cn' = get_cn_index cn' in
					   let ms = get_ms_index ms in
					   begin
						 match load class_path program cn' with
						   | `Interface _ -> failwith "invokestatic on interface"
						   | `Class cl' -> 
						   	   add_call cn meth.cm_index cl' ms
					   end;
					   add_clinits class_path cn meth cn' call_graph workset program
				   | OpInvoke (`Special cn',ms) -> 
					   let cn' = get_cn_index cn' in
					   let ms = get_ms_index ms in
					   begin
						 match load class_path program cn' with
						   | `Interface _ -> failwith "invokespecial on interface"
						   | `Class cl' -> 
							   if (snd ms).ms_name = "<init>" then
								 add_call cn meth.cm_index cl' ms
							   else
								 let (cl',_) = resolve_method cl' ms in
								 match ClassMap.find cn !program with
								   | `Interface _ -> failwith "invokespecial in an interface"
								   | `Class cl ->
									   begin
										   match cl.c_super_class with
											 | None -> ()
											 | Some cl_super ->
												 if subclass_test cl_super cl' then
												   add_call cn meth.cm_signature cl_super ms
												 else add_call cn meth.cm_signature cl' ms
									   end
					   end					   
				   | _ -> ()
				) (Lazy.force c).c_code
	done;
	  (!program,!call_graph)

let main_signature : JClass.method_signature =
  {   ms_name = "main";
	  ms_parameters = [TObject (TArray (TObject (TClass ["java";"lang";"String"])))];
	  ms_return_type = None
  }

let initializeSystemClass : class_name * method_signature = 
  (["java";"lang";"System"],
   {ms_name = "initializeSystemClass";
	ms_parameters = [];
	ms_return_type = None})

(** [classname] is the entry class name *)
let start_rta class_path classname =  
  let class_path = JFile.class_path class_path in
  let to_be_explored = 
	[(classname,main_signature);initializeSystemClass] in
  let (program,call_graph) = rta class_path to_be_explored in
	JFile.close_class_path class_path;
	Printf.printf "%d classes parsed\n"
	  (ClassMap.fold (fun _ _ n -> n+1) program 0)

