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
  
module Opcodes =
struct
  type rta_opcode =
      New of class_name_index | GetStatic of class_name_index
    | PutStatic of class_name_index
    | InvokeVirtual of class_name_index * method_signature_index
    | InvokeSpecial of class_name_index * method_signature_index
    | InvokeStatic of class_name_index * method_signature_index
    | InvokeInterface of class_name_index * method_signature_index
    | Nop
	
  type implementation_cache = { mutable usable : bool;
				mutable oplist : rta_opcode list;
				mutable oplines : int list;
				v_calls : ClassMethSet.t ref;
				impl : implementation ref;
				dic : dictionary ref }
      
  let new_cache impl dic v_calls = { oplist = []; oplines = [];
				     usable = false; impl = impl;
				     v_calls = v_calls;
				     dic = dic }
  let op2rtaop op cache =
    let dic = !(cache.dic) in
      match op with
	| OpNew cn -> New (dic.get_cn_index cn)
	| OpGetStatic (cn,_) -> GetStatic (dic.get_cn_index cn)
	| OpPutStatic (cn,_) -> PutStatic (dic.get_cn_index cn)
	| OpInvoke (`Virtual t, ms) ->
	    let cni = match t with 
	      | TClass cn -> dic.get_cn_index cn
	      | TArray _ -> (* should only happen with [clone()] *)
		  java_lang_object_index
	    and msi = dic.get_ms_index ms in
	      InvokeVirtual(cni,msi)
	| OpInvoke(`Special cn, ms) ->
	    let cni = dic.get_cn_index cn
	    and msi = dic.get_ms_index ms in
	      InvokeSpecial(cni,msi)
	| OpInvoke(`Static cn, ms) ->
	    let cni = dic.get_cn_index cn
	    and msi = dic.get_ms_index ms in
	      InvokeStatic(cni,msi)
	| OpInvoke(`Interface cn, ms) ->
	    let cni = dic.get_cn_index cn
	    and msi = dic.get_ms_index ms in
	      InvokeInterface(cni,msi)
	| _ -> Nop
	    
  let iteri f cache =
    if ( cache.usable ) then
      List.iter2 f cache.oplines cache.oplist
    else
      let new_ops = ref []
      and new_ops_lines = ref [] in
	(match !(cache.impl) with 
	   | Native -> ()
	   | Java c ->
	       Array.iteri
		 (fun i op ->
		    let rta_op = op2rtaop op cache in
		      match rta_op with
			| Nop -> ()
			| New _ | GetStatic _ | PutStatic _
			| InvokeStatic (_,_) | InvokeSpecial (_,_) ->
			    (* In rta algorithm, New, GetStatic, PutStatic,
			       InvokeStatic and InvokeSpecial should be treated
			       only one time, so we don't add it to the cache *)
			    f i rta_op
			| InvokeVirtual (cni,msi) | InvokeInterface (cni,msi) ->
			    (if not( ClassMethSet.mem (cni,msi)
				       !(cache.v_calls) ) then
			       (* we only add the instructions invoke_virtual or
				  invoke_interface that have never been found in
				  a former parsed method *)
			       (new_ops := rta_op :: !new_ops;
				new_ops_lines := i :: !new_ops_lines;
				cache.v_calls := ClassMethSet.add (cni,msi)
				  !(cache.v_calls)
			       )
			    );
			    f i rta_op
		 )
		 (Lazy.force c).c_code);
	cache.usable <- true;
	cache.oplist <- !new_ops;
	cache.oplines <- !new_ops_lines
	  
  let iter f cache =
    iteri (fun _ x -> f x) cache
end

module Dllist =
struct
  exception NilNode
  exception HeadNode
  exception TailNode
  exception NoHeadNode
  exception NoTailNode
  exception NoHeadNode
  exception CellNotFound
    
  type 'a link = 'a cellule ref
  and 'a content = Content of 'a | Head | Tail
  and 'a cellule = { mutable prev : 'a link;
		     content : 'a content;
		     mutable next : 'a link }
  and 'a dllist = 'a cellule ref

  let create () =
    let rec head = ref { prev = tail; content = Head; next = tail }
    and tail = ref { prev = head; content = Tail; next = head }
    in head
  let get (l:'a dllist) =
    match (!l).content with
      | Content c -> c
      | Head -> raise HeadNode
      | Tail -> raise TailNode
  let next (l:'a dllist) : 'a dllist = (!l).next
  let prev (l:'a dllist) : 'a dllist = (!l).prev
  let tail (l:'a dllist) : 'a dllist =
    match (!l).content with
      | Head -> (!l).prev
      | _ -> raise NoHeadNode

  let add (e:'a) (l:'a dllist) =
    match (!l).content with
      | Head ->
	  let new_elm = ref { prev = l;
			      content = Content e;
			      next = (!l).next } in
	  let cell = (!new_elm).next in
	    (!cell).prev <- new_elm;
	    (!l).next <- new_elm
      | _ -> raise NoHeadNode

  let del (l:'a dllist) =
    match (!l).content with
      | Head -> raise HeadNode
      | Tail -> raise TailNode
      | _ ->
	  (!((!l).next)).prev <- (!l).prev;
	  (!((!l).prev)).next <- (!l).next

  let rec mem (e:'a) (l:'a dllist) =
    match (!l).content with
      | Head ->
	  let cell = (!l).next in
	    mem e cell
      | Tail -> false
      | Content c ->
	  if (c = e) then
	    true
	  else let cell = (!l).next in
	    mem e cell
	    
  let add_ifn e l =
    if not( mem e l ) then add e l

  let rec size ?(s=0) (l:'a dllist) =
    match (!l).content with
      | Head ->
	  let cell = (!l).next in
	    size ~s:0 cell
      | Tail -> s
      | Content _ ->
	  let cell = (!l).next in
	    size ~s:(s+1) cell
	    
  let rec iter (f:'a -> unit) (l:'a dllist) =
    match (!l).content with
      | Head ->
	  let cell = (!l).next in
	    iter f cell
      | Tail -> ()
      | Content c ->
	  f c;
	  let cell = (!l).next in
	       iter f cell

  let rec iter_until_cell (f:'a -> unit) (bound:'a dllist) (l:'a dllist) =
    match (!l).content with
      | Head ->
	  let cell = (!l).next in
	    iter_until_cell f bound cell
      | Tail -> raise CellNotFound
      | Content c ->
	  if not( bound == l) then
	    (f c;
	     let cell = (!l).next in
	       iter_until_cell f bound cell)

  let rec iter_to_head_i (f:'a dllist -> 'a -> unit) (l:'a dllist) =
    match (!l).content with
      | Head -> ()
      | Tail ->
	  let cell = (!l).prev in
	    iter_to_head_i f cell
      | Content c ->
	  f l c;
	  let cell = (!l).prev in
	    iter_to_head_i f cell

  let iter_to_head (f:'a -> unit) (l:'a dllist) =
    iter_to_head_i (fun _ x -> f x) l 

  let map (f:'a -> 'b) (l:'a dllist) =
    let tail = tail l
    and lm = ref [] in
      iter_to_head (fun x -> lm := (f x) :: !lm) tail;
      !lm
end

module Program =
struct
  type class_info =
      { class_data : JClass.interface_or_class ref;
	mutable is_instantiated : bool;
	mutable children : class_name_index list;
	instantiated_subclasses : int Dllist.dllist option;
	supers : class_name_index list;
	implemented_interfaces : class_name_index list option;
	mutable resolved_methods : (class_name_index *
				      method_signature_index) MethodMap.t;
	(* used for compatibility with JProgram *)
	methods_data : methods_info }
  and method_info = JProgram.jmethod * Opcodes.implementation_cache option
  and methods_info = method_info MethodMap.t (* TODO : useless ? *)
      
  type callgraph_info =
      { mutable tested_new_instances : int Dllist.dllist;
	mutable lookup : ClassMethSet.t
      }
  type program_cache =
      { mutable classes : class_info ClassMap.t;
	mutable interfaces : class_name_index list ClassMap.t;
	mutable callgraph : callgraph_info ClassMethMap.t;
	dic : dictionary ref;
	workset : ((class_name_index * method_signature_index) *
		     (Opcodes.implementation_cache option ref)) Dllist.dllist;
	v_calls : ClassMethSet.t ref;
	mutable finished : bool;
	classpath : JFile.class_path }
      
  exception Method_not_found
    
  let methodmap2methodinfo p mm =
    let dic = !(p.dic) in
    let meth_info = ref MethodMap.empty in
      JClass.MethodMap.iter
	(fun ms m ->
	   let cache = match m with
	     | JClass.ConcreteMethod cm ->
		 (ConcreteMethod(ccm2pcm dic cm),
		  Some (Opcodes.new_cache
			  (ref cm.JClass.cm_implementation)
			  (ref dic) p.v_calls))
	     | JClass.AbstractMethod am ->
		 (AbstractMethod(cam2pam dic am), None) in
	     meth_info := MethodMap.add (dic.get_ms_index ms)
	       cache !meth_info) mm;
      !meth_info
	
  let abstract_methodmap2methodinfo p mm i_initializer =
    let dic = !(p.dic) in
    let meth_info = ref MethodMap.empty in
      (match i_initializer with
	 | None -> ()
	 | Some cm ->
	     let cache =
	       (ConcreteMethod(ccm2pcm dic cm),
		Some (Opcodes.new_cache
			(ref cm.JClass.cm_implementation)
			(ref dic) p.v_calls)) in
	       meth_info := MethodMap.add clinit_index cache !meth_info
      );
      JClass.MethodMap.iter
	(fun ms am ->
	   meth_info := MethodMap.add (dic.get_ms_index ms)
	     (AbstractMethod(cam2pam dic am), None) !meth_info) mm;
      !meth_info
	
  let rec get_class_info p cni =
    try
      ClassMap.find cni p.classes
    with
      | Not_found ->
	  let dic = !(p.dic) in
	  let cn = dic.retrieve_cn cni in
	    add_class p cn;
	    try
	      ClassMap.find cni p.classes
	    with _ ->
	      failwith ("Can't load class or interface "
			^ (JDumpBasics.class_name cn))
  and add_class p cn =
    (* We assume that a call to add_class is done only when a class has never *)
    (* been loaded in the program. Loading a class implies loading all its *)
    (* superclasses recursively. *)
    let dic = !(p.dic) in
    let ioc = JFile.get_class p.classpath (String.concat "." cn) in
    let ioc_index = (dic.get_cn_index (JClass.get_name ioc)) in
      match ioc with
	| `Class c ->
	    let supers =
	      (match c.JClass.c_super_class with
		 | None -> []
		 | Some sc ->
		     let sc_index = (dic.get_cn_index sc) in
		     let sc_info = get_class_info p sc_index in
		       sc_info.children <- ioc_index :: sc_info.children;
		       sc_index :: sc_info.supers)
	    and implemented_interfaces =
	      List.map (fun iname -> 
			  let i = dic.get_cn_index iname in
			    (* We do this to load the implemented interfaces and
			       all their super interfaces *)
		       	    ignore (get_class_info p i);
			    i
		       ) c.JClass.c_interfaces in
	      
	      (* For each implemented interface we add cni in the program
		 interfaces map *)
	      List.iter
		(fun i ->
		   if (ClassMap.mem i p.interfaces ) then
		     p.interfaces <- ClassMap.add i
		       (ioc_index ::(ClassMap.find i p.interfaces)) p.interfaces
		   else
		     p.interfaces <- ClassMap.add i [ioc_index] p.interfaces
		) implemented_interfaces;

	      let ioc_info =
		{ class_data = ref ioc;
		  is_instantiated = false;
		  children = [];
		  instantiated_subclasses = Some (Dllist.create());
		  supers = supers;
		  implemented_interfaces = Some (implemented_interfaces);
		  resolved_methods = MethodMap.empty;
		  methods_data = methodmap2methodinfo p c.JClass.c_methods }
	      in
		p.classes <- ClassMap.add ioc_index ioc_info p.classes;
		
		(* Now we add the clinit method to the workset *)
		(if ( MethodMap.mem clinit_index ioc_info.methods_data ) then
		   add_to_workset p (ioc_index,clinit_index))
	| `Interface i ->
	    List.iter
	      (* An interface should know which interfaces extend it in order
		 to achieve the lookup_interface *)
	      (fun iname ->
		 let si_index = (dic.get_cn_index iname) in
		 let si_info = get_class_info p si_index in
		   si_info.children <- ioc_index :: si_info.children)
	      i.JClass.i_interfaces;
	    let ioc_info =
	      { class_data = ref ioc;
		is_instantiated = false;
		(* An interface will never be instantiated *)
		children = [];
		instantiated_subclasses = None;
		supers = [];
		(* An interface don't need to know which super interfaces
		   it extends *)
		implemented_interfaces = None;
		resolved_methods = MethodMap.empty;
		methods_data = abstract_methodmap2methodinfo p
		  i.JClass.i_methods i.JClass.i_initializer }
	    in
	      p.classes <- ClassMap.add ioc_index ioc_info p.classes;
	      (* Now we add the clinit method to the workset *)
	      (if ( MethodMap.mem clinit_index ioc_info.methods_data ) then
		 add_to_workset p (ioc_index,clinit_index))
		
  and get_method_info p cni msi : method_info =
    let cl_info = get_class_info p cni in
      try
	let meth_impl = MethodMap.find msi cl_info.methods_data in
	  meth_impl
      with
	| Not_found -> raise Method_not_found
	    
  and make_workset_item p (cni,msi) =
    ((cni,msi), ref (snd (get_method_info p cni msi)))

  and add_to_workset p (cni,msi) =
    Dllist.add (make_workset_item p (cni,msi)) p.workset;
    match (fst (get_method_info p cni msi)) with
      | ConcreteMethod cm ->
	  cm.cm_has_been_parsed <- true
      | AbstractMethod _ -> ()

  let get_instantiated_subclasses c_info =
    match c_info.instantiated_subclasses with
      | None -> failwith ("Can't instanciate an interface ")
      | Some subclasses -> subclasses
    
  let rec add_instantiated_class p cni =
    let cl_info = get_class_info p cni in
      if not( cl_info.is_instantiated ) then
	(cl_info.is_instantiated <- true;
	 p.finished <- false;
	 (* We complete the list of instantiated subclasses for cn and
	    its superclasses *)
	 List.iter
	   (fun scni ->
	      let scl_info = get_class_info p scni in
		Dllist.add cni (get_instantiated_subclasses scl_info)
	   ) (cni :: cl_info.supers)
	)
	  
  (* Method resolution *)
  let rec resolve_method p cni msi =
    let class_info = get_class_info p cni in
      try
	MethodMap.find msi class_info.resolved_methods
      with _ ->
	let supers = class_info.supers in
	  try
	    let meth_info = get_method_info p cni msi in
	      match (snd meth_info) with
		| None ->
		    failwith ("what to do when a resolve operation lead to an abstract method ???")
		| Some _ ->
		    class_info.resolved_methods <-
		      MethodMap.add msi (cni,msi) class_info.resolved_methods;
		    (cni,msi)
	  with Method_not_found ->
	    try
	      resolve_method p (List.hd supers) msi
	    with _ ->
	      failwith ("Failing resolving (" ^ (string_of_int cni) ^ ","
			^ (string_of_int msi) ^ ")")

  let update_lookup_set p msi callsites cni =
    if ( callsites = ClassMethSet.empty ) then
      ClassMethSet.empty
    else
      try
	let call_info = ClassMethMap.find (cni,msi) p.callgraph in
	let new_callsites =
	  ClassMethSet.diff callsites call_info.lookup in
	  if not( new_callsites = ClassMethSet.empty ) then
	    (call_info.lookup <-
	       ClassMethSet.union new_callsites call_info.lookup;
	     new_callsites)
	  else ClassMethSet.empty
      with Not_found ->
	(* we add the information in the callgraph even if (cni,msi) is 
	   never invoked in the program *)
	let class_info = get_class_info p cni in
	let tested_new_instances =
	  Dllist.tail (get_instantiated_subclasses class_info) in
	  p.callgraph <- ClassMethMap.add (cni,msi)
	    { tested_new_instances = tested_new_instances;
	      lookup = callsites } p.callgraph;
	  callsites

  let invoke_virtual_lookup p cni msi =
    let class_info = get_class_info p cni in
      (try
	 let call_info = ClassMethMap.find (cni,msi) p.callgraph in
	 let resolved_methods = ref ClassMethSet.empty in
	   Dllist.iter_to_head
	     (fun new_cni ->
		resolved_methods := ClassMethSet.add
		  (resolve_method p new_cni msi) !resolved_methods)
	     call_info.tested_new_instances;
	   (* we update the tested new instances *)
	   call_info.tested_new_instances <-
	     Dllist.next (get_instantiated_subclasses class_info);
	   let new_callsites =
	     ClassMethSet.diff !resolved_methods call_info.lookup in
	     if not( new_callsites = ClassMethSet.empty ) then
	       (call_info.lookup <-
		  ClassMethSet.union new_callsites call_info.lookup;
		let new_callsites =
		  List.fold_left (update_lookup_set p msi)
		    new_callsites class_info.supers in
		  (* we add the new call sites to the workset *)
		  ClassMethSet.iter
		    (fun x -> add_to_workset p x) new_callsites
	       )
       with Not_found ->
	 let resolved_methods = ref ClassMethSet.empty
	 and tested_new_instances =
	   Dllist.next (get_instantiated_subclasses class_info) in
	   Dllist.iter
	     (fun new_cni ->
		resolved_methods := ClassMethSet.add
		  (resolve_method p new_cni msi) !resolved_methods)
	     (get_instantiated_subclasses class_info);
	   p.callgraph <- ClassMethMap.add (cni,msi)
	     { tested_new_instances = tested_new_instances;
	       lookup = !resolved_methods } p.callgraph;
	   if not( !resolved_methods = ClassMethSet.empty ) then
	     (let new_callsites =
		List.fold_left (update_lookup_set p msi)
		  !resolved_methods class_info.supers in
		(* we add the new call sites to the workset *)
		ClassMethSet.iter
		  (fun x -> add_to_workset p x) new_callsites
	     )
      )

  let rec invoke_interface_lookup p cni msi =
    let i_info = get_class_info p cni in
    let i_children = i_info.children in
      if ( ClassMap.mem cni p.interfaces ) then
	(List.iter
	   (fun x -> invoke_virtual_lookup p x msi)
	   (ClassMap.find cni p.interfaces));
      List.iter
	(fun x -> invoke_interface_lookup p x msi)
	i_children


  let rec invoke_special_lookup p current_class_index cni msi =
    let current_class_info = get_class_info p current_class_index in
    let (rcni,msi) = resolve_method p cni msi in
      if ( msi = init_index
	  || not(List.mem rcni current_class_info.supers) ) then
	(let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
	   ignore(update_lookup_set p msi s cni);
	   (* we add (cni,msi) to the workset *)
	   add_to_workset p (rcni,msi)
	)
      else
	let (rcni,msi) = resolve_method p
	  (List.hd current_class_info.supers) msi in
	  (let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
	     ignore(update_lookup_set p msi s cni);
	     (* we add (cni,msi) to the workset *)
	     add_to_workset p (rcni,msi)
	  )
	  
  let rec invoke_static_lookup p cni msi =
    let class_info = get_class_info p cni in
    let tested_new_instances =
      Dllist.tail (get_instantiated_subclasses class_info) in
    let (rcni,msi) = resolve_method p cni msi in
      (if not( ClassMethMap.mem (cni,msi) p.callgraph ) then
       	 let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
       	   p.callgraph <-
       	     ClassMethMap.add (cni,msi)
       	     { tested_new_instances = tested_new_instances;
       	       lookup = s} p.callgraph;
	   add_to_workset p (rcni,msi)
      )
	
  let parse_instruction p current_class_index (op:Opcodes.rta_opcode) =
    match op with
      | Opcodes.New cni ->
	  add_instantiated_class p cni
      | Opcodes.GetStatic cni
      | Opcodes.PutStatic cni ->
	  ignore(get_class_info p cni)
      | Opcodes.InvokeVirtual (cni,msi) ->
	  invoke_virtual_lookup p cni msi
      | Opcodes.InvokeInterface (cni,msi) ->
	  invoke_interface_lookup p cni msi
      | Opcodes.InvokeSpecial (cni,msi) ->
      	  invoke_special_lookup p current_class_index cni msi
      | Opcodes.InvokeStatic (cni,msi) ->
      	  invoke_static_lookup p cni msi
      | _ -> ()

  let iter_workset p =
    let tail = Dllist.tail p.workset in
      Dllist.iter_to_head_i
	(fun e ((cni,msi),cache) ->
	   match !cache with
	     | None ->
		 failwith ("Impossible to find an implementation of method "
			   ^ (string_of_int msi) ^ "in class "
			   ^ (string_of_int cni))
	     | Some impl ->
		 if ( impl.Opcodes.usable
		      && impl.Opcodes.oplist = [] ) then
		   (* we delete the elements of the workset that have no
		      new invoke_virtual instructions *)
		   Dllist.del e
		 else
		   Opcodes.iter (parse_instruction p cni) impl
	) tail

  let new_program_cache debug cn classpath =
    let dic = make_dictionary () in
    let v_calls = ref ClassMethSet.empty in
    let cni = dic.get_cn_index cn
    and initializeSystemClass : class_name * method_signature = 
      (["java";"lang";"System"],
       {ms_name = "initializeSystemClass";
	ms_parameters = [];
	ms_return_type = None})
    and workset = Dllist.create () in
    let p =
      { classes = ClassMap.empty; interfaces = ClassMap.empty;
	callgraph = ClassMethMap.empty; dic = ref dic;
	workset = workset; v_calls = v_calls;
	finished = false; classpath = classpath } in
    let class_info = get_class_info p cni in
      if not( MethodMap.mem main_index class_info.methods_data) then
	failwith ("No main method found in class "
		  ^ (JDumpBasics.class_name cn));

      (* The main class MUST be the first to enter the dictionary
	 (after java.lang.Object) *)
      add_to_workset p (cni,main_index);
      (if not( debug ) then
	 add_to_workset p
	   (dic.get_cn_index (fst initializeSystemClass),
      	    dic.get_ms_index (snd initializeSystemClass))
      );
      p
	
  let parse_program ?(debug = false) classpath cn =
    let classpath = JFile.class_path classpath in
    let p = new_program_cache debug cn classpath in
      while not( p.finished ) do
	p.finished <- true;
	iter_workset p
      done;
      JFile.close_class_path classpath;
      p

  let parse_program_bench ?(debug = false) classpath cn =
    let time_start = Sys.time() in
    let p = parse_program ~debug classpath cn in
    let s = Dllist.size p.workset in
      Printf.printf "Workset of size %d\n" s;
      let time_stop = Sys.time() in
	Printf.printf "program parsed in %fs.\n" (time_stop-.time_start);
	Printf.printf "%d classes and %d methods parsed.\n"
	  (!(p.dic).cni_table.cni_next) (!(p.dic).msi_table.msi_next)

end

let retrieve_invoke_index p_cache op =
  let dic = !(p_cache.Program.dic) in
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

let static_lookup p_cache cni msi pp =
  let m = fst (Program.get_method_info p_cache cni msi) in
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
			     let x = retrieve_invoke_index p_cache op in
			       (try
				  (ClassMethMap.find x
				     p_cache.Program.callgraph).Program.lookup
				with _ ->
				  raise Not_found
			       )
			 | _ ->
			     failwith "Invalid opcode found at specified program point"
		   with _ ->
		     failwith "Invalid program point")


module JProgramConverter =
struct
  let rec get_class_file p ioc_index class_file_map =
    try
      ClassMap.find ioc_index !class_file_map
    with _ ->
      let class_file =  ioc2iocfile p ioc_index class_file_map in
	class_file_map := ClassMap.add ioc_index class_file !class_file_map;
	class_file
  and to_class_file ioc =
    match ioc with
      | `Class c -> c
      | `Interface _ -> failwith "to_class_file applied on interface !"
  and to_interface_file ioc =
    match ioc with
      | `Class _ -> failwith "to_interface_file applied on class !"
      | `Interface i -> i

  and ioc2iocfile p ioc_index class_file_map =
    let dic = !(p.Program.dic) in
    let ioc_info = Program.get_class_info p ioc_index in
    let ioc = !(ioc_info.Program.class_data)
    and methods = MethodMap.map (fun x -> fst x) ioc_info.Program.methods_data in
      match ioc with
	| `Class c ->
	    let c_struct =
	      {c_index = ioc_index;
	       c_name = c.JClass.c_name; (* TODO: why not reuse the name stored in the dictionary ? (save memory) *)
	       c_version = c.JClass.c_version;
	       c_access = c.JClass.c_access;
	       c_generic_signature = c.JClass.c_generic_signature;
	       c_final = c.JClass.c_final;
	       c_abstract = c.JClass.c_abstract;
	       c_synthetic = c.JClass.c_synthetic;
	       c_enum = c.JClass.c_enum;
	       c_other_flags = c.JClass.c_other_flags;
	       c_super_class =
		  (match c.JClass.c_super_class with
		     | None -> None
		     | Some cn ->
			let cni = dic.get_cn_index cn in
			  Some(to_class_file
				 (get_class_file p cni class_file_map)));
	       c_consts = c.JClass.c_consts;
	       c_interfaces =
		  (let c_interfaces = ref ClassMap.empty in
		     List.iter
		       (fun x ->
			  let i_index = dic.get_cn_index x in
			    c_interfaces := ClassMap.add i_index
			      (to_interface_file
				 (get_class_file p i_index class_file_map))
			      !c_interfaces)
		       c.JClass.c_interfaces;
		     !c_interfaces);
	       c_sourcefile = c.JClass.c_sourcefile;
	       c_deprecated = c.JClass.c_deprecated;
	       c_enclosing_method = c.JClass.c_enclosing_method;
	       c_source_debug_extention =c.JClass.c_source_debug_extention;
	       c_inner_classes = c.JClass.c_inner_classes;
	       c_other_attributes = c.JClass.c_other_attributes;
	       c_fields = c.JClass.c_fields;
	       c_methods = methods;
	       c_resolve_methods = MethodMap.empty;
	       c_may_be_instanciated = ioc_info.Program.is_instantiated;
	       c_children = ClassMap.empty} in

	      class_file_map := ClassMap.add ioc_index (`Class c_struct)
		!class_file_map;
	      c_struct.c_resolve_methods <-
		(MethodMap.map
		   (fun (cni,msi) ->
		      let m = fst (Program.get_method_info p cni msi) in
			(to_class_file (get_class_file p cni class_file_map),
			 m)
		   )
		   ioc_info.Program.resolved_methods
		);
	      (List.iter
		 (fun cni ->
		    c_struct.c_children <-
		      ClassMap.add cni (to_class_file
				       (get_class_file p cni class_file_map))
		      c_struct.c_children
		 )
		 ioc_info.Program.children
	      );
	      `Class c_struct
	| `Interface i ->
	    let i_struct =
	      {i_index = ioc_index;
	       i_name = i.JClass.i_name; (* TODO: why not reuse the name stored in the dictionary ? (save memory) *)
	       i_version = i.JClass.i_version;
	       i_access = i.JClass.i_access;
	       i_generic_signature = i.JClass.i_generic_signature;
	       i_consts = i.JClass.i_consts;
	       i_annotation = i.JClass.i_annotation;
	       i_other_flags = i.JClass.i_other_flags;
	       i_interfaces =
		  (let i_interfaces = ref ClassMap.empty in
		     List.iter
		       (fun x ->
			  let i_index = dic.get_cn_index x in
			    i_interfaces := ClassMap.add i_index
			      (to_interface_file
				 (get_class_file p i_index class_file_map))
			      !i_interfaces)
		       i.JClass.i_interfaces;
		     !i_interfaces);
	       i_sourcefile = i.JClass.i_sourcefile;
	       i_deprecated = i.JClass.i_deprecated;
	       i_source_debug_extention = i.JClass.i_source_debug_extention;
	       i_inner_classes = i.JClass.i_inner_classes;
	       i_other_attributes = i.JClass.i_other_attributes;
	       i_children_interface = ClassMap.empty;
	       i_children_class = ClassMap.empty;
	       i_super =
		  (let object_file = get_class_file p java_lang_object_index
		     class_file_map in
		     match object_file with
		       | `Class c -> c
		       | `Interface _ ->
			   failwith "java.lang.object is an interface !");
	       i_initializer =
		  (try
		     let clinit_method = MethodMap.find clinit_index methods in
		       match clinit_method with
			 | ConcreteMethod cm -> Some cm
			 | _ -> None (* Impossible case *)
		   with
		       _ -> None);
	       i_fields = i.JClass.i_fields;
	       i_methods = 
		  (let i_methods = ref MethodMap.empty in
		     MethodMap.iter
		       (fun i m ->
			  match m with
			    | ConcreteMethod _ -> ()
			    | AbstractMethod am ->
				i_methods :=
				  MethodMap.add i am !i_methods
		       )
		       methods;
		     !i_methods
		  )
	      } in

	      class_file_map := ClassMap.add ioc_index (`Interface i_struct)
		!class_file_map;
	      (List.iter
		 (fun cni ->
		    i_struct.i_children_interface <-
		      ClassMap.add cni (to_interface_file
				       (get_class_file p cni class_file_map))
		      i_struct.i_children_interface
		 )
		 ioc_info.Program.children
	      );
	      (try
		 let l = ClassMap.find ioc_index p.Program.interfaces in
		   List.iter
		     (fun cni ->
			i_struct.i_children_class <-
			  ClassMap.add cni
			  (to_class_file
			     (get_class_file p cni class_file_map))
			  i_struct.i_children_class)
		     l
	       with _ -> ()
	      );
	      `Interface i_struct

  let pcache2jprogram p_cache =
    let class_file_map = ref ClassMap.empty in
      { classes =
	  ClassMap.mapi
	    (fun i _ -> get_class_file p_cache i class_file_map)
	    p_cache.Program.classes;
	static_lookup = static_lookup p_cache;
	dictionary = !(p_cache.Program.dic) }
end
	    
let parse_program ?(debug = false) classpath cn =
  let p_cache = (Program.parse_program ~debug classpath cn) in
    JProgramConverter.pcache2jprogram p_cache
      
let parse_program_bench ?(debug = false) classpath cn =
  let time_start = Sys.time() in
    ignore(parse_program ~debug classpath cn);
    let time_stop = Sys.time() in
      Printf.printf "program parsed in %fs.\n" (time_stop-.time_start)

let get_method_calls p cni msi workset work_done =
  if not( ClassMethSet.mem (cni,msi) !work_done ) then
    (let l = ref [] in
     let f_lookup = p.static_lookup in
     let dic = p.dictionary in
     let ioc = ClassMap.find cni p.classes in
     let m = get_method ioc msi in
       (match m with
	  | AbstractMethod _ ->
	      failwith "Can't parse Abstract methods"
	  | ConcreteMethod cm ->
	      (match cm.cm_implementation with
		 | Native -> failwith "Can't parse Native methods"
		 | Java code ->
		     let c = (Lazy.force code).c_code in
		       Array.iteri
			 (fun pp op ->
			    match op with
			      | OpInvoke _ ->
				  let callsites = (f_lookup cni msi pp) in
				  let new_reachable_callsites =
				    ClassMethSet.diff callsites !work_done in
				  let callsites_list =
				    ClassMethSet.elements callsites in
				  let new_reachable_callsites_list =
				    ClassMethSet.elements
				      new_reachable_callsites in
				    workset := !workset @
				      new_reachable_callsites_list;
				    work_done := ClassMethSet.union !work_done
				      new_reachable_callsites;
				    l := 
				      !l @ (List.map
					      (fun x -> (pp,x)) callsites_list)
			      | _ -> ()
			 ) c
	      )
       );
       List.map (fun (pp,(ccni,cmsi)) ->
		   ((dic.retrieve_cn cni,
		     dic.retrieve_ms msi,pp),
		    (dic.retrieve_cn ccni,
		     dic.retrieve_ms cmsi))) !l
    )
  else []

let get_callgraph p =
  let rec get_callgraph_aux p
      ?(workset = ref [(main_class_index, main_index)])
      ?(work_done = ref ClassMethSet.empty) callgraph =
    if not( !workset = [] ) then
      let (cni,msi) = List.hd !workset in
	workset := List.tl !workset;
	get_callgraph_aux p ~workset ~work_done
	  (callgraph @
	     (get_method_calls p cni msi workset work_done))
    else callgraph
  in
    get_callgraph_aux p []
