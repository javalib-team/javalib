(*
 * This file is part of JavaLib
 * Copyright (c)2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see 
 * <http://www.gnu.org/licenses/>.
 *)

(* TODO :
   - Pour le static_lookup : retourner un couple (class,method) plutôt
   qu'un couple d'entiers
*)


open JBasics
open JClass
open JProgram

module Dllist =
struct
  exception NilNode
  exception HeadNode
  exception TailNode
  exception NoHeadNode
  exception NoTailNode
  exception NoHeadNode
  exception CellNotFound

  type 'a link = 'a cellule
  and 'a content = Content of 'a | Head | Tail
  and 'a cellule = { mutable prev : 'a link;
		     content : 'a content;
		     mutable next : 'a link }
  and 'a dllist = 'a cellule

  let create () =
    let rec head = { prev = tail; content = Head; next = tail }
    and tail = { prev = head; content = Tail; next = head }
    in head
  let get (l:'a dllist) =
    match l.content with
      | Content c -> c
      | Head -> raise HeadNode
      | Tail -> raise TailNode
  let next (l:'a dllist) : 'a dllist = l.next
  let prev (l:'a dllist) : 'a dllist = l.prev
  let tail (l:'a dllist) : 'a dllist =
    match l.content with
      | Head -> l.prev
      | _ -> raise NoHeadNode

  let add (e:'a) (l:'a dllist) =
    match l.content with
      | Head ->
	  let new_elm = { prev = l;
			  content = Content e;
			  next = l.next } in
	  let cell = new_elm.next in
	    cell.prev <- new_elm;
	    l.next <- new_elm
      | _ -> raise NoHeadNode

  let del (l:'a dllist) =
    match l.content with
      | Head -> raise HeadNode
      | Tail -> raise TailNode
      | _ ->
	  l.next.prev <- l.prev;
	  l.prev.next <- l.next

  let rec mem (e:'a) (l:'a dllist) =
    match l.content with
      | Head ->
	  let cell = l.next in
	    mem e cell
      | Tail -> false
      | Content c ->
	  if (c = e) then
	    true
	  else let cell = l.next in
	    mem e cell

  let add_ifn e l =
    if not( mem e l ) then add e l

  let rec size ?(s=0) (l:'a dllist) =
    match l.content with
      | Head ->
	  let cell = l.next in
	    size ~s:0 cell
      | Tail -> s
      | Content _ ->
	  let cell = l.next in
	    size ~s:(s+1) cell

  let rec iter (f:'a -> unit) (l:'a dllist) =
    match l.content with
      | Head ->
	  let cell = l.next in
	    iter f cell
      | Tail -> ()
      | Content c ->
	  f c;
	  let cell = l.next in
	    iter f cell

  let rec iter_until_cell (f:'a -> unit) (bound:'a dllist) (l:'a dllist) =
    match l.content with
      | Head ->
	  let cell = l.next in
	    iter_until_cell f bound cell
      | Tail -> raise CellNotFound
      | Content c ->
	  if not( bound == l) then
	    (f c;
	     let cell = l.next in
	       iter_until_cell f bound cell)

  let rec iter_to_head_i (f:'a dllist -> 'a -> unit) (l:'a dllist) =
    match l.content with
      | Head -> ()
      | Tail ->
	  let cell = l.prev in
	    iter_to_head_i f cell
      | Content c ->
	  f l c;
	  let cell = l.prev in
	    iter_to_head_i f cell

  let iter_to_head (f:'a -> unit) (l:'a dllist) =
    iter_to_head_i (fun _ x -> f x) l

  let map (f:'a -> 'b) (l:'a dllist) =
    let tail = tail l
    and lm = ref [] in
      iter_to_head (fun x -> lm := (f x) :: !lm) tail;
      !lm
end

type ioc_field = [ `CField of class_field | `IField of interface_field ]

module Program =
struct
  type class_info =
      { class_data : JProgram.interface_or_class;
	mutable is_instantiated : bool;
	mutable instantiated_subclasses : ClassSet.t;
	super_classes : class_name_index list;
	super_interfaces : ClassSet.t;
	mutable memorized_virtual_calls : MethodSet.t;
	mutable memorized_interface_calls : MethodSet.t;
	methods : JProgram.jmethod MethodMap.t;
	fields : ioc_field FieldMap.t }
	
  type program_cache =
      { mutable classes : class_info ClassMap.t;
	(* for each interface, interfaces maps a list of classes
	   that implements this interface or one of its subinterfaces *)
	mutable interfaces : ClassSet.t ClassMap.t;
	mutable static_virtual_lookup : ClassMethSet.t ClassMethMap.t;
	mutable static_static_lookup : ClassMethSet.t ClassMethMap.t;
	mutable static_special_lookup : (ClassMethSet.t
					   ClassMethMap.t) ClassMap.t;
	(* the clinits fields contains a set of class indexes whose clinit
	   methods have already been added to the workset *)
	mutable clinits : ClassSet.t;
	dic : dictionary;
	workset : (class_name_index * JProgram.concrete_method) Dllist.dllist;
	classpath : JFile.class_path;
	mutable native_methods : ClassMethSet.t;
	parse_natives : bool;
	native_methods_info : JNativeStubs.t}

  exception Method_not_found

  let mmap2pmap p mm =
    let dic = p.dic in
    let meth_info = ref MethodMap.empty in
      JClass.MethodMap.iter
	(fun ms m ->
	   let meth = match m with
	     | JClass.ConcreteMethod cm ->
		 ConcreteMethod(ccm2pcm dic cm)
	     | JClass.AbstractMethod am ->
		 AbstractMethod(cam2pam dic am) in
	     meth_info := MethodMap.add (dic.get_ms_index ms)
	       meth !meth_info) mm;
      !meth_info
	
  let abstract_mmap2pmap p mm i_initializer =
    let dic = p.dic in
    let meth_info = ref MethodMap.empty in
      (match i_initializer with
	 | None -> ()
	 | Some cm ->
	     let meth =
	       ConcreteMethod(ccm2pcm dic cm) in
	       meth_info := MethodMap.add clinit_index meth !meth_info
      );
      JClass.MethodMap.iter
	(fun ms am ->
	   meth_info := MethodMap.add (dic.get_ms_index ms)
	     (AbstractMethod(cam2pam dic am)) !meth_info) mm;
      !meth_info

  let cfmap2iocfmap p fm =
    let imap = ref FieldMap.empty in
      JClass.FieldMap.iter
	(fun fs cf ->
	   let fsi = p.dic.get_fs_index fs in
	     imap := FieldMap.add fsi (`CField cf) !imap) fm;
      !imap
    
  let ifmap2iocfmap p fm =
    let imap = ref FieldMap.empty in
      JClass.FieldMap.iter
	(fun fs cf ->
	   let fsi = p.dic.get_fs_index fs in
	     imap := FieldMap.add fsi (`IField cf) !imap) fm;
      !imap

  let iocfmap2cfmap fmap =
    FieldMap.map
      (fun iocf ->
	 match iocf with
	   | `CField cf -> cf
	   | `IField _ ->
	       failwith "Trying to convert an interface field to a class field")
      fmap

  let iocfmap2ifmap fmap =
    FieldMap.map
      (fun iocf ->
	 match iocf with
	   | `CField _ ->
	       failwith "Trying to convert a class field to an interface field"
	   | `IField ifield -> ifield)
      fmap
      
  let rec to_class_file ioc =
    match ioc with
      | `Class c -> c
      | `Interface _ -> failwith "to_class_file applied on interface !"
  and to_interface_file ioc =
    match ioc with
      | `Class _ -> failwith "to_interface_file applied on class !"
      | `Interface i -> i

  and ioc2iocfile p ioc_index ioc methods fields =
    let dic = p.dic in
    let ioc_name = dic.retrieve_cn ioc_index in
      match ioc with
	| `Class c ->
	    `Class
	      { c_index = ioc_index;
		c_name = ioc_name;
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
				  (get_class_info p cni).class_data));
		c_consts = c.JClass.c_consts;
		c_interfaces =
		  (let c_interfaces = ref ClassMap.empty in
		     List.iter
		       (fun x ->
			  let i_index = dic.get_cn_index x in
			    c_interfaces := ClassMap.add i_index
			      (to_interface_file
				 (get_class_info p i_index).class_data)
			      !c_interfaces)
		       c.JClass.c_interfaces;
		     !c_interfaces);
		c_sourcefile = c.JClass.c_sourcefile;
		c_deprecated = c.JClass.c_deprecated;
		c_enclosing_method = c.JClass.c_enclosing_method;
		c_source_debug_extention =c.JClass.c_source_debug_extention;
		c_inner_classes = c.JClass.c_inner_classes;
		c_other_attributes = c.JClass.c_other_attributes;
		c_fields = iocfmap2cfmap fields;
		c_methods = methods;
		c_resolve_methods = MethodMap.empty;
		c_may_be_instantiated = false;
		c_children = ClassMap.empty }

	| `Interface i ->
	    `Interface
	      { i_index = ioc_index;
		i_name = ioc_name;
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
				 (get_class_info p i_index).class_data)
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
		  (let object_file =
		     (get_class_info p java_lang_object_index).class_data in
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
		i_fields = iocfmap2ifmap fields;
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
		  ) }

  and load_class p cni =
    if not( ClassMap.mem cni p.classes ) then
      let cn = p.dic.retrieve_cn cni in
	add_class p cn

  and get_class_info p cni =
    try
      ClassMap.find cni p.classes
    with
      | Not_found ->
	  let cn = p.dic.retrieve_cn cni in
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
    let dic = p.dic in
    let ioc = JFile.get_class p.classpath (String.concat "." cn) in
    let ioc_index = (dic.get_cn_index (JClass.get_name ioc)) in
      match ioc with
	| `Class c ->
	    let super_classes =
	      (match c.JClass.c_super_class with
		 | None -> []
		 | Some sc ->
		     let sc_index = (dic.get_cn_index sc) in
		     let sc_info = get_class_info p sc_index in
		       sc_index :: sc_info.super_classes)
	    and implemented_interfaces =
	      let s = ref ClassSet.empty in
		List.iter (fun iname ->
			     let i = dic.get_cn_index iname in
			       s := ClassSet.add i !s) c.JClass.c_interfaces;
		!s in

	    (* For each implemented interface and its super interfaces we add
	       cni in the program interfaces map *)
	    let super_implemented_interfaces =
	      (ClassSet.fold
		 (fun i_index s ->
		    let i_info = get_class_info p i_index in
		      ClassSet.add i_index
			(ClassSet.union s i_info.super_interfaces)
		 ) implemented_interfaces ClassSet.empty) in
	      ClassSet.iter
		(fun i ->
		   if ( ClassMap.mem i p.interfaces ) then
		     p.interfaces <- ClassMap.add i
		       (ClassSet.add ioc_index (ClassMap.find i p.interfaces))
		       p.interfaces
		   else
		     p.interfaces <- ClassMap.add i
		       (ClassSet.add ioc_index ClassSet.empty) p.interfaces
		) super_implemented_interfaces;
	      
	      let methods = mmap2pmap p c.JClass.c_methods in
	      let fields = cfmap2iocfmap p c.JClass.c_fields in
		
	      let ioc_info =
		{ class_data = ioc2iocfile p ioc_index ioc methods fields;
		  is_instantiated = false;
		  instantiated_subclasses = ClassSet.empty;
		  super_classes = super_classes;
		  (* for a class super_interfaces contains
		     the transitively implemented interfaces *)
		  super_interfaces = super_implemented_interfaces;
		  memorized_virtual_calls = MethodSet.empty;
		  memorized_interface_calls = MethodSet.empty;
		  methods = methods;
		  fields = fields }
	      in
	      let c = to_class_file ioc_info.class_data in
		ClassSet.iter
		  (fun i_index ->
		     let i = to_interface_file
		       ((get_class_info p i_index).class_data) in
		       i.i_children_class <-
			 ClassMap.add ioc_index c i.i_children_class
		  )
		  implemented_interfaces;
		List.iter
		  (fun sc_index ->
		     let sc = to_class_file
		       (get_class_info p sc_index).class_data in
		       sc.c_children <-
			 ClassMap.add ioc_index c sc.c_children
		  )
		  super_classes;
		p.classes <- ClassMap.add ioc_index ioc_info p.classes;
	| `Interface i ->
	    let super_interfaces =
	      let s = ref ClassSet.empty in
		List.iter (fun si ->
			     let si_index = dic.get_cn_index si in
			     let si_info = get_class_info p si_index in
			       s := ClassSet.add si_index !s;
			       s := ClassSet.union si_info.super_interfaces !s
			  ) i.JClass.i_interfaces;
		!s in

	    let methods = abstract_mmap2pmap p
	      i.JClass.i_methods i.JClass.i_initializer in
	    let fields = ifmap2iocfmap p i.JClass.i_fields in

	    let ioc_info =
	      { class_data = ioc2iocfile p ioc_index ioc methods fields;
		is_instantiated = false;
		(* An interface will never be instantiated *)
		instantiated_subclasses = ClassSet.empty;
		super_classes = [];
		super_interfaces = super_interfaces;
		memorized_virtual_calls = MethodSet.empty;
		memorized_interface_calls = MethodSet.empty;
		methods = methods;
		fields = fields }
	    in
	    let i = to_interface_file ioc_info.class_data in
	      ClassSet.iter
		(fun si_index ->
		   let si = to_interface_file
		     (get_class_info p si_index).class_data in
		     si.i_children_interface <-
		       ClassMap.add ioc_index i si.i_children_interface
		)
		super_interfaces;
	      p.classes <- ClassMap.add ioc_index ioc_info p.classes;

  and add_clinit p ioc_index =
    let ioc_info = get_class_info p ioc_index in
      if ( not(ClassSet.mem ioc_index p.clinits)
	   && MethodMap.mem clinit_index ioc_info.methods ) then
	(
	  add_to_workset p (ioc_index,clinit_index);
	  p.clinits <- ClassSet.add ioc_index p.clinits
	)

  and add_class_clinits p ioc_index =
    let ioc_info = get_class_info p ioc_index in
      List.iter
	(fun cni -> add_clinit p cni)
	(ioc_index :: ioc_info.super_classes)
	
  and get_method p cni msi =
    let cl_info = get_class_info p cni in
      try
	let meth = MethodMap.find msi cl_info.methods in
	  meth
      with
	| Not_found -> raise Method_not_found

  and make_workset_item p (cni,msi) =
    let m = get_method p cni msi in
      match m with
	| ConcreteMethod cm -> cm
	| AbstractMethod _ ->
	    failwith "Can't add an Abstract Method to the workset"
              
  and add_to_workset p (cni,msi) =
    let cm = make_workset_item p (cni,msi) in
      match cm.cm_implementation with
	| Native ->
	    if not(cm.cm_has_been_parsed) then
	      (cm.cm_has_been_parsed <- true;
               p.native_methods <- ClassMethSet.add (cni,msi) p.native_methods;
	       if (p.parse_natives) then Dllist.add (cni,cm) p.workset
	      )
	| Java _ ->
	    if not( cm.cm_has_been_parsed ) then
	      (cm.cm_has_been_parsed <- true;
	       Dllist.add (cni,cm) p.workset)

  let resolve_method p cni msi =
    let ioc = (get_class_info p cni).class_data in
      match ioc with
	| `Class c ->
	    let rc = JControlFlow.resolve_method' msi c in
	      rc.c_index
	| `Interface _ -> failwith "Can't resolve an Interface Method"

  let resolve_field p cni fsi =
    let ioc = (get_class_info p cni).class_data in
    let rioc_list = JControlFlow.resolve_field fsi ioc in
      List.map
	(fun rioc ->
	   match rioc with
	     | `Class rc -> rc.c_index
	     | `Interface ri -> ri.i_index) rioc_list

  let update_virtual_lookup_set p (cni,msi) new_cni =
    let rcni = resolve_method p new_cni msi in
      add_to_workset p (rcni,msi);
      let s = ClassMethMap.find (cni,msi) p.static_virtual_lookup in
	p.static_virtual_lookup <-
	  ClassMethMap.add (cni,msi)
	  (ClassMethSet.add (rcni,msi) s) p.static_virtual_lookup
	  
  let invoke_virtual_lookup p cni msi =
    (* If this virtual call site appears for the first time, *)
    (* we will update the static_lookup_virtual map, otherwise *)
    (* no work has to be done. *)
    let c_info = get_class_info p cni in
      if not( MethodSet.mem msi c_info.memorized_virtual_calls ) then
	(c_info.memorized_virtual_calls <-
	   MethodSet.add msi c_info.memorized_virtual_calls;
	 p.static_virtual_lookup <-
	   ClassMethMap.add (cni,msi)
	   ClassMethSet.empty p.static_virtual_lookup;
	 let instantiated_classes =
	   if ( c_info.is_instantiated ) then
	     ClassSet.add cni c_info.instantiated_subclasses
	   else c_info.instantiated_subclasses in
	   ClassSet.iter
	     (fun new_cni ->
		update_virtual_lookup_set p (cni,msi) new_cni
	     )
	     instantiated_classes
	)

  let interface_lookup_action interfaces cni f =
    if ( ClassMap.mem cni interfaces ) then
      (ClassSet.iter
	 f (ClassMap.find cni interfaces))
    else ()
      (* otherwise, the classes implementing the interface have not
	 been charged yet so we can't do anything *)

  let invoke_interface_lookup p cni msi =
    let i_info = get_class_info p cni in
      i_info.memorized_interface_calls <-
	MethodSet.add msi i_info.memorized_interface_calls;
      interface_lookup_action p.interfaces cni
	(fun x -> invoke_virtual_lookup p x msi)
	
  let update_interface_lookup_set p interfaces =
    ClassSet.iter
      (fun i ->
	 let i_info = get_class_info p i in
	   MethodSet.iter
	     (fun msi ->
		invoke_interface_lookup p i msi
	     )
	     i_info.memorized_interface_calls
      )
      interfaces
      (* transitivly implemented interfaces *)
      
  let add_instantiated_class p cni =
    let cl_info = get_class_info p cni in
      if not( cl_info.is_instantiated ) then
	(cl_info.is_instantiated <- true;
	 (to_class_file cl_info.class_data).c_may_be_instantiated <- true;
	 (* Now we need to update the static_lookup_virtual map *)
	 (* for each virtual call that already occurred on A and *)
	 (* its super classes. *)
	 (let calls = cl_info.memorized_virtual_calls in
	    MethodSet.iter
	      (fun msi ->
		 update_virtual_lookup_set p (cni,msi) cni
	      ) calls;
	    update_interface_lookup_set p cl_info.super_interfaces;
	    List.iter
	      (fun scni ->
	   	 let s_info = get_class_info p scni in
		   (* We complete the list of instantiated subclasses for cn
		      and its superclasses *)
		   s_info.instantiated_subclasses <-
		     ClassSet.add cni s_info.instantiated_subclasses;
	   	   (let calls = s_info.memorized_virtual_calls in
	   	      MethodSet.iter
	   		(fun msi ->
			   update_virtual_lookup_set p (scni,msi) cni
	   		) calls
		   );
		   update_interface_lookup_set p s_info.super_interfaces
	      )
	      cl_info.super_classes);
	)

  let update_special_lookup_set p current_class_index cni msi s =
    let cmmap =
      try ClassMap.find current_class_index p.static_special_lookup
      with _ -> ClassMethMap.empty in
    let cmset =
      try ClassMethMap.find (cni,msi) cmmap
      with _ -> ClassMethSet.empty in
      p.static_special_lookup <-
	(ClassMap.add current_class_index
	   (ClassMethMap.add (cni,msi)
	      (ClassMethSet.union cmset s) cmmap) p.static_special_lookup)

  let rec invoke_special_lookup p current_class_index cni ms msi =
    let current_class_info = get_class_info p current_class_index in
    let rcni = resolve_method p cni msi in
      if ( ms.ms_name = "<init>"
	  || not(List.mem rcni current_class_info.super_classes) ) then
	(let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
	   update_special_lookup_set p current_class_index cni msi s;
	   (* we add (cni,msi) to the workset *)
	   add_to_workset p (rcni,msi)
	)
      else
	let rcni = resolve_method p
	  (List.hd current_class_info.super_classes) msi in
	  (let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
	     update_special_lookup_set p current_class_index cni msi s;
	     (* we add (cni,msi) to the workset *)
	     add_to_workset p (rcni,msi)
	  )

  let rec invoke_static_lookup p cni msi =
    let rcni = resolve_method p cni msi in
      (if not( ClassMethMap.mem (cni,msi) p.static_static_lookup ) then
       	 let s = ClassMethSet.add (rcni,msi) ClassMethSet.empty in
       	   p.static_static_lookup <-
       	     ClassMethMap.add (cni,msi) s p.static_static_lookup;
	   add_to_workset p (rcni,msi)
      );
      rcni

  let parse_instruction p current_class_index op =
    match op with
      | OpNew cn ->
	  let cni = p.dic.get_cn_index cn in
	    add_instantiated_class p cni;
	    add_class_clinits p cni
      | OpConst (`Class _) ->
	  let cni = p.dic.get_cn_index ["java";"lang";"Class"] in
	    add_instantiated_class p cni;
	    add_class_clinits p cni
      | OpGetStatic (cn,fs)
      | OpPutStatic (cn,fs) ->
	  let cni = p.dic.get_cn_index cn in
	  let fsi = p.dic.get_fs_index fs in
	  let rcni_list = resolve_field p cni fsi in
	    List.iter
	      (fun rcni ->
		 let ioc_info = get_class_info p rcni in
		   (match ioc_info.class_data with
		      | `Class _ -> add_class_clinits p rcni
		      | `Interface _ -> add_clinit p rcni
		   )
	      ) rcni_list
      | OpInvoke(`Virtual _,_) ->
	  let (cni,msi) = retrieve_invoke_index p.dic op in
	    invoke_virtual_lookup p cni msi
      | OpInvoke(`Interface _,_) ->
	  let (cni,msi) = retrieve_invoke_index p.dic op in
	    invoke_interface_lookup p cni msi
      | OpInvoke(`Special _,ms) ->
	  let (cni,msi) = retrieve_invoke_index p.dic op in
      	    invoke_special_lookup p current_class_index cni ms msi
      | OpInvoke(`Static _,_) ->
	  let (cni,msi) = retrieve_invoke_index p.dic op in
      	  let rcni = invoke_static_lookup p cni msi in
	    add_class_clinits p rcni
      | _ -> ()

  let parse_native_method p allocated_classes calls =
    let normalize_cn cn =
      if (cn <> []) then
	let head = List.hd cn in
	  (String.sub head 1 ((String.length head) - 1)) :: List.tl cn
      else [] in
      List.iter
	(fun signature ->
	   match JParseSignature.parse_objectType signature with
	     | TArray _ -> ()
	     | TClass cn ->
		 (* hack : why a class should not be encapsulated by L; ? *)
		 let cn = normalize_cn cn in
		 let cni = p.dic.get_cn_index cn in
		   add_instantiated_class p cni;
		   add_class_clinits p cni
	) allocated_classes;
      List.iter
      	(fun (m_class,m_name,m_signature) ->
	   let cn =
	     match JParseSignature.parse_objectType m_class with
	       | TArray _ -> failwith "Bad class"
	       | TClass cn -> normalize_cn cn in
	   let (parameters,rettype) =
	     JParseSignature.parse_method_descriptor m_signature in
	   let ms = { ms_name = m_name;
		      ms_parameters = parameters;
		      ms_return_type = rettype } in
	   let cni = p.dic.get_cn_index cn in
	   let msi = p.dic.get_ms_index ms in
	     add_to_workset p (cni,msi)
	) calls
    
  let iter_workset p =
    let tail = Dllist.tail p.workset
    in
      Dllist.iter_to_head
	(fun (cni,cm) ->
	   match cm.cm_implementation with
	     | Native ->
		 if not(p.parse_natives) then
		   failwith "A Native Method shouldn't be found in the workset"
		 else
		   let ms = cm.cm_signature in
		   let m_class =
		     "L" ^ (JUnparseSignature.unparse_objectType
			      (TClass (p.dic.retrieve_cn cni))) ^ ";"
		   and m_name = ms.ms_name
		   and m_signature = JUnparseSignature.unparse_method_descriptor
		     (ms.ms_parameters, ms.ms_return_type) in
		   let m = (m_class,m_name,m_signature) in
		     (try
			let (m_alloc, m_calls) =
			  (JNativeStubs.get_native_method_allocations m
			     p.native_methods_info,
			   JNativeStubs.get_native_method_calls m
			     p.native_methods_info) in
			  parse_native_method p m_alloc m_calls
		      with _ ->
			prerr_endline ("warning : found native method " ^ m_class
				       ^ "." ^ m_name ^ ":" ^ m_signature
				       ^ " not present in the stub file.")
		     )
	     | Java t ->
		 let code = (Lazy.force t).c_code
		 in
		   Array.iter (parse_instruction p cni) code)
	tail

  let new_program_cache entrypoints native_stubs classpath =
    let dic = make_dictionary () in
    let (parse_natives,native_methods_info) =
      match native_stubs with
	| None -> (false, JNativeStubs.empty_info)
	| Some file -> (true,
			JNativeStubs.parse_native_info_file file) in
    let entrypoints =
      List.map
	(fun (cn,ms) -> (dic.get_cn_index cn, dic.get_ms_index ms))
	entrypoints
    and workset = Dllist.create () in
    let p =
      { classes = ClassMap.empty;
	interfaces = ClassMap.empty;
	dic = dic;
	static_virtual_lookup = ClassMethMap.empty;
	static_static_lookup = ClassMethMap.empty;
	static_special_lookup = ClassMap.empty;
	clinits = ClassSet.empty;
	workset = workset;
	classpath = classpath;
	native_methods = ClassMethSet.empty;
	parse_natives = parse_natives;
	native_methods_info = native_methods_info }
    in
      List.iter
	(fun (cni,msi) ->
	   add_class_clinits p cni;
	   if MethodMap.mem msi (get_class_info p cni).methods
	   then add_to_workset p (cni,msi))
	entrypoints;
      p

  let parse_program entrypoints native_stubs classpath =
    let classpath = JFile.class_path classpath in
    let p = new_program_cache entrypoints native_stubs classpath in
      iter_workset p;
      if not (ClassMethSet.is_empty p.native_methods)
      then prerr_endline "The program contains native method. Beware that native methods' side effects may invalidate the result of the analysis.";
      JFile.close_class_path classpath;
      p

  let parse_program_bench entrypoints classpath =
    let time_start = Sys.time() in
    let p = parse_program entrypoints None classpath in
    let s = Dllist.size p.workset in
      Printf.printf "Workset of size %d\n" s;
      let time_stop = Sys.time() in
	Printf.printf "program parsed in %fs.\n" (time_stop-.time_start);
	Printf.printf "%d classes and %d methods parsed.\n"
	  (p.dic.cni_table.cni_next) (p.dic.msi_table.msi_next)

end

let static_virtual_lookup virtual_lookup_map cni msi =
  try
    ClassMethMap.find (cni,msi) virtual_lookup_map
  with _ ->
    (* probably dead code *)
    ClassMethSet.empty

let static_static_lookup static_lookup_map cni msi =
  ClassMethMap.find (cni,msi) static_lookup_map

let static_interface_lookup interface_lookup_map virtual_lookup_map
    interfaces_map cni msi =
  try
    ClassMethMap.find (cni,msi) !interface_lookup_map
  with
    | _ ->
	let s = ref ClassMethSet.empty in
	let f =
	  (fun x ->
	     let calls =
	       static_virtual_lookup virtual_lookup_map x msi in
	       s := ClassMethSet.union !s calls) in
	  Program.interface_lookup_action interfaces_map cni f;
	  interface_lookup_map :=
	    ClassMethMap.add (cni,msi) !s !interface_lookup_map;
	  !s

let static_special_lookup special_lookup_map cni ccni cmsi =
  ClassMethMap.find (ccni,cmsi) (ClassMap.find cni special_lookup_map)

let static_lookup dic virtual_lookup_map special_lookup_map static_lookup_map
    interfaces_map classes_map cni msi pp =
  let m = MethodMap.find msi
    (ClassMap.find cni classes_map).Program.methods in
  let interface_lookup_map = ref ClassMethMap.empty in
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
			       virtual_lookup_map interfaces_map ccni cmsi
			 | OpInvoke (`Virtual _,_) ->
			     static_virtual_lookup virtual_lookup_map ccni cmsi
			 | OpInvoke (`Static _,_) ->
			     static_static_lookup static_lookup_map ccni cmsi
			 | OpInvoke (`Special _,_) ->
                             static_special_lookup special_lookup_map cni ccni cmsi
			 | _ ->
			     failwith "Invalid opcode found at specified program point"
		   with
		     | Not_found -> failwith "Invalid program point"
		     | e -> raise e
	  )

let pcache2jprogram p =
  { classes =
      ClassMap.mapi
	(fun i _ -> (Program.get_class_info p i).Program.class_data)
	p.Program.classes;
    static_lookup =
      static_lookup p.Program.dic
	p.Program.static_virtual_lookup
	p.Program.static_special_lookup
	p.Program.static_static_lookup
	p.Program.interfaces
	p.Program.classes;
    dictionary = p.Program.dic }

(* cf. openjdk6/hotspot/src/share/vm/runtime/thread.cpp *)
let default_entrypoints =
  let initializeSystemClass : class_name * method_signature =
    (["java";"lang";"System"],
     {ms_name = "initializeSystemClass";
      ms_parameters = [];
      ms_return_type = None})
  in
    (["java";"lang";"Object"],clinit_signature)::
      (["java";"lang";"String"],clinit_signature)::
      (["java";"lang";"System"],clinit_signature)::
      initializeSystemClass::
      (["java";"lang";"ThreadGroup"],clinit_signature)::
      (["java";"lang";"Thread"],clinit_signature)::
      (["java";"lang";"reflect";"Method"],clinit_signature)::
      (["java";"lang";"ref";"Finalizer"],clinit_signature)::
      (["java";"lang";"Class"],clinit_signature)::
      (["java";"lang";"OutOfMemoryError"],clinit_signature)::
      (["java";"lang";"NullPointerException"],clinit_signature)::
      (["java";"lang";"ClassCastException"],clinit_signature)::
      (["java";"lang";"ArrayStoreException"],clinit_signature)::
      (["java";"lang";"ArithmeticException"],clinit_signature)::
      (["java";"lang";"StackOverflowError"],clinit_signature)::
      (["java";"lang";"IllegalMonitorStateException"],clinit_signature)::
      (["java";"lang";"Compiler"],clinit_signature)::
      (["java";"lang";"reflect";"Field"],clinit_signature)::
      []

let parse_program ?(other_entrypoints=default_entrypoints) ?(native_stubs=None)
    classpath cnms =
  let p_cache = (Program.parse_program (cnms::other_entrypoints) native_stubs
		   classpath) in
    pcache2jprogram p_cache

let parse_program_bench ?(other_entrypoints=default_entrypoints) classpath cnms =
    Program.parse_program_bench (cnms::other_entrypoints) classpath



