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


module ClassMap = Ptmap
module MethodMap = Ptmap
module FieldMap = Ptmap

module ClassIndexMap = Map.Make(
  struct
    type t = class_name
    let compare = compare
  end)

module MethodIndexMap = JClass.MethodMap
module FieldIndexMap = JClass.FieldMap

type field_signature_index = int
type field_signature_index_table =
    { mutable fsi_map : field_signature_index FieldIndexMap.t;
      mutable fs_map : field_signature FieldMap.t;
      mutable fsi_next : field_signature_index }
type method_signature_index = int
type method_signature_index_table =
    { mutable msi_map : method_signature_index MethodIndexMap.t;
      mutable ms_map : method_signature MethodMap.t;
      mutable msi_next : method_signature_index }
type class_name_index = int
type class_name_index_table =
    { mutable cni_map : class_name_index ClassIndexMap.t;
      mutable cn_map : class_name ClassMap.t;
      mutable cni_next : class_name_index }

module ClassSet = Set.Make(
  struct
    type t = class_name_index
    let compare = compare
  end)

module MethodSet = Set.Make(
  struct
    type t = method_signature_index
    let compare = compare
  end)

let get_ms_index tab ms =
  try
    MethodIndexMap.find ms tab.msi_map
  with Not_found ->
    begin
      let current = tab.msi_next
      in
	tab.msi_map <- MethodIndexMap.add ms current tab.msi_map;
	tab.ms_map <- MethodMap.add current ms tab.ms_map;
	tab.msi_next <- succ current;
	current
    end

let get_cn_index tab cn =
  try
    ClassIndexMap.find cn tab.cni_map
  with Not_found ->
    begin
      let current = tab.cni_next
      in
	tab.cni_map <- ClassIndexMap.add cn current tab.cni_map;
	tab.cn_map <- ClassMap.add current cn tab.cn_map;
	tab.cni_next <- succ current;
	current
    end

let get_fs_index tab fs =
  try
    FieldIndexMap.find fs tab.fsi_map
  with Not_found ->
    begin
      let current = tab.fsi_next
      in
	tab.fsi_map <- FieldIndexMap.add fs current tab.fsi_map;
	tab.fs_map <- FieldMap.add current fs tab.fs_map;
	tab.fsi_next <- succ current;
	current
    end
  
exception RetrieveError

let retrieve_cn tab cni =
  try
    ClassMap.find cni tab.cn_map
  with _ -> raise RetrieveError

let retrieve_ms tab msi =
  try
    MethodMap.find msi tab.ms_map
  with _ -> raise RetrieveError

let retrieve_fs tab fsi =
  try
    FieldMap.find fsi tab.fs_map
  with _ -> raise RetrieveError

type dictionary = { msi_table : method_signature_index_table;
		    cni_table : class_name_index_table;
		    fsi_table : field_signature_index_table;
		    get_fs_index : FieldIndexMap.key -> field_signature_index;
		    get_ms_index : MethodIndexMap.key -> method_signature_index;
		    get_cn_index : ClassIndexMap.key -> class_name_index;
		    retrieve_fs : field_signature_index -> field_signature;
		    retrieve_ms : method_signature_index -> method_signature;
		    retrieve_cn : class_name_index -> class_name }

let clinit_index = 0
let init_index = 1

let java_lang_object_index = 0

let make_dictionary () =
  let msi_table =
    { msi_map =
	MethodIndexMap.add init_signature init_index
	  (MethodIndexMap.add clinit_signature clinit_index MethodIndexMap.empty);
      ms_map =
	MethodMap.add init_index init_signature
	  (MethodMap.add clinit_index clinit_signature MethodMap.empty);
      msi_next = 2 }
  and cni_table =
    { cni_map =
	ClassIndexMap.add java_lang_object java_lang_object_index ClassIndexMap.empty;
      cn_map = ClassMap.add java_lang_object_index java_lang_object ClassMap.empty;
      cni_next = 1 }
  and fsi_table =
    { fsi_map = FieldIndexMap.empty;
      fs_map = FieldMap.empty;
      fsi_next = 0 } in
    { msi_table = msi_table;
      cni_table = cni_table;
      fsi_table = fsi_table;
      get_fs_index = get_fs_index fsi_table;
      get_ms_index = get_ms_index msi_table;
      get_cn_index = get_cn_index cni_table;
      retrieve_fs = retrieve_fs fsi_table;
      retrieve_ms = retrieve_ms msi_table;
      retrieve_cn = retrieve_cn cni_table }

module ClassMethSet = Set.Make(
  struct
    type t = int * int
    let compare = compare
  end)

module ClassMethMap = Map.Make(
  struct
    type t = int * int
    let compare = compare
  end)

module ClassnameSet = Set.Make(
  struct
    type t = JBasics.class_name
    let compare = compare
  end)

module MethodsignatureSet = Set.Make(
  struct
    type t = JClass.method_signature
    let compare = compare
  end)

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

and abstract_method = {
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

and jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of concrete_method

and class_file = {
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
  mutable c_resolve_methods : (class_file * jmethod) MethodMap.t;
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

and interface_or_class = [
| `Interface of interface_file
| `Class of class_file
]

let get_name = function
  | `Interface i -> i.i_name
  | `Class c -> c.c_name

let get_index = function
  | `Class c -> c.c_index
  | `Interface i -> i.i_index

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

let equal c1 c2 = match c1,c2 with
  | `Class c1, `Class c2 -> c1==c2
  | `Interface c1, `Interface c2 -> c1==c2
  | _, _ -> false

type program = { classes : interface_or_class ClassMap.t;
		 static_lookup : class_name_index -> method_signature_index ->
							  int -> ClassMethSet.t;
		 dictionary : dictionary }
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


let defines_method msi = function
  | `Interface i ->
      if msi = clinit_index then i.i_initializer <> None
      else MethodMap.mem msi i.i_methods
  | `Class c -> MethodMap.mem msi c.c_methods
let defines_field fsi = function
  | `Interface {i_fields=fm;} -> FieldMap.mem fsi fm
  | `Class {c_fields=fm;} -> FieldMap.mem fsi fm


let get_interface_or_class program cn =
  let cni = program.dictionary.get_cn_index cn in
    ClassMap.find cni program.classes

let super_class c : class_file option = super c

let get_method c msi = match c with
  | `Interface i ->
      if msi = clinit_index
      then
	match i.i_initializer with
	  | Some m -> ConcreteMethod m
	  | None -> raise Not_found
      else
	AbstractMethod (MethodMap.find msi i.i_methods)
  | `Class c -> MethodMap.find msi c.c_methods

let get_methods = function
  | `Interface i ->
      let init =
	if i.i_initializer = None then []
	else [clinit_index]
      in MethodMap.fold (fun ms _ l -> ms::l) i.i_methods init
  | `Class {c_methods = mm;} ->
      MethodMap.fold (fun ms _ l -> ms::l) mm []

let get_field c fsi = match c with
  | `Interface i -> InterfaceField (FieldMap.find fsi i.i_fields)
  | `Class c -> ClassField (FieldMap.find fsi c.c_fields)

let get_fields c =
  let to_list fs _f l = fs::l in
    match c with
      | `Interface i -> FieldMap.fold to_list i.i_fields []
      | `Class c -> FieldMap.fold to_list c.c_fields []

let ccm2pcm p_dic m = {
  cm_has_been_parsed = false;
  cm_index = p_dic.get_ms_index m.JClass.cm_signature;
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
  cm_implementation = m.JClass.cm_implementation;
}

let pcm2ccm m = {
  JClass.cm_signature = m.cm_signature;
  JClass.cm_static = m.cm_static;
  JClass.cm_final = m.cm_final;
  JClass.cm_synchronized = m.cm_synchronized;
  JClass.cm_strict = m.cm_strict;
  JClass.cm_access = m.cm_access;
  JClass.cm_generic_signature = m.cm_generic_signature;
  JClass.cm_bridge = m.cm_bridge;
  JClass.cm_varargs = m.cm_varargs;
  JClass.cm_synthetic = m.cm_synthetic;
  JClass.cm_other_flags = m.cm_other_flags;
  JClass.cm_exceptions = m.cm_exceptions;
  JClass.cm_attributes = m.cm_attributes;
  JClass.cm_implementation = m.cm_implementation;
}

let cam2pam p_dic m = {
  am_index = p_dic.get_ms_index m.JClass.am_signature;
  am_signature = m.JClass.am_signature;
  am_access = m.JClass.am_access;
  am_generic_signature = m.JClass.am_generic_signature;
  am_bridge = m.JClass.am_bridge;
  am_varargs = m.JClass.am_varargs;
  am_synthetic = m.JClass.am_synthetic;
  am_other_flags = m.JClass.am_other_flags;
  am_exceptions = m.JClass.am_exceptions;
  am_attributes = m.JClass.am_attributes;
}

let pam2cam m = {
  JClass.am_signature = m.am_signature;
  JClass.am_access = m.am_access;
  JClass.am_generic_signature = m.am_generic_signature;
  JClass.am_bridge = m.am_bridge;
  JClass.am_varargs = m.am_varargs;
  JClass.am_synthetic = m.am_synthetic;
  JClass.am_other_flags = m.am_other_flags;
  JClass.am_exceptions = m.am_exceptions;
  JClass.am_attributes = m.am_attributes;
}

let ptree2mmap get_ms f ptm =
  let mmap = ref JClass.MethodMap.empty in
    MethodMap.iter
    (fun _ m ->
	 mmap := JClass.MethodMap.add (get_ms m) (f m) !mmap) ptm;
    !mmap

let ptree2fmap get_fs ptm =
  let fmap = ref JClass.FieldMap.empty in
    FieldMap.iter
    (fun _ cf ->
	 fmap := JClass.FieldMap.add (get_fs cf) cf !fmap) ptm;
    !fmap

let to_class = function
  | `Interface c -> `Interface
      {JClass.i_name = c.i_name;
       JClass.i_version = c.i_version;
       JClass.i_access = c.i_access;
       JClass.i_generic_signature = c.i_generic_signature;
       JClass.i_consts = c.i_consts;
       JClass.i_annotation = c.i_annotation;
       JClass.i_other_flags = c.i_other_flags;
       JClass.i_interfaces =
	  ClassMap.fold (fun _ c l -> c.i_name::l) c.i_interfaces [];
       JClass.i_sourcefile = c.i_sourcefile;
       JClass.i_deprecated = c.i_deprecated;
       JClass.i_source_debug_extention = c.i_source_debug_extention;
       JClass.i_inner_classes = c.i_inner_classes;
       JClass.i_other_attributes = c.i_other_attributes;
       JClass.i_initializer =
	  begin
	    match c.i_initializer with
	      | Some m -> Some (pcm2ccm m)
	      | None -> None
	  end;
       JClass.i_fields = ptree2fmap (fun ifield -> ifield.if_signature)
	  c.i_fields;
       JClass.i_methods = ptree2mmap (fun am -> am.am_signature)
	  (fun m -> pam2cam m) c.i_methods;
      }
  | `Class c -> `Class
      {JClass.c_name = c.c_name;
       JClass.c_version = c.c_version;
       JClass.c_access = c.c_access;
       JClass.c_generic_signature = c.c_generic_signature;
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
	  ClassMap.fold (fun _ c l -> c.i_name::l) c.c_interfaces [];
       JClass.c_sourcefile = c.c_sourcefile;
       JClass.c_deprecated = c.c_deprecated;
       JClass.c_enclosing_method = c.c_enclosing_method;
       JClass.c_source_debug_extention = c.c_source_debug_extention;
       JClass.c_inner_classes = c.c_inner_classes;
       JClass.c_other_attributes = c.c_other_attributes;
       JClass.c_fields = ptree2fmap (fun cf -> cf.cf_signature) c.c_fields;
       JClass.c_methods =
	  ptree2mmap
	    (function
	      | AbstractMethod m -> m.am_signature
	      | ConcreteMethod m -> m.cm_signature)
	    (function
	      | AbstractMethod m -> JClass.AbstractMethod (pam2cam m)
	      | ConcreteMethod m -> JClass.ConcreteMethod (pcm2ccm m))
	    c.c_methods;
      }

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
let fold f s p = ClassMap.fold (fun _ c s -> f s c) p.classes s
let iter f p = ClassMap.iter (fun _ c -> f c) p.classes

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

let rec rem_dbl = function
  | e::(_::_ as l) -> e:: (List.filter ((!=)e) (rem_dbl l))
  | l -> l

let implemented_interfaces c = rem_dbl (implemented_interfaces' c)

let rec firstCommonSuperClass (c1:class_file) (c2:class_file) : class_file =
  if extends_class' c1 c2
  then c2
  else match super_class (`Class c2) with
    | Some c3 -> firstCommonSuperClass c1 c3
    | None -> raise (Failure "firstCommonSuperClass: c1 and c2 has been found such that c1 does not extends c2 and c2 has no super class.")

let rec resolve_interface_method ?(acc=[]) msi (c:interface_or_class) : interface_file list =
  ClassMap.fold
    (fun _ i acc ->
      if defines_method msi (`Interface i)
      then i::acc
      else resolve_interface_method ~acc msi (`Interface i))
    (get_interfaces c)
    acc

let rec resolve_implemented_method ?(acc=[]) msi (c:class_file) : (class_file option * interface_file list) =
  match c.c_super_class with
    | None -> (None,resolve_interface_method ~acc msi (`Class c))
    | Some sc ->
	if defines_method msi (`Class sc)
	then (Some sc,resolve_interface_method ~acc msi (`Class c))
	else resolve_implemented_method ~acc:(resolve_interface_method ~acc msi (`Class c)) msi sc

let get_loaded_classes p =
  let dic = (p.dictionary) in
  let max_classes = dic.cni_table.cni_next - 1 in
  let s = ref ClassnameSet.empty in
    for i = 0 to max_classes do
      s := ClassnameSet.add (dic.retrieve_cn i) !s
    done;
    !s

let get_loaded_methods p =
  let dic = (p.dictionary) in
  let max_methods = dic.msi_table.msi_next - 1 in
  let s = ref MethodsignatureSet.empty in
    for i = 0 to max_methods do
      s := MethodsignatureSet.add (dic.retrieve_ms i) !s
    done;
    !s

let get_instantiated_classes p =
  let dic = (p.dictionary) in
  let s = ref ClassnameSet.empty in
    ClassMap.iter
      (fun cni ioc ->
	 match ioc with
	   | `Interface _ -> ()
	   | `Class c ->
	       if ( c.c_may_be_instanciated = true ) then
		 s := ClassnameSet.add (dic.retrieve_cn cni) !s)
      p.classes;
    !s

exception Invoke_not_found of (class_name * method_signature
			       * class_name * method_signature)

let get_method_calls p cni m =
  let l = ref [] in
  let f_lookup = p.static_lookup in
  let dic = p.dictionary in
  let method2callsite cni msi pp (ccni,cmsi) =
    ((dic.retrieve_cn cni,dic.retrieve_ms msi,pp),
     (dic.retrieve_cn ccni,dic.retrieve_ms cmsi))
  in
    begin
      match m with
	| ConcreteMethod ({cm_implementation = Java code} as cm)
	    when cm.cm_has_been_parsed ->
	    let msi = cm.cm_index in
	      Array.iteri
		(fun pp op ->
		   match op with
		     | OpInvoke _ ->
			 let callsites = (f_lookup cni msi pp) in
			 let callsites_list =
			   ClassMethSet.elements callsites
			 in
			   l :=
			     List.rev_append
			       (List.map (method2callsite cni msi pp) callsites_list)
			       !l
		     | _ -> ())
		(Lazy.force code).c_code
	| _ -> ()
    end;
    !l

let retrieve_invoke_index dic op =
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

type callgraph = ((JBasics.class_name * JClass.method_signature * int)
		  * (JBasics.class_name * JClass.method_signature)) list

let get_callgraph p =
  let classes = p.classes in
  let calls = ref [] in
    ClassMap.iter
      (fun _ ioc ->
	 match ioc with
	   | `Interface i ->
	       (match i.i_initializer with
		  | None -> ()
		  | Some m -> calls := (get_method_calls p i.i_index
						   (ConcreteMethod m)) @ !calls)
	   | `Class c ->
	       let l = ref [] in
		 MethodMap.iter
		   (fun _ m ->
		      l :=
			!l @ (get_method_calls p c.c_index m)) c.c_methods;
		 calls := !l @ !calls
      ) classes;
    !calls

let store_callgraph callgraph file =
  let out = IO.output_channel (open_out file) in
    List.iter
      (fun ((cn,ms,pp),(ccn,cms)) ->
	 IO.nwrite out
	   ((JDumpBasics.class_name cn) ^ "."
	    ^ ms.ms_name
	    ^ (JUnparseSignature.unparse_method_descriptor
		 (ms.ms_parameters, ms.ms_return_type)) ^ ","
	    ^ (string_of_int pp) ^ " -> "
	    ^ (JDumpBasics.class_name ccn) ^ "."
	    ^ cms.ms_name
	    ^ (JUnparseSignature.unparse_method_descriptor
		 (cms.ms_parameters, cms.ms_return_type)) ^ "\n")
      )
      callgraph;
    IO.close_out out
