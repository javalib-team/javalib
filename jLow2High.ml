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
open JClassLow
open JClass

let debug = ref 1

let rec flags2access = function
  | `AccPublic::l ->
      if List.exists (fun a -> a = `AccPrivate || a= `AccProtected) l
      then raise (Class_structure_error "Access flags Public and Private or Protected cannot be set at the same time")
      else (`Public,l)
  | `AccPrivate::l ->
      if List.exists (fun a -> a = `AccPublic || a= `AccProtected) l
      then raise (Class_structure_error "Access flags Private and Public or Protected cannot be set at the same time")
      else (`Private,l)
  | `AccProtected::l ->
      if List.exists (fun a -> a = `AccPrivate || a= `AccPublic) l
      then raise (Class_structure_error "Access flags Protected and Private or Public cannot be set at the same time")
      else (`Protected,l)
  | f::l -> let (p,fl) = flags2access l in (p,f::fl)
  | [] -> (`Default,[])

let rec get_flag flag = function
  | [] -> (false,[])
  | f::fl when f=flag -> (true,List.filter ((<>)f) fl)
  | f::fl -> let (b,fl) = get_flag flag fl in (b,f::fl)

(* convert a list of  attributes to a list of couple of string, as for AttributeUnknown. *)
let low2high_other_attributes consts : JClassLow.attribute list ->  (string*string) list =
  List.map
    (function
       | AttributeUnknown (name, contents) -> name, contents
       | a ->
	   let (name,contents) = JUnparse.unparse_attribute_to_strings consts a
	   in
	     if !debug >0 then prerr_endline ("Warning: unexpected attribute found: "^name);
	     name,contents)

let attribute_to_signature (al:JClassLow.attribute list) : string option =
  match List.find_all (function AttributeSignature _ -> true | _ -> false) al with
    | [] -> None
    | [AttributeSignature s] -> Some s
    | _::_::_ -> 
	raise
	  (Class_structure_error
	     "A Signature attribute can only be specified at most once per element.")

(* convert a list of  attributes to an [attributes] structure. *)
let low2high_attributes consts (al:JClassLow.attribute list) :attributes =
  {synthetic = List.exists ((=)AttributeSynthetic) al;
   deprecated = List.exists ((=)AttributeDeprecated) al;
   signature = attribute_to_signature al;
   other =
      low2high_other_attributes consts
	(List.filter
	   (function AttributeDeprecated | AttributeSynthetic | AttributeSignature _ -> false | _ -> true)
	   al);
  }


let low2high_code consts = function c ->
  {
    c_max_stack = c.JClassLow.c_max_stack;
    c_max_locals = c.JClassLow.c_max_locals;
    c_code = JInstruction.opcodes2code (DynArray.to_array consts) c.JClassLow.c_code;
    c_exc_tbl = c.JClassLow.c_exc_tbl;
    c_line_number_table =
      begin
	let rec find_lineNumberTable = function
	  | AttributeLineNumberTable l::l' ->
	      if find_lineNumberTable l' <> None
	      then raise (Class_structure_error "Only one AttributeLineNumberTable can be attached to a method.");
	      Some l
	  | _::l -> find_lineNumberTable l
	  | [] -> None
	in find_lineNumberTable c.JClassLow.c_attributes
      end;
    c_local_variable_table =
      begin
	let rec find_LocalVariableTable = function
	  | AttributeLocalVariableTable l::l' ->
	      if find_LocalVariableTable l' <> None
	      then raise (Class_structure_error "Only one AttributeLocalVariableTable can be attached to a method.");
	      Some l
	  | _::l -> find_LocalVariableTable l
	  | [] -> None
	in find_LocalVariableTable c.JClassLow.c_attributes
      end;
    c_stack_map =
      begin
	let rec find_StackMap = function
	  | AttributeStackMap l::l' ->
	      if find_StackMap l' <> None
	      then raise (Class_structure_error "Only one StackMap attribute can be attached to a method.");
	      Some l
	  | _::l -> find_StackMap l
	  | [] -> None
	in find_StackMap c.JClassLow.c_attributes
      end;
    c_attributes = low2high_other_attributes consts
      (List.filter
	  (function
	    | AttributeStackMap _ | AttributeLocalVariableTable _
	    | AttributeLineNumberTable _ -> false
	    | _ -> true)
	  c.JClassLow.c_attributes);
  }

let low2high_cfield consts fs = function f ->
  let flags = f.f_flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (access,flags) = flags2access flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_volatile,flags) = get_flag `AccVolatile flags in
  let (is_transient,flags) = get_flag `AccTransient flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_enum,flags) = get_flag `AccEnum flags in
  let flags =
    List.map (function
      | `AccRFU i -> i
      | _ ->
	  prerr_endline "unexcepted flag in JLow2High.low2high_cfield: bug in JavaLib";
	  assert false)
      flags
  in
  let kind =
    if is_final
    then
      if is_volatile
      then raise (Class_structure_error "A field cannot be final and volatile.")
      else Final
    else
      if is_volatile then Volatile else NotFinal
  in
  let (cst,other_att) =
    List.partition (function AttributeConstant _ -> true | _ -> false) f.f_attributes in
  let (cst,other_att) =
    match cst with
      | [] -> None,other_att
      | AttributeConstant c::oc when not is_static ->  (* it seems quite common *)
	  if !debug > 1 then prerr_endline "A non-static field has been found with a constant value associated.";
	  None, (AttributeConstant c::(oc@other_att))
      | AttributeConstant c::[] ->
	  Some c, other_att
      | AttributeConstant c::oc ->
	  if !debug > 0 then prerr_endline "A field contains more than one constant value associated.";
	  Some c, (oc@other_att)
      | _ -> assert false
  in
    {
      cf_signature = fs;
      cf_access = access;
      cf_static = is_static;
      cf_kind = kind;
      cf_value = cst;
      cf_transient = is_transient;
      cf_synthetic = is_synthetic;
      cf_enum = is_enum;
      cf_other_flags = flags;
      cf_attributes =
	low2high_attributes consts other_att;
    }

let low2high_ifield consts fs = function f ->
  let flags = f.f_flags in
  let (is_public,flags) = get_flag `AccPublic flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
    if not(is_public && is_static && is_final)
    then raise (Class_structure_error "A field of an interface must be : Public, Static and Final.");
    let flags = List.map
      (function
	| `AccRFU i -> i
	| _ -> raise (Class_structure_error "A field of an interface may only have it `AccSynthetic flag set in addition of `AccPublic, `AccStatic and `AccFinal."))
      flags
    in
    {
      if_signature = fs;
      if_value =
	begin
	  let rec find_Constant = function
	    | AttributeConstant c::_ -> Some c
	    | _::l -> find_Constant l
	    | [] -> None
	  in find_Constant f.f_attributes
	end;
      if_synthetic = is_synthetic;
      if_other_flags = flags;
      if_attributes =
	low2high_attributes consts
	  (List.filter
	      (function AttributeConstant _ -> false| _ -> true)
	      f.f_attributes);
    }

let low2high_amethod consts ms = function m ->
  let flags = m.m_flags in
  let (access,flags) = flags2access flags in
  let (is_abstract,flags) = get_flag `AccAbstract flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_bridge,flags) = get_flag `AccBridge flags in
  let (is_varargs,flags) = get_flag `AccVarArgs flags in
  let access =
    match access with
      | `Private -> raise (Class_structure_error "Abstract method cannot be private")
      | `Default -> `Default
      | `Protected -> `Protected
      | `Public -> `Public
  in
  let flags =
    List.map
      (function
	| `AccRFU i -> i
	| _ -> raise (Class_structure_error (
	    "If a method has its ACC_ABSTRACT flag set it may not have any"
	    ^ "of its ACC_FINAL, ACC_NATIVE, ACC_PRIVATE, ACC_STATIC, "
	    ^ "ACC_STRICT, or ACC_SYNCHRONIZED flags set.")))
      flags
  in
    assert(is_abstract);
    {
      am_signature = ms;
      am_access = access;
      am_synthetic = is_synthetic;
      am_bridge = is_bridge;
      am_varargs = is_varargs;
      am_other_flags = flags;
      am_exceptions =
	begin
	  let rec find_Exceptions = function
	    | AttributeExceptions cl::l ->
		if find_Exceptions l <> []
		then raise (Class_structure_error "Only one Exception attribute is allowed in a method.")
		else cl
	    | _::l -> find_Exceptions l
	    | [] -> []
	  in find_Exceptions m.m_attributes
	end;
      am_attributes =
	low2high_attributes consts
	  (List.filter
	      (function AttributeExceptions _ -> false| _ -> true)
	      m.m_attributes);
    }

let low2high_cmethod consts ms = function m ->
  if m.m_name = "<init>" &&
    List.exists (fun a -> a=`AccStatic || a=`AccFinal || a=`AccSynchronized || a=`AccNative || a=`AccAbstract)
    m.m_flags
  then raise (Class_structure_error ("A specific instance initialization method may have at most "
				      ^ "one of its ACC_PRIVATE, ACC_PROTECTED, and ACC_PUBLIC flags set "
				      ^ "and may also have its ACC_STRICT flag set."));
  let flags = m.m_flags in
  let (access,flags) = flags2access flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_synchronized,flags) = get_flag `AccSynchronized flags in
  let (is_strict,flags) = get_flag `AccStrict flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_bridge,flags) = get_flag `AccBridge flags in
  let (is_varargs,flags) = get_flag `AccVarArgs flags in
  let (is_native,flags) = get_flag `AccNative flags in
  let flags = List.map
    (function
      | `AccRFU i -> i
      | `AccAbstract -> raise (Class_structure_error "Non abstract class cannot have abstract methods.")
      | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_cmethod : unexpected flag found."))
    flags
  in
    {
      cm_signature = ms;
      cm_static = is_static;
      cm_final = is_final;
      cm_synchronized = is_synchronized;
      cm_strict = is_strict;
      cm_access = access;
      cm_bridge = is_bridge;
      cm_varargs = is_varargs;
      cm_synthetic = is_synthetic;
      cm_other_flags = flags;
      cm_exceptions =
	begin
	  let rec find_Exceptions = function
	    | AttributeExceptions cl::l ->
		if find_Exceptions l <> []
		then raise (Class_structure_error "Only one Exception attribute is allowed in a method.")
		else cl
	    | _::l -> find_Exceptions l
	    | [] -> []
	  in find_Exceptions m.m_attributes
	end;
      cm_attributes =
	low2high_attributes consts
	  (List.filter
	      (function |AttributeExceptions _ | AttributeCode _ -> false| _ -> true)
	      m.m_attributes);
      cm_implementation =
	begin
	  let rec find_Code = function
	    | AttributeCode c::l ->
		if List.exists (function AttributeCode _ -> true| _-> false) l
		then raise (Class_structure_error "Only one Code attribute is allowed in a method.");
		if not is_native
		then Java (lazy (low2high_code consts (Lazy.force c)))
		else begin
		  if !debug > 0 then prerr_endline "A method declared as Native with code has been found.";
		  Native
		end
	    | _::l -> find_Code l
	    | [] ->
		if is_native
		then Native
		else begin
		  if !debug > 0 then prerr_endline "A method not declared as Native, nor Abstract has been found without code.";
		  Native
		end
	  in find_Code m.m_attributes
	end;
    }

let low2high_acmethod consts ms = function m ->
  if List.exists ((=)`AccAbstract) m.m_flags
  then AbstractMethod (low2high_amethod consts ms m)
  else ConcreteMethod (low2high_cmethod consts ms m)

let low2high_methods consts = function ac ->
  List.fold_left
    (fun map meth ->
      let ms =
	{ms_name = meth.m_name;
	 ms_parameters = fst meth.m_descriptor;
	 ms_return_type = snd meth.m_descriptor;}
      in
	if !debug > 0 && MethodMap.mem ms map
	then
	  prerr_endline
	    ("2 methods have been found with the same signature ("^ms.ms_name
	      ^"("^ String.concat ", " (List.map (JDumpBasics.value_signature) ms.ms_parameters) ^"))");
	MethodMap.add
	  ms
	  (try low2high_acmethod consts ms meth
	    with Class_structure_error msg -> raise (Class_structure_error ("in method " ^JDumpBasics.signature meth.m_name (SMethod meth.m_descriptor)^": "^msg)))
	map)
    MethodMap.empty
    ac.j_methods

let low2high_innerclass = function
    (inner_class_info,outer_class_info,inner_name,flags) ->
      let (access,flags) = flags2access flags in
      let (is_final,flags) = get_flag `AccFinal flags in
      let (is_static,flags) = get_flag `AccStatic flags in
      let (is_interface,flags) = get_flag `AccInterface flags in
      let (is_abstract,flags) = get_flag `AccAbstract flags in
      let (is_synthetic,flags) = get_flag `AccSynthetic flags in
      let (is_annotation,flags) = get_flag `AccAnnotation flags in
      let (is_enum,flags) = get_flag `AccEnum flags in
      let flags = List.map
	(function
	  | `AccRFU i -> i
	  | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_cmethod : unexpected flag found."))
	flags
      in
	{
	  ic_class_name = inner_class_info;
	  ic_outer_class_name = outer_class_info;
	  ic_source_name = inner_name;
	  ic_access = access;
	  ic_static = is_static;
	  ic_final = is_final;
	  ic_synthetic = is_synthetic;
	  ic_annotation = is_annotation;
	  ic_enum = is_enum;
	  ic_other_flags = flags;
	  ic_type =
	    if is_interface
	    then `Interface
	    else
	      if is_abstract
	      then `Abstract
	    else `ConcreteClass
	}


let low2high_class cl =
  if cl.j_super = None && cl.j_name <> java_lang_object
  then raise (Class_structure_error "Only java.lang.Object is allowed not to have a super-class.");
  let flags = cl.j_flags in
  let (access,flags) = flags2access (flags :> access_flag list) in
  let (accsuper,flags) = get_flag `AccSuper flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_interface,flags) = get_flag `AccInterface flags in
  let (is_abstract,flags) = get_flag `AccAbstract flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_annotation,flags) = get_flag `AccAnnotation flags in
  let (is_enum,flags) = get_flag `AccEnum flags in
  let flags =
    List.map
      (function
	 | `AccRFU i -> i
	 | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_class : unexpected flag found."))
      flags
  in
    (try assert ((accsuper || is_interface)) with _ -> raise (Class_structure_error "ACC_SUPER must be set for all classes"));
    (try assert (not(is_final && is_abstract)) with _ -> raise (Class_structure_error "An abstract class cannot be final."));
    let consts = DynArray.of_array cl.j_consts in
    let my_name = cl.j_name in
    let my_version = cl.j_version in
    let my_access =
      match access with
	| `Public -> `Public
	| `Default -> `Default
	| _ -> raise (Class_structure_error "Invalid visibility for a class.")
    and my_interfaces = cl.j_interfaces
    and my_sourcefile =
      let rec find_SourceFile = function
	| AttributeSourceFile s::_ -> Some s
	| _::l -> find_SourceFile l
	| [] -> None
      in find_SourceFile cl.j_attributes
    and my_deprecated = List.exists ((=)AttributeDeprecated) cl.j_attributes
    and my_signature = attribute_to_signature cl.j_attributes
    and my_inner_classes =
      let rec find_InnerClasses = function
	| AttributeInnerClasses icl::_ -> List.rev_map low2high_innerclass icl
	| _::l -> find_InnerClasses l
	| [] -> []
      in find_InnerClasses cl.j_attributes
    and my_other_attributes =
      low2high_other_attributes consts
	(List.filter
	   (function
	      | AttributeSignature _ | AttributeSourceFile _ 
	      | AttributeDeprecated | AttributeInnerClasses _ -> false
	      | _ -> true)
	   cl.j_attributes);
    in
      if is_interface
      then
	begin
	  (try assert is_abstract
	   with _ -> raise (Class_structure_error "Class file with their `AccInterface flag set must also have their `AccAbstract flag set."));
	  (try assert (cl.j_super = Some java_lang_object) with _ -> raise (Class_structure_error "The super-class of interfaces must be java.lang.Object."));
	  (*if accsuper
	    then prerr_endline "Warning : the ACC_SUPER flag has no meaning for a class and will be discard.";*)
	  if is_enum || is_synthetic
	  then raise (Class_structure_error "Class file with their `AccInterface flag set must not have their `AccEnum or `AccSynthetic flags set.");
	  let (init,methods) =
	    match
	      List.partition
		(fun m ->
		   m.m_name = clinit_signature.ms_name
		    && fst m.m_descriptor = clinit_signature.ms_parameters)
		cl.j_methods
	    with
	      | [m],others -> Some (low2high_cmethod consts clinit_signature m),others
	      | [],others -> None, others
	      | _ -> raise (Class_structure_error "has more than one class initializer <clinit>")
	  in
	    `Interface {
	      i_name = my_name;
	      i_version = my_version;
	      i_access = my_access;
	      i_interfaces = my_interfaces;
	      i_consts = DynArray.to_array consts;
	      i_sourcefile = my_sourcefile;
	      i_deprecated = my_deprecated;
	      i_signature = my_signature;
	      i_inner_classes = my_inner_classes;
	      i_other_attributes = my_other_attributes;
	      i_initializer = init;
	      i_annotation = is_annotation;
	      i_other_flags = flags;
	      i_fields = List.fold_left
		(fun m f ->
		   let fs = {fs_name=f.f_name;fs_type=f.f_descriptor} in
		     if !debug > 0 && FieldMap.mem fs m
		     then
		       prerr_endline
			 ("Warning: 2 fields have been found with the same signature ("
			  ^JDumpBasics.value_signature fs.fs_type^" "^ fs.fs_name^")");
		     FieldMap.add
		       fs
		       (try low2high_ifield consts fs f
			with Class_structure_error msg ->
			  raise (Class_structure_error ("field " ^JDumpBasics.signature f.f_name (SValue f.f_descriptor)^": "^msg)))
		       m)
		FieldMap.empty
		cl.j_fields;
	      i_methods = List.fold_left
		(fun map meth ->
		   let ms =
		     {ms_name=meth.m_name;
		      ms_parameters = fst meth.m_descriptor;
		      ms_return_type = snd meth.m_descriptor;}
		   in
		     if !debug > 0 && MethodMap.mem ms map
		     then
		       prerr_endline
			 ("2 methods have been found with the same signature ("^ms.ms_name
			  ^"("^ String.concat ", " (List.map (JDumpBasics.value_signature) ms.ms_parameters) ^"))");
		     MethodMap.add
		       ms
		       (try low2high_amethod consts ms meth
			with Class_structure_error msg ->
			  let sign = JDumpBasics.signature meth.m_name (SMethod meth.m_descriptor)
			  in raise (Class_structure_error ("in class "^JDumpBasics.class_name my_name^": method " ^sign^": "^msg)))
		       map)
		MethodMap.empty
		methods;
	    }
	end
      else
	begin
	  if is_annotation
	  then raise (Class_structure_error "Class file with their `AccAnnotation flag set must also have their `AccInterface flag set.");
	  let my_methods =
	    try low2high_methods consts cl
	    with
	      | Class_structure_error msg -> raise (Class_structure_error ("in class "^JDumpBasics.class_name my_name^": "^msg))
	  and my_fields =
	    List.fold_left
	      (fun m f ->
		 let fs = {fs_name=f.f_name;fs_type=f.f_descriptor} in
		   if !debug > 0 && FieldMap.mem fs m
		   then
		     prerr_endline
		       ("Warning: 2 fields have been found with the same signature ("
			^JDumpBasics.value_signature fs.fs_type^" "^fs.fs_name ^")");
		   FieldMap.add
		     fs
		     (try low2high_cfield consts fs f
		      with Class_structure_error msg -> raise (Class_structure_error ("in class "^JDumpBasics.class_name my_name^": in field " ^JDumpBasics.signature f.f_name (SValue f.f_descriptor)^": "^msg)))
		     m)
	      FieldMap.empty
	      cl.j_fields
	  in
	    `Class {
	      c_name = my_name;
	      c_version = my_version;
	      c_super_class = cl.j_super;
	      c_final = is_final;
	      c_abstract = is_abstract;
	      c_access = my_access;
	      c_synthetic = is_synthetic;
	      c_enum = is_enum;
	      c_other_flags = flags;
	      c_interfaces = my_interfaces;
	      c_consts = DynArray.to_array consts;
	      c_sourcefile = my_sourcefile;
	      c_deprecated = my_deprecated;
	      c_signature = my_signature;
	      c_inner_classes = my_inner_classes;
	      c_other_attributes = my_other_attributes;
	      c_fields = my_fields;
	      c_methods = my_methods;
	    }
	end
