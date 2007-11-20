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
open JClassLow
open JClass

let rec flags2access = function
  | AccPublic::l ->
      if List.exists (fun a -> a = AccPrivate || a= AccProtected) l
      then raise (Class_structure_error "Access flags Public and Private or Protected cannot be set at the same time")
      else `Public
  | AccPrivate::l ->
      if List.exists (fun a -> a = AccPublic || a= AccProtected) l
      then raise (Class_structure_error "Access flags Private and Public or Protected cannot be set at the same time")
      else `Private
  | AccProtected::l ->
      if List.exists (fun a -> a = AccPrivate || a= AccPublic) l
      then raise (Class_structure_error "Access flags Protected and Private or Public cannot be set at the same time")
      else `Protected
  | _::l -> flags2access l
  | [] -> `Default

(** convert a list of  attributes to a list of couple of string, as for AttributeUnknown. *)
let low2high_other_attributes consts : JClassLow.attribute list ->  (string*string) list =
  List.map
    (function
       | AttributeUnknown (name, contents) -> name, contents
       | a -> 
	   let (name,contents) = JUnparse.unparse_attribute_to_strings consts a
	   in
	     prerr_endline ("Warning: unexpected attribute found: "^name);
	     name,contents)

(** convert a list of  attributes to an [attributes] structure. *)
let low2high_attributes consts (al:JClassLow.attribute list) :attributes =
  {synthetic = List.exists ((=)AttributeSynthetic) al;
   deprecated = List.exists ((=)AttributeDeprecated) al;
   other =
      low2high_other_attributes consts
	(List.filter (fun a -> a <> AttributeDeprecated && a <> AttributeSynthetic) al);
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

let low2high_cfield consts = function f ->
  let is_static = List.exists ((=) AccStatic) f.f_flags in
  let (cst,other_att) =
    List.partition (function AttributeConstant _ -> true | _ -> false) f.f_attributes in
  let (cst,other_att) =
    match cst with
      | [] -> None,other_att
      | AttributeConstant c::oc when not is_static ->  (* it seems quite common *)
	  (* prerr_endline "A non-static field has been found with a constant value associated."; *)
	  None, (AttributeConstant c::(oc@other_att))
      | AttributeConstant c::[] ->
	  Some c, other_att
      | AttributeConstant c::oc ->
	  prerr_endline "A field contains more than one constant value associated.";
	  Some c, (oc@other_att)
      | _ -> assert false
  in
    {
      cf_access = flags2access f.f_flags;
      cf_static = is_static;
      cf_type =
	begin
	  let rec find_field_type = function
	    | AccFinal::_ -> Final
	    | AccVolatile::_ -> Volatile
	    | _::l -> find_field_type l
	    | [] -> NotFinal
	  in find_field_type f.f_flags
	end;
      cf_value = cst;
      cf_transient = List.exists ((=)AccTransient) f.f_flags;
      cf_attributes =
	low2high_attributes consts other_att;
    }

let low2high_ifield consts = function f ->
  if not
    (List.exists ((=)AccPublic) f.f_flags
      && List.exists ((=)AccStatic) f.f_flags
      && List.exists ((=)AccFinal) f.f_flags) ||
    [] <> (List.filter (fun a -> a<>AccPublic && a<>AccStatic && a<>AccFinal) f.f_flags)
  then raise (Class_structure_error "A field of an interface must have as only flag : Public, Static and Final.")
  else
    {
      if_value =
	begin
	  let rec find_Constant = function
	    | AttributeConstant c::_ -> Some c
	    | _::l -> find_Constant l
	    | [] -> None
	  in find_Constant f.f_attributes
	end;
      if_attributes =
	low2high_attributes consts
	  (List.filter
	      (function AttributeConstant _ -> false| _ -> true)
	      f.f_attributes);
    }

let low2high_amethod consts = function m ->
  begin
    if List.exists
      (fun a -> a=AccFinal || a=AccNative || a=AccPrivate || a=AccStatic || a=AccStrict || a=AccSynchronized)
      m.m_flags
    then
      match
	List.hd
	  (List.filter
	      (fun a -> a=AccFinal || a=AccNative || a=AccPrivate || a=AccStatic || a=AccStrict || a=AccSynchronized)
	      m.m_flags)
      with
	| AccFinal -> prerr_endline "A method should not have its AccAbstract and AccFinal flags both set."
	| AccNative -> prerr_endline "A method should not have its AccAbstract and AccNative flags both set."
	| AccPrivate -> prerr_endline "A method should not have its AccAbstract and AccPrivate flags both set."
	| AccStatic -> prerr_endline "A method should not have its AccAbstract and AccStatic flags both set."
	| AccStrict -> prerr_endline "A method should not have its AccAbstract and AccStrict flags both set."
	| AccSynchronized -> prerr_endline "A method should not have its AccAbstract and AccSynchronized flags both set."
	| _ -> raise (Class_structure_error ("If a method has its ACC_ABSTRACT flag set it may not have any"
				      ^ "of its ACC_FINAL, ACC_NATIVE, ACC_PRIVATE, ACC_STATIC, "
				      ^ "ACC_STRICT, or ACC_SYNCHRONIZED flags set."))
  end;
  {
    am_access =
      begin
	match flags2access m.m_flags with
	  | `Private -> raise (Class_structure_error "Abstract method cannot be private")
	  | `Default -> `Default
	  | `Protected -> `Protected
	  | `Public -> `Public
      end;
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
    am_return_type = snd m.m_signature
  }

let low2high_cmethod consts = function m ->
  if m.m_name = "<init>" &&
    List.exists (fun a -> a=AccStatic || a=AccFinal || a=AccSynchronized || a=AccNative || a=AccAbstract)
    m.m_flags
  then raise (Class_structure_error ("A specific instance initialization method may have at most "
			      ^ "one of its ACC_PRIVATE, ACC_PROTECTED, and ACC_PUBLIC flags set "
			      ^ "and may also have its ACC_STRICT flag set."))
  else
    if List.exists ((=)AccAbstract) m.m_flags
    then raise (Class_structure_error "Non abstract class cannot have abstract methods.")
  else
    {
      cm_static = List.exists ((=) AccStatic) m.m_flags;
      cm_final = List.exists ((=) AccFinal) m.m_flags;
      cm_synchronized = List.exists  ((=) AccSynchronized) m.m_flags;
      cm_strict =  List.exists ((=) AccStrict) m.m_flags;
      cm_access = flags2access m.m_flags;
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
      implementation =
	begin
	  let rec find_Code = function
	    | AttributeCode c::l ->
		if List.exists (function AttributeCode _ -> true| _-> false) l
		then raise (Class_structure_error "Only one Code attribute is allowed in a method.")
		else
		  if List.exists ((=) AccNative) m.m_flags
		  then
		    (prerr_endline "A method declared as Native with code has been found.";
		     Native)
		else Java (low2high_code consts c)
	    | _::l -> find_Code l
	    | [] ->
		if List.exists ((=) AccNative) m.m_flags
		then Native
		else
		  (prerr_endline "A method not declared as Native, nor Abstract has been found without code.";
		   Native)
	  in find_Code m.m_attributes
	end;
      cm_return_type = snd m.m_signature
    }

let low2high_acmethod consts = function m ->
  if List.exists ((=)AccAbstract) m.m_flags
  then AbstractMethod (low2high_amethod consts m)
  else ConcreteMethod (low2high_cmethod consts m)


let low2high_concrete_class consts = function nc ->
  {
    cc_final = List.exists ((=)AccFinal) nc.j_flags;
    cc_super_class = nc.j_super;
    cc_fields = List.fold_left
      (fun m f ->
	FieldMap.add
	  {fs_name=f.f_name;fs_type=f.f_signature}
	  (try low2high_cfield consts f
	  with Class_structure_error msg -> raise (Class_structure_error ("in field " ^JDumpBasics.signature f.f_name (SValue f.f_signature)^": "^msg)))
	  m)
      FieldMap.empty
      nc.j_fields;
    cc_methods = List.fold_left
      (fun map meth ->
	MethodMap.add
	  {ms_name=meth.m_name;
	   ms_parameters=fst meth.m_signature}
	  (try low2high_cmethod consts meth
	  with Class_structure_error msg -> raise (Class_structure_error ("in method " ^JDumpBasics.signature meth.m_name (SMethod meth.m_signature)^": "^msg)))
	  map)
      MethodMap.empty
      nc.j_methods;
  }

let low2high_abstract_class consts = function ac ->
  {
    ac_super_class = ac.j_super;
    ac_fields = List.fold_left
      (fun m f ->
	FieldMap.add
	  {fs_name=f.f_name;fs_type=f.f_signature}
	  (try low2high_cfield consts f
	  with Class_structure_error msg -> raise (Class_structure_error ("in field " ^JDumpBasics.signature f.f_name (SValue f.f_signature)^": "^msg)))
	  m)
      FieldMap.empty
      ac.j_fields;
    ac_methods = List.fold_left
      (fun map meth ->
	MethodMap.add
	  {ms_name=meth.m_name;
	   ms_parameters=fst meth.m_signature}
	  (try low2high_acmethod consts meth
	  with Class_structure_error msg -> raise (Class_structure_error ("in method " ^JDumpBasics.signature meth.m_name (SMethod meth.m_signature)^": "^msg)))
	  map)
      MethodMap.empty
      ac.j_methods;
  }

let low2high_innerclass = function
    (inner_class_info,outer_class_info,inner_name,inner_class_access_flags) ->
      {
	ic_class_name = inner_class_info;
	ic_outer_class_name = outer_class_info;
	ic_source_name = inner_name;
	ic_access = flags2access inner_class_access_flags;
	ic_static = List.exists ((=)AccStatic) inner_class_access_flags;
	ic_final = List.exists ((=)AccFinal) inner_class_access_flags;
	ic_type =
	  if List.exists ((=)AccInterface) inner_class_access_flags
	  then `Interface
	  else if List.exists ((=)AccAbstract) inner_class_access_flags
	  then `Abstract
	  else `ConcreteClass
      }


let low2high_class cl =
  if cl.j_super = None && cl.j_name <> java_lang_object
  then raise (Class_structure_error "Only java.lang.Object is allowed not to have a super-class.")
  else
    let consts = DynArray.of_array cl.j_consts in
    let my_name = cl.j_name in
    let my_access =
      if List.exists ((=)AccPublic) cl.j_flags
      then `Public
      else `Default
    and my_interfaces = cl.j_interfaces
    and my_sourcefile =
      let rec find_SourceFile = function
	| AttributeSourceFile s::_ -> Some s
	| _::l -> find_SourceFile l
	| [] -> None
      in find_SourceFile cl.j_attributes
    and my_deprecated = List.exists ((=)AttributeDeprecated) cl.j_attributes
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
	      | AttributeSourceFile _ | AttributeDeprecated | AttributeInnerClasses _ -> false
	      | _ -> true)
	    cl.j_attributes);
    in
      if List.exists ((=)AccInterface) cl.j_flags
      then
	begin
	  if not (List.exists ((=)AccAbstract) cl.j_flags)
	  then
	    raise (Class_structure_error "Class file with their AccInterface flags set must also have their AccAbstract flags set.")
	  else
	    begin
	      if List.exists ((=)AccFinal) cl.j_flags
	      then raise (Class_structure_error "An interface cannot be final.")
	      else
		if cl.j_super <> Some java_lang_object
		then raise (Class_structure_error "The super-class of interfaces must be java.lang.Object.");
	      let (init,methods) =
		match List.partition
		  (fun m -> m.m_name = clinit_signature.ms_name && fst m.m_signature = clinit_signature.ms_parameters) cl.j_methods with
		    | [m],others -> Some (low2high_cmethod consts m),others
		    | [],others -> None, others
		    | _ -> raise (Class_structure_error "has more than one class initializer <clinit>")
	      in
		`Interface {
		  i_name = my_name;
		  i_access = my_access;
		  i_interfaces = my_interfaces;
		  i_consts = DynArray.to_array consts;
		  i_sourcefile = my_sourcefile;
		  i_deprecated = my_deprecated;
		  i_inner_classes = my_inner_classes;
		  i_other_attributes = my_other_attributes;
		  i_super = java_lang_object;
		  i_initializer = init;
		  i_fields = List.fold_left
		    (fun m f ->
		      FieldMap.add
			{fs_name=f.f_name;fs_type=f.f_signature}
			(try low2high_ifield consts f
			  with Class_structure_error msg -> raise (Class_structure_error ("field " ^JDumpBasics.signature f.f_name (SValue f.f_signature)^": "^msg)))
			m)
		    FieldMap.empty
		    cl.j_fields;
		  i_methods = List.fold_left
		    (fun map meth ->
		      MethodMap.add
			{ms_name=meth.m_name;
			 ms_parameters=fst meth.m_signature}
			(try low2high_amethod consts meth
			  with Class_structure_error msg -> raise (Class_structure_error ("method " ^JDumpBasics.signature meth.m_name (SMethod meth.m_signature)^": "^msg)))
			map)
		    MethodMap.empty
		    methods;
		}
	    end
	end
      else
	let my_class_file_type =
	  try
	    if List.exists ((=)AccAbstract) cl.j_flags
	    then
	      if List.exists ((=)AccFinal) cl.j_flags
	      then raise (Class_structure_error "An abstract class cannot be final.")
	      else AbstractClass (low2high_abstract_class consts cl)
	    else
	      ConcreteClass (low2high_concrete_class consts cl)
	  with
	    | Class_structure_error msg -> raise (Class_structure_error ("in class "^JDumpBasics.class_name my_name^": "^msg))
	in
	  `Class {
	    c_name = my_name;
	    c_access = my_access;
	    c_interfaces = my_interfaces;
	    c_consts = DynArray.to_array consts;
	    c_sourcefile = my_sourcefile;
	    c_deprecated = my_deprecated;
	    c_inner_classes = my_inner_classes;
	    c_other_attributes = my_other_attributes;
	    c_class_file_type = my_class_file_type;
	  }
