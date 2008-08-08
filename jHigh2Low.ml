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

open JClassLow
open JClass

let deprecated_to_attribute = function
  | false -> []
  | true -> [AttributeDeprecated]

let synthetic_to_attribute = function
  | false -> []
  | true -> [AttributeSynthetic]

let h2l_other_attributes l = List.map (fun (n,a) -> AttributeUnknown (n,a)) l

let h2l_attributes a =
  deprecated_to_attribute a.deprecated
  @ synthetic_to_attribute a.synthetic
  @ h2l_other_attributes a.other

let access2flags = function
  | `Default -> []
  | `Public -> [`AccPublic]
  | `Private -> [`AccPrivate]
  | `Protected -> [`AccProtected]

let h2l_inner_classes = function
  | [] -> []
  | icl ->
      let h2l_ic ic =
	let inner_class_info = ic.ic_class_name
	and outer_class_info = ic.ic_outer_class_name
	and inner_name = ic.ic_source_name
	and inner_class_access_flags =
	  (access2flags ic.ic_access)
	  @ (if ic.ic_static then [`AccStatic] else [])
	  @ (if ic.ic_final then [`AccFinal] else [])
	  @ (if ic.ic_synthetic then [`AccSynthetic] else [])
	  @ (if ic.ic_annotation then [`AccAnnotation] else [])
	  @ (if ic.ic_enum then [`AccEnum] else [])
	  @ (List.map (fun i -> `AccRFU i) ic.ic_other_flags)
	  @ (match ic.ic_type with
	    | `Interface -> [`AccAbstract;`AccInterface]
	    | `Abstract -> [`AccAbstract]
	    | `ConcreteClass -> [])
	in (inner_class_info,outer_class_info,inner_name,inner_class_access_flags)
      in
	[AttributeInnerClasses (List.map h2l_ic icl)]

let h2l_code2attribute consts = function
  | Native -> []
  | Java code ->
      let code =
	{JClassLow.c_max_stack = code.c_max_stack;
	 JClassLow.c_max_locals = code.c_max_locals;
	 JClassLow.c_code = JInstruction.code2opcodes consts code.c_code;
	 JClassLow.c_exc_tbl = code.c_exc_tbl;
	 JClassLow.c_attributes =
	    (match code.c_stack_map with
	      | Some sm -> [AttributeStackMap sm]
	      | None -> [])
	    @ (match code.c_line_number_table with
	      | Some lnt -> [AttributeLineNumberTable lnt]
	      | None -> [])
	    @ (match code.c_local_variable_table with
	      | Some lvt -> [AttributeLocalVariableTable lvt]
	      | None -> [])
	    @ h2l_other_attributes code.c_attributes;
	}
      in [AttributeCode code]

let h2l_cfield _consts f =
  {f_name = f.cf_signature.fs_name;
   f_descriptor = f.cf_signature.fs_type;
   f_flags =
      (if f.cf_transient then [`AccTransient] else [])
      @ (match f.cf_kind with
	| Final -> [`AccFinal]
	| Volatile -> [`AccVolatile]
	| NotFinal -> [])
      @ (if f.cf_static then [`AccStatic] else [])
      @ (if f.cf_synthetic then [`AccSynthetic] else [])
      @ (if f.cf_enum then [`AccEnum] else [])
      @ (List.map (fun i -> `AccRFU i) f.cf_other_flags)
      @ (access2flags f.cf_access);
   f_attributes =
      (match f.cf_value with Some c -> [AttributeConstant c] | None -> [] )
      @ (h2l_attributes f.cf_attributes);
  }

let h2l_ifield _consts f =
  {f_name = f.if_signature.fs_name;
   f_descriptor = f.if_signature.fs_type;
   f_flags = 
      (if f.if_synthetic then [`AccSynthetic] else [])
      @ (List.map (fun i -> `AccRFU i) f.if_other_flags)
      @ [`AccPublic;`AccStatic;`AccFinal];
   f_attributes =
      (match f.if_value with Some c -> [AttributeConstant c] | None -> [] )
      @ (h2l_attributes f.if_attributes);
  }

let h2l_cmethod consts m =
  let code = h2l_code2attribute consts m.cm_implementation
  in
    {m_name = m.cm_signature.ms_name;
     m_descriptor =
	(m.cm_signature.ms_parameters, m.cm_signature.ms_return_type);
     m_flags =
	(if m.cm_static then [`AccStatic] else [])
	@ (if m.cm_final then [`AccFinal] else [])
	@ (if m.cm_synchronized then [`AccSynchronized] else [])
	@ (if m.cm_strict then [`AccStrict] else [])
	@ (match m.cm_implementation with Native -> [`AccNative] |_ -> [])
	@ (if m.cm_bridge then [`AccBridge] else [])
	@ (if m.cm_varargs then [`AccVarArgs] else [])
	@ (if m.cm_synthetic then [`AccSynthetic] else [])
	@ (List.map (fun i -> `AccRFU i) m.cm_other_flags)
	@ (access2flags m.cm_access);
     m_attributes =
	(match m.cm_exceptions with
	  | [] -> []
	  | l -> [AttributeExceptions l])
	@ code
	@ h2l_attributes m.cm_attributes;
    }

let h2l_amethod _consts m =
  {m_name = m.am_signature.ms_name;
   m_descriptor = (m.am_signature.ms_parameters, m.am_signature.ms_return_type);
   m_flags =
      (if m.am_bridge then [`AccBridge] else [])
      @ (if m.am_varargs then [`AccVarArgs] else [])
      @ (if m.am_synthetic then [`AccSynthetic] else [])
      @ (List.map (fun i -> `AccRFU i) m.am_other_flags)
      @ (`AccAbstract::access2flags m.am_access);
   m_attributes =
      (match m.am_exceptions with
	| [] -> []
	| l -> [AttributeExceptions l])
      @ h2l_attributes m.am_attributes;
  }

let h2l_acmethod consts = function
  | AbstractMethod m -> h2l_amethod consts m
  | ConcreteMethod m -> h2l_cmethod consts m

let h2l_concretemethods consts c' mm =
  {c' with
    j_methods = MethodMap.fold (fun _fs f l -> h2l_cmethod consts f::l) mm [];
  }

let h2l_methods consts c' mm =
  {c' with
    j_methods = MethodMap.fold (fun _fs f l -> h2l_acmethod consts f::l) mm [];
  }


let high2low_class c =
  let consts = DynArray.of_array c.c_consts in
  let c' =
    {j_name = c.c_name;
     j_super = c.c_super_class;
     j_interfaces = c.c_interfaces;
     j_consts = c.c_consts; (*will be updated later on*)
     j_flags =
	(if c.c_abstract then [`AccAbstract] else [])
	@ (if c.c_synthetic then [`AccSynthetic] else [])
	@ (if c.c_enum then [`AccEnum] else [])
	@ (if c.c_final then [`AccFinal] else [])
	@ (List.map (fun i -> `AccRFU i) c.c_other_flags)
	@ (`AccSuper::
	     match c.c_access with
	       | `Default -> []
	       | `Public -> [`AccPublic]);
     j_fields = FieldMap.fold (fun _fs f l -> h2l_cfield consts f::l) c.c_fields [];
     j_methods = []; (*will be set later on*)
     j_attributes =
	(deprecated_to_attribute c.c_deprecated)
	@ (h2l_inner_classes c.c_inner_classes)
	@ (match c.c_sourcefile with None -> [] | Some s -> [AttributeSourceFile s])
	@ (h2l_other_attributes c.c_other_attributes);
    } in
  let c'= h2l_methods consts c' c.c_methods
  in {c' with j_consts = DynArray.to_array consts}

let high2low_interface (c:jinterface) =
  let consts = DynArray.of_array c.i_consts in
  let c' =
    {j_name = c.i_name;
     j_super = Some JBasics.java_lang_object;
     j_interfaces = c.i_interfaces;
     j_consts = c.i_consts; (*will be updated later on*)
     j_flags =
	(if c.i_annotation then [`AccAnnotation] else [])
	@ (List.map (fun i -> `AccRFU i) c.i_other_flags)
	@ `AccInterface::`AccAbstract::
	     (match c.i_access with
		| `Default -> []
		| `Public -> [`AccPublic]);
     j_fields =
	FieldMap.fold (fun _fs f l -> h2l_ifield consts f::l) c.i_fields [];
     j_methods =
	(match c.i_initializer with None -> [] | Some m -> [h2l_cmethod consts m])
	@ MethodMap.fold (fun _ms m l -> h2l_amethod consts m::l) c.i_methods [];
     j_attributes =
	(deprecated_to_attribute c.i_deprecated)
	@ (h2l_inner_classes c.i_inner_classes)
	@ (match c.i_sourcefile with None -> [] | Some s -> [AttributeSourceFile s])
	@ (h2l_other_attributes c.i_other_attributes);
    } in
    {c' with j_consts = DynArray.to_array consts}

let high2low = function
  | `Interface i -> high2low_interface i
  | `Class c -> high2low_class c
