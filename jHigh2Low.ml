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
  | `Public -> [AccPublic]
  | `Private -> [AccPrivate]
  | `Protected -> [AccProtected]

let h2l_inner_classes = function
  | [] -> []
  | icl ->
      let h2l_ic ic =
	let inner_class_info = ic.ic_class_name
	and outer_class_info = ic.ic_outer_class_name
	and inner_name = ic.ic_source_name
	and inner_class_access_flags =
	  (access2flags ic.ic_access)
	  @ (if ic.ic_static then [AccStatic] else [])
	  @ (if ic.ic_final then [AccFinal] else [])
	  @ (match ic.ic_type with
	    | `Interface -> [AccAbstract;AccInterface]
	    | `Abstract -> [AccAbstract]
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
	    (match code.stack_map with
	      | Some sm -> [AttributeStackMap sm]
	      | None -> [])
	    @ (match code.line_number_table with
	      | Some lnt -> [AttributeLineNumberTable lnt]
	      | None -> [])
	    @ (match code.local_variable_table with
	      | Some lvt -> [AttributeLocalVariableTable lvt]
	      | None -> [])
	    @ h2l_other_attributes code.c_attributes;
	}
      in [AttributeCode code]

let h2l_cfield consts fs f =
  {f_name = fs.fs_name;
   f_signature = fs.fs_type;
   f_flags = 
      (if f.cf_transient then [AccTransient] else [])
      @ (match f.cf_type with 
	| Final -> [AccFinal]
	| Volatile -> [AccVolatile]
	| NotFinal -> [])
      @ (if f.cf_static then [AccStatic] else [])
      @ (access2flags f.cf_access);
   f_attributes = 
      (match f.cf_value with Some c -> [AttributeConstant c] | None -> [] )
      @ (h2l_attributes f.cf_attributes);
  }

let h2l_ifield consts fs f =
  {f_name = fs.fs_name;
   f_signature = fs.fs_type;
   f_flags = [AccPublic;AccStatic;AccFinal];
   f_attributes = 
      (match f.if_value with Some c -> [AttributeConstant c] | None -> [] )
      @ (h2l_attributes f.if_attributes);
  }

let h2l_cmethod consts ms m =
  let code = h2l_code2attribute consts m.implementation 
  in 
    {m_name = ms.ms_name;
     m_signature = (ms.ms_parameters, m.cm_return_type);
     m_flags =
	(if m.cm_static then [AccStatic] else [])
	@ (if m.cm_final then [AccFinal] else [])
	@ (if m.cm_synchronized then [AccSynchronized] else [])
	@ (if m.cm_strict then [AccStrict] else [])
	@ (match m.implementation with Native -> [AccNative] |_ -> [])
	@ (access2flags m.cm_access);
     m_code = 
	begin
	  match code with
	    | [AttributeCode c] -> Some c
	    | _ -> None
	end;
     m_attributes =
	(match m.cm_exceptions with
	  | [] -> [] 
	  | l -> [AttributeExceptions l])
	@ code
	@ h2l_attributes m.cm_attributes;
    }

let h2l_amethod consts ms m = 
  {m_name = ms.ms_name;
   m_signature = (ms.ms_parameters, m.am_return_type);
   m_flags = AccAbstract::(access2flags m.am_access);
   m_code = None;
   m_attributes =
	(match m.am_exceptions with
	  | [] -> [] 
	  | l -> [AttributeExceptions l])
      @ h2l_attributes m.am_attributes;
  }

let h2l_acmethod consts ms = function
  | AbstractMethod m -> h2l_amethod consts ms m
  | ConcreteMethod m -> h2l_cmethod consts ms m

let h2l_concreteclass consts c c' =
  {c' with 
    j_super = c.cc_super_class;
    j_flags = AccSynchronized::(if c.cc_final then AccFinal::c'.j_flags else c'.j_flags);
    j_methods = MethodMap.fold (fun fs f l -> h2l_cmethod consts fs f::l) c.cc_methods [];
    j_fields = FieldMap.fold (fun fs f l -> h2l_cfield consts fs f::l) c.cc_fields [];
  }

let h2l_abstractclass consts c c' =
  {c' with 
    j_super = c.ac_super_class;
    j_flags = AccSynchronized::AccAbstract::c'.j_flags;
    j_methods = MethodMap.fold (fun fs f l -> h2l_acmethod consts fs f::l) c.ac_methods [];
    j_fields = FieldMap.fold (fun fs f l -> h2l_cfield consts fs f::l) c.ac_fields [];
  }

let h2l_interface consts c c' =
  let clinit_signature = {ms_name="<clinit>";ms_parameters=[]} in
  {c' with 
    j_super = Some ["java";"lang";"Object"];
    j_flags = AccInterface::AccAbstract::c'.j_flags;
    j_methods = 
      (match c.i_initializer with None -> [] | Some m -> [h2l_cmethod consts clinit_signature m])
      @ MethodMap.fold (fun ms m l -> h2l_amethod consts ms m::l) c.i_methods [];
    j_fields = FieldMap.fold (fun fs f l -> h2l_ifield consts fs f::l) c.i_fields [];
  }

let high2low_class c =
  let consts = DynArray.of_array c.c_consts in
  let c' = 
    {j_name = c.name;
     j_super = None; (*will be set later on*)
     j_interfaces = c.interfaces;
     j_consts = c.c_consts; (*will be updated later on*)
     j_flags = access2flags c.c_access; (*will be updated later on*)
     j_fields = []; (*will be set later on*)
     j_methods = []; (*will be set later on*)
     j_attributes =  (*will be updated later on*)
	(deprecated_to_attribute c.c_deprecated)
	@ (h2l_inner_classes c.inner_classes)
	@ (match c.sourcefile with None -> [] | Some s -> [AttributeSourceFile s])
	@ (h2l_other_attributes c.c_other_attributes);
    } in
  let c'=match c.class_file_type with
    | ConcreteClass c -> h2l_concreteclass consts c c'
    | AbstractClass c -> h2l_abstractclass consts c c'
    | Interface c -> h2l_interface consts c c'
  in {c' with j_consts = DynArray.to_array consts}
