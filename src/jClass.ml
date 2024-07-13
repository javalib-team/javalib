(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.
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

(* This implementation is only to provide MapFieldSignature and MapMethodSignature.*)

open JBasics

(* Visibility modifiers. *)
type access = [ `Default | `Public | `Private | `Protected ]

(* Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  deprecated : bool;
  other : (string * string) list;
}

type method_parameter_attribute = {
  mp_name : string option;
  mp_final : bool;
  mp_synthetic : bool;
  mp_mandated : bool;
}

type visibility = RTVisible | RTInvisible

type method_annotations = {
  ma_global : (annotation * visibility) list;
  ma_parameters : (annotation * visibility) list list;
}

(* {2 Fields of classes and interfaces.} *)
(*******************************)

type field_kind = NotFinal | Final | Volatile

type constant_attribute =
  [ `Long of int64
  | `Float of float
  | `Double of float
  | `Int of int32
  | `String of jstr ]

type class_field = {
  cf_signature : field_signature;
  cf_class_signature : class_field_signature;
  cf_generic_signature : JSignature.fieldTypeSignature option;
  cf_access : access;
  cf_static : bool;
  cf_synthetic : bool;
  cf_enum : bool;
  cf_kind : field_kind;
  cf_value : constant_attribute option; (* Only if the field is static final. *)
  cf_transient : bool;
  cf_annotations : (annotation * visibility) list;
  cf_other_flags : int list;
  cf_attributes : attributes;
}

(* Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_signature : field_signature;
  if_class_signature : class_field_signature;
  if_generic_signature : JSignature.fieldTypeSignature option;
  if_synthetic : bool;
  if_value : constant_attribute option;
      (* a constant_attribute is not mandatory,
         especially as it can be initialized by
         the class initializer <clinit>. *)
  if_annotations : (annotation * visibility) list;
  if_other_flags : int list;
  if_attributes : attributes;
}

type any_field = InterfaceField of interface_field | ClassField of class_field

(* {2 Methods of classes and interfaces.} *)
(******************************************)

type 'a implementation = Native | Java of 'a Lazy.t

(* l'attribut final n'a pas vraiment de sens pour une méthode
   statique, mais c'est autorisé dans la spec JVM. *)
type 'a concrete_method = {
  cm_signature : method_signature;
  cm_class_method_signature : class_method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access : access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge : bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_parameters : method_parameter_attribute list;
  cm_annotations : method_annotations;
  cm_implementation : 'a implementation;
}

type abstract_method = {
  am_signature : method_signature;
  am_class_method_signature : class_method_signature;
  am_access : [ `Public | `Protected | `Default | `Private ];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge : bool;
  am_varargs : bool;
  am_synthetic : bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  am_parameters : method_parameter_attribute list;
  am_annotations : method_annotations;
  am_annotation_default : element_value option;
}

(* {2 Classes and interfaces.} *)
(***************************)

type 'a jmethod =
  | AbstractMethod of abstract_method
  | ConcreteMethod of 'a concrete_method

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_synthetic : bool;
  ic_annotation : bool;
  ic_enum : bool;
  ic_other_flags : int list;
  ic_type : [ `ConcreteClass | `Abstract | `Interface ];
}

type 'a jclass = {
  c_name : class_name;
  c_version : version;
  c_access : [ `Public | `Default ];
  c_final : bool;
  c_abstract : bool;
  c_super_class : class_name option;
  c_generic_signature : JSignature.classSignature option;
  c_fields : class_field FieldMap.t;
  c_interfaces : class_name list;
  c_consts : constant array;
      (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  c_source_debug_extention : string option;
  c_inner_classes : inner_class list;
  c_synthetic : bool;
  c_enum : bool;
  c_annotations : (annotation * visibility) list;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : 'a jmethod MethodMap.t;
}

(* Interfaces cannot be final. Their super class is [java.lang.Object].*)
type 'a jinterface = {
  i_name : class_name;
  i_version : version;
  i_access : [ `Public | `Default ];
  i_interfaces : class_name list;
  i_generic_signature : JSignature.classSignature option;
  i_consts : constant array;
      (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  i_inner_classes : inner_class list;
  i_annotation : bool;
  i_annotations : (annotation * visibility) list;
  i_other_attributes : (string * string) list;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : 'a jmethod MethodMap.t;
}

type 'a interface_or_class = JInterface of 'a jinterface | JClass of 'a jclass

let get_name = function JInterface i -> i.i_name | JClass c -> c.c_name
let get_consts = function JInterface i -> i.i_consts | JClass c -> c.c_consts
let get_access = function JInterface i -> i.i_access | JClass c -> c.c_access

let get_sourcefile = function
  | JInterface i -> i.i_sourcefile
  | JClass c -> c.c_sourcefile

let is_deprecated = function
  | JInterface i -> i.i_deprecated
  | JClass c -> c.c_deprecated

let is_final = function JInterface _ -> false | JClass c -> c.c_final

let get_inner_classes = function
  | JInterface i -> i.i_inner_classes
  | JClass c -> c.c_inner_classes

let get_other_attributes = function
  | JInterface i -> i.i_other_attributes
  | JClass c -> c.c_other_attributes

let get_initializer = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } -> (
      try
        match MethodMap.find clinit_signature mmap with
        | ConcreteMethod m -> Some m
        | AbstractMethod _ ->
            raise
              (Class_structure_error "A class initializer cannot be abstract")
      with Not_found -> None)

let get_other_flags = function
  | JInterface i -> i.i_other_flags
  | JClass c -> c.c_other_flags

let get_method ioc ms =
  match ioc with
  | JInterface i -> MethodMap.find ms i.i_methods
  | JClass c -> MethodMap.find ms c.c_methods

let get_concrete_method ioc ms =
  match ioc with
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } -> (
      match MethodMap.find ms mmap with
      | ConcreteMethod cm -> cm
      | AbstractMethod _ -> raise Not_found)

let get_methods = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } -> mmap

let get_concrete_methods = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.fold
        (fun ms m mmap ->
          match m with
          | AbstractMethod _ -> mmap
          | ConcreteMethod cm -> MethodMap.add ms cm mmap)
        mmap MethodMap.empty

let get_field c fs =
  match c with
  | JInterface i -> InterfaceField (FieldMap.find fs i.i_fields)
  | JClass c -> ClassField (FieldMap.find fs c.c_fields)

let get_fields = function
  | JInterface i -> FieldMap.map (fun f -> InterfaceField f) i.i_fields
  | JClass c -> FieldMap.map (fun f -> ClassField f) c.c_fields

let defines_method ioc ms =
  match ioc with
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.mem ms mmap

let defines_field ioc fs =
  match ioc with
  | JInterface { i_fields = fm } -> FieldMap.mem fs fm
  | JClass { c_fields = fm } -> FieldMap.mem fs fm

let is_static_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_static

let is_final_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_final

let is_synchronized_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_synchronized

let get_method_visibility = function
  | AbstractMethod m -> (m.am_access :> access)
  | ConcreteMethod m -> m.cm_access

let get_method_signature = function
  | AbstractMethod m -> m.am_signature
  | ConcreteMethod m -> m.cm_signature

let get_class_method_signature = function
  | AbstractMethod m -> m.am_class_method_signature
  | ConcreteMethod m -> m.cm_class_method_signature

let get_field_signature = function
  | InterfaceField { if_signature = fs } | ClassField { cf_signature = fs } ->
      fs

let is_final_field = function
  | InterfaceField _ -> true
  | ClassField f -> f.cf_kind = Final

let is_static_field = function
  | InterfaceField _ -> true
  | ClassField f -> f.cf_static

let get_field_visibility = function
  | ClassField f -> f.cf_access
  | InterfaceField _ -> `Public

let get_class_field_signature = function
  | InterfaceField { if_class_signature = cfs }
  | ClassField { cf_class_signature = cfs } ->
      cfs

let identity x = x

let cf_iter f = function
  | JInterface _ -> ()
  | JClass c -> FieldMap.iter (fun _ fi -> f fi) c.c_fields

let if_iter f = function
  | JInterface i -> FieldMap.iter (fun _ fi -> f fi) i.i_fields
  | JClass _ -> ()

let f_iter f = function
  | JInterface i -> FieldMap.iter (fun _ fi -> f (InterfaceField fi)) i.i_fields
  | JClass c -> FieldMap.iter (fun _ fi -> f (ClassField fi)) c.c_fields

let f_fold f = function
  | JInterface i ->
      FieldMap.fold (fun _ fi v -> f (InterfaceField fi) v) i.i_fields
  | JClass c -> FieldMap.fold (fun _ fi v -> f (ClassField fi) v) c.c_fields

let if_fold f = function
  | JInterface i -> FieldMap.fold (fun _ fi v -> f fi v) i.i_fields
  | JClass _ -> identity

let cf_fold f = function
  | JInterface _ -> identity
  | JClass c -> FieldMap.fold (fun _ fi v -> f fi v) c.c_fields

let m_iter f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.iter (fun _ m -> f m) mmap

let cm_iter f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.iter
        (fun _ -> function AbstractMethod _ -> () | ConcreteMethod m -> f m)
        mmap

let am_iter f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.iter
        (fun _ -> function AbstractMethod m -> f m | ConcreteMethod _ -> ())
        mmap

let am_fold f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.fold
        (fun _ -> function
          | AbstractMethod m -> f m
          | ConcreteMethod _ -> fun acc -> acc)
        mmap

let cm_fold f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.fold
        (fun _ -> function
          | AbstractMethod _ -> fun acc -> acc
          | ConcreteMethod m -> f m)
        mmap

let m_fold f = function
  | JInterface { i_methods = mmap } | JClass { c_methods = mmap } ->
      MethodMap.fold (fun _ m acc -> f m acc) mmap

(* val cm_fold : ('b -> 'a concrete_method -> 'b) -> 'b -> 'a node -> 'b *)
(* val am_fold : ('b -> 'a abstract_method -> 'b) -> 'b -> 'a node -> 'b *)
(* val m_fold : ('b -> 'a jmethod -> 'b) -> 'b -> 'a node -> 'b *)

let map_concrete_method_with_native f cm =
  {
    cm_signature = cm.cm_signature;
    cm_class_method_signature = cm.cm_class_method_signature;
    cm_static = cm.cm_static;
    cm_final = cm.cm_final;
    cm_synchronized = cm.cm_synchronized;
    cm_strict = cm.cm_strict;
    cm_access = cm.cm_access;
    cm_generic_signature = cm.cm_generic_signature;
    cm_bridge = cm.cm_bridge;
    cm_varargs = cm.cm_varargs;
    cm_synthetic = cm.cm_synthetic;
    cm_other_flags = cm.cm_other_flags;
    cm_exceptions = cm.cm_exceptions;
    cm_attributes = cm.cm_attributes;
    cm_parameters = cm.cm_parameters;
    cm_annotations = cm.cm_annotations;
    cm_implementation = f cm.cm_implementation;
  }

let map_concrete_method ?(force = false) f =
  map_concrete_method_with_native (function
    | Native -> Native
    | Java c ->
        if force then
          let new_c = f (Lazy.force c) in
          Java (lazy new_c)
        else Java (lazy (f (Lazy.force c))))

let map_concrete_method_with_native_context f cm =
  map_concrete_method_with_native (f cm) cm

let map_concrete_method_context ?(force = false) f cm =
  map_concrete_method ~force (f cm) cm

let map_method ?(force = false) f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> ConcreteMethod (map_concrete_method ~force f cm)

let map_method_context ?(force = false) f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm ->
      ConcreteMethod (map_concrete_method_context ~force f cm)

let map_method_with_native f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> ConcreteMethod (map_concrete_method_with_native f cm)

let map_method_with_native_context f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm ->
      ConcreteMethod (map_concrete_method_with_native_context f cm)

let map_class_gen map_method f c =
  {
    c_name = c.c_name;
    c_version = c.c_version;
    c_access = c.c_access;
    c_final = c.c_final;
    c_abstract = c.c_abstract;
    c_super_class = c.c_super_class;
    c_generic_signature = c.c_generic_signature;
    c_fields = c.c_fields;
    c_interfaces = c.c_interfaces;
    c_consts = c.c_consts;
    c_sourcefile = c.c_sourcefile;
    c_deprecated = c.c_deprecated;
    c_enclosing_method = c.c_enclosing_method;
    c_source_debug_extention = c.c_source_debug_extention;
    c_inner_classes = c.c_inner_classes;
    c_synthetic = c.c_synthetic;
    c_enum = c.c_enum;
    c_annotations = c.c_annotations;
    c_other_flags = c.c_other_flags;
    c_other_attributes = c.c_other_attributes;
    c_methods = MethodMap.map (map_method f) c.c_methods;
  }

let map_class_with_native_context f c =
  map_class_gen map_method_with_native_context f c

let map_class_with_native f c = map_class_gen map_method_with_native f c

let map_class_context ?(force = false) f c =
  map_class_gen (map_method_context ~force) f c

let map_class ?(force = false) f c = map_class_gen (map_method ~force) f c

let map_interface_gen map_method f i =
  {
    i_name = i.i_name;
    i_version = i.i_version;
    i_access = i.i_access;
    i_interfaces = i.i_interfaces;
    i_generic_signature = i.i_generic_signature;
    i_consts = i.i_consts;
    i_sourcefile = i.i_sourcefile;
    i_deprecated = i.i_deprecated;
    i_source_debug_extention = i.i_source_debug_extention;
    i_inner_classes = i.i_inner_classes;
    i_other_attributes = i.i_other_attributes;
    i_annotation = i.i_annotation;
    i_annotations = i.i_annotations;
    i_other_flags = i.i_other_flags;
    i_fields = i.i_fields;
    i_methods = MethodMap.map (map_method f) i.i_methods;
  }

let map_interface_context ?(force = false) f i =
  map_interface_gen (map_method_context ~force) f i

let map_interface ?(force = false) f i =
  map_interface_gen (map_method ~force) f i

let map_interface_with_native_context f i =
  map_interface_gen map_method_with_native_context f i

let map_interface_with_native f i = map_interface_gen map_method_with_native f i

let map_interface_or_class_with_native f = function
  | JInterface i -> JInterface (map_interface_with_native f i)
  | JClass c -> JClass (map_class_with_native f c)

let map_interface_or_class_with_native_context f = function
  | JInterface i -> JInterface (map_interface_with_native_context f i)
  | JClass c -> JClass (map_class_with_native_context f c)

let map_interface_or_class ?(force = false) f = function
  | JInterface i -> JInterface (map_interface ~force f i)
  | JClass c -> JClass (map_class ~force f c)

let map_interface_or_class_context ?(force = false) f = function
  | JInterface i -> JInterface (map_interface_context ~force f i)
  | JClass c -> JClass (map_class_context ~force f c)

open JCode

let add_methods ioc methods =
  let merge_methods_with cmethods =
    MethodMap.fold
      (fun ms m methods -> MethodMap.add ms m methods)
      methods cmethods
  in
  match ioc with
  | JInterface i ->
      JInterface { i with i_methods = merge_methods_with i.i_methods }
  | JClass c -> JClass { c with c_methods = merge_methods_with c.c_methods }

let get_bridge_md cn info =
  let mh = info.lambda_handle in
  match mh with
  | `InvokeStatic (`InterfaceMethod (_, ms)) | `InvokeStatic (`Method (_, ms))
    ->
      make_md (ms_args ms, ms_rtype ms)
  | `InvokeVirtual (_, ms) | `InvokeInterface (_, ms) ->
      make_md (TObject (TClass cn) :: ms_args ms, ms_rtype ms)
  | `InvokeSpecial (`InterfaceMethod (_, ms)) | `InvokeSpecial (`Method (_, ms))
    ->
      make_md (TObject (TClass cn) :: ms_args ms, ms_rtype ms)
  | `NewInvokeSpecial (cn, ms) ->
      make_md (ms_args ms, Some (TObject (TClass cn)))
  | `GetField _ -> failwith "GetField reference not implemented."
  | `GetStatic _ -> failwith "GetStatic reference not implemented."
  | `PutField _ -> failwith "PutField reference not implemented."
  | `PutStatic _ -> failwith "PutStatic reference not implemented."

let vtype_to_jvm_type v : jvm_type =
  match v with
  | TObject _ -> `Object
  | TBasic b -> (
      match b with
      | `Bool | `Byte | `Char | `Short | `Int -> `Int2Bool
      | `Double -> `Double
      | `Float -> `Float
      | `Long -> `Long)

let vtype_to_jvm_rtype v : jvm_return_type =
  match v with
  | None -> `Void
  | Some (TObject _) -> `Object
  | Some (TBasic b) -> (
      match b with
      | `Bool | `Byte | `Char | `Short | `Int -> `Int2Bool
      | `Double -> `Double
      | `Float -> `Float
      | `Long -> `Long)

let vtype_size v =
  match v with
  | TObject _ -> 1
  | TBasic b -> (
      match b with
      | `Bool | `Byte | `Char | `Short | `Int | `Float -> 1
      | `Double | `Long -> 2)

let get_stack_size args = List.fold_left ( + ) 0 (List.map vtype_size args)

let make_empty_method lnt cn ms is_static =
  let rtype = ms_rtype ms in
  let opcodes = Array.of_list [ OpReturn (vtype_to_jvm_rtype rtype) ] in
  let args = ms_args ms in
  let nargs = List.fold_left ( + ) 0 (List.map vtype_size args) in
  let code =
    {
      c_max_stack = 1;
      c_max_locals = 1 + nargs;
      c_code = opcodes;
      c_exc_tbl = [];
      c_line_number_table = lnt;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map = None;
      c_attributes = [];
    }
  in
  ConcreteMethod
    {
      cm_signature = ms;
      cm_class_method_signature = make_cms cn ms;
      cm_static = is_static;
      cm_final = false;
      cm_synchronized = false;
      cm_strict = false;
      cm_access = `Public;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = true;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = true; deprecated = false; other = [] };
      cm_parameters = [];
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy code);
    }

let insert_method_code stack_incr m opcodes =
  let cm, code =
    match m with
    | AbstractMethod _ -> failwith "An abstract method has no code."
    | ConcreteMethod cm -> (
        match cm.cm_implementation with
        | Native -> failwith "A native method has no code."
        | Java lcode -> (cm, Lazy.force lcode))
  in
  let max_stack = code.c_max_stack + stack_incr in
  let new_opcodes = Array.append (Array.of_list opcodes) code.c_code in
  let new_code = { code with c_max_stack = max_stack; c_code = new_opcodes } in
  let m' =
    ConcreteMethod { cm with cm_implementation = Java (lazy new_code) }
  in
  m'

let combine3 l1 l2 l3 =
  let rec combine3 l1 l2 l3 lres =
    match (l1, l2, l3) with
    | [], [], [] -> lres
    | v1 :: tl1, v2 :: tl2, v3 :: tl3 ->
        combine3 tl1 tl2 tl3 ((v1, v2, v3) :: lres)
    | _ -> raise (Invalid_argument "Cannot combine lists of different sizes")
  in
  List.rev (combine3 l1 l2 l3 [])

let init_fields_opcodes cn arg_types field_names =
  let arg_sizes = List.map (fun v -> vtype_size v) arg_types in
  let opcodes = ref [] in
  let next_local = ref 1 in
  let () =
    List.iter
      (fun (vtype, sz, fname) ->
        opcodes :=
          [
            OpLoad (`Object, 0);
            OpLoad (vtype_to_jvm_type vtype, !next_local);
            OpInvalid;
            OpPutField (cn, make_fs fname vtype);
            OpInvalid;
            OpInvalid;
          ]
          :: !opcodes;
        next_local := !next_local + sz)
      (combine3 arg_types arg_sizes field_names)
  in
  List.flatten (List.rev !opcodes)

let get_fields_opcodes cn arg_types field_names =
  let opcodes = ref [] in
  let () =
    List.iter
      (fun (vtype, fname) ->
        opcodes :=
          [
            OpLoad (`Object, 0);
            OpGetField (cn, make_fs fname vtype);
            OpInvalid;
            OpInvalid;
          ]
          :: !opcodes)
      (List.combine arg_types field_names)
  in
  List.flatten (List.rev !opcodes)

let get_object_type v =
  match v with TObject o -> o | _ -> failwith "Value type is not an Object."

let get_arguments_opcodes info is_static =
  let args = ms_args (snd (cms_split info.functional_interface)) in
  let check_args = info.checkcast_arguments in
  let arg_sizes = List.map (fun v -> vtype_size v) args in
  let opcodes = ref [] in
  let next_local = ref (if is_static then 0 else 1) in
  let () =
    List.iter
      (fun (vtype, sz, checktype) ->
        let check_opcodes =
          if vtype = checktype then []
          else [ OpCheckCast (get_object_type checktype); OpInvalid; OpInvalid ]
        in
        opcodes :=
          (OpLoad (vtype_to_jvm_type vtype, !next_local)
          :: OpInvalid :: check_opcodes)
          :: !opcodes;
        next_local := !next_local + sz)
      (combine3 args arg_sizes check_args)
  in
  let stack_incr = get_stack_size args in
  (List.flatten (List.rev !opcodes), stack_incr)

let get_ms_opcodes ms is_static =
  let args = ms_args ms in
  let arg_sizes = List.map (fun v -> vtype_size v) args in
  let opcodes = ref [] in
  let next_local = ref (if is_static then 0 else 1) in
  let () =
    List.iter
      (fun (vtype, sz) ->
        opcodes :=
          [ OpLoad (vtype_to_jvm_type vtype, !next_local); OpInvalid ]
          :: !opcodes;
        next_local := !next_local + sz)
      (List.combine args arg_sizes)
  in
  List.flatten (List.rev !opcodes)

let invoke_lambda_opcodes info =
  let mh = info.lambda_handle in
  match mh with
  | `InvokeStatic (`InterfaceMethod (cn, ms)) ->
      [ OpInvoke (`Static (`Interface, cn), ms); OpInvalid; OpInvalid ]
  | `InvokeStatic (`Method (cn, ms)) ->
      [ OpInvoke (`Static (`Class, cn), ms); OpInvalid; OpInvalid ]
  | `InvokeVirtual (ot, ms) ->
      [ OpInvoke (`Virtual ot, ms); OpInvalid; OpInvalid ]
  | `InvokeInterface (cn, ms) ->
      [
        OpInvoke (`Interface cn, ms); OpInvalid; OpInvalid; OpInvalid; OpInvalid;
      ]
  | `InvokeSpecial (`InterfaceMethod (cn, ms)) ->
      [ OpInvoke (`Special (`Interface, cn), ms); OpInvalid; OpInvalid ]
  | `InvokeSpecial (`Method (cn, ms)) ->
      [ OpInvoke (`Special (`Class, cn), ms); OpInvalid; OpInvalid ]
  | `NewInvokeSpecial (cn, ms) ->
      [ OpInvoke (`Special (`Class, cn), ms); OpInvalid; OpInvalid ]
  | _ -> failwith "Lambda invocation type not implemented."

let invoke_bridge_opcodes icn ms =
  [ OpInvoke (`Static icn, ms); OpInvalid; OpInvalid ]

let make_init_method lnt cn arg_types field_names =
  let ms = make_ms "<init>" arg_types None in
  let m = make_empty_method lnt cn ms false in
  let ms_obj = make_ms "<init>" [] None in
  let cn_obj = make_cn "java.lang.Object" in
  let opcodes_creation =
    [
      OpLoad (`Object, 0);
      OpInvoke (`Special (`Class, cn_obj), ms_obj);
      OpInvalid;
      OpInvalid;
    ]
  in
  let opcodes_putfields = init_fields_opcodes cn arg_types field_names in
  let stack_incr = max 1 (get_stack_size arg_types) in
  insert_method_code stack_incr m (opcodes_creation @ opcodes_putfields)

let get_newinvokespecial_opcodes mh =
  match mh with
  | `NewInvokeSpecial (cn, _) -> ([ OpNew cn; OpInvalid; OpInvalid; OpDup ], 2)
  | _ -> ([], 0)

let make_functional_method lnt bridge_icn bridge_ms cn info field_names =
  let arg_types = info.captured_arguments in
  let _, ms_func = cms_split info.functional_interface in
  let m_func = make_empty_method lnt cn ms_func false in
  let fields_opcodes = get_fields_opcodes cn arg_types field_names in
  let args_opcodes, n = get_arguments_opcodes info false in
  let invoke_opcodes = invoke_bridge_opcodes bridge_icn bridge_ms in
  let stack_incr = n + get_stack_size arg_types in
  insert_method_code stack_incr m_func
    (fields_opcodes @ args_opcodes @ invoke_opcodes)

let make_bridge_method lnt cn bridge_name info =
  let bridge_md = get_bridge_md cn info in
  let bridge_ms =
    make_ms bridge_name (md_args bridge_md) (md_rtype bridge_md)
  in
  let m_bridge = make_empty_method lnt cn bridge_ms true in
  let newinvokespecial_opcodes, n =
    get_newinvokespecial_opcodes info.lambda_handle
  in
  let args_opcodes = get_ms_opcodes bridge_ms true in
  let invoke_opcodes = invoke_lambda_opcodes info in
  let stack_incr = n + get_stack_size (ms_args bridge_ms) in
  let m_bridge =
    insert_method_code stack_incr m_bridge
      (newinvokespecial_opcodes @ args_opcodes @ invoke_opcodes)
  in
  (bridge_ms, m_bridge)

let get_callsite_ms ms_name info =
  let args = info.captured_arguments in
  let rtype, _ = cms_split info.functional_interface in
  make_ms ms_name args (Some (TObject (TClass rtype)))

let make_callsite_method lnt parent_cn lambda_cn ms_name info =
  let ms_call = get_callsite_ms ms_name info in
  let m_callsite = make_empty_method lnt parent_cn ms_call true in
  let ms_init = make_ms "<init>" info.captured_arguments None in
  let args_opcodes = get_ms_opcodes ms_call true in
  let stack_incr = 2 + get_stack_size (ms_args ms_call) in
  let m_callsite =
    insert_method_code stack_incr m_callsite
      ([ OpNew lambda_cn; OpInvalid; OpInvalid; OpDup ]
      @ args_opcodes
      @ [
          OpInvoke (`Special (`Class, lambda_cn), ms_init); OpInvalid; OpInvalid;
        ])
  in
  (ms_call, m_callsite)

let make_class_field cn fname ftype =
  let fs = make_fs fname ftype in
  let cfs = make_cfs cn fs in
  {
    cf_signature = fs;
    cf_class_signature = cfs;
    cf_generic_signature = None;
    cf_access = `Private;
    cf_static = false;
    cf_synthetic = true;
    cf_enum = false;
    cf_kind = NotFinal;
    cf_value = None;
    cf_transient = false;
    cf_annotations = [];
    cf_other_flags = [];
    cf_attributes = { synthetic = true; deprecated = false; other = [] };
  }

let make_lambda_class version sourcefile lnt cn info bridge_icn bridge_ms =
  let iname, ms_func = cms_split info.functional_interface in
  let arg_types = info.captured_arguments in
  let field_names =
    List.init (List.length arg_types) (fun i -> Printf.sprintf "arg%d" (i + 1))
  in
  let fields =
    List.fold_left
      (fun m (fname, ftype) ->
        let cf = make_class_field cn fname ftype in
        FieldMap.add cf.cf_signature cf m)
      FieldMap.empty
      (List.combine field_names arg_types)
  in
  let ms_init = make_ms "<init>" arg_types None in
  let m_init = make_init_method lnt cn arg_types field_names in
  let m_func =
    make_functional_method lnt bridge_icn bridge_ms cn info field_names
  in
  let methods =
    MethodMap.add ms_func m_func (MethodMap.add ms_init m_init MethodMap.empty)
  in
  JClass
    {
      c_name = cn;
      c_version = version;
      c_access = `Default;
      c_final = false;
      c_abstract = false;
      c_super_class = Some (make_cn "java.lang.Object");
      c_generic_signature = None;
      c_fields = fields;
      c_interfaces = [ iname ];
      c_consts = [||];
      c_sourcefile = sourcefile;
      c_deprecated = false;
      c_enclosing_method = None;
      c_source_debug_extention = None;
      c_inner_classes = [];
      c_synthetic = true;
      c_enum = false;
      c_annotations = [];
      c_other_flags = [];
      c_other_attributes = [];
      c_methods = methods;
    }

let get_version ioc =
  match ioc with JClass c -> c.c_version | JInterface i -> i.i_version

let get_cm_code ioc ms =
  let m = get_method ioc ms in
  match m with
  | AbstractMethod _ ->
      failwith
        "An abstract method can not contain an invokedynamic instruction."
  | ConcreteMethod cm -> (
      match cm.cm_implementation with
      | Native -> None
      | Java lcode -> Some (Lazy.force lcode))

let is_metafactory bm =
  match bm.bm_ref with
  | `InvokeStatic (`Method (cn, ms)) ->
      if
        cn_name cn = "java.lang.invoke.LambdaMetafactory"
        && ms_name ms = "metafactory"
      then true
      else false
  | _ -> false

let replace_invokedynamic code pp icn ms_name =
  match code.c_code.(pp) with
  | OpInvoke (`Dynamic bm, ms) ->
      let info = build_lambda_info bm ms in
      let ms_call = get_callsite_ms ms_name info in
      let callsite_call =
        Array.of_list (invoke_bridge_opcodes icn ms_call @ [ OpNop; OpNop ])
      in
      (* The OpNop instructions are inserted to fill the original size
         of the invokedynamic instruction. *)
      let n = Array.length callsite_call in
      let () = Array.blit callsite_call 0 code.c_code pp n in
      info
  | _ -> failwith "No invokedynamic found at given program point."

let make_lnt code pp =
  let line = get_source_line_number pp code in
  match line with None -> None | Some i -> Some [ (0, i) ]

let iter_code_lambdas ioc code pp prefix mmap cmap =
  match code.c_code.(pp) with
  | OpInvoke (`Dynamic bm, _) when is_metafactory bm ->
      let lnt = make_lnt code pp in
      let parent_cn = get_name ioc in
      let forged_name = Printf.sprintf "%s%d" prefix pp in
      let bridge_icn =
        match ioc with
        | JClass _ -> (`Class, parent_cn)
        | JInterface _ -> (`Interface, parent_cn)
      in
      let callsite_name = "callsite_" ^ forged_name in
      let info = replace_invokedynamic code pp bridge_icn callsite_name in
      let lambda_cn = make_cn forged_name in
      let call_ms, m_callsite =
        make_callsite_method lnt parent_cn lambda_cn callsite_name info
      in
      let bridge_name = "access_" ^ forged_name in
      let bridge_ms, m_bridge =
        make_bridge_method lnt parent_cn bridge_name info
      in
      let methods = MethodMap.add call_ms m_callsite mmap in
      let methods = MethodMap.add bridge_ms m_bridge methods in
      let version = get_version ioc in
      let sourcefile = get_sourcefile ioc in
      let ioc_lambda =
        make_lambda_class version sourcefile lnt lambda_cn info bridge_icn
          bridge_ms
      in
      let lambda_classes = ClassMap.add lambda_cn ioc_lambda cmap in
      (pp + 5, methods, lambda_classes)
  | _ -> (pp + 1, mmap, cmap)

let remove_invokedynamic ioc ms pp ~prefix =
  match get_cm_code ioc ms with
  | None ->
      failwith "A native method can not contain an invokedynamic instruction."
  | Some code ->
      let _, methods, lambda_classes =
        iter_code_lambdas ioc code pp prefix MethodMap.empty ClassMap.empty
      in
      let ioc' = add_methods ioc methods in
      let _, ioc_lambda, _ = ClassMap.choose_and_remove lambda_classes in
      (ioc', ioc_lambda)

let remove_invokedynamics_in_method ioc ms ~prefix =
  match get_cm_code ioc ms with
  | None -> (ioc, ClassMap.empty)
  | Some code ->
      let mmap = ref MethodMap.empty in
      let cmap = ref ClassMap.empty in
      let pp = ref 0 in
      let () =
        while !pp < Array.length code.c_code do
          let tpp, tmethods, tlambda_classes =
            iter_code_lambdas ioc code !pp prefix !mmap !cmap
          in
          pp := tpp;
          mmap := tmethods;
          cmap := tlambda_classes
        done
      in
      let ioc' = add_methods ioc !mmap in
      (ioc', !cmap)

let remove_invokedynamics ioc ~prefix =
  let methods = get_concrete_methods ioc in
  let m_counter = ref 0 in
  let ioc', cmap =
    MethodMap.fold_ordered
      (fun ms _ (ioc, cmap) ->
        incr m_counter;
        let prefix = Printf.sprintf "%s_%d_" prefix !m_counter in
        let ioc', cmap' = remove_invokedynamics_in_method ioc ms ~prefix in
        (ioc', ClassMap.merge (fun c _ -> c) cmap cmap'))
      methods (ioc, ClassMap.empty)
  in
  (ioc', cmap)
