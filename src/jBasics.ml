(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
 * Copyright (c)2016 David Pichardie (ENS Rennes)
 * Copyright (c)2016 Laurent Guillo (CNRS)
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

(* {2 Basic Elements.} *)

type class_name = int * string

(* Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(* JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(* JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(* JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(* JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(* Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(* Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(* Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(* Field descriptor *)
type field_descriptor = value_type

(* Method descriptor *)
type method_descriptor = value_type list * value_type option

type field_signature_data = string * field_descriptor
type method_signature_data = string * method_descriptor

type method_signature = int * method_signature_data
type field_signature = int * field_signature_data

type class_field_signature_data = class_name * field_signature
type class_method_signature_data = class_name * method_signature
type class_field_signature = int * class_field_signature_data
type class_method_signature = int * class_method_signature_data


(* Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor =
  | SValue of field_descriptor
  | SMethod of method_descriptor

(* Constant value. *)
type jstr = string
let make_jstr s = s
let jstr_pp s   = String.escaped s
let jstr_raw s  = s


type constant_value =
  | ConstString of jstr
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type

(* Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * field_signature)
  | ConstMethod of (object_type * method_signature)
  | ConstInterfaceMethod of (class_name * method_signature)
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstUnusable

(* Stackmap type. *)
type verification_type =
	| VTop
	| VInteger
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of object_type
	| VUninitialized of int (* creation point *)

type stackmap = (int * verification_type list * verification_type list)

type version = {major :int; minor:int;}

let clinit_signature_data = ("<clinit>", ([], None))
let clinit_signature = (0, clinit_signature_data)

let java_lang_object = (0, "java.lang.Object")

exception No_class_found of string

exception Class_structure_error of string

(* Annotations *)

type element_value =
  | EVCstByte of int
  | EVCstChar of int
  | EVCstInt of int32
  | EVCstShort of int
  | EVCstBoolean of int
  | EVCstDouble of float
  | EVCstFloat of float
  | EVCstLong of int64
  | EVCstString of string
  | EVEnum of (class_name * string)
      (* (type_name_index,const_name_index) cf. JLS 13.1 *)
      (* TODO: this should probably be modified but I have not understand how *)
  | EVClass of value_type option
  | EVAnnotation of annotation
  | EVArray of element_value list

and annotation = {
  kind : class_name;
  element_value_pairs : (string * element_value) list;
}

(* Definition of dictionary for indexation. *)

module ClassNameMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

module MethodSignatureMap = Map.Make(
  struct
    type t = method_signature_data
    let compare = compare
  end)

module FieldSignatureMap = Map.Make(
  struct
    type t = field_signature_data
    let compare = compare
  end)

module ClassFieldSignatureMap = Map.Make(
  struct
    type t = class_field_signature_data
    let compare : class_field_signature_data -> class_field_signature_data -> int =
      fun (((cni1,_),(fsi1,_))) ((cni2,_),(fsi2,_)) ->
        compare (cni1,fsi1) (cni2,fsi2)
  end)

module ClassMethodSignatureMap = Map.Make(
  struct
    type t = class_method_signature_data
    let compare : class_method_signature_data -> class_method_signature_data -> int =
      fun (((cni1,_),(msi1,_))) ((cni2,_),(msi2,_)) ->
        compare (cni1,msi1) (cni2,msi2)
  end)

type field_signature_table =
    { mutable fsi_map : field_signature FieldSignatureMap.t;
      mutable fsi_next : int }

type method_signature_table =
    { mutable msi_map : method_signature MethodSignatureMap.t;
      mutable msi_next : int }

type class_name_table =
    { mutable cni_map : class_name ClassNameMap.t;
      mutable cni_next : int }

type class_field_signature_table =
    { mutable cfsi_map : class_field_signature ClassFieldSignatureMap.t;
      mutable cfsi_next : int }

type class_method_signature_table =
    { mutable cmsi_map : class_method_signature ClassMethodSignatureMap.t;
      mutable cmsi_next : int }

type dictionary = { class_name_table : class_name_table;
		    field_signature_table : field_signature_table;
		    method_signature_table : method_signature_table;
		    class_field_signature_table : class_field_signature_table;
		    class_method_signature_table : class_method_signature_table;
		  }

let make_dictionary () =
  { class_name_table =
      { cni_map =
	  ClassNameMap.add (snd java_lang_object) java_lang_object
	    ClassNameMap.empty;
	cni_next = 1
      };
    field_signature_table =
      { fsi_map = FieldSignatureMap.empty;
	fsi_next = 0
      };
    method_signature_table =
      { msi_map =
	  MethodSignatureMap.add (snd clinit_signature) clinit_signature
	    MethodSignatureMap.empty;
	msi_next = 1
      };
    class_field_signature_table =
      { cfsi_map = ClassFieldSignatureMap.empty;
	cfsi_next = 0
      };
    class_method_signature_table =
      { cmsi_map = ClassMethodSignatureMap.empty;
	cmsi_next = 0
      };
  }

let common_dictionary = make_dictionary ()


let make_cn : string -> class_name =
(*  let valid_class_name =
    Str.regexp "^\\([a-zA-Z_$-][a-zA-Z_$0-9-]*\\.\\)*\\([a-zA-Z_0-9-]+\\$\\)*[a-zA-Z_0-9-]+$"
  in *)function cn ->
    let dic = common_dictionary in
    let cnt = dic.class_name_table in
      try
	ClassNameMap.find cn cnt.cni_map
      with _ ->
	let cni = cnt.cni_next in
        let new_cn = (cni,cn) in
(*	  if not (Str.string_match valid_class_name cn 0)
	  then invalid_arg ("Error : " ^ cn ^ " is not a valid name for a class");*)
	  cnt.cni_map <- ClassNameMap.add cn new_cn cnt.cni_map;
	  cnt.cni_next <- cni + 1;
	  new_cn

let make_ms mname margs mrtype =
  let dic = common_dictionary in
  let mst = dic.method_signature_table
  and ms = (mname,(margs,mrtype)) in
    try
      MethodSignatureMap.find ms mst.msi_map
    with _ ->
      let msi = mst.msi_next in
      let new_ms = (msi,ms) in
	mst.msi_map <- MethodSignatureMap.add ms new_ms mst.msi_map;
	mst.msi_next <- msi + 1;
	new_ms

let make_fs fname fdesc =
  let dic = common_dictionary in
  let fst = dic.field_signature_table
  and fs = (fname,fdesc) in
    try
      FieldSignatureMap.find fs fst.fsi_map
    with _ ->
      let fsi = fst.fsi_next in
      let new_fs = (fsi,fs) in
	fst.fsi_map <- FieldSignatureMap.add fs new_fs fst.fsi_map;
	fst.fsi_next <- fsi + 1;
        new_fs

let make_cfs cs ms =
  let dic = common_dictionary in
  let cfs = (cs,ms) in
  let cfst = dic.class_field_signature_table in
    try
      ClassFieldSignatureMap.find cfs cfst.cfsi_map
    with _ ->
      let cfsi = cfst.cfsi_next in
      let new_cfs = (cfsi,cfs) in
	cfst.cfsi_map <-
	  ClassFieldSignatureMap.add cfs new_cfs cfst.cfsi_map;
	cfst.cfsi_next <- cfsi + 1;
	new_cfs

let make_cms cs ms =
  let dic = common_dictionary in
  let cms = (cs,ms) in
  let cmst = dic.class_method_signature_table in
    try
      ClassMethodSignatureMap.find cms cmst.cmsi_map
    with _ ->
      let cmsi = cmst.cmsi_next in
      let new_cms = (cmsi,cms) in
	cmst.cmsi_map <-
	  ClassMethodSignatureMap.add cms new_cms cmst.cmsi_map;
	cmst.cmsi_next <- cmsi + 1;
	new_cms

let default_native_throwable = 
[ (*RuntimeException that could be thrown by VM.*)
    make_cn "java.lang.ArithmeticException";
    make_cn "java.lang.ArrayStoreException";
    make_cn "java.lang.ClassCastException";
    make_cn "java.lang.IllegalMonitorStateException";
    make_cn "java.lang.IndexOutOfBoundsException";
    make_cn "java.lang.NegativeArraySizeException";
    make_cn "java.lang.NullPointerException";
    make_cn "java.lang.SecurityException";
    (*Error that could be thrown by VM.*)
    make_cn "java.lang.ClassFormatError";
    make_cn "java.lang.ClassCircularityError";
    make_cn "java.lang.NoClassDefFoundError";
    make_cn "java.lang.UnsupportedClassVersionError";
    make_cn "java.lang.NoSuchFieldError";
    make_cn "java.lang.NoSuchMethodError";
    make_cn "java.lang.InstantiationError";
    make_cn "java.lang.IllegalAccessError";
    make_cn "java.lang.VerifyError";
    make_cn "java.lang.ExceptionInInitializerError";
    make_cn "java.lang.AbstractMethodError";
    make_cn "java.lang.UnsatisfiedLinkError";
    make_cn "java.lang.InternalError";
    make_cn "java.lang.OutOfMemoryError";
    make_cn "java.lang.StackOverflowError";
    make_cn "java.lang.UnknownError";
 ]

(* Comparison operations. *)

let i_compare (i1,_:int*'a) (i2,_:int*'b) = i1 - i2
(* NB: [i_compare] is correct for positive integers.
       but in general, this could overflow *)

let cn_compare = i_compare
let ms_compare = i_compare
let fs_compare = i_compare
let cfs_compare = i_compare
let cms_compare = i_compare

let i_equal (i1,_:int*'a) (i2,_:int*'b) = i1 == i2
let cn_equal = i_equal
let ms_equal = i_equal
let fs_equal = i_equal
let cfs_equal = i_equal
let cms_equal = i_equal


(* Conversion operations. *)

let cn_name cn = snd cn

let cn_hash cn = fst cn

let split_package_class csig =
  let cn = snd csig in
  let l =  ExtString.String.nsplit cn "." in
    match l with
      | [] -> assert false
      | _ -> let rl = List.rev l in
	let cn = List.hd rl in
	let p = List.rev (List.tl rl) in (p,cn)

let cn_package cn = fst (split_package_class cn)

let cn_simple_name csig = snd (split_package_class csig)

let ms_name msig = fst (snd msig)

let ms_hash msig = fst msig

let ms_args msig = fst (snd (snd msig))

let ms_rtype msig = snd (snd (snd msig))

let fs_name (_i,(fn,_ft):field_signature) = fn

let fs_hash (i,(_fn,_ft):field_signature) = i

let fs_type (_i,(_fn,ft):field_signature) = ft

let cfs_hash (i,_:class_field_signature) = i

let cfs_split (_i,(cn,fs):class_field_signature) = (cn, fs)

let cms_split (_i,(cn,ms):class_method_signature) = (cn,ms)

(* Containers. *)

module ClassSet = GenericSet.Make(struct type t = class_name let get_hash = fst end)

module MethodSet = GenericSet.Make(struct type t = method_signature let get_hash = fst end)

module FieldSet = GenericSet.Make(struct type t = field_signature let get_hash = fst end)

module ClassFieldSet = GenericSet.Make(struct type t = class_field_signature let get_hash = fst end)

module ClassMethodSet = GenericSet.Make(struct type t = class_method_signature let get_hash = fst end)
 
module ClassMethodMap = GenericMap.Make (struct type t = class_method_signature let get_hash = fst end)

module ClassMap = GenericMap.Make (struct type t = class_name let get_hash = fst end)

module MethodMap = GenericMap.Make (struct type t = method_signature let get_hash = fst end)

module FieldMap = GenericMap.Make (struct type t = field_signature let get_hash = fst end)

module ClassFieldMap = GenericMap.Make (struct type t = class_field_signature let get_hash = fst end)

module ClassMethodMaptoSet =
  GenericMap.MaptoSet(struct type t = class_method_signature end)(ClassMethodMap)(ClassMethodSet)

(* javalib tuning *)
let permissive = ref false
let set_permissive b = permissive := b
let get_permissive _ = !permissive
