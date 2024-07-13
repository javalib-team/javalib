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

(** Basic elements of class files. *)

(** {1 Definition of basic types and descriptors.} *)

type class_name
(** Type representing a class name. e.g. [java.lang.Object] *)

type method_signature
(** Type representing a method signature.
    A method signature contains the method name, the types of parameters
    and the method return type. Two methods in two different classes can
    have the same [method_signature]. *)

type method_descriptor
(** Type representing a method descriptor.
    A method descriptor contains the types of parameters and the method
    return type. *)

type field_signature
(** Type representing a field signature.
    A field signature contains the field name and the field type. *)

type class_field_signature
(** Type representing a signature for a field in a particular class.
    Each field of each class has a unique [class_field_signature]. *)

type class_method_signature
(** Type representing a signature for a method in a particular class.
    Each method of each class has a unique [class_method_signature]. *)

type other_num = [ `Long | `Float | `Double ]
(** Numerical types that are not smaller than int. *)

type jvm_basic_type = [ `Int2Bool | other_num ]
(** JVM basic type (int = short = char = byte = bool). *)

type jvm_type = [ jvm_basic_type | `Object ]
(** JVM type (int = short = char = byte = bool, all objects have the same type). *)

type jvm_array_type = [ `Int | `Short | `Char | `ByteBool | other_num | `Object ]
(** JVM array element type (byte = bool, all objects have the same type). *)

type jvm_return_type = [ jvm_basic_type | `Object | `Void ]
(** JVM return type (byte = bool, all objects have the same type). *)

type java_basic_type = [ `Int | `Short | `Char | `Byte | `Bool | other_num ]
(** Java basic type. *)

(** Java object type *)
type object_type = TClass of class_name | TArray of value_type

(** Java type *)
and value_type = TBasic of java_basic_type | TObject of object_type

type jstr
(** Abstract datatype for Java strings *)

type version = { major : int; minor : int }
(** Version number of the class file. Extract of the specification: An
    implementation of Java 1.k sould support class file formats of
    versions in the range of 45.0 through 44+k.0 inclusive. E.g. Java
    1.6 implementations support class file formats of versions up to
    50.0. *)

(** {1 Basic types manipulation.} *)

(** Creating and manipulating {!class_name} values. *)

val java_lang_object : class_name
(** [java.lang.Object] class name. *)

val default_native_throwable : class_name list

val make_cn : string -> class_name
(** Builds a [class_name] from a string representing the Java fully qualified
    name of the class. e.g. "java.lang.Object".

    @raise Invalid_argument if the class name given as argument is not a valid
    class name (ie. it must match the regular expression
    ["^\\([a-zA-Z_$][a-zA-Z_$0-9]*\\.\\)*\\([a-zA-Z_0-9]+\\$\\)*[a-zA-Z_0-9]+$]").  *)

val cn_hash : class_name -> int
(** Returns the hash value of the given [class_name]. *)

val cn_name : class_name -> string
(** Retrieves the Java fully qualified name of a class.
    e.g. "java.lang.Object". *)

val cn_simple_name : class_name -> string
(** Retrieves the Java simple name of a class, omitting the package name.
    e.g. "Object" instead of "java.lang.Object". *)

val cn_package : class_name -> string list
(** Retrieves the package name from a [class_name]. *)

val cn_compare : class_name -> class_name -> int
(** Compares two classes names. *)

val cn_equal : class_name -> class_name -> bool
(** Returns [true] if two classes names are equal, [false] otherwise. *)

(** Creating and manipulating {!method_signature} values. *)

val clinit_signature : method_signature
(** [<clinit>] method signature. *)

val make_ms : string -> value_type list -> value_type option -> method_signature
(** Builds a [method_signature]. *)

val ms_hash : method_signature -> int
(** Returns the hash value of the given [method_signature]. *)

val ms_name : method_signature -> string
(** Retrieves the method name from a [method_signature]. *)

val ms_args : method_signature -> value_type list
(** Retrieves method parameters types from a [method_signature]. *)

val ms_rtype : method_signature -> value_type option
(** Retrieves method return type from a [method_signature]. *)

val ms_compare : method_signature -> method_signature -> int
(** Compares two method signatures. *)

val ms_equal : method_signature -> method_signature -> bool
(** Returns [true] if two method signatures are equal, [false] otherwise. *)

(** Creating and manipulating {!method_descriptor} values. *)

val make_md : value_type list * value_type option -> method_descriptor
(** Builds a [method_descriptor]. *)

val md_split : method_descriptor -> value_type list * value_type option
(** Splits a [method_descriptor] into arguments list and return type. *)

val md_args : method_descriptor -> value_type list
(** Returns the [method_descriptor] arguments list. *)

val md_rtype : method_descriptor -> value_type option
(** Returns the [method_descriptor] return type. *)

(** Creating and manipulating {!field_signature} values. *)

val make_fs : string -> value_type -> field_signature
(** Builds a [field_signature]. *)

val fs_hash : field_signature -> int
(** Returns the hash value of the given [field_signature]. *)

val fs_name : field_signature -> string
(** Retrieves the field name from a [field_signature]. *)

val fs_type : field_signature -> value_type
(** Retrieves the field type from a [field_signature]. *)

val fs_compare : field_signature -> field_signature -> int
(** Compares two field signatures. *)

val fs_equal : field_signature -> field_signature -> bool
(** Returns [true] if two field signatures are equal, [false] otherwise. *)

(** Creating and manipulating {!class_field_signature} values. *)

val make_cfs : class_name -> field_signature -> class_field_signature
(** Builds a [class_field_signature]. *)

val cfs_split : class_field_signature -> class_name * field_signature
(** Retrieves the [class_name] and [field_signature] from a [class_field_signature]. *)

val cfs_compare : class_field_signature -> class_field_signature -> int
(** Compares two [class_field_signature]. *)

val cfs_equal : class_field_signature -> class_field_signature -> bool
(** Returns [true] if two [class_field_signature] are equal, [false] otherwise. *)

val cfs_hash : class_field_signature -> int
(** Returns the hash value of the given [class_field_signature]. *)

(** Creating and manipulating {!class_method_signature} values. *)

val make_cms : class_name -> method_signature -> class_method_signature
(** Builds a [class_method_signature]. *)

val cms_split : class_method_signature -> class_name * method_signature
(** Retrieves the [class_name] and [method_signature] from a [class_method_signature]. *)

val cms_compare : class_method_signature -> class_method_signature -> int
(** Compares two [class_method_signature]. *)

val cms_equal : class_method_signature -> class_method_signature -> bool
(** Returns [true] if two [class_method_signature] are equal, [false] otherwise. *)

val make_jstr : string -> jstr
(** Builds a [jstr]. *)

val jstr_pp : jstr -> string
(** Returns a [string] where all characters outside the ASCII printable range (32..126) are escaped. *)

val jstr_raw : jstr -> string
(** Returns the original [string]. *)

(** {1 Bootstrap method and method handle types.} *)

(** Features introduced in Java 8 to implement lambdas. *)

type jmethod_or_interface =
  [ `Method of class_name * method_signature
  | `InterfaceMethod of class_name * method_signature ]

type method_handle =
  [ `GetField of class_name * field_signature
  | `GetStatic of class_name * field_signature
  | `PutField of class_name * field_signature
  | `PutStatic of class_name * field_signature
  | `InvokeVirtual of object_type * method_signature
  | `NewInvokeSpecial of class_name * method_signature
  | `InvokeStatic of jmethod_or_interface
  | `InvokeSpecial of jmethod_or_interface
  | `InvokeInterface of class_name * method_signature ]
(** Method handle. cf JVM Spec se8 §4.4.8. *)

type bootstrap_argument =
  [ `String of jstr
  | `Class of object_type
  | `Int of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `MethodHandle of method_handle
  | `MethodType of method_descriptor ]
(** Bootstrap argument. cf JVM Spec se8 §4.7.23. *)

type bootstrap_method = {
  bm_ref : method_handle;
  bm_args : bootstrap_argument list;
}
(** Bootstrap method called by the [invokedynamic] instruction.
    cf JVM Spec se8 §4.7.23. *)

(** {1 Constant pool.} *)

(** You should not need this for normal usage, as the
    parsing/unparsing functions take care of the constant pool. This
    is typically useful for user-defined attributes that refer to the
    constant pool. *)

type bootstrap_method_index = int

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor = SValue of value_type | SMethod of method_descriptor

(** Constant pool values. *)
type constant =
  | ConstString of jstr
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type
  | ConstField of (class_name * field_signature)
  | ConstMethod of (object_type * method_signature)
  | ConstInterfaceMethod of (class_name * method_signature)
  | ConstMethodType of method_descriptor
  | ConstMethodHandle of method_handle
  | ConstInvokeDynamic of bootstrap_method_index * method_signature
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstModule of string
  | ConstPackage of string
  | ConstUnusable

(** {1 Stackmaps}  *)

(** Verification type. *)
type verification_type =
  | VTop
  | VInteger
  | VFloat
  | VDouble
  | VLong
  | VNull
  | VUninitializedThis
  | VObject of object_type
  | VUninitialized of int  (** creation point *)

(** Stackmap type. *)
type stackmap_frame =
  | SameFrame of int
  | SameLocals of int * verification_type
  | SameLocalsExtended of int * int * verification_type
  | ChopFrame of int * int
  | SameFrameExtended of int * int
  | AppendFrame of int * int * verification_type list
  | FullFrame of int * int * verification_type list * verification_type list

(** {1 Errors}  *)

(** The library may throw the following exceptions, in addition to [Invalid_argument].
    Any other exception (in particular, an [Assert_failure])
    should be interpreted as a bug in [javalib]. *)

exception No_class_found of string
(** Indicates that a class name could not be found in a given classpath. *)

exception Class_structure_error of string
(** Indicates the argument of a parsing/unparsing function does not
    satisfy a structural constraint of class files. *)

(** {1 Annotations} *)

(** [element_value] represents a constant value, either a number, a string, a
    class, an enum, an array of [element_value]s or another annotation. *)
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
  | EVClass of value_type option
  | EVAnnotation of annotation
  | EVArray of element_value list

and annotation = {
  kind : class_name;
  element_value_pairs : (string * element_value) list;
}
(** An [annotation] contains the name ([kind]) of the annotation an a list of
    element-value pairs (the name of the element and its value).  The names
    given here should corresponds to the elements declared during the definition
    of the annotation, but this is not checked (as it would need to load
    additional class files). *)

(** {1 Containers.} *)

module ClassMap : GenericMap.GenericMapSig with type key = class_name
(** This module allows to build maps of elements indexed by [class_name] values. *)

module MethodMap : GenericMap.GenericMapSig with type key = method_signature
(** This module allows to build maps of elements indexed by [method_signature] values. *)

module FieldMap : GenericMap.GenericMapSig with type key = field_signature
(** This module allows to build maps of elements indexed by [field_signature] values. *)

module ClassFieldMap :
  GenericMap.GenericMapSig with type key = class_field_signature
(** This module allows to build maps of elements indexed by [class_field_signature] values. *)

module ClassMethodMap :
  GenericMap.GenericMapSig with type key = class_method_signature
(** This module allows to build maps of elements indexed by [class_method_signature] values. *)

module ClassSet : GenericSet.GenericSetSig with type elt = class_name
(** This module allows to build sets of [class_name] values. *)

module MethodSet : GenericSet.GenericSetSig with type elt = method_signature
(** This module allows to build sets of [method_signature] values. *)

module FieldSet : GenericSet.GenericSetSig with type elt = field_signature
(** This module allows to build sets of [field_signature] values. *)

module ClassFieldSet :
  GenericSet.GenericSetSig with type elt = class_field_signature
(** This module allows to build sets of [class_field_signature] values. *)

module ClassMethodSet :
  GenericSet.GenericSetSig with type elt = class_method_signature
(** This module allows to build sets of [class_method_signature] values. *)

module ClassMethodMaptoSet : sig
  val to_set : 'a ClassMethodMap.t -> ClassMethodSet.t
end

(** {1 Tuning JavaLib.} *)

val set_permissive : bool -> unit
(** [set_permissive true] disables some checking in JavaLib.  It can
    allow to parse some files that do not strictly comply with the
    official specification.  *)

val get_permissive : unit -> bool
