(*
 *  This file is part of JavaLib
 *  Copyright (c)2004 Nicolas Cannasse
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

(** Ocaml representation of a Java class file. *)

(** {2 Basic Elements.} *)

(** Fully qualified ordinary class or interface name (not an array).
    For example: [\["java" ; "lang" ; "Object"\]]. *)
type class_name = string list

(** Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(** JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(** JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(** JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(** JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(** Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(** Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(** Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(** Field signature. *)
type field_signature = value_type

(** Method signature. *)
type method_signature = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type signature =
  | SValue of field_signature
  | SMethod of method_signature

(** Constant value. *)
type constant_value =
  | ConstString of string
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type (** This is not documented in the JVM spec. *)

(** Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * string * field_signature)
  | ConstMethod of (object_type * string * method_signature)
  | ConstInterfaceMethod of (class_name * string * method_signature)
  | ConstNameAndType of string * signature
  | ConstStringUTF8 of string
  | ConstUnusable

(** Instruction. *)
type opcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int (** index, increment *)

  (* Stack permutation *)
  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  (* Constant loading *)
  | OpConst of [
      `ANull (** AConstNull  *)
    | `I of int32
    | `L of int64
    | `F of float
    | `D of float
    | `B of int (** BIPush *)
    | `S of int
    ]
  | OpLdc of constant_value

  (* Arithmetic *)
  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  (* Logic *)
  | OpIShl (* Use an I/L argument *)
  | OpLShl
  | OpIShr
  | OpLShr
  | OpIUShr
  | OpLUShr
  | OpIAnd
  | OpLAnd
  | OpIOr
  | OpLOr
  | OpIXor
  | OpLXor

  (* Conversion *)
  | OpI2L (* Use `I of [`L | `F  | `D] *)
  | OpI2F
  | OpI2D
  | OpL2I
  | OpL2F
  | OpL2D
  | OpF2I
  | OpF2L
  | OpF2D
  | OpD2I
  | OpD2L
  | OpD2F
  | OpI2B (* Those three are different *)
  | OpI2C
  | OpI2S

  | OpCmp of [`L | `FL | `FG | `DL | `DG]

  (* Conditional jump *)
  | OpIf of [`Eq | `Ne | `Lt | `Ge | `Gt | `Le | `Null | `NonNull] * int
  | OpIfCmp of [`IEq | `INe | `ILt | `IGe | `IGt | `ILe | `AEq | `ANe] * int

  (* Unconditional jump *)
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  (* Heap and static fields *)
  | OpNew of class_name
  | OpNewArray of value_type
  | OpAMultiNewArray of object_type * int (** ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * string * field_signature
  | OpPutStatic of class_name * string * field_signature
  | OpGetField of class_name * string * field_signature
  | OpPutField of class_name * string * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke
      of [
	`Virtual of object_type
      | `Special of class_name
      | `Static of class_name
      | `Interface of class_name
      ]
	* string * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid

type opcodes = opcode array

(** Exception handler. *)
type jexception = {
	e_start : int;
	e_end : int;
	e_handler : int;
	e_catch_type : class_name option
}

(** Stackmap type. *)
type verification_type = 
	| VTop 
	| VInteger 
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of object_type
	| VUninitialized of int (** creation point *)

(** {2 Substructures.} *)

type jcode = {
	c_max_stack : int;
	c_max_locals : int;
	c_code : opcodes;
	c_exc_tbl : jexception list;
	c_attributes : attribute list;
}

and attribute =
	| AttributeSourceFile of string
	| AttributeConstant of constant_value
	| AttributeCode of jcode
	| AttributeLineNumberTable of (int * int) list
	| AttributeLocalVariableTable of (int * int * string * value_type * int) list
	| AttributeUnknown of string * string
	| AttributeStackMap of (int*(verification_type list)*(verification_type list)) list

type access_flag =
	| AccPublic
	| AccPrivate
	| AccProtected
	| AccStatic
	| AccFinal
	| AccSynchronized (** Also used as "Special" *)
	| AccVolatile
	| AccTransient
	| AccNative
	| AccInterface
	| AccAbstract
	| AccStrict
	| AccRFU of int (** Four bits (RFU 1 .. RFU 4) reserved for future use *)

type access_flags = access_flag list

(* Flags and attributes should be related for fields, methods and classes. *)

type jfield = {
	f_name : string;
	f_signature : field_signature;
	f_flags : access_flags (* Should be specialized *);
	f_attributes : attribute list (* Should be specialized *);
}

type jmethod = {
	m_name : string;
	m_signature : method_signature;
	m_flags : access_flags (* Should be specialized *);	
	m_code : jcode option; (* Remove that *)
	m_attributes : attribute list (* Should be specialized *);
}

type jclass = {
	j_name : class_name;
	j_super : class_name option (* Remove the option *);
	j_interfaces : class_name list;
	j_consts : constant array;
	j_flags : access_flags (* Should be specialized *);
	j_fields : jfield list;
	j_methods : jmethod list;
	j_attributes : attribute list (* Should be specialized *);
}

(* Visibility modifiers. *)
type access = [
| `Default
| `Public
| `Private
| `Protected
]

(* Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  deprecated : bool;
  other : (string * string) list
}

(* Class and interface fields. *)
(*******************************)

type field_type =
  | NotFinal
  | Final
  | Volatile

(* Nouveaux types pour les champs ; ce serait pas mal de mettre en commun un certain nombre de choses. *)
type class_field = {
  cf_access: access;
  cf_static : bool;
  field_type : field_type;
  cf_signature : field_signature;
  constant_value : constant_value option; (* Valable seulement pour un champ static final. *)
  transient : bool;
  cf_attributes : attributes
}

type interface_field = {
  (* implicitement public *)
  (* implicitement static *)
  (* implicitement final *)
  if_signature : field_signature;
  value : constant_value;
  if_attributes : attributes
}

(* Class and interface methods. *)
(********************************)

type code = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : opcodes;
  c_exc_tbl : jexception list;
  line_number_table : (int * int) list option;
  local_variable_table : (int * int * string * value_type * int) list option
}

type implementation =
  | Native
  | Java of code

type concrete_method = {
  static : bool;
  final : bool;
  synchronized : bool;
  strict : bool;
  implementation : implementation
}
(* l'attribut final n'a pas vraiment de sens pour une méthode statique. *)

type class_method_type =
  | Abstract
  | Concrete of concrete_method

type class_method = {
  cm_access: access;
  cm_signature : method_signature;
  method_type : class_method_type;
  cm_exceptions : class_name list;
  cm_attributes : attributes
}

type interface_method = {
  im_access: [`Public | `Protected | `Default];
  im_signature : method_signature;
  im_exceptions : class_name list;
  im_attributes : attributes
}

(* Contrainte supplémentaire : une méthode d'initialisation (d'instance)
   est forcément concrète, et ne peut pas être statique, finale ou
   synchronisée :

"A specific instance initialization method (§3.9) may have at most one
of its ACC_PRIVATE, ACC_PROTECTED, and ACC_PUBLIC flags set and may also
have its ACC_STRICT flag set, but may not have any of the other flags
in Table 4.5 set."

*)

(* Question : qu'est-ce qui empêche une classe concrète d'avoir des
   méthodes ou des champs abstraits ? *)

(* Classes and interfaces. *)
(***************************)

type class_type =
  | Final
  | Abstract
  | Normal

type pclass = {
  class_type : class_type;
  super : bool;
  super_class : class_name; (* useless if we use a map *)
  c_fields : class_field Map.Make(String).t;
  c_methods : class_method Map.Make(String).t
}

(* Finalement, la seule différence entre une classe abstraite et une
   classe concrète, c'est le final, donc autant tout fusionner (sauf
   si on ajoute des contraintes sur le contenu). *)

type pinterface = {
  (* super-class must be java.lang.Object. *)
  i_fields : interface_field Map.Make(String).t;
  i_methods : interface_method Map.Make(String).t
}

type class_file_type =
  | Class of pclass
  | Interface of pinterface

type class_file = {
  c_access : [`Public | `Default];
  interfaces : class_name list; (* useless if we use a map *)
  sourcefile : string option;
  c_deprecated : bool;
  c_other_attributes : (string * string) list;
  class_file_type : class_file_type
}

(* Ici, on peut peut-être réutiliser les attributs, même si synthetic n'a pas de sens *)

type program = class_file Map.Make(String).t

(* Pour les structures à rajouter autour, on peut mettre, par exemple, la hierarchie
   de classes complète ainsi que les relations de sous-typage. *)

(* Deux options pour améliorer cette interface :
   - utiliser des types paramétrés pour réutiliser les champs communs :
   pas beau et compliqué à lire
   - relâcher le typage sur certains aspects. *)

(*
(* Tout ça marche bien si on dit que les noms de champs sont des strings.
   Il faudrait autoriser plusieurs types différents. ça, on peut pas trop
   le faire sans un type map générique, et encore c'est pas top. *)

(* autre solution : utiliser des fonctions d'accès. *)

type (tous mes paramètres) 'class
val fields : ('fields, ...) 'class -> fields...

(* Définition alternative : *)
type 'kind class_file = {
  fields : 'kind field Map.Make(String).t;
  methods : 'kind meth Map.Make(String).t;
  attributes : class_attribute list
}

(* Solution avec des objets. *)
(*****************************)

(* Les objets, c'est pas mal, mais plus chiant à construire.
   On ne voit pas clairement que tout est constant.
   En plus, il faudrait utiliser des classes pour pouvoir
   réutiliser du code. On ne peut pas bien modéliser le fait
   que certaines sous-classes spécialisent des champs
   (toujours public, etc.) *)

class type element = object
  method synthetic : bool
  method deprecated : bool
  method custom_attributes : (string * string) list
end
(* Jusque là, on peut faire, sauf pour access, à moins qu'on
   passe le truc en paramètre de la classe. *)

class type field = object
  inherit element
  method if_signature : field_signature
end

class type oclass_field = object
  inherit field
  method access : access
  method transient : bool
  method static : bool
  method field_type : field_type
  method constant_value : constant_value option
    (* Valable seulement pour un champ static final. *)
end

class type ointerface_field = object
  inherit field
  (* implicitement public *)
  (* implicitement static *)
  (* implicitement final *)
  method value : constant_value
end

class type o_method = object
  inherit element
  method m_signature : method_signature
  method exceptions : class_name list
end

class type oclass_method = object
  inherit o_method
  method access : access
  method kind : method_type
end

(* On retombe sur le même problème : le type, ça veut rien dire. *)
class type ointerface_method = object
  inherit o_method
  method access : [`public | `default | `protected]
end

*)
