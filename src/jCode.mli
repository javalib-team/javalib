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

(** High level Ocaml representation of JVM opcodes. *)

open JBasics

(** {1 Bytecode instructions.} *)
(********************************)

type jconst = [
  | `ANull                                                (** AConstNull  *)
  | `Int of int32                                         (** IConst / ldc / ldc_w *)
  | `Long of int64                                        (** LConst / ldc2w  *)
  | `Float of float                                       (** FConst / ldc / ldc_w *)
  | `Double of float                                      (** DConst / ldc2w  *)
  | `Byte of int                                          (** BIPush *)
  | `Short of int                                         (** SIPush *)
  | `String of jstr                                       (** ldc / ldc_w  *)
  | `Class of object_type                                 (** ldc / ldc_w *)
  | `MethodType of method_descriptor                      (** ldc / ldc_w *)
  | `MethodHandle of method_handle                        (** ldc / ldc_w *)
]

type jinterface_or_class = [ `Class | `Interface ]

type jopcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int                 (** index, increment *)

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

  (* Constant loading / it corresponds to instructions *const* and ldc* *)
  | OpConst of jconst

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

  | OpCmp of [`L   (*Compare long value. See lcmp in jvm spec.*)
             |`FL  (*Compare float value. Push -1 as result if one of the
                     compared value is NaN. See fcmp in jvm spec.*)
             |`FG  (*Compare float value. Same as `FL but push 1 as result if
                     one of the compared value is NaN. See fcmp in jvm spec.*)
             |`DL  (*Compare double value. Push -1 as result if one of the
                     compared value is NaN. See dcmp in jvm spec.*)
             |`DG  (*Compare double value. Same as `DL but push 1 as result of
                     one of the compared value is NaN. See dcmp in jvm spec.*)
    ]

  (* Conditional jump *)
  | OpIf of [`Eq | `Ne | `Lt | `Ge | `Gt | `Le | `Null | `NonNull] * int
  | OpIfCmp of [`IEq
               | `INe
               | `ILt
               | `IGe
               | `IGt
               | `ILe
               | `AEq (*reference equality*)
               | `ANe] (*reference inequality*)
    * int

  (* Unconditional jump *)
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int * int32 * int32 * int array (** (default,low,high,jump offsets) *)
  | OpLookupSwitch of int * (int32 * int) list       (** (default, (match,offset) list) *)

  (* Heap and static fields *)
  | OpNew of class_name
  | OpNewArray of value_type
  | OpAMultiNewArray of object_type * int (** ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * field_signature
  | OpPutStatic of class_name * field_signature
  | OpGetField of class_name * field_signature
  | OpPutField of class_name * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke of [
    | `Virtual of object_type
    | `Special of jinterface_or_class * class_name
    | `Static of jinterface_or_class * class_name
    | `Interface of class_name
    | `Dynamic of bootstrap_method
    ]
	        * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid
      (** if [opcodes.(i) = OpInvalid] it means that there is an opcode that
          starts at position j, with j<i, an covers positions up to k, with
          k>=i.  If an opcode array is forged, the number of OpInvalid plus one
          must match the number of bytes on which the preceding instruction must
          be encoded. E.g. [[|OpLoad (`Int2Bool,1); OpInvalid|]] is encoded as
          an [iload 0X01]; [[|OpLoad (`Int2Bool,1)|]] is encoded as an [iload_1];
          [[|OpLoad (`Int2Bool,1); OpInvalid; OpInvalid; OpInvalid|]] is encoded
          as an [wide;iload 0x0001].  *)

type jopcodes = jopcode array

(** Exception handler. *)
type exception_handler = {
	e_start : int; (*Inclusive, first instr in the try block. *)
	e_end : int; (*Exclusive, first instr after try block. *)
	e_handler : int;
	e_catch_type : class_name option
}

(** High level representation of code. *)

(** Code structure. *)
type jcode = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : jopcodes;
  c_exc_tbl : exception_handler list;
  (** The list is ordered in the same way as in the bytecode (See JVM Spec se8 §2.10). *)
  c_line_number_table : (int * int) list option;
  (** (start_pc, line_number) *)
  c_local_variable_table : (int * int * string * value_type * int) list option;
  (** (start_pc, length, name, type, index) *)
  c_local_variable_type_table : (int * int * string * JSignature.fieldTypeSignature * int) list option;
  (** LocalVariableTable for generics, described in the JVM Spec se8, §4.7.14 *)
  c_stack_map : stackmap_frame list option;
  c_attributes : (string * string) list;
}

(** Empty list of opcodes *)
val empty : jcode

(** {1 Access functions.} *)

(** [get_source_line_number pp m] returns the source line number corresponding
    to the program point [pp] of the method code [m].  The line number give a
    rough idea and may be wrong. It uses the attribute LineNumberTable
    (cf. JVM Spec se8 §4.7.12). *)
val get_source_line_number : int -> jcode -> int option

(** [get_source_line_number pp lnt] returns the source line number corresponding
    to the program point [pp] given the LineNumberTable attribute [lnt]. The
    line number give a rough idea and may be wrong. It uses the attribute
    LineNumberTable (cf. JVM Spec se8 §4.7.12).  *)
val get_source_line_number' : int -> (int * int) list -> int option

(** [get_local_variable_info i pp m] returns the name and signature of
    the local variable [i] at program point [pp] in the method code
    [m] (including the [pp] of the first assignement of [i], it's not
    the case in [c_local_variable_table]), if they are defined in the
    local variable table (The bytecode needs to be compiled with the
    -g option). Otherwise the value [None] is returned.
*)
val get_local_variable_info :
  int -> int -> jcode -> (string * value_type) option

(** {1 Code modification functions.} *)

(** [replace_code code pp l] replaces the opcode present at program
   point [pp] in the [code] with the instructions contained in [l].
   For consistency, the program point [pp] must not refer to an
   [OpInvalid] instruction, and the list of instructions [l] should
   contain a correct number of [OpInvalid] after each core instruction
   in order to fulfill a correct line numbering of the generated
   bytecode. *)
val replace_code : jcode -> int -> jopcode list -> jcode

(** [insert_code code pp l] insert the the instructions contained in
   [l] at program point [pp] in the [code]. For consistency, the
   program point [pp] must not refer to an [OpInvalid] instruction,
   and the list of instructions [l] should contain a correct number of
   [OpInvalid] after each core instruction in order to fulfill a
   correct line numbering of the generated bytecode. *)
val insert_code : jcode -> int -> jopcode list -> jcode

(** [gen_stack_map_info e cn ms is_static code] returns a tuple
   [(max_stack, max_locals, stackmap_frames)] for the [code] contained
   in method [ms] of class [cn], necessary for the Java Bytecode
   Verifier to execute properly. The generated [max_stack] and
   [max_locals] should replace [code.c_max_stack] and
   [code.c_max_locals] respectively, while the frame list is meant to
   replace [code.c_stack_map]. The environment [e] which maps each
   class encountered in [code.c_code] to its superclass needs to be
   provided. The interfaces should not appear in [e]. *)
val gen_stack_map_info : class_name ClassMap.t ->
                        class_name -> method_signature -> bool -> jcode ->
                        int * int * stackmap_frame list

(** {1 Lambda manipulation.} *)
  
type lambda_info = {
  functional_interface : class_method_signature;
  captured_arguments : value_type list;
  checkcast_arguments : value_type list;
  lambda_handle : method_handle;
  }

(** [build_lambda_info bm ms] builds an information caracterizing the
   construction of the lambda expression referenced by the opcode
   [OpInvoke (`Dynamic bm, ms)]. *)
val build_lambda_info : bootstrap_method -> method_signature -> lambda_info
