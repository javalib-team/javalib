(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
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

open JBasics
open JSignature

(* Validate an utf8 string and return a stream of characters. *)
let read_utf8 s =
  JLib.UTF8.validate s;
  let index = ref 0 in
    Stream.from
      (function _ ->
	 if JLib.UTF8.out_of_range s ! index
	 then None
	 else
	   let c = JLib.UTF8.look s ! index in
	     index := JLib.UTF8.next s ! index;
	     Some c)

(* Java ident, with unicode letter and numbers, starting with a letter. *)
let parse_ident buff =
  let parse_char = parser
    | [< 'c when c <> JLib.UChar.of_char ';'  (* should be a letter or a number *)
	   && c <> JLib.UChar.of_char '/'
	   && c <> JLib.UChar.of_char ':'
	   && c <> JLib.UChar.of_char '.'
	   && c <> JLib.UChar.of_char '>'
	   && c <> JLib.UChar.of_char '<' >] -> c
  in
  let rec parse_more_ident buff = parser
      (* TODO : it seems to be relatively inefficient *)
    | [< c = parse_char;
	 name =
	  (JLib.UTF8.Buf.add_char buff c;
	   parse_more_ident buff) >] -> name
    | [< >] ->
	JLib.UTF8.Buf.contents buff
  in
    parser
      | [< c = parse_char; (* should be a letter *)
	   name =
	    (JLib.UTF8.Buf.add_char buff c;
	     parse_more_ident buff) >] -> name


(* Qualified name (internally encoded with '/'). *)
let rec parse_name = parser
  | [< ident = parse_ident (JLib.UTF8.Buf.create 0);
       name = parse_more_name >] -> ident :: name

and parse_more_name = parser
  | [< 'slash when slash = JLib.UChar.of_char '/';
       name = parse_name >] -> name
  | [< >] -> []

let get_name x = make_cn (String.concat "." (parse_name x))

(* Java type. *)
let parse_base_type = parser
  | [< 'b when b = JLib.UChar.of_char 'B' >] -> `Byte
  | [< 'c when c = JLib.UChar.of_char 'C' >] -> `Char
  | [< 'd when d = JLib.UChar.of_char 'D' >] -> `Double
  | [< 'f when f = JLib.UChar.of_char 'F' >] -> `Float
  | [< 'i when i = JLib.UChar.of_char 'I' >] -> `Int
  | [< 'j when j = JLib.UChar.of_char 'J' >] -> `Long
  | [< 's when s = JLib.UChar.of_char 'S' >] -> `Short
  | [< 'z when z = JLib.UChar.of_char 'Z' >] -> `Bool

let rec parse_object_type = parser
  | [< 'l when l = JLib.UChar.of_char 'L';
       name = get_name;
       'semicolon when semicolon = JLib.UChar.of_char ';' >] -> TClass name
  | [< a = parse_array >] -> a

and parse_array = parser
  | [< 'lbracket when lbracket = JLib.UChar.of_char '['; typ = parse_type >] ->
      TArray typ

and parse_type = parser
  | [< b = parse_base_type >] -> TBasic b
  | [< o = parse_object_type >] -> TObject o

let rec parse_types = parser
  | [< typ = parse_type ; types = parse_types >] -> typ :: types
  | [< >] -> []

let parse_type_option = parser
  | [< typ = parse_type >] -> Some typ
  | [< >] -> None

(* A class name, possibly an array class. *)
let parse_ot = parser
  | [< array = parse_array >] -> array
  | [< name = get_name >] -> TClass name

let parse_method_sig = parser
  | [< 'lpar when lpar = JLib.UChar.of_char '(';
       types = parse_types;
       'rpar when rpar = JLib.UChar.of_char ')';
       typ = parse_type_option >] ->
     make_md (types, typ)

(* Java signature. *)
let parse_sig = parser
    (* We cannot delete that because of "NameAndType" constants. *)
  | [< typ = parse_type >] -> SValue typ
  | [< sign = parse_method_sig >] -> SMethod sign

let parse_method_descriptor s =
  try
    parse_method_sig (read_utf8 s)
  with
    Stream.Failure -> raise (Class_structure_error ("Illegal method signature: " ^ s))

let parse_descriptor s =
  try
    parse_sig (read_utf8 s)
  with
    Stream.Failure -> raise (Class_structure_error ("Illegal signature: " ^ s))

type tokens_mfd =
  | MFDChain of int * int
  | MFDSep | MFDSlash | MFDOpen | MFDClose
  
let rec tokenize_mfd s i l =
  if (i = String.length s) then List.rev l
  else
    match s.[i] with
    | ';' -> tokenize_mfd s (i+1) (MFDSep :: l)
    | '/' -> tokenize_mfd s (i+1) (MFDSlash :: l)
    | '(' -> tokenize_mfd s (i+1) (MFDOpen :: l)
    | ')' -> tokenize_mfd s (i+1) (MFDClose :: l)    
    | _ -> (match l with
            | (MFDChain (a,_)) :: tl -> tokenize_mfd s (i+1) ((MFDChain (a,i)) :: tl)
            | _ -> tokenize_mfd s (i+1) ((MFDChain (i,i)) :: l)
           )
         
let tokenize_mfd s = tokenize_mfd s 0 []

let rec parse_value_type l s =
  match l with
  | MFDChain (a, b) :: l' ->
     if (b = a && l'=[]) then
       (match s.[a] with
        | 'B' -> (TBasic `Byte, l')
        | 'C' -> (TBasic `Char, l')
        | 'D' -> (TBasic `Double, l')
        | 'F' -> (TBasic `Float, l')
        | 'I' -> (TBasic `Int, l')
        | 'J' -> (TBasic `Long, l')
        | 'S' -> (TBasic `Short, l')
        | 'Z' -> (TBasic `Bool, l')
        | _ -> failwith "Invalid ValueType"
       )
     else
       let obj, l' = parse_object_type l s in
       (TObject obj, l')
  | _ -> failwith "Invalid FieldDescriptor"
       
and parse_object_type l s =
  match l with
  | MFDChain (a, b) :: l' ->
     (match s.[a] with
      | 'L' -> parse_class_type (MFDChain (a+1, b) :: l') s []
      | '[' ->
         let vt, l'' = parse_value_type (MFDChain (a+1,b) :: l') s in
         (TArray vt, l'')
      | _ -> failwith ("Invalid ObjectType : " ^ s)
     )
  | _ -> failwith ("Invalid ObjectType : " ^ s)

and parse_class_type l s lcn =
  match l with
  | MFDChain (a, b) :: MFDSlash :: l' ->
     parse_class_type l' s ((String.sub s a (b-a+1)) :: lcn)
  | MFDChain (a, b) :: MFDSep :: l' ->
     let lcn = List.rev ((String.sub s a (b-a+1)) :: lcn) in
     let cl = TClass (make_cn (String.concat "." lcn)) in
     (cl, l')
  | _ -> failwith ("Invalid ClassType : " ^ s)

let parse_field_descriptor s =
  let vt, l = parse_value_type (tokenize_mfd s) s in
  match l with
  | [] -> vt
  | _ -> failwith ("Invalid FieldDescriptor : " ^ s)

let parse_objectType s =
  let s =
    match s.[0] with
    | '[' | 'L' -> s
    | _ -> Printf.sprintf "L%s;" s in
  let obj, l = parse_object_type (tokenize_mfd s) s in
  match l with
  | [] -> obj
  | _ -> failwith ("Invalid ObjectType : " ^ s)
         
(* Java 5 signature *)

type tokens =
  | Chain of int * int
  | Slash | Sep | WildStar | WildPlus | WildMinus
  | Suffix | Bound | Throws
  | OpenSig | CloseSig
  | OpenGen | CloseGen
  
let rec tokenize s i l =
  if (i = String.length s) then List.rev l
  else
    match s.[i] with
    | '.' -> tokenize s (i+1) (Suffix :: l)
    | '<' -> tokenize s (i+1) (OpenGen :: l)
    | '>' -> tokenize s (i+1) (CloseGen :: l)
    | '(' -> tokenize s (i+1) (OpenSig :: l)
    | ')' -> tokenize s (i+1) (CloseSig :: l)
    | '/' -> tokenize s (i+1) (Slash :: l)
    | '*' -> tokenize s (i+1) (WildStar :: l)
    | '+' -> tokenize s (i+1) (WildPlus :: l)
    | '-' -> tokenize s (i+1) (WildMinus :: l)
    | ':' -> tokenize s (i+1) (Bound :: l)
    | '^' -> tokenize s (i+1) (Throws :: l)
    | ';' -> tokenize s (i+1) (Sep :: l)
    | _ -> (match l with
            | (Chain (a,_)) :: tl -> tokenize s (i+1) ((Chain (a,i)) :: tl)
            | _ -> tokenize s (i+1) ((Chain (i,i)) :: l)
           )
let tokenize s = tokenize s 0 []

let parse_BaseType l s =
  match l with
  | Chain (a, b) :: tl ->
     let l' = if (a+1 > b) then tl else Chain (a+1, b) :: tl in
     (match s.[a] with
      | 'B' -> `Byte, l'
      | 'C' -> `Char, l'
      | 'D' -> `Double, l'
      | 'F' -> `Float, l'
      | 'I' -> `Int, l'
      | 'J' -> `Long, l'
      | 'S' -> `Short, l'
      | 'Z' -> `Bool, l'
      | _ -> failwith "Invalid BaseType"
     )
  | _ -> failwith "Invalid BaseType"

let parse_VoidDescriptor l s =
  match l with
  | Chain (a, b) :: tl ->
     let l' = if (a+1 > b) then tl else failwith "Invalid VoidDescriptor" in
     (match s.[a] with
      | 'V' -> (None, l')
      | _ -> failwith "Invalid VoidDescriptor"
     )
  | _ -> failwith "Invalid VoidDescriptor"
       
let rec parse_PackageSpecifier l s lpack =
  match l with
  | Chain (a, b) :: Slash :: l' ->
     parse_PackageSpecifier l' s (String.sub s a (b-a+1) :: lpack)
  | _ -> (List.rev lpack, l)

let parse_PackageSpecifier l s = parse_PackageSpecifier l s []

let parse_TypeVariableSignature l s =
  match l with
  | Chain (a, b) :: Sep :: l' ->
     (TypeVariable (String.sub s a (b-a+1)), l')
  | _ -> failwith "Invalid TypeVariableSignature"

let rec parse_TypeArguments l s largs =
  match l with
  | WildStar :: l' ->
     parse_TypeArguments l' s (ArgumentIsAny :: largs)
  | WildPlus :: l' ->
     let rts, l'' = parse_ReferenceTypeSignature l' s in
     parse_TypeArguments l'' s (ArgumentExtends rts :: largs)
  | WildMinus :: l' ->
     let rts, l'' = parse_ReferenceTypeSignature l' s in
     parse_TypeArguments l'' s (ArgumentInherits rts :: largs)
  | CloseGen :: l' ->
     (List.rev largs, l')
  | l' ->
     let rts, l'' = parse_ReferenceTypeSignature l' s in
     parse_TypeArguments l'' s (ArgumentIs rts :: largs)

and parse_SimpleClassTypeSignature l s =
  match l with
  | Chain (a, b) :: OpenGen :: l' ->
     let type_args, l'' = parse_TypeArguments l' s [] in
     ({ scts_name = String.sub s a (b-a+1);
        scts_type_arguments = type_args; }, l'')
  | Chain (a, b) :: l' ->
     ({ scts_name = String.sub s a (b-a+1);
        scts_type_arguments = []; }, l')
  | _ -> failwith "InvalidSimpleClassTypeSignature"

and parse_ClassTypeSignatureSuffix l s lsuff =
  match l with
  | Suffix :: l' ->
     let scts, l'' = parse_SimpleClassTypeSignature l' s in
     parse_ClassTypeSignatureSuffix l'' s (scts :: lsuff)
  | _ -> (List.rev lsuff, l)

and parse_ClassTypeSignature l s =
  let package, l = parse_PackageSpecifier l s in
  let scts, l = parse_SimpleClassTypeSignature l s in
  let enclosing_classes, l = parse_ClassTypeSignatureSuffix l s [] in
  let cts = { cts_package = package;
              cts_enclosing_classes = enclosing_classes;
              cts_simple_class_type_signature = scts;
            } in
  match l with
  | Sep :: l' -> (cts, l')
  | _ -> failwith "Invalid ClassTypeSignature"

and parse_ReferenceTypeSignature l s =
  match l with
  | Chain (a, b) :: tl ->
     let l' = Chain (a+1, b) :: tl in
     (match s.[a] with
      | 'L' ->
         let cts, l'' = parse_ClassTypeSignature l' s in
         (GClass cts, l'')
      | 'T' ->
         let tv, l'' = parse_TypeVariableSignature l' s in
         (GVariable tv, l'')
      | '[' ->
         let jts, l'' = parse_JavaTypeSignature l' s in
         (GArray jts, l'')
      | _ -> failwith "Invalid ReferenceTypeSignature"
     )
  | _ -> failwith "Invalid ReferenceTypeSignature"

and parse_JavaTypeSignature l s =
  match l with
  | Chain (a, _) :: _ ->
     let c = s.[a] in
     if (c = 'L' || c = 'T' || c = '[') then
       let rts, l' = parse_ReferenceTypeSignature l s in
       (GObject rts, l')
     else
       let bt, l' = parse_BaseType l s in
       (GBasic bt, l')
  | _ -> failwith "Invalid JavaTypeSignature"

let parse_Result l s =
  match l with
  | Chain (a, _) :: _ ->
     if (s.[a] = 'V') then parse_VoidDescriptor l s
     else
       let jts, l' = parse_JavaTypeSignature l s in
       (Some jts, l')
  | _ -> failwith "Invalid Result"

let rec parse_InterfaceBounds l s lib =
  match l with
  | Bound :: l' ->
     let rts, l'' = parse_ReferenceTypeSignature l' s in
     parse_InterfaceBounds l'' s (rts :: lib)
  | _ -> (List.rev lib, l)

let rec parse_TypeParameters l s ltp =
  match l with
  | Chain (a, b) :: Bound :: l' ->
     let name = String.sub s a (b-a+1) in
     (match l' with
      | Bound :: _ ->
         let i_bounds, l'' = parse_InterfaceBounds l' s [] in
         parse_TypeParameters l'' s ({ ftp_name = name;
                                       ftp_class_bound = None;
                                       ftp_interface_bounds = i_bounds;
                                     } :: ltp)
      | _ ->
         let rts, l'' = parse_ReferenceTypeSignature l' s in
         let i_bounds, l'' = parse_InterfaceBounds l'' s [] in
         parse_TypeParameters l'' s ({ ftp_name = name;
                                       ftp_class_bound = Some rts;
                                       ftp_interface_bounds = i_bounds;
                                     } :: ltp)
     )
  | CloseGen :: l' -> (List.rev ltp, l')
  | _ -> failwith "Invalid TypeParameters"

let parse_TypeParameters_opt l s =
  match l with
  | OpenGen :: l' -> parse_TypeParameters l' s []
  | _ -> ([], l)

let rec parse_SuperInterfaceSignatures l s lsis =
  match l with
  | [] -> List.rev lsis
  | Chain (a, b) :: l' ->
     if (s.[a] = 'L') then 
       let sis, l'' = parse_ClassTypeSignature (Chain (a+1,b) :: l') s in
       parse_SuperInterfaceSignatures l'' s (sis :: lsis)
     else
       failwith "Invalid SuperInterfaceSignatures"
  | _ -> failwith "Invalid SuperInterfaceSignatures"
       
let parse_ClassSignature l s =
  let ftp, l = parse_TypeParameters_opt l s in
  let scs, l =
    match l with
    | Chain (a,b) :: l' ->
       if (s.[a] = 'L') then parse_ClassTypeSignature (Chain (a+1,b) :: l') s
       else failwith "Invalid ClassSignature"
    | _ -> failwith "Invalid ClassSignature" in
  let sis = parse_SuperInterfaceSignatures l s [] in
  { cs_formal_type_parameters = ftp;
    cs_super_class = scs;
    cs_super_interfaces = sis;
  }

let parse_ClassSignature s = parse_ClassSignature (tokenize s) s

let rec parse_JavaTypeSignature_opt l s ljts =
  match l with
  | CloseSig :: l' -> (List.rev ljts, l')
  | _ ->
     let jts, l' = parse_JavaTypeSignature l s in
     parse_JavaTypeSignature_opt l' s (jts :: ljts)
    
let parse_TypeSignature l s =
  match l with
  | OpenSig :: l' -> parse_JavaTypeSignature_opt l' s []
  | _ -> failwith "Invalid TypeSignature"
      
let rec parse_ThrowsSignature l s ls =
  match l with
  | [] -> List.rev ls
  | Throws :: Chain (a, b) :: l' ->
     let l' = Chain (a+1, b) :: l' in
     if (s.[a] = 'L') then
       let cts, l'' = parse_ClassTypeSignature l' s in
       parse_ThrowsSignature l'' s (ThrowsClass cts :: ls)
     else if (s.[a] = 'T') then
       let cts, l'' = parse_TypeVariableSignature l' s in
       parse_ThrowsSignature l'' s (ThrowsTypeVariable cts :: ls)
     else failwith "Invalid ThrowsSignature"
  | _ -> failwith "Invalid ThrowsSignature"

let parse_MethodSignature l s =
  let ftp, l = parse_TypeParameters_opt l s in
  let ts, l = parse_TypeSignature l s in
  let rt, l = parse_Result l s in
  let th = parse_ThrowsSignature l s [] in
  { mts_formal_type_parameters = ftp;
    mts_type_signature = ts;
    mts_return_type = rt;
    mts_throws = th; }

let parse_MethodTypeSignature s = parse_MethodSignature (tokenize s) s

let parse_FieldTypeSignature s =
  let fs, l = parse_ReferenceTypeSignature (tokenize s) s in
  match l with
  | [] -> fs
  | _ -> failwith "Invalid FieldTypeSignature"

