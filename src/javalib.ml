(*
 * This file is part of Javalib
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
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

include JClass
include JFile

open JBasics

let replace_dot s =
  let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '.' then s.[i] <- '/'
    done;
    s

let unparse_class ioc out =
  let out_chan = IO.output_channel out in
    JUnparse.unparse_class out_chan ioc

module JPrint =
struct
  let class_name ?(jvm=false) cn =
    let cname = cn_name cn in
      if jvm then
	"L" ^ (replace_dot cname) ^ ";"
      else cname

  let cn_package cn =
    String.concat "." (cn_package cn)

  let cn_simple_name = cn_simple_name

  let java_basic_type ?(jvm=false) bt =
    match bt with
      | `Bool ->
	  if jvm then "Z" else "bool"
      | `Byte ->
	  if jvm then "B" else "byte"
      | `Char ->
	  if jvm then "C" else "char"
      | `Double ->
	  if jvm then "D" else "double"
      | `Float ->
	  if jvm then "F" else "float"
      | `Int ->
	  if jvm then "I" else "int"
      | `Long ->
	  if jvm then "J" else "long"
      | `Short ->
	  if jvm then "S" else "short"

  let rec object_type ?(jvm=false) ot =
    match ot with
      | TClass cn -> class_name ~jvm:jvm cn
      | TArray vt ->
	  if jvm then
	    "[" ^ (value_type ~jvm:true vt)
	  else (value_type vt) ^ "[]"

  and value_type ?(jvm=false) vt =
    match vt with
      | TBasic bt -> java_basic_type ~jvm:jvm bt
      | TObject ot -> object_type ~jvm:jvm ot

  let field_descriptor = value_type

  let value_type_list ?names l =
    let prms =
      match names with
	| None -> List.map value_type l
	| Some names ->
	    (* names must have the same length than l *)
	    try
	      List.map2
		(fun v name ->
		   (value_type v) ^ " " ^ name) l names
	    with
		_ -> invalid_arg "Bad length for names list."
    in
      "(" ^ (String.concat ", " prms) ^ ")"

  let return_type ?(jvm=false) v =
    match v with
      | None ->
	  if jvm then "V"
	  else "void"
      | Some v -> value_type ~jvm:jvm v

  let method_descriptor ?(jvm=false) args ret =
    let ret = return_type ~jvm:jvm ret in
      if jvm then
	"(" ^ (String.concat "" (List.map (value_type ~jvm:true) args))
	^ ")" ^ ret
      else
	ret ^ " " ^ (value_type_list args)

  let method_signature ?(jvm=false) ?callee ?param_names ms =
    let mname = ms_name ms in
    let margs = ms_args ms in
    let mrtype = ms_rtype ms in
    let cname =
      match callee with
	| None -> ""
	| Some ot -> (object_type ot) ^ "." in
      if jvm then
	cname ^ mname ^ ":" ^ (method_descriptor ~jvm:true margs mrtype)
      else
	let ret = return_type mrtype
        and args =
        match param_names with
          | Some names -> value_type_list ~names margs
          | None -> value_type_list margs
        in
	  ret ^ " " ^ cname ^ mname ^ args

  let class_method_signature ?jvm ?param_names cms =
    let (cn,ms) = cms_split cms in
    let callee = TClass cn in
      match param_names,jvm with
        | None, None
            -> method_signature ~callee ms
        | Some param_names, None
            -> method_signature ~param_names ~callee ms
        | None, Some jvm
            -> method_signature ~jvm ~callee ms
        | Some param_names, Some jvm
            -> method_signature ~jvm ~param_names ~callee ms

  let field_signature ?(jvm=false) ?declared_in fs =
    let fname = fs_name fs in
    let fd = fs_type fs in
    let cn = match declared_in with
      | None -> ""
      | Some cn -> class_name cn ^"."
    in
      if jvm then
	cn ^ fname ^ ":" ^ (field_descriptor ~jvm:true fd)
      else
	(field_descriptor fd) ^ " " ^ cn ^ fname

  let class_field_signature ?jvm cfs =
    let (declared_in,fs) = cfs_split cfs in
      match jvm with
        | None -> field_signature ~declared_in fs
        | Some jvm -> field_signature ~jvm ~declared_in fs

  let signature ?(jvm=false) name d =
    match d with
      | SValue fd ->
	  let fs = make_fs name fd in
	    field_signature ~jvm:jvm fs
      | SMethod md ->
	  let ms = make_ms name (fst md) (snd md) in
	    method_signature ~jvm:jvm ms

  let constant_value = function
    | ConstString s -> "string '" ^ s ^ "'"
    | ConstInt i -> "int " ^ (Int32.to_string i)
    | ConstFloat f -> "float " ^ (string_of_float f)
    | ConstLong i -> "long " ^ (Int64.to_string i)
    | ConstDouble f -> "double " ^ (string_of_float f)
    | ConstClass cl -> "class " ^ (object_type cl)

  let constant = function
    | ConstValue v -> constant_value v
    | ConstField (cl,fname,fd) ->
	"field : " ^ (field_descriptor fd) ^ "  " ^ (class_name cl)
	^ "::" ^ fname
    | ConstMethod (ot,mname,md) ->
	let ms = make_ms mname (fst md) (snd md) in
	  "method : "
	  ^ (method_signature ~callee:ot ms)
    | ConstInterfaceMethod (cn,mname,md) ->
	let ms = make_ms mname (fst md) (snd md) in
	  "interface-method : "
	  ^ (method_signature ~callee:(TClass cn) ms)
    | ConstNameAndType (s,d) -> "name-and-type : " ^ (signature s d)
    | ConstStringUTF8 s -> "utf8 " ^ s
    | ConstUnusable -> "unusable"

  let constant_pool p =
    let s = ref "" in
      Array.iteri
	(fun i c ->
	   s :=
	     !s ^ "    " ^ (string_of_int i) ^ "  " ^ (constant c) ^ "\n") p;
      !s

  let stack_map (offset,locals,stack) =
    let verif_info = function
      | VTop -> "Top"
      | VInteger -> "Integer"
      | VFloat -> "Float"
      | VDouble -> "Double"
      | VLong -> "Long"
      | VNull -> "Null"
      | VUninitializedThis -> "UninitializedThis"
      | VObject c -> Printf.sprintf "Object %s" (object_type c)
      | VUninitialized off -> Printf.sprintf "Uninitialized %d" off
    in
    let s = ref (Printf.sprintf "\n      offset=%d,\n      locals=[" offset) in
      List.iter
	(fun t ->
	   s := !s ^ Printf.sprintf  "\n        %s" (verif_info t)) locals;
      s := !s ^ "],\n      stack=[";
      List.iter
	(fun t ->
	   s := Printf.sprintf "\n        %s" (verif_info t)) stack;
      !s

  open JCode
  let exception_handler exc =
    let s = ref (Printf.sprintf "\n      [%d-%d] -> %d ("
		   exc.e_start exc.e_end exc.e_handler) in
      (match exc.e_catch_type with
	 | None -> s := !s ^ "<finally>"
	 | Some cl -> s := !s ^ Printf.sprintf "class %s" (class_name cl));
      !s ^ ")"

  let jopcode ?(jvm=false) op =
    let s = JDump.opcode op in
      if jvm then s
      else
	match op with
	  | OpNew cn -> Printf.sprintf "new %s" (cn_name cn)
	  | OpNewArray v ->
	      (match v with
		 | TBasic b -> Printf.sprintf "newarray %s" (java_basic_type b)
		 | TObject o ->
		     Printf.sprintf "anewarray %s" (object_type o)
	      )
	  | OpAMultiNewArray (o,i) ->
	      Printf.sprintf "amultinewarray %s %d" (object_type o) i
	  | OpCheckCast t -> Printf.sprintf "checkcast %s" (object_type t)
	  | OpInstanceOf t -> Printf.sprintf "instanceof %s" (object_type t)
	  | OpGetStatic (cn,fs) ->
	      Printf.sprintf "getstatic %s.%s : %s" (cn_name cn) (fs_name fs)
		(value_type (fs_type fs))
	  | OpPutStatic (cn,fs) ->
	      Printf.sprintf "putstatic %s.%s : %s" (cn_name cn) (fs_name fs)
		(value_type (fs_type fs))
	  | OpGetField (cn,fs) ->
	      Printf.sprintf "getfield %s.%s : %s" (cn_name cn) (fs_name fs)
		(value_type (fs_type fs))
	  | OpPutField (cn,fs) ->
	      Printf.sprintf "putfield %s.%s : %s" (cn_name cn) (fs_name fs)
		(value_type (fs_type fs))
	  | OpInvoke ((`Virtual o),ms) ->
	      Printf.sprintf "invokevirtual %s.%s%s : %s" (object_type o)
		(ms_name ms) (value_type_list (ms_args ms))
		(return_type (ms_rtype ms))
	  | OpInvoke ((`Interface cn),ms) ->
	      Printf.sprintf "invokeinterface %s.%s%s : %s" (cn_name cn)
		(ms_name ms) (value_type_list (ms_args ms))
		(return_type (ms_rtype ms))
	  | OpInvoke ((`Static cn),ms) ->
	      Printf.sprintf "invokestatic %s.%s%s : %s" (cn_name cn)
		(ms_name ms) (value_type_list (ms_args ms))
		(return_type (ms_rtype ms))
	  | OpInvoke ((`Special cn),ms) ->
	      Printf.sprintf "invokespecial %s.%s%s : %s" (cn_name cn)
		(ms_name ms) (value_type_list (ms_args ms))
		(return_type (ms_rtype ms))
	  | _ -> s

  let jcode ?(jvm=false) code =
    let cl = Array.to_list (Array.mapi (fun i op -> (i,op)) code.c_code) in
      ExtList.List.filter_map
	(fun (i,op) ->
	   match op with
	     | OpInvalid -> None
	     | _ -> Some(Printf.sprintf "%d: %s" i (jopcode ~jvm:jvm op))
	) cl

  let method_access m =
    let access a =
      match a with
	| `Default -> None
	| `Protected -> Some "protected"
	| `Public -> Some "public"
	| `Private -> Some "private" in
      match m with
	| AbstractMethod am ->
	    access am.am_access
	| ConcreteMethod cm ->
	    access cm.cm_access

  let field_access f =
    let access a =
      match a with
	| `Default -> None
	| `Protected -> Some "protected"
	| `Public -> Some "public"
	| `Private -> Some "private" in
      match f with
	| ClassField cf -> access cf.cf_access
	| InterfaceField _ -> Some "public"

  let interface_or_class_access ioc =
    let access = get_access ioc in
      match access with
	| `Default -> None
	| `Public -> Some "public"

  let method_kind m =
    match m with
      | AbstractMethod _ -> Some "abstract"
      | ConcreteMethod cm ->
	  (match cm.cm_implementation with
	     | Java _ -> None
	     | Native -> Some "native"
	  )

  let field_kind f =
    match f with
      | ClassField cf ->
	  (match cf.cf_kind with
	     | Final -> Some "final"
	     | Volatile -> Some "volatile"
	     | NotFinal -> None
	  )
      | InterfaceField _ -> None

  let method_static m =
    if (is_static_method m) then Some "static"
    else None

  let field_static f =
    match f with
      | ClassField cf ->
	  if cf.cf_static then Some "static"
	  else None
      | InterfaceField _ -> Some "static"

  let interface_or_class_abstract ioc =
    match ioc with
      | JInterface _ -> None
      | JClass c ->
	  if c.c_abstract then Some "abstract"
	  else None

  let method_final m =
    if (is_final_method m) then Some "final"
    else None

  let interface_or_class_final ioc =
    match ioc with
      | JInterface _ -> None
      | JClass c ->
	  if c.c_final then Some "final"
	  else None

  let method_synchronized m =
    if (is_synchronized_method m) then Some "synchronized"
    else None

  let acmethod ?(jvm=false) (m:'a jmethod) (f:'a -> string list) =
    let ms = get_method_signature m in
    let header =
      String.concat " " (ExtList.List.filter_map
			   (fun x -> x)
			   [method_access m; method_static m;
			    method_final m; method_synchronized m;
			    method_kind m]) in
    let header = if header = "" then header else header ^ " " in
      match m with
	| AbstractMethod _ ->
	    if jvm then
	      (method_signature ~jvm:true ms, [])
	    else
	      (Printf.sprintf "%s%s" header (method_signature ms), [])
	| ConcreteMethod cm ->
	    (match cm.cm_implementation with
	       | Native ->
		   if jvm then
		     (method_signature ~jvm:true ms, [])
		   else
		     (Printf.sprintf "%s%s" header (method_signature ms), [])
	       | Java impl ->
                   let impl = Lazy.force impl in
		   if jvm then
		     (method_signature ~jvm:true ms, f impl)
		   else
		     (Printf.sprintf "%s%s" header (method_signature ms),
		      f impl)
	    )

  let any_field ?(jvm=false) (f : any_field) : string =
    let fs = get_field_signature f in
    let header =
      String.concat " " (ExtList.List.filter_map
			   (fun x -> x)
			   [field_access f; field_static f;
			    field_kind f]) in
    let header = if header = "" then header else header ^ " " in
      if jvm then field_signature ~jvm:true fs
      else
	Printf.sprintf "%s%s" header (field_signature fs)

  let print_method_fmt indent_val header instructions fmt =
    Format.pp_open_vbox fmt indent_val;
    Format.pp_print_string fmt (header ^ " {");
    Format.pp_force_newline fmt ();
    let len = List.length instructions in
      ExtList.List.iteri
	(fun i s ->
	   Format.pp_print_string fmt s;
	   if (i < len - 1) then Format.pp_force_newline fmt ()) instructions;
      Format.pp_close_box fmt ();
      Format.pp_force_newline fmt ();
      Format.pp_print_string fmt "}";
      Format.pp_force_newline fmt ();
      Format.pp_print_flush fmt ()

  let print_method ?(jvm=false) (m:'a jmethod) (f:'a -> string list)
      (out:out_channel) =
    let indent_val = 3 in
    let (header,instructions) = acmethod ~jvm:jvm m f in
    let fmt = Format.formatter_of_out_channel out in
      print_method_fmt indent_val header instructions fmt

  (* TODO : maybe add implemented interfaces and extended classes in class description. *)
  let print_class ?(jvm=false) (ioc:'a interface_or_class) (f:'a -> string list)
      (out:out_channel) =
    let indent_val = 3 in
    let fmt = Format.formatter_of_out_channel out in
    let name = cn_name (get_name ioc) in
    let header = String.concat " "
      (ExtList.List.filter_map
	 (fun x -> x)
	 [interface_or_class_access ioc; interface_or_class_final ioc;
	  interface_or_class_abstract ioc]) in
    let header = if header = "" then header else header ^ " " in
    let fields = get_fields ioc in
      match ioc with
	| JInterface i ->
	    Format.pp_print_string fmt
	      (Printf.sprintf "%sinterface %s {" header name);
	    Format.pp_force_newline fmt ();
	    if not(fields = FieldMap.empty) then
	      begin
		Format.pp_open_vbox fmt indent_val;
		FieldMap.iter
		  (fun _ f ->
		     Format.pp_force_newline fmt ();
		     Format.pp_print_string fmt (any_field ~jvm:jvm f);
		     Format.pp_force_newline fmt ()
		  ) fields;
		Format.pp_close_box fmt ();
		Format.pp_force_newline fmt ()
	      end;
	    MethodMap.iter
	      (fun _ m ->
		 Format.pp_open_vbox fmt indent_val;
		 Format.pp_force_newline fmt ();
		 let (header,instructions) =
		   acmethod ~jvm:jvm (AbstractMethod m) f in
		   print_method_fmt indent_val header instructions fmt;
		   Format.pp_close_box fmt ()
	      ) i.i_methods;
	    Format.pp_force_newline fmt ();
	    Format.pp_print_string fmt "}";
	    Format.pp_force_newline fmt ();
	    Format.pp_print_flush fmt ()
	| JClass c ->
	    Format.pp_print_string fmt
	      (Printf.sprintf "%s class %s {" header name);
	    Format.pp_force_newline fmt ();
	    if not(fields = FieldMap.empty) then
	      begin
		Format.pp_open_vbox fmt indent_val;
		FieldMap.iter
		  (fun _ f ->
		     Format.pp_force_newline fmt ();
		     Format.pp_print_string fmt (any_field ~jvm:jvm f);
		     Format.pp_force_newline fmt ()
		  ) fields;
		Format.pp_close_box fmt ();
		Format.pp_force_newline fmt ()
	      end;
	    MethodMap.iter
	      (fun _ m ->
		 Format.pp_open_vbox fmt indent_val;
		 Format.pp_force_newline fmt ();
		 let (header,instructions) = acmethod ~jvm:jvm m f in
		   print_method_fmt indent_val header instructions fmt;
		   Format.pp_close_box fmt ()
	      ) c.c_methods;
	    Format.pp_force_newline fmt ();
	    Format.pp_print_string fmt "}";
	    Format.pp_force_newline fmt ();
	    Format.pp_print_flush fmt ()

  let print_jasmin c ch =
    let ioch = IO.output_channel ch in
    let clow = JHigh2Low.high2low c in
      JDumpJasmin.dump ioch clow
end

let iter = JFile.iter
