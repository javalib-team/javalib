(*
 * This file is part of JavaLib
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

open Genlex
  
type jmethod = { m_type : string;
		 m_class : string;
		 m_name : string;
		 m_signature : string
	       }
    
let jmethod_compare m1 m2 =
  if (m1.m_type = m2.m_type) then
    if (m1.m_class = m2.m_class) then
      if (m1.m_name = m2.m_name) then
	if (m1.m_signature = m2.m_signature) then 0
	else compare m1.m_signature m2.m_signature
      else compare m1.m_name m2.m_name
    else compare m1.m_class m2.m_class
  else compare m1.m_type m2.m_type
    
module ClassSignatureSet = Set.Make(
  struct
    type t = string
    let compare = compare
  end)
  
module MethodSet = Set.Make(
  struct
    type t = jmethod
    let compare = jmethod_compare
  end)
  
module MethodMap = Map.Make(
  struct
    type t = jmethod
    let compare = jmethod_compare
  end)
  
module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)
  
type native_method_info = { native_alloc : ClassSignatureSet.t;
			    native_calls : MethodSet.t }
    
type native_info = native_method_info MethodMap.t
    
let keywords = ["{"; "}"]
  
let rec parse_methodentry_attrs expr =
  match expr with parser
    | [< 'Ident "thread"; 'Ident "="; 'String mthread; p = parse_methodentry_attrs >] ->
	StringMap.add "thread" mthread p
    | [< 'Ident "type"; 'Ident "="; 'String mtype; p = parse_methodentry_attrs >] ->
	StringMap.add "type" mtype p
    | [< 'Ident "class"; 'Ident "="; 'String cname; p = parse_methodentry_attrs >] ->
	StringMap.add "class" cname p
    | [< 'Ident "name"; 'Ident "="; 'String mname; p = parse_methodentry_attrs >] ->
	StringMap.add "name" mname p
    | [< 'Ident "signature"; 'Ident "="; 'String msign; p = parse_methodentry_attrs >] ->
	StringMap.add "signature" msign p
    | [< >] -> StringMap.empty
	
let parse_methodexit_attrs expr =
  match expr with parser
    | [< 'Ident "thread"; 'Ident "="; 'String mthread >] ->
	StringMap.add "thread" mthread StringMap.empty
	  
let rec parse_vmalloc_attrs expr =
  match expr with parser
    | [< 'Ident "thread"; 'Ident "="; 'String alloc_thread; p = parse_vmalloc_attrs >] ->
	StringMap.add "thread" alloc_thread p
    | [< 'Ident "class"; 'Ident "="; 'String alloc_class ; p = parse_vmalloc_attrs >] ->
	StringMap.add "class" alloc_class p
    | [< >] -> StringMap.empty
	
let callstackmap = ref StringMap.empty
  
let rec parse_jvmti native_info expr =
  let empty_method_info = { native_alloc = ClassSignatureSet.empty;
			    native_calls = MethodSet.empty } in
    match expr with parser
      | [< 'Ident "MethodEntry"; 'Kwd "{";
	   attrs = parse_methodentry_attrs; 'Kwd "}" >] ->
	  let threadname = StringMap.find "thread" attrs in
	  let stack =
	    try
	      StringMap.find threadname !callstackmap
	    with _ ->
	      let s = Stack.create () in
		callstackmap := StringMap.add threadname s !callstackmap;
		s in
	  let previous_method_attrs =
	    try
	      Stack.top stack
	    with _ ->
	      StringMap.add "type" "None" StringMap.empty in
	  let m_type = StringMap.find "type" attrs
	  and m_class = StringMap.find "class" attrs
	  and m_name = StringMap.find "name" attrs
	  and m_signature = StringMap.find "signature" attrs in
	  let jmethod = { m_type = m_type;
			  m_class = m_class;
			  m_name = m_name;
			  m_signature = m_signature } in
	  let new_native_info =
	    if ((StringMap.find "type" previous_method_attrs) = "Native") then
	      let prev_m_type = StringMap.find "type" previous_method_attrs
	      and prev_m_class = StringMap.find "class" previous_method_attrs
	      and prev_m_name = StringMap.find "name" previous_method_attrs
	      and prev_m_signature = StringMap.find "signature"
		previous_method_attrs in
	      let jprevmethod = { m_type = prev_m_type;
				  m_class = prev_m_class;
				  m_name = prev_m_name;
				  m_signature = prev_m_signature } in
	      let meth_info =
		try
		  MethodMap.find jprevmethod native_info
		with _ -> empty_method_info in
	      let new_meth_info =
		{ meth_info with
		    native_calls = MethodSet.add jmethod
		    meth_info.native_calls } in
		MethodMap.add jprevmethod new_meth_info native_info
	    else
	      if ((StringMap.find "type" attrs) = "Native") then
		MethodMap.add jmethod empty_method_info native_info
	      else native_info in
	    Stack.push attrs stack;
	    parse_jvmti new_native_info expr
      | [< 'Ident "MethodExit"; 'Kwd "{";
	   attrs = parse_methodexit_attrs; 'Kwd "}" >] ->
	  let threadname = StringMap.find "thread" attrs in
	  let stack =
	    try
	      StringMap.find threadname !callstackmap
	    with _ -> failwith "MethodExit without MethodEntry !" in
	    ignore (Stack.pop stack);
	    parse_jvmti native_info expr
      | [< 'Ident "VMAlloc"; 'Kwd "{";
	   attrs = parse_vmalloc_attrs; 'Kwd "}" >] ->
	  let threadname = StringMap.find "thread" attrs in
	  let stack =
	    try
	      StringMap.find threadname !callstackmap
	    with _ -> failwith "VMalloc anywhere !" in
	  let method_attrs = Stack.top stack in
	    if ((StringMap.find "type" method_attrs) = "Native") then
	      let allocated_class = StringMap.find "class" attrs in
	      let m_type = StringMap.find "type" method_attrs
	      and m_class = StringMap.find "class" method_attrs
	      and m_name = StringMap.find "name" method_attrs
	      and m_signature = StringMap.find "signature" method_attrs in
	      let jmethod = { m_type = m_type;
			      m_class = m_class;
			      m_name = m_name;
			      m_signature = m_signature } in
	      let meth_info =
		try
		  MethodMap.find jmethod native_info
		with _ -> empty_method_info in
	      let new_meth_info =
		{ meth_info with
		    native_alloc = ClassSignatureSet.add allocated_class
		    meth_info.native_alloc } in
		parse_jvmti (MethodMap.add
			       jmethod new_meth_info native_info) expr
	    else parse_jvmti native_info expr
      | [< >] -> native_info
	
let parse_jvmti_callstrace_file file =
  let native_info = MethodMap.empty in
  let ic = open_in file in
  let stream = Stream.of_channel ic in
    callstackmap := StringMap.empty;
    let new_native_info = parse_jvmti native_info
      (make_lexer keywords stream) in
      close_in ic;
      new_native_info
	
let rec parse_method_attrs expr =
  match expr with parser
    | [< 'Ident "type"; 'Ident "="; 'String mtype;
	 p = parse_method_attrs >] ->
	StringMap.add "type" mtype p
    | [< 'Ident "class"; 'Ident "="; 'String cname;
	 p = parse_method_attrs >] ->
	StringMap.add "class" cname p
    | [< 'Ident "name"; 'Ident "="; 'String mname;
	 p = parse_method_attrs >] ->
	StringMap.add "name" mname p
    | [< 'Ident "signature"; 'Ident "="; 'String msign;
	 p = parse_method_attrs >] ->
	StringMap.add "signature" msign p
    | [< >] -> StringMap.empty
	
let rec parse_native_alloc expr =
  match expr with parser
    | [< 'String class_signature; allocated_classes = parse_native_alloc >] ->
	ClassSignatureSet.add class_signature allocated_classes
    | [< >] -> ClassSignatureSet.empty
	
let rec parse_native_calls expr =
  match expr with parser
    | [< 'Ident "Method"; 'Kwd "{"; method_attrs = parse_method_attrs;
	 'Kwd "}"; native_calls = parse_native_calls >] ->
	let m_type = StringMap.find "type" method_attrs
	and m_class = StringMap.find "class" method_attrs
	and m_name = StringMap.find "name" method_attrs
	and m_signature = StringMap.find "signature" method_attrs in
	let jmethod = { m_type = m_type;
			m_class = m_class;
			m_name = m_name;
			m_signature = m_signature } in
	  MethodSet.add jmethod native_calls
    | [< >] -> MethodSet.empty
	
let rec parse_native_alloc_calls method_attrs native_info expr =
  let empty_method_info = { native_alloc = ClassSignatureSet.empty;
			    native_calls = MethodSet.empty } in
  let m_type = StringMap.find "type" method_attrs
  and m_class = StringMap.find "class" method_attrs
  and m_name = StringMap.find "name" method_attrs
  and m_signature = StringMap.find "signature" method_attrs in
  let jmethod = { m_type = m_type;
		  m_class = m_class;
		  m_name = m_name;
		  m_signature = m_signature } in
    match expr with parser
      | [< 'Ident "VMAlloc"; 'Kwd "{";
	   allocated_classes = parse_native_alloc; 'Kwd "}" >] ->
	  let method_info =
	    try MethodMap.find jmethod native_info
	    with _ -> empty_method_info in
	  let new_method_info =
	    { method_info with
		native_alloc = ClassSignatureSet.union
		allocated_classes method_info.native_alloc } in
	  let new_native_info =
	    MethodMap.add jmethod new_method_info native_info in
	    parse_native_alloc_calls method_attrs new_native_info expr
      | [< 'Ident "Invokes"; 'Kwd "{";
	   native_calls = parse_native_calls; 'Kwd "}" >] ->
	  let method_info =
	    try MethodMap.find jmethod native_info
	    with _ -> empty_method_info in
	  let new_method_info =
	    { method_info with
		native_calls = MethodSet.union
		native_calls method_info.native_calls } in
	  let new_native_info =
	    MethodMap.add jmethod new_method_info native_info in
	    parse_native_alloc_calls method_attrs new_native_info expr
      | [< >] ->
	  if (MethodMap.mem jmethod native_info) then
	    native_info
	  else MethodMap.add jmethod empty_method_info native_info
	  
let rec parse_native_info_stream native_info expr =
  match expr with parser
    | [< 'Ident "Method"; 'Kwd "{";
	 attrs = parse_method_attrs; 'Kwd "}";
	 'Kwd "{"; new_native_info = parse_native_alloc_calls attrs native_info;
	 'Kwd "}" >] -> parse_native_info_stream new_native_info expr
    | [< _ = Stream.empty >] -> native_info
	
let parse_native_info_file file =
  let native_info = MethodMap.empty in
  let ic = open_in file in
  let stream = Stream.of_channel ic in
  let new_native_info = parse_native_info_stream native_info
    (make_lexer keywords stream) in
    close_in ic;
    new_native_info
      
let indent_size = 4
let indent = String.make indent_size ' '
  
let string_of_method jmethod =
  "Method{type=\"" ^ jmethod.m_type
  ^ "\" class=\"" ^ jmethod.m_class
  ^ "\" name=\"" ^ jmethod.m_name
  ^ "\" signature=\"" ^ jmethod.m_signature ^ "\"}"
    
let fprint_native_info native_info file =
  let oc = open_out file in
    MethodMap.iter
      (fun jmethod meth_info ->
	 Printf.fprintf oc "%s{\n" (string_of_method jmethod);
	 if (meth_info.native_alloc <> ClassSignatureSet.empty) then
	   begin
	     Printf.fprintf oc "%sVMAlloc{\n%s" indent (indent ^ indent);
	     Printf.fprintf oc "%s"
	       (String.concat ("\n" ^ indent ^ indent)
		  (List.map
		     (fun x -> "\"" ^ x ^ "\"")
		     (ClassSignatureSet.elements meth_info.native_alloc)));
	     Printf.fprintf oc "\n%s}\n" indent;
	   end;
	 if (meth_info.native_calls <> MethodSet.empty) then
	   begin
	     Printf.fprintf oc "%sInvokes{\n%s" indent (indent ^ indent);
	     Printf.fprintf oc "%s"
	       (String.concat ("\n" ^ indent ^ indent)
		  (List.map (fun jmeth -> string_of_method jmeth)
		     (MethodSet.elements meth_info.native_calls)));
	     Printf.fprintf oc "\n%s}\n" indent;
	   end;
	 Printf.fprintf oc "}\n\n";
      ) native_info;
    close_out oc
      
type t = native_info

let empty_info = MethodMap.empty

let get_native_methods info =
  let methods = ref [] in
    MethodMap.iter
      (fun jmethod _ ->
	 if (jmethod.m_type = "Native") then
	   methods := (jmethod.m_class,jmethod.m_name,jmethod.m_signature)
	   :: !methods) info;
    List.rev !methods
      
let get_native_method_allocations (m_class,m_name,m_signature) info =
  let native_alloc =
    (MethodMap.find { m_type = "Native";
		      m_class = m_class;
		      m_name = m_name;
		      m_signature = m_signature } info).native_alloc in
    ClassSignatureSet.elements native_alloc
      
let get_native_method_calls (m_class,m_name,m_signature) info =
  let native_calls =
    (MethodMap.find { m_type = "Native";
		      m_class = m_class;
		      m_name = m_name;
		      m_signature = m_signature } info).native_calls in
    List.map (fun jmethod ->
		(jmethod.m_class,jmethod.m_name,jmethod.m_signature)
	     ) (MethodSet.elements native_calls)
      
let merge_native_info info1 info2 =
  MethodMap.fold
    (fun jmeth methinfo1 native_info ->
       if (MethodMap.mem jmeth native_info) then
	 let methinfo2 = MethodMap.find jmeth native_info in
	 let new_methinfo =
	   { native_alloc = ClassSignatureSet.union
	       methinfo1.native_alloc methinfo2.native_alloc;
	     native_calls = MethodSet.union
	       methinfo1.native_calls methinfo2.native_calls } in
	   MethodMap.add jmeth new_methinfo native_info
       else
	 MethodMap.add jmeth methinfo1 native_info) info1 info2
    
let merge_native_info_files file1 file2 =
  let info1 = parse_native_info_file file1
  and info2 = parse_native_info_file file2 in
    merge_native_info info1 info2
