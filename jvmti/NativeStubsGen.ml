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
open JNativeStubs
  
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
      make_t new_native_info
