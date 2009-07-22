open Genlex

type jmethod = { m_type : string;
		 m_class : string;
		 m_name : string;
		 m_signature : string
	       }

module ClassSignatureSet = Set.Make(
  struct
    type t = string
    let compare = compare
  end)

module MethodSet = Set.Make(
  struct
    type t = jmethod
    let compare m1 m2 =
      (compare m1.m_type m2.m_type) + (compare m1.m_class m2.m_class)
      + (compare m1.m_name m2.m_name) + (compare m1.m_signature m2.m_signature)
  end)

module MethodMap = Map.Make(
  struct
    type t = jmethod
    let compare m1 m2 =
      (compare m1.m_type m2.m_type) + (compare m1.m_class m2.m_class)
      + (compare m1.m_name m2.m_name) + (compare m1.m_signature m2.m_signature)
  end)

module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

type native_method_info = { native_alloc : ClassSignatureSet.t;
			    native_calls : MethodSet.t }

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
	let new_native_info =
	  if ((StringMap.find "type" previous_method_attrs) = "Native") then
	    let prev_m_type = StringMap.find "type" previous_method_attrs
	    and prev_m_class = StringMap.find "class" previous_method_attrs
	    and prev_m_name = StringMap.find "name" previous_method_attrs
	    and prev_m_signature = StringMap.find "signature" previous_method_attrs in
	    let jmethod = { m_type = m_type;
			    m_class = m_class;
			    m_name = m_name;
			    m_signature = m_signature } in
	    let jprevmethod = { m_type = prev_m_type;
				m_class = prev_m_class;
				m_name = prev_m_name;
				m_signature = prev_m_signature } in
	    let meth_info =
	      try
		MethodMap.find jprevmethod native_info
	      with _ -> { native_alloc = ClassSignatureSet.empty;
			  native_calls = MethodSet.empty } in
	    let new_meth_info =
	      { meth_info with
		  native_calls = MethodSet.add jmethod
		  meth_info.native_calls } in
	      MethodMap.add jprevmethod new_meth_info native_info
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
	      with _ -> { native_alloc = ClassSignatureSet.empty;
			  native_calls = MethodSet.empty } in
	    let new_meth_info =
	      { meth_info with
		  native_alloc = ClassSignatureSet.add allocated_class
		  meth_info.native_alloc } in
	      parse_jvmti (MethodMap.add jmethod new_meth_info native_info) expr
	  else parse_jvmti native_info expr
    | [< >] -> native_info

let parse_jvmti_callstrace file =
  let native_info = MethodMap.empty in
  let ic = open_in file in
  let stream = Stream.of_channel ic in
    callstackmap := StringMap.empty;
    let new_native_info = parse_jvmti native_info
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

let fprint_native_info file native_info =
  let oc = open_out file in
    MethodMap.iter
      (fun jmethod meth_info ->
	 Printf.fprintf oc "%s{\n" (string_of_method jmethod);
	 if (meth_info.native_alloc <> ClassSignatureSet.empty) then
	   begin
	     Printf.fprintf oc "%sVMAlloc{\n%s" indent (indent ^ indent);
	     Printf.fprintf oc "%s"
	       (String.concat ("\n" ^ indent ^ indent)
		  (ClassSignatureSet.elements meth_info.native_alloc));
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
