open Javalib
open JBasics
open JCode

open SeaOfNodes

let get_jopcodes c =
  match c with
  | JInterface _ -> assert false
  | JClass _ ->
    let methods = get_methods c in
    let m = List.hd @@ MethodMap.value_elements methods in
    match m with
    | AbstractMethod _ -> assert false
    | ConcreteMethod cm -> begin
        match cm.cm_implementation with
        | Native -> assert false
        | Java code ->
          let jcode = Lazy.force code in
          jcode.c_code
      end

let rec map_option (f : 'a -> 'b option) (l: 'a list) : 'b list =
  begin match l with
    | [] -> []
    | hd::tl -> begin
        match f hd with
        | None -> map_option f tl
        | Some hd -> hd::(map_option f tl)
      end
  end

let _ =
  let cp = Javalib.class_path "java_files" in
  let (class_name, _) = Javalib.extract_class_name_from_file "java_files/Test1.class" in
  let jclass = Javalib.get_class cp class_name in

  let jopcodes = get_jopcodes jclass in

  let graph = Translator.translate_jopcodes jopcodes in

  let ret = List.hd
    @@ map_option (fun (_, n) ->
        match n with
        | Type.Node.Control (Type.Control.Return _) as ret -> Some ret
        | _ -> None)
    @@ SeaOfNodes.Type.Son.bindings graph in

  let bir = BirBuilder.node_to_bir ret in
  assert (Bir.eval_instr bir = Some 42);

  Printf.printf "+ [BIR] Test passed successfully.\n"
