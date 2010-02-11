(*
 * This file is part of Javalib
 * Copyright (c)2010 Laurent Hubert (CNRS)
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

(* Described in JVM Spec 5, §4.8.15 to §4.8.19 *)

open IO
open IO.BigEndian         (* BigEndian overrides some functions defined in IO *)


type element_value =
  | EVCst of JBasics.constant_value
  | EVEnum of (string*string) (* (type_name_index,const_name_index) cf. JLS 13.1 *)
      (* TODO: this should probably be modified but I have not understand how *)
  | EVClass of JBasics.value_type option (* None encodes "void" *)
  | EVAnnotation of annotation
  | EVArray of element_value list

and annotation = {
  kind:JBasics.class_name;              (* field type_index in the spec *)
  element_value_pairs:(string*element_value) list;
  (* the fst element of the couple is "the name of the annotation type element
     represented by this element_value_pairs entry" (maybe the mean the field
     name?) *)
}

type rt_annotations =
  | RTVisible of annotation list
  | RTInvisible of annotation list
  | RTVisibleParameter of annotation list list (* a list for each parameter *)
  | RTInvisibleParameter of annotation list list (* a list for each parameter *)

type default_annotation =
    AnnotationDefault of element_value (* cf. §4.8.19 of JVM Spec 5 *)

let print_list sep print fmt = function
  | [] -> ()
  | hd::tl ->
      print fmt hd;
      List.iter
        (fun annot -> sep fmt; print fmt annot)
        tl

let rec pp_element_value fmt = function
  | EVCst cst ->
      Format.pp_print_string fmt (JPrint.constant (JBasics.ConstValue cst))
  | EVEnum (s1,s2) ->
      Format.pp_print_string fmt ("(" ^ s1 ^ "," ^ s2 ^ ")")
  | EVClass (None) ->
      Format.pp_print_string fmt "Void.class"
  | EVClass (Some vt) ->
      Format.pp_print_string fmt (Javalib.JPrint.value_type vt)
  | EVAnnotation annot ->
      pp_annotation fmt annot
  | EVArray evlist ->
      let print_evlist =
        print_list (fun fmt -> Format.fprintf fmt ";@ ") pp_element_value
      in
        Format.fprintf fmt "[|@[<3>%a|]@]" print_evlist evlist

and pp_annotation fmt annot =
  let print_pair fmt (s,ev) =
    Format.fprintf fmt "@[<2>%s =@ %a@]" s pp_element_value ev
  in
  let print_pairs = print_list (fun fmt -> Format.fprintf fmt ",@ ") print_pair
  in
    Format.fprintf fmt "@@%s(@[%a)@]"
      (JBasics.cn_name annot.kind) print_pairs annot.element_value_pairs


let pp_rt_annotations fmt annot =
  let print_space fmt = Format.pp_print_space fmt () in
    Format.pp_open_hvbox fmt 2;
    begin
      match annot with
        | RTVisible annots ->
            Format.pp_print_string fmt "[RuntimeVisibleAnnotations";
            print_space fmt;
            print_list print_space pp_annotation fmt annots;
        | RTInvisible annots ->
            Format.pp_print_string fmt "[RuntimeInvisibleAnnotations";
            print_space fmt;
            print_list print_space pp_annotation fmt annots;

        | RTVisibleParameter annots ->
            Format.pp_print_string fmt "[RuntimeVisibleParameterAnnotations";
            print_space fmt;
            print_list print_space
              (fun fmt param_annot ->
                 Format.fprintf fmt "[@[<2>";
                 print_list print_space pp_annotation fmt param_annot;
                 Format.fprintf fmt "]@]")
              fmt
              annots
        | RTInvisibleParameter annots ->
            Format.pp_print_string fmt "[RuntimeInvisibleParameterAnnotations";
            print_space fmt;
            print_list print_space
              (fun fmt param_annot ->
                 Format.fprintf fmt "[@[<2>";
                 print_list print_space pp_annotation fmt param_annot;
                 Format.fprintf fmt "]@]")
              fmt
              annots
    end;
    Format.pp_print_string fmt "]";
    Format.pp_close_box fmt ()

let pp_default_annotation fmt = function
  | AnnotationDefault value ->
      Format.fprintf fmt
        "@[<2>AnnotationDefault@ %a@]"
        pp_element_value value

(* shortcuts *)
type constant_pool = JBasics.constant array
type attribute = string * string

let rec parse_element_value (csts:constant_pool) ch =
  let tag = read_byte ch in 
    match Char.chr tag with
      | 'B' | 'C' | 'I' | 'S' | 'Z'  (* int constant *)
      | 'D'                          (* double *)
      | 'F'                          (* float *)
      | 'J'                          (* long *)
      | 's'                          (* string *)
        ->
          let constant_value_index = read_ui16 ch in
          let cst = JBasicsLow.get_constant_value csts constant_value_index in
            EVCst cst
      | 'e' ->                          (* enum constant *)
          (* TODO *)
          failwith "not implemented"
      | 'c' ->                          (* class constant *)
          begin
            try
              EVClass (Some (JParseSignature.parse_field_descriptor
                               (JBasicsLow.get_string csts (read_ui16 ch))))
            with JBasics.Class_structure_error _ ->
              EVClass None
          end
      | '@' ->                          (* annotation type *)
          EVAnnotation (parse_annotation csts ch)
      | '[' ->                          (* array *)
          let num_values = read_ui16 ch in
          let values =
            ExtList.List.init num_values (fun _ -> parse_element_value csts ch)
          in EVArray values
      | _ ->
          raise (JBasics.Class_structure_error
                   "invalid tag in a element_value of an annotation")

and parse_annotation (csts:constant_pool) ch =
  let type_index = read_ui16 ch
  and nb_ev_pairs = read_ui16 ch
  in
  let kind =
    let kind_value_type =
      JParseSignature.parse_field_descriptor
        (JBasicsLow.get_string csts type_index)
    in
    match kind_value_type with
      | JBasics.TObject (JBasics.TClass cn) -> cn
      | _ ->
          raise
            (JBasics.Class_structure_error
               "An annotation should only be a class")
  and ev_pairs =
    ExtList.List.init
      nb_ev_pairs
      (fun _ ->
         let name = JBasicsLow.get_string csts (read_ui16 ch)
         and value = parse_element_value csts ch
         in (name, value))
  in
    {kind = kind;
     element_value_pairs = ev_pairs}

let parse_annotations csts ch =
  let num_annotations = read_ui16 ch
  in
    ExtList.List.init num_annotations (fun _ -> parse_annotation csts ch)

let parse_parameter_annotations csts ch =
  let num_parameters = read_byte ch
  in
    ExtList.List.init num_parameters (fun _ -> parse_annotations csts ch)

let parse_RTVisibleAnnotations
    (csts:constant_pool) (att_name,att_value:attribute) =
  if att_name <> "RuntimeVisibleAnnotations"
  then invalid_arg "parse_RTVisibleAnnotations"
  else
    RTVisible (parse_annotations csts (IO.input_string att_value))

let parse_RTInvisibleAnnotations
    (csts:constant_pool) (att_name,att_value:attribute) =
  if att_name <> "RuntimeInvisibleAnnotations"
  then invalid_arg "parse_RTInvisibleAnnotations"
  else
      RTInvisible (parse_annotations csts (IO.input_string att_value))

let parse_RTVisibleParameterAnnotations
    (csts:constant_pool) (att_name,att_value:attribute) =
  if att_name <> "RuntimeVisibleParameterAnnotations"
  then invalid_arg "parse_RTVisibleParameterAnnotations"
  else
    RTVisibleParameter
      (parse_parameter_annotations csts (IO.input_string att_value))

let parse_RTInvisibleParameterAnnotations
    (csts:constant_pool) (att_name,att_value:attribute) =
  if att_name <> "RuntimeInvisibleParameterAnnotations"
  then invalid_arg "parse_RTInvisibleParameterAnnotations"
  else
    RTInvisibleParameter
      (parse_parameter_annotations csts (IO.input_string att_value))

let parse_AnnotationDefault (csts:constant_pool) (att_name,att_value:attribute) =
  if att_name <> "AnnotationDefault"
  then invalid_arg "parse_AnnotationDefault"
  else
    AnnotationDefault (parse_element_value csts (IO.input_string att_value))
