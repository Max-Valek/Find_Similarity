let file_lines fname =
  let in_file = open_in fname in
  let rec loop acc =
    let next_line = try Some (input_line in_file) with End_of_file -> None in
    match next_line with
    | (Some l) -> loop (l::acc)
    | None -> acc
  in
  let lines = try List.rev (loop []) with _ -> [] in
  let () = close_in in_file in
  lines

let file_as_string fname = String.concat "\n" (file_lines fname)

(* a way of representing a document as a collection of Element.e values *)
module type Summary = sig
  type e
  type t
  val empty : t
  val union_size : t -> t -> int
  val inter_size : t -> t -> int
  val size : t -> int
  val of_list : e list -> t
end

(* a way of representing some of the contents of a document *)
module type Element = sig
  type e
  val of_string : string -> e list
end

(* These two are not for real use, just for testing *)
module Nilement = struct
  type e = int
  let of_string s = [String.length s]
end

module Nummary = struct
  type e = int
  type t = int
  let empty = 0
  let of_list = function [] -> 0 | h::t -> h
  let size s = s
  let inter_size s1 s2 = min s1 s2
  let union_size s1 s2 = max s1 s2
end

(* now back to real uses *)
(* allows different sizes of n-gram *)
module Ngram(N : sig val n : int end) = struct
  type e = string
  let of_string str = (List.filter (fun x -> not (String.contains x ' ')) (List.init ((String.length str)-(N.n-1)) (fun f -> (String.sub str f N.n)))) (* does the same this as n_grams from hw3. It also "cleans up" the strings so there are no spaces *)
end

(* turns a word into its "stem" *)
module Stem = struct
  type e = string
  let of_string str = List.map Stemmer.stem (Str.split (Str.regexp {|\b|}) str)
end

(* Your multiset implementation from HW3 *)
module ListMSet = struct
  type e = string
  type t = (string * int) list
  let empty = []
  let size m1 = List.fold_right (fun (_,y) x -> y+x) m1 0 (* sum of the multiplicities *)
  let of_list lst = (List.fold_right (fun x y -> if (List.mem_assoc x y) then ((x, (List.assoc x y)+1)::(List.remove_assoc x y)) else ((x,1)::y)) lst []) (* implementation of multiset_of_list from hw3 *)
  let inter_size m1 m2 = List.fold_left (fun acc (x,y) ->
      if ((List.mem_assoc x m2) && (List.assoc x m2) > 0) then (acc + (min (y) (List.assoc x m2))) else acc) 0 m1     (* intersection size between the two sets *)
  let union_size m1 m2 = ((size m1) + (size m2) - (inter_size m1 m2)) (* the size of the union of the two sets *)
end

(* acts like the Set.Make module applied to the String module and makes the necessary calculations *)
module SetSummary = struct
  include Set.Make(String) (* gives the ability to implement the Set.Make functor, no need to use Set.Make(String).anyFunction *)
  type e = string
  type t = Set.Make(String).t (* make it so the type of t is the same as Set.Make applied to the String module *)
  let empty = empty  (* Set.make function for empty *)
  let of_list lst = of_list lst
  let size s = fold (fun x y -> y + 1) s 0
  let inter_size s1 s2 = size (inter s1 s2)   (* the size of the set created by calling inter on the two sets *)
  let union_size s1 s2 = size (union s1 s2)   (* the size of the set created by calling union on the two sets *)
end

(* acts like the Map.Make module applied to the String module and makes the necessary calculations *)
module MapMSet = struct
  include Map.Make(String)    (* make it so the type of t is the same as Map.Make applied to the String module *)
  type e = key
  type t = int Map.Make(String).t   (* make it so the type of t is the same as Map.Make applied to the String module *)
  let empty = empty
  let of_list lst = List.fold_left (fun x y -> if mem y x then add y ((find y x)+1) x else add y 1 x) empty lst
  let size s = fold (fun k v x -> x+v) s 0
  let union_size s1 s2 = size (fold add s1 s2)      (* size of the union of the two lists *)
  let inter_size s1 s2 = (size s1) + (size s2) - (union_size s1 s2)       (* size of the intersection of the two lists *)

end

module FindSim (E : Element) (MS : Summary with type e = E.e) = struct
  include MS
  let similarity s1 s2 = (float_of_int (MS.inter_size s1 s2)) /. (float_of_int (MS.union_size s1 s2))
  let main replist_name target_name =
    let repfile_list = file_lines replist_name in
    let rep_contents = List.map (file_as_string) (repfile_list) in
    let target_contents = file_as_string target_name in
    let rep_elems = List.map E.of_string rep_contents in (* _almost_ the same as your rep_ngrams *)
    let target_elems = E.of_string target_contents in (* _almost the same as your target_ngrams *)
    let rep_summaries = List.map MS.of_list rep_elems in (* change this to the right thing *)
    let target_summary = MS.of_list target_elems in (* this too *)
    let repsims = List.combine (List.map (similarity target_summary) rep_summaries) repfile_list in (*your repsims*)
    List.stable_sort (Fun.flip compare) repsims
end
