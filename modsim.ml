open Similar
type etype = NG of int | ST

let rec take n ls = match (n,ls) with
| (0,_) | (_,[]) -> []
| (_,h::t) -> h::(take (n-1) t)

let do_main ms elem tn rl tname =
let mainfunc = match (ms,elem) with
| ("mset",NG n) ->
  let module Sim = FindSim(Ngram(struct let n=n end))(ListMSet) in Sim.main
| ("set",NG n) ->
  let module Sim = FindSim(Ngram(struct let n=n end))(SetSummary) in Sim.main
| ("map",NG n) ->
  let module Sim = FindSim(Ngram(struct let n=n end))(MapMSet) in Sim.main
| ("mset",ST) ->
  let module Sim = FindSim(Stem)(ListMSet) in Sim.main
| ("set",ST) ->
  let module Sim = FindSim(Stem)(SetSummary) in Sim.main
| ("map",ST) ->
  let module Sim = FindSim(Stem)(MapMSet) in Sim.main
| _ -> failwith "not implemented"
in
  let rlist = mainfunc rl tname in
  List.iter (fun (s,n) -> Printf.printf "%0.4f\t%s\n" s n) (take tn rlist)

let rec setargs arglist ms elem tn = match arglist with
| rlistname::tname::[] -> do_main ms elem tn rlistname tname
| "--mset"::tl -> setargs tl "mset" elem tn
| "--set"::tl -> setargs tl "set" elem tn
| "--map"::tl -> setargs tl "map" elem tn
| "--ngram"::n::tl -> setargs tl ms (NG (int_of_string n)) tn
| "--stem"::tl -> setargs tl ms ST tn
| "--top"::n::tl -> setargs tl ms elem (int_of_string n)
| _ -> print_endline "\nmodsim: modular similarity comparison tool. options:\n--ngram n : use <n>-grams\n--stem : use word stems\n--set : use set similarity\n--mset : use multiset similarity (list implementation)\n--map : use multiset similarity (map implementation)\n--top k : print top <k> matches only"

let () = setargs (List.tl (Array.to_list Sys.argv)) "mset" (NG 3) max_int
