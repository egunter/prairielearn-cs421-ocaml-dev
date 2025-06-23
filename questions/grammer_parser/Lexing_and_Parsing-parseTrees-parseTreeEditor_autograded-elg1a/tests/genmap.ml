
open List
open Genset

type ('a, 'b) map = Map of ('a * 'b) set

let map_eq (k1, _) (k2, _) = k1 = k2

let empty_map () = Map (empty_set map_eq)
let one_map n = Map (one_set n map_eq)
let list_map l = Map (list_set l map_eq)

let size_map (Map s) = size_set s


let has_key k (k', _) = k = k'

let take_map (Map s) = match take_set s with
	| None -> None
	| Some ((k, v), s') -> Some (k, v, Map s')

let in_map (Map s) k = in_set_pred s (has_key k)

let lookup_map (Map s) k = match subtract_set_pred s (has_key k) with
	| None -> None
	| Some ((_, v), _) -> Some v

let subtract_map (Map s) k = match subtract_set_pred s (has_key k) with
	| None -> None
	| Some ((_, v), s') -> Some (v, Map s')


let add_map (Map s) k v = Map (add_set s (k, v))

let update_map m k v = match subtract_map m k with
	| None -> add_map m k v
	| Some (_, m') -> add_map m' k v

