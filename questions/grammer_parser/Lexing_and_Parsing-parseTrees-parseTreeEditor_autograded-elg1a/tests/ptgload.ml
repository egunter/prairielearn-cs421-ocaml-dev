
open Genmap

type grammar = (string, string list list) map

type problem = Problem of grammar * string * string * bool

	(* the data structures used from an `original` version of this file, which also had functions loading in these data structures. *)

type vertex = Vertex of string list * string * float * float

type parse_tree = Tree of string * parse_tree list

type 'a answer =
	| MalformAns
	| NoTreeAns of int
	| SomeTreeAns of 'a

type tree_stub =
	| BranchStub of string * string * float * tree_stub list
	| ChildStub of string

type tree_result =
	| SingleTree of parse_tree
	| GoodForest of parse_tree list
	| ErrorTree of tree_stub
	| BadForest of (string, tree_stub) map
	| NoTree
	| ExtraInfo of tree_result list
	| VertexInfo of (string, vertex) map

