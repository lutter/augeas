(*
Module: Yaml
  Only valid for the following subset:

> defaults: &anchor
>   repo1: master
>
> host:
>   # Inheritance
>   <<: *anchor
>   repo2: branch

Author: Dimitar Dimitrov <mitkofr@yahoo.fr>
*)
module YAML =

(* Group: helpers *)
let colon = Sep.colon
let space = Sep.space
let val = store Rx.word
let eol = Util.eol
let empty = Util.empty
let comment = Util.comment_noindent
let dels = Util.del_str

(*
View: indent
  the imposed indent is 2 spaces
*)
let indent = del /[ \t]+/ "  "

let mline = [ label "@line" . indent . store Rx.space_in . eol ]+

(* Map a literal occurrence of D into a node with label L and value V.
   We use that below as (map_value "stuff") which is a
   function string -> string -> lens, i.e. one that has L filled in already
 *)
let map_value (l:string) (v:string) (d:string) =
  [ label l . value v . dels d . eol . mline ]

let mlit =
  let map_mlit = map_value "@mlit" in
  (map_mlit "clip" "|") | (map_mlit "strip" "|-") | (map_mlit "keep" "|+")

let mfold =
  let map_mfold = map_value "@mfold" in
  (map_mfold "clip" ">") | (map_mfold "strip" ">-") | (map_mfold "keep" ">+")

let mval = (mlit | mfold)

(*
View: inherit
> <<: *anchor
*)
let _inherit = [ key "<<" . colon . space . dels "*" . val . eol ]
let inherit = indent . _inherit . (indent . comment)*

(*
View: repo
> { "repo" = "branch" }
*)
let _repo = [ key Rx.word . colon . space . (val | mval) . eol ]
let repo = indent . _repo . (indent . comment)*

(*
View: anchor
> &anchor
*)
let anchor = dels "&" . val

(*
View: entry
> host:
>   # Inheritance
>   <<: *anchor
>   repo2: branch
*)
let entry = [ key Rx.word . colon . (space . anchor)? . eol
            . (indent . comment)*
            . ((inherit . (repo+)?) | repo+)
            ]

(* View: header *)
let header = [ label "@yaml" . dels "---"
             . (Sep.space . store Rx.space_in)? . eol ]

(*
View: lns
  The yaml lens
*)
let lns = ((empty|comment)* . header)? . (entry | comment | empty)*
