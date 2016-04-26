(*
Module: Simplevars
  Parses simple key = value conffiles

Author: Raphael Pinson <raphink@gmail.com>

About: License
   This file is licenced under the LGPL v2+, like the rest of Augeas.

About: Lens Usage
   To be documented

About: Examples
   The <Test_Simplevars> file contains various examples and tests.
*)

module Simplevars =

autoload xfm

(* Variable: to_comment_re
   The regexp to match the value *)
let to_comment_re =
     let to_comment_squote = /'[^\n']*'/
  in let to_comment_dquote = /"[^\n"]*"/
  in let to_comment_noquote = /[^\n \t'"#][^\n#]*[^\n \t#]|[^\n \t'"#]/
  in to_comment_squote | to_comment_dquote | to_comment_noquote

(* View: entry *)
let entry =
  (* Handle the three different kinds of lines we need to deal with:
     key=value  -> { "key" = "value" }
     key=\n     -> { "key" = "" }
     key\n      -> { "key" } *)
  let entry_for (l:lens) =
    [ Util.indent . key Rx.word . l . (Util.eol | Util.comment_eol) ] in
  let some_value = Sep.space_equal . store to_comment_re in
  let empty_value = del /[ \t]*=/ "=" . store "" in
  (entry_for some_value
  |entry_for empty_value
  |(* This is entry_for with no lens at all *)
    [Util.indent . key Rx.word . (Util.eol | Util.comment_eol) ])

(* View: lns *)
let lns = (Util.empty | Util.comment | entry)*

(* Variable: filter *)
let filter = incl "/etc/kernel-img.conf"
           . incl "/etc/kerneloops.conf"
           . incl "/etc/wgetrc"
           . incl "/etc/zabbix/*.conf"
           . incl "/etc/audit/auditd.conf"
           . incl "/etc/mixerctl.conf"
           . incl "/etc/wsconsctlctl.conf"
           . incl "/etc/selinux/semanage.conf"

let xfm = transform lns filter
