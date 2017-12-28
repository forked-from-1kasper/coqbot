Require Import Unicode.Utf8.
Require Import Lists.List.
Require Import Strings.String.
Import ListNotations.
Open Scope string_scope.

(* default nat is ugly on big numbers *)
Require Import ZArith.
Open Scope N_scope.

Require Extraction.

Extraction Language Ocaml.

Require Import ExtrOcamlBasic.
Require Import Coq.extraction.ExtrOcamlNatInt.
Extract Inductive unit => "unit" [ "()" ].
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Constant Ascii.ascii_dec => "(=)".
Extract Constant string_dec => "(=)".

Module bot_core.

Definition server := "chat.freenode.net".
Definition port := 6667.

Inductive IO :=
| Delay : IO
| IOString : string → IO → IO.

Notation "a $ b" := ((λ _, a b) tt) (at level 51, right associativity).

(* Definition simpleIO :=
    PutStrLn "What is your name?" >>
    GetLine tt >>=
    λ name, PutStrLn ("Nice to meet you, " ++ name) Unit.
 *)

Fixpoint string_to_list (s : string) :=
  let space := Ascii.Ascii false false false false false true false false in
  let fix support acc s:=
    match s with
      | EmptyString => [acc]
      | String c xs =>
        if (Ascii.ascii_dec c space) then acc :: (support EmptyString xs)
        else support (append acc (String c EmptyString)) xs
    end
    in support EmptyString s.

Notation " a !+ b " := (a ++ " " ++ b) (at level 40, left associativity).

Fixpoint guard (clauses : list (string * string)) : bool :=
  match clauses with
    | [] => true
    | (s1, s2) :: xs => if string_dec s1 s2 then guard xs else false
  end.

Notation "'guard' a ; .. ; b 'then' P" :=
  (if guard (cons a .. (cons b nil) ..) then P
   else [])
  (at level 30, no associativity).

Definition handler (input : string) : list IO :=
  match string_to_list input with
    | [command; server] =>
      guard (command, "PING") then
        [IOString ("PONG" !+ server) Delay]
    | [nick; command; channel; msg] =>
      guard (command, "PRIVMSG"); (msg, ":ping") then
        [IOString ("PRIVMSG" !+ channel !+ "pong") Delay;
         IOString ("PRIVMSG" !+ channel !+ "pong‐pong") Delay]
    | _ => []
  end.

Theorem ping_correct : ∀ (server : string),
  [IOString ("PONG" !+ server) Delay] = (handler ("PING" !+ server)).
  induction server0.
  simpl.
  unfold handler.
  simpl string_to_list.
  simpl.
  reflexivity.
  
  unfold handler.
  simpl string_to_list.

Definition init :=
  IOString "USER coqbot coqbot coqbot coqbot" $
  IOString "NICK coqbot" $
  IOString "JOIN #lor" Delay.

End bot_core.

Extraction "logic.ml" bot_core.