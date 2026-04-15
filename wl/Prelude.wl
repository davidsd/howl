(*******

Prelude.wl: This file contains definitions included in the addBuiltins
command that can be formulated purely in the Wolfram Language. Several
builtin routines are defined in Haskell in Builtins.hs.

********)

(* NumericFunctionQ is defined in Builtins.hs *)
NumericQ[Pi] := True;
NumericQ[E] := True;
NumericQ[f_[xs___]] := NumericFunctionQ[f] && And @@ Map[NumericQ, {xs}];
NumericQ[_] := False;

SetAttributes[If, HoldRest];
If[True,  x_, _ ] := x;
If[False, _,  y_] := y;

Not[True]    := False;
Not[False]   := True;
Not[Not[x_]] := x;

Apply[h_,hp_[xs___]] := h[xs];

(* Used for Optional pattern matching inside these heads *)
Default[Times] := 1;
Default[Plus] := 0;
Default[And] := True;
Default[Or] := False;

UnsameQ[xs___] := Not[SameQ[xs]];
MemberQ[xs_,y_] := Or@@((SameQ[y,#]&)/@xs);

Total[x_List] := Plus@@x;

Prepend[{xs___}, x_] := {x, xs};
Append[{xs___}, x_] := {xs, x};
RotateLeft[h_[x_,xs___]] := h[xs,x];

(* MultinomialPowerExpand is defined in StdLib.hs *)
(* TODO: This should probably be a builtin routine for performance reasons *)
Expand[a_ * b_Plus] := (Expand[a*#]&) /@ b;
Expand[b_Plus] := Expand /@ b;
Expand[Power[b_Plus, c_Integer]]      /; c >= 0 := Expand /@ MultinomialPowerExpand[b,c];
Expand[a_ * Power[b_Plus, c_Integer]] /; c >= 0 := (Expand[a*#]&) /@ MultinomialPowerExpand[b,c];
Expand[expr_] := expr;
