ScalarQ[a] := False;
ScalarQ[aDag] := False;
ScalarQ[n_] /; NumericQ[n] := True;
ScalarQ[x_ y_] := SclalarQ[x] && ScalarQ[y];
ScalarQ[x_Plus] := And@@(ScalarQ/@(List@@x));
ScalarQ[OpProduct[___]] := False;
ScalarQ[_] := True;

SetAttributes[OpProduct, Flat];
OpProduct[x___,y_Plus,z___] := (OpProduct[x,#,z]&) /@ y;
OpProduct[x___,y_?ScalarQ z_, w___] := y OpProduct[x,z,w];

(* The commutation relation *)
OpProduct[x___,a,aDag,y___] := OpProduct[x,aDag,a,y] + OpProduct[x,y];

(* MatrixElt[m][op][n] represents the matrix element <m|op|n> between
harmonic oscillator eigeinstates. *)
MatrixElt[m_][OpProduct[]][n_] := If[m==n, 1, 0];
MatrixElt[m_][x_Plus][n_] := (MatrixElt[m][#][n]&) /@ x;
MatrixElt[m_][x_?ScalarQ y_][n_] := x MatrixElt[m][y][n];
MatrixElt[m_][OpProduct[x___,a]][n_] := Sqrt[n] MatrixElt[m][OpProduct[x]][n-1];
MatrixElt[m_][OpProduct[x___,aDag]][n_] := Sqrt[n+1] MatrixElt[m][OpProduct[x]][n+1];

x  = OpProduct[a+aDag]/Sqrt[2];
pi = OpProduct[a-aDag]/Sqrt[2];

HHarmonicOsc = OpProduct[x,x]/2 - OpProduct[pi,pi]/2;

HAnharmonicOsc = HHarmonicOsc + g OpProduct[x,x,x,x];

test := Expand[MatrixElt[n][OpProduct[x,x,x,x,x,x,x,x]][n] /. Equal[n, Plus[n,x_]] :> Equal[0,x]];
