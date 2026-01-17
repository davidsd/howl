SetAttributes[CenterDot, Orderless];
ScalarQ[x_]/;NumericQ[x] := True;
ScalarQ[_] := False;
CenterDot[0, v_] := 0;
CenterDot[x_Plus,y_] := (CenterDot[#,y]&) /@ x;
CenterDot[a_*u_, v_] /; ScalarQ[a] := a*CenterDot[u,v];
CenterDot /: Times[CenterDot[u_,basis[i_]], CenterDot[v_,basis[i_]], xs___] := Times[CenterDot[u,v],xs];
CenterDot /: Power[CenterDot[u_,basis[i_]],2] := CenterDot[u,u];
CenterDot[basis[i_],basis[i_]] := dim;
deriv[u_,x_][expr_Plus] := deriv[u,x] /@ expr;
deriv[u_,x_][a_*b_] := deriv[u,x][a]*b + a*deriv[u,x][b];
deriv[u_,x_][a_^b_] := b*deriv[u,x][a]*a^(b-1);
deriv[u_,x_][CenterDot[x_,x_]] := 2*CenterDot[u,x];
deriv[u_,x_][CenterDot[x_,y_]] := CenterDot[u,y];
deriv[_,_][n_] /; NumericQ[n] := 0;
deriv[_,_][dim] := 0;
laplacian[x_][expr_] := Module[{i}, deriv[basis[i],x][deriv[basis[i],x][expr]]];

