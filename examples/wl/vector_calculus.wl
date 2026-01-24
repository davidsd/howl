(* ScalarQ[x] determines whether x should be treated as a scalar *)
(* Numeric values are scalars *)
ScalarQ[x_]/;NumericQ[x] := True;

(* The spatial dimension dim is a scalar *)
ScalarQ[dim] := True;
                
(* For now nothing else is a scalar *)
ScalarQ[_] := False;

(* CenterDot[u,v] will be the dot product. The dot product is symmetric *)
SetAttributes[CenterDot, Orderless];

(* The dot product with 0 is 0 *)
CenterDot[0, v_] := 0;

(* The dot product distributes over addition *)
CenterDot[x_Plus,y_] := (CenterDot[#,y]&) /@ x;

(* The dot product is linear (really bilinear, since it is Orderless) *)
CenterDot[a_ u_, v_] /; ScalarQ[a] := a CenterDot[u,v];

(* These two rules implement Einstein summation notation. basis[i]
represents a basis vector in the i-th direction. The dot product
u.basis[i] represents the component u_i. Einstein notation says that
u_i v_i = u.v. This rule implements that. *)
CenterDot /: Times[CenterDot[u_,basis[i_]], CenterDot[v_,basis[i_]], xs___] := Times[CenterDot[u,v],xs];

(* We need a special case for when the vectors are the same *)
CenterDot /: Power[CenterDot[u_,basis[i_]],2] := CenterDot[u,u];

(* basis[i].basis[j] is the Kronecker delta, so when i and j are the
same, we get dim *)
CenterDot[basis[i_],basis[i_]] := dim;

(* Now we define the directional derivative: deriv[u,x] represents
(u.d/dx), i.e. the derivative with respect to x in the direction of
u. *)

(* The derivative of a sum is the sum of the derivatives *)
deriv[u_,x_][expr_Plus] := deriv[u,x] /@ expr;

(* The derivative satisfies the Leibniz rule *)
deriv[u_,x_][a_ b_] := deriv[u,x][a] b + a deriv[u,x][b];

(* The derivative of a power. NB: For simplicity, we assume the exponent is x-independent. *)
deriv[u_,x_][a_^b_] := b deriv[u,x][a] a^(b-1);

(* The derivative of a dot product *)
deriv[u_,x_][CenterDot[x_,x_]] := 2 CenterDot[u,x];

(* The derivative of a dot product *)
deriv[u_,x_][CenterDot[x_,y_]] := CenterDot[u,y];

(* The derivative of a numeric quantity vanishes *)
deriv[_,_][n_] /; NumericQ[n] := 0;

(* The derivative of the spatial dimension dim vanishes *)
deriv[_,_][dim] := 0;

(* Here is a definition of the Laplacian. We use Module to introduce a
unique new variable that won't clash with other variables that might
be in the expression. *)
laplacian[x_][expr_] := Module[{i}, deriv[basis[i],x][deriv[basis[i],x][expr]]];

