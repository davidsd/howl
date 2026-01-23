ScalarQ[x_]/;NumericQ[x] := True;
SetAttributes[CenterDot, Orderless];
CenterDot[0, v_] := 0;
