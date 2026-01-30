(*

This file path:

Get["/Users/davidsd/Dropbox/Notes/2026/matching/examples/susyO2/susyO2.wl"]

 *)

(**** To add to StdLib: ****)

Accumulate[{}] := {};
Accumulate[{x_, xs___}] := Prepend[(x + # &) /@ Accumulate[{xs}], x];

Extract[xs_,{i_}] := xs[[i]];

(* TODO: Complex numbers and Conjugate *)

(**** ****)


alpha=1; (* Sqrt[2]*)
gamma=2;

(*q:=(i/2)[Z,Zb]-- when inserted into a trace we expand it into words*)
(*qWord:=(I/2) (id@{Z,Zb}-id@{Zb,Z});*)

fermionQ[x_]:=MemberQ[{\[Psi],\[Psi]b},x];
signLeft[trterm_tr,i_Integer]:=(-1)^Count[Take[List@@trterm,i-1],_?(fermionQ)];

commQitem[Z]:=0;
commQitem[Zb]:=alpha*\[Psi]b;              (*[Q,Zb]=\[Alpha]\bar\[Psi]*)
commQitem[P]:=- (gamma/2) (id@{\[Psi],Z}-id@{Z,\[Psi]});
commQitem[Pb]:=-(gamma/2) (id@{Zb,\[Psi]}-id@{\[Psi],Zb});

acommQitem[\[Psi]]:=-alpha*id@{P}; (*{Q,\[Psi]}=i \[Alpha] P*)
acommQitem[\[Psi]b]:=(-gamma/2)*(id@{Z,Zb}-id@{Zb,Z}); (*{Q, \bar\[Psi]}=i \[Gamma] q=(i \[Gamma])(i/2)[Z,Zb]=-(\[Gamma]/2)[Z,Zb]*)

(*----------graded recursion on a single trace----------*)
acommQ[trterm_tr]:=Let[{n=Length@trterm},Total@Table[signLeft[trterm,i]*MapAt[If[fermionQ[#],acommQitem[#],commQitem[#]]&,trterm,i],{i,n}]/. {id->Identity}];
acommQ[c_?NumericQ*tr[x__]]:=c*acommQ[tr[x]];
acommQ[x_Plus]:=(acommQ/@x);

(*----------tr[...]----------*)
Clear@tr;
(* tr[a1___,a_?ArrayQ,a2___]:=tr@@Flatten[{a1,a,a2}]; *)
(* TODO: Since ArrayQ and Flatten haven't been implemented, maybe just use this? *)
tr[a1___,List[a___],a2___]:=tr[a1,a,a2];
tr[a1___,c_?NumericQ*a_,a2___]:=c tr[a1,a,a2];
(* TODO: Needed to add parentheses around anonymous function because it isn't parsed correctly *)
tr[a1___,a_Plus,a2___]:=(tr[a1,#,a2]&)/@a;
tr[a1___,0,a2___]:=0;
(*tr[]:=1;*)

(** hermitian conjugation **)
(** (-1)^(#P+#Pb) * reverse(Z<->Zb, P\[LeftRightArrow]Pb) **)

cconj[c_?NumericQ*tr[x__]]:=c*cconj[tr[x]];
cconj[x_Plus]:=(cconj/@x);
cconj[tr[x1___]]:=tr[Reverse[ref2/@{x1}]];

ref2[Z]=Zb;
ref2[Zb]=Z;
ref2[P]=-Pb;
ref2[Pb]=-P;
ref2[\[Psi]]=\[Psi]b;
ref2[\[Psi]b]=\[Psi];

mirror[tr[a___]]:=(cconj@tr[a])-tr[a];
(*mirror[tr[a___]]:=((-1)^(Count[tr[a],P|Pb])*Reverse@tr[a])-tr[a];*)

(** reflect: Y\[LeftRightArrow]-Y implies Z<->Zb, P\[LeftRightArrow]Pb **)
(** this uses O2 **)
(*reflect[tr[a___]]:=(ref/@tr[a])-tr[a];*)
ref[Z]=Zb;
ref[Zb]=Z;
ref[P]=Pb;
ref[Pb]=P;
ref[\[Psi]]=\[Psi]b;
ref[\[Psi]b]=\[Psi];

projectReflectionEven[x_tr] := Expand[(ref/@x + x)/2];
projectReflectionOdd[x_tr] := Expand[(ref/@x - x)/2];

(** canonical conjugates, used in cyclicity constraints **)
conj[Z]=Pb;
conj[Zb]=P;
conj[P]=Zb;
conj[Pb]=Z;
conj@\[Psi]=\[Psi]b;
conj@\[Psi]b=\[Psi];

charge[Z]:=1;
charge[P]:=1;
charge[Zb]:=-1;
charge[Pb]:=-1;
charge[\[Psi]]:=1/2;
charge[\[Psi]b]:=-1/2;

(* =====Fermion detection and helpers=====*)
deg[x_]:=If[fermionQ[x],1,0];                (*0=boson,1=fermion*)
nF[list_List]:=Count[list,_?(fermionQ)];
momentumQ[x_]:=MemberQ[{P,Pb},x];
extraPairSign[l_]:=If[momentumQ[l],-1,1];         (*your original (-1)^(l\[Element]{P,Pb})*)

(* TODO:
maptoZ2singlet (needs: DeleteDuplicates, Rest, Union)
XPlistCharge
XPlistNeutral
*)

(*admissible \[OpenCurlyDoubleQuote]cut\[CloseCurlyDoubleQuote] positions:conj[l] at k and cumulative charge match*)
validPositions[tail_List,l_]:=Let[
  {
    ppos=Flatten@Position[tail,conj@l],
    pos0=Flatten@Position[Accumulate[charge/@tail],charge[conj@l]]
  },
  Intersection[ppos,pos0]
];
                    
(*graded cyclic rotation sign:moving first letter l past whole tail*)
rotSign[l_,tail_List]:=(-1)^(deg[l]*nF[tail]);
(*set rotSign[...]:=1 if you want plain (non-graded) trace cyclicity*)
(*graded sign when removing conj[l] at position k:move (possibly odd) l past the k-1 preceding fermions,keep your P/Pb sign*)
pairSign[l_,tail_List,k_Integer]:=extraPairSign[l]*(-1)^(deg[l]*nF[Take[tail,k-1]]);

(* =====Graded cyclic kernel with correct DOUBLE-TRACE split=====*)
ClearAll[cycAny];
cycAny[tr[]]:=0;
cycAny[tr[l_,oplist___]]:=
  Let[
    {
      t     = tr[l,oplist],
      tail  = {oplist},
      rot   = rotSign[l,tail]*RotateLeft@t (*tr[tail,l] with graded sign*),
      pp    = validPositions[tail,l]       (*positions of conj[l] in tail*),
      split = Function[k,
        pairSign[l,tail,k]*(tr@@Take[tail,k-1])*(*LEFT piece as a trace*)(tr@@Drop[tail,k])
      ]
    },
    (*RIGHT piece as a trace*)
    t-rot-Total[split/@pp]
  ];

gauge[tr[oplist___]]:=
  tr[oplist,Z,Pb]
  -tr[oplist,Pb,Z]
  +tr[oplist,Zb,P]
  -tr[oplist,P,Zb]
  -tr[oplist,\[Psi],\[Psi]b]
  -tr[oplist,\[Psi]b,\[Psi]]
  -tr[oplist];

inner2[x_,tr[y__] c2_?NumericQ]:=c2 inner2[x,tr[y]];
inner2[z_,x_Plus]:=(inner2[z,#1]&)/@x;
inner2[x_Plus,z_]:=(inner2[#1,z]&)/@x;
inner2[tr[y__] c2_?NumericQ,x_]:=Conjugate[c2] inner2[tr[y],x];
inner2[x_,0]:=0;
inner2[0,x_]:=0;
inner2[tr[x1___],tr[x2___]]:=tr@@Join[Reverse[ref2/@{x1}],{x2}];
(*alpha=E; 
gamma=EulerGamma;
acommQ[-E tr[\[Psi]b,P]-1/2 EulerGamma tr[\[Psi],Z,Zb]+1/2EulerGamma tr[\[Psi],Zb,Z]]//Expand
acommQ[E tr[\[Psi],Pb]-1/2 EulerGamma tr[\[Psi]b,Z,Zb]+1/2EulerGamma tr[\[Psi]b,Zb,Z]]//Expand*)

canonicalizeZ2xZ2[tr[x__]] := Let[
  {
    tmplist = {tr[x],ref/@tr[x],Reverse[tr[x]],Reverse[ref/@tr[x]]},
    ind = Ordering[tmplist,1]
  },
  If[
    tmplist[[1]]===tmplist[[3]]&&OddQ[Count[tr[x],P|Pb]],
    0,
    If[
      First[ind]<=2,
      Extract[tmplist,ind],
      (-1)^Count[tr[x],P|Pb]*Extract[tmplist,ind]
    ]
  ]
];
canonicalizeZ2xZ2[tr[]] := tr[];
canonicalizeZ2xZ2[c_?NumericQ*x_] := c*canonicalizeZ2xZ2[x]; 
canonicalizeZ2xZ2[x_Plus] := canonicalizeZ2xZ2/@x;
canonicalizeZ2xZ2[x_oq] := x;
canonicalizeZ2xZ2[c_?NumericQ*x_oq] := c*x; 
canonicalizeZ2xZ2[x_List] := canonicalizeZ2xZ2/@x;
canonicalizeZ2xZ2[c_?NumericQ] := c;
canonicalizeZ2xZ2[tr[x___]^2] := canonicalizeZ2xZ2[tr[x]]^2;
canonicalizeZ2xZ2[tr[x___]*tr[y___]] := canonicalizeZ2xZ2[tr[x]] canonicalizeZ2xZ2[tr[y]];

(* TODO: Complex numbers are not implemented, but this should be
correct right now because the only NumericQ's are real at the moment
*)
Conjugate[x_?NumericQ] := x;

canonZ2xZ2cycAny[op_]:=canonicalizeZ2xZ2[cycAny[op]];
canonZ2xZ2gauge[op_]:=canonicalizeZ2xZ2[gauge[op]];
canonZ2xZ2acommQ[op_]:=Expand[canonicalizeZ2xZ2[acommQ[op]]];
canonZ2xZ2inner2[op1_,op2_]:=Expand[canonicalizeZ2xZ2[inner2[op1,op2]]];
