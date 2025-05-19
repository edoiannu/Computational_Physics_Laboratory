(* ::Package:: *)

(*indici di Lorentz*)
Format[{mu[a_]}] := Subscript[\[Mu], a];
Format[{nu[a_]}] := Subscript[\[Nu], a];
(*delta*)
Format[delta[f_[a_], f_[b_]]] := Subscript[\[Delta], Subscript[f, a], Subscript[f, b]];
(*costanti di struttura*)
Format[f[f1_[a_], f2_[b_], f3_[c_]]] := Subscript[f,Subscript[f1, a], Subscript[f2, b], Subscript[f3, c]];
(*metrica*)
Format[SP[a_, b_]]:=Subscript[g, a, b];
(*Format[SP[{nu[a_]},{nu[b_]}]]:=Subscript[g,Subscript[\[Nu], a],Subscript[\[Nu], b]];
Format[SP[{mu[a_]},{nu[b_]}]]:=Subscript[g,Subscript[\[Mu], a],Subscript[\[Nu], b]];*)
(*quadrimpulso*)
Format[SP[{mu[b_]}, p[a_]]] := Subscript[Subscript[p,a], Subscript[\[Mu], b]];
Format[SP[{nu[b_]}, p[a_]]] := Subscript[Subscript[p,a], Subscript[\[Nu], b]];
Format[p[a_]] := Subscript[p, a];
MakeBoxes[SP[p[a_],{f_[_]}]^2,StandardForm]:=SubscriptBox[SubscriptBox[SymbolName[p],ToString[a]],"2"];
MakeBoxes[SP[e[a_],{f_[_]}]^2,StandardForm]:=SubscriptBox[SubscriptBox[SymbolName[\[Epsilon]],ToString[a]],"2"];
Format[SP[f_[a_], f_[b_]]] := Subscript[f, a] Subscript[f, b];
(*quadrivettore polarizzazione*)
Format[SP[e[a_], {mu[b_]}]] := Subscript[Subscript[\[Epsilon],a], Subscript[\[Mu], b]];
Format[SP[e[a_], {nu[b_]}]] := Subscript[Subscript[\[Epsilon],a], Subscript[\[Nu], b]];
Format[e[a_]] := Subscript[\[Epsilon], a];
MakeBoxes[SP[e[a_],{mu[_]}]^2,StandardForm]:=SubscriptBox[SubscriptBox[SymbolName[\[Epsilon]],ToString[a]],"2"];
(*prodotto tra un vettore momento e uno polarizzazione*)
Format[SP[p[a_], e[b_]]] := Subscript[p, a] Subscript[\[Epsilon], b];

Format[SP[{nu[b_]}, p[a_]]] := Subscript[Subscript[p,a], Subscript[\[Nu], b]];
Format[SP[{nu[b_]}, n]] := Subscript[n, Subscript[\[Nu], b]];
Format[SP[{mu[b_]}, n]] := Subscript[n, Subscript[\[Mu], b]];
Format[SP[p[i_], n]] := Subscript[p, i] * n;
Format[SP[e[i_], n]] := Subscript[e, i] * n;
