(* ::Package:: *)

(*BeginPackage["`"]*)
(*comando che aggiunge un vertice a 3 a una linea di un grafo
    pos_ : indice del segmento/coppia di numeri che voglio rimpiazzare
  ext_ : indice del punto esterno che voglio aggiungere
  int_ : indice del punto interno che voglio aggiungere
grafo_ : grafo che sto considerando*)
inserisci3[pos_, ext_, int_, grafo_] := Module[{temp, pippo} (*'pippo' \[EGrave] un contenitore temporaneo*),
(*rimuovo un segmento e lo rimpiazzo con 3 altri segmenti*)
(*aggiunta di un punto nel grafico*)
temp = ReplacePart[grafo[[1]],
pippo[ {ext,int}(*punto esterno che si attacca al nuovo vertice*) , {grafo[[1,pos,1]],int} , {grafo[[1,pos,2]],int} (*divide in due la linea esterna pos-esima aggiungendo un punto int al suo interno*)],
pos]; (*sostituisco l'elemento pos-esimo*)
(*1 -> 3 non e' una sostituzione ammessa, per cui usiamo il contenitore pippo come un unico oggetto*)
temp =temp/.{pippo->Sequence};
(*aggiunta degli indici di Lorentz e di colore*)
Clear[pippo];
pippo = Join[grafo[[2]], {mu[ext], If[MemberQ[grafo[[2]], mu[grafo[[1, pos, 2]]]], Nothing, mu[grafo[[1, pos, 2]]]](*mu[grafo[[1, pos, 2]]]*), mu[int]}];
temp = Join[{temp}, {pippo}];
Clear[pippo];
pippo = Join[grafo[[3]], {c[ext],  If[MemberQ[grafo[[3]], c[grafo[[1, pos, 2]]]], Nothing, c[grafo[[1, pos, 2]]]](*c[grafo[[1, pos, 2]]]*), c[int]}];
temp = Join[temp, {pippo}];

Return[temp](*ritorna il grafo modificato*)
]; 

inserzione3[grafo_] := Module[{nuovigrafi, indici, npt, nvert},
indici = Flatten[grafo[[1]]]; (*comando che rimuove un livello di parentesi*)
npt = Max[indici]; (*'indici' \[EGrave] una lista di liste, 'npt' \[EGrave] una lista che contiene l'elemento pi\[UGrave] grande di
ciascuna lista*)
nvert = Min[indici]; (*analogamente a npt ma con gli elementi minori*)
(*su quale segmento agire, inserisci un punto esterno un vertice sull'elemento grafo tutto questo per ciascun segmento del mio grafo*)
nuovigrafi = Table[inserisci3[i, npt + 1, nvert -1, grafo], {i, 1,  Length[grafo[[1]]]}
];
(*Lenght conta le coppie presenti nel grafo (il numero delle linee interne)*)
Return[nuovigrafi]
];

inserisci4[vertice_,ext_,grafo_]:=If[
Count[grafo[[1]],vertice,2]===3, (*Count cerca un pattern (il vertice) all'interno del grafo al livello 2 di parentesi e restituisce quante volte appare*)
(*Se e' verificato la condizione, ovvero che vertice e' un vertice a 3, allora aggiungo un nuovo punto esterno a suddetto vertice, tramite un 'Module'*)
Module[{temp},

temp = Append[grafo[[1]],{ext,vertice}];
temp = Join[{temp}, {Append[grafo[[2]], mu[ext]]}, {Append[grafo[[3]], c[ext]]}];

Return[temp]
];
,
{}(*altrimenti non fare nulla, restituisci una lista vuota che alla fine butter\[OGrave] via*)
];

(*Aggiunta di vertice a 4 linee*)
inserzione4[grafo_]:=Module[{indici,npt,nvert,nuovigrafi},
indici = Flatten[grafo[[1]]]; (*converto a lista di numeri*)
npt = Max[indici];
nvert = Min[indici];
nuovigrafi = Table[inserisci4[i,npt+1,grafo], (*inserisci un nuovo punto npt+1 al grafo*)
{i,-1,nvert,-1} (*loop sui vertici, da -1 a n vert con passi di -1*)
];
Return[nuovigrafi]
];

(*procedura che iterativamente aggiunge punti esterni fino a raggiungere n punti esterni,
aggiungendo vertici nuovi su tutti i grafi di ordine n-1 in tutti i modi possibili*)
generagrafi[n_, grafo_] := Module[{temp, listagrafi,nuovigrafi3, nuovigrafi4, indici},
indici = Flatten[grafo[[1]]];
i = Max[indici]; (*numero di punti esterni*)
listagrafi = {grafo}; (*grafobase \[EGrave] un grafo di una lista di grafi che sta crescendo*)
While[i < n,
(*voglio agire su questi grafi vettorialmente*)
nuovigrafi3 = Map[inserzione3, listagrafi]; (*applico 'inserzione3' ai grafi di tutta la lista di grafi*)
nuovigrafi3 = Flatten[nuovigrafi3, 1]; (*per rimuovere un livello di parentesi*)
nuovigrafi4=Map[inserzione4,listagrafi];
nuovigrafi4 = Flatten[nuovigrafi4,1];
listagrafi = Join[nuovigrafi3, nuovigrafi4];
listagrafi = DeleteCases[listagrafi,{}];
++i;
];
Return[listagrafi]
];

(*EndPackage[];*)
