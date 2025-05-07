(* ::Package:: *)

(*struttura del grafo di partenza:*)
(*grafobase = {{{1, -1}, mu[1], c[1]}, {{2, -1}, mu[2], c[2]}, {{3, -1}, mu[3], c[3]}};*)

(*comando che aggiunge un vertice a 3 a una linea di un grafo,
rimuovendo un segmento e rimpiazzandolo con altri tre*)
inserisci3[pos_ (*indice del segmento/coppia di numeri che voglio rimpiazzare*),
ext_ (*indice del punto esterno che voglio aggiungere*),
int_ (*indice del punto interno che voglio aggiungere*),
grafo_ (*grafo che sto considerando*)] := Module[{temp, pippo, pos1, pos2},
(*'pippo' e 'temp' sono due contenitori temporanei*)
pos1 = grafo[[pos, 1, 1]]; (*primo indice della linea pos-esima che voglio rimuovere*)
pos2 = grafo[[pos, 1, 2]]; (*secondo indice della linea pos-esima che voglio rimuovere*)
temp = ReplacePart[grafo,
pippo[ {{ext,int}, mu[ext], c[ext]}(*punto esterno che si attacca al nuovo vertice/punto interno che abbiamo aggiunto*),
{{pos1,int}, mu[pos1], c[pos1], If[pos1 < 0, Sequence @@ {mu[int], c[int]}, Nothing]},
{{pos2,int}, mu[pos2], c[pos2], If[pos2 < 0,  Sequence @@ {mu[int], c[int]}, Nothing]}
(*divide in due la linea pos-esima aggiungendo un punto al suo interno, e se una delle nuove linee \[EGrave] descritta da
due indici negativi (\[EGrave] una linea interna) allora aggiungo gli indici di entrambi i suoi estremi (\[EGrave] un propagatore)*)],
pos (*sostituisco l'elemento pos-esimo*)];
(*1 -> 3 non e' una sostituzione ammessa, per cui usiamo il contenitore pippo come un unico oggetto
per poi sostituirlo con una normale 'Sequence'*)
temp =temp/.{pippo->Sequence};

temp (*ritorna il grafo modificato*)
]; 

(*comando che preso un grafo aggiunge ad esso tutti i possibili vertici a 3*)
inserzione3[grafo_] := Module[{nuovigrafi (*lista dei nuovi grafi ottenuti da riempire*), indici,
npt (*numero di punti esterni del grafo di partenza*), nvert (*numero di vertici del grafo di partenza*)},
indici = Flatten[grafo[[All, 1]]]; (*'Flatten' \[EGrave] un comando che rimuove i livelli di parentesi*)
npt = Max[indici]; (*'Max' restituisce l'elemento maggiore ella lista 'indici'*)
nvert = Min[indici]; (*analogamente a 'Max' ma con l'elemento minore*)
(*costruiamo la lista 'nuovigrafi' aggiungendo un vertice a 3 ad ogni riga usando una 'Table'*)
nuovigrafi = Table[inserisci3[i, npt + 1, nvert -1, grafo], {i, 1,  Length[grafo]
(*'Lenght' conta le coppie presenti nel grafo (il numero delle linee interne)*)}
];
Return[nuovigrafi]
];

(*comando che aggiunge una linea esterna a un vertice a 3 del grafo,*)
inserisci4[vertice_ (*indice del vertice al quale voglio attaccare un segmento*),
ext_ (*punto esterno che voglio aggiungere*), grafo_ (*grafo che sto considerando*)] := If[
Count[grafo, vertice, Infinity]===3, (*'Count' cerca un pattern ('vertice') all'interno del grafo e restituisce
quante volte appare. Se e' verificata la condizione, ovvero che 'vertice' appare 3 volte (e quindi \[EGrave] un vertice a 3),
allora aggiungo un nuovo punto esterno al suddetto*)
Append[grafo, {{ext, vertice}, mu[ext], c[ext]}],
{}(*altrimenti non faccio nulla, restituisco una lista vuota che alla fine butter\[OGrave] via*)
];

(*comando che preso un grafo aggiunge ad esso tutti i possibili vertici a 4*)
inserzione4[grafo_]:=Module[{nuovigrafi (*lista dei nuovi grafi ottenuti da riempire*),
npt (*numero di punti esterni del grafo di partenza*), nvert (*numero di vertici del grafo di partenza*), indici},
indici = Flatten[grafo[[All, 1]]]; (*converto a una lista di singoli numeri le coppie del grafo*)
npt = Max[indici];
nvert = Min[indici];
nuovigrafi = Table[inserisci4[i, npt+1, grafo] (*inserisco un nuovo punto npt+1 al grafo*),
{i, -1, nvert, -1} (*loop sui vertici, da -1 a nvert con passi di -1*)
];
Return[nuovigrafi]
];

(*comando che iterativamente aggiunge punti esterni a un grafo di partenza fino a raggiungere n punti esterni,
aggiungendo vertici nuovi su tutti i grafi di ordine n-1 in tutti i modi possibili*)
generagrafi[n_, grafo_] := Module[{temp, listagrafi (*lista da riempire di tutti i nuovi grafi possibili*),
nuovigrafi3 (*nuovi grafi ottenuti aggiungendo vertici a 3*),
nuovigrafi4 (*nuovi grafi ottenuti aggiungendo vertici a 4*), indici},
indici = Flatten[grafo[[All, 1]]];
i = Max[indici]; (*numero di punti esterni di partenza*)
listagrafi = {grafo}; (*lista di grafi che sta crescer\[AGrave]*)
While[i < n,
(*voglio agire su questi grafi vettorialmente usando la procedura 'Map'*)
nuovigrafi3 = Map[inserzione3, listagrafi]; (*applico 'inserzione3' a tutti i grafi ottenuti fino ad ora*)
nuovigrafi3 = Flatten[nuovigrafi3, 1]; (*per rimuovere un livello di parentesi*)
nuovigrafi4 = Map[inserzione4,listagrafi]; (*applico 'inserzione4'*)
nuovigrafi4 = Flatten[nuovigrafi4, 1];
listagrafi = Join[nuovigrafi3, nuovigrafi4]; (*metto insieme con 'Join' tutti i grafi ottenuti*)
listagrafi = DeleteCases[listagrafi,{}]; (*butto via gli elementi vuoti*)
++i;
];
Return[listagrafi]
];


