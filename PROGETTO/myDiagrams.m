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
pos1 = grafo[[pos, (*1,*) 1]]; (*primo indice della linea pos-esima che voglio rimuovere*)
pos2 = grafo[[pos, (*1,*) 2]]; (*secondo indice della linea pos-esima che voglio rimuovere*)
temp = ReplacePart[grafo,
pippo[ (*{*){ext,int}(*, mu[ext], c[ext]}*)(*punto esterno che si attacca al nuovo vertice/punto interno che abbiamo aggiunto*),
(*{*){pos1,int}(*, mu[pos1], c[pos1], If[pos1 < 0, Sequence @@ {mu[int], c[int]}, Nothing]}*),
(*{*){pos2,int}(*, mu[pos2], c[pos2], If[pos2 < 0,  Sequence @@ {mu[int], c[int]}, Nothing]}*)
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
indici = Flatten[grafo(*[[All, 1]]*)]; (*'Flatten' \[EGrave] un comando che rimuove i livelli di parentesi*)
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
Append[grafo, (*{*){ext, vertice}(* mu[ext], c[ext]}*)],
{}(*altrimenti non faccio nulla, restituisco una lista vuota che alla fine butter\[OGrave] via*)
];

(*comando che preso un grafo aggiunge ad esso tutti i possibili vertici a 4*)
inserzione4[grafo_]:=Module[{nuovigrafi (*lista dei nuovi grafi ottenuti da riempire*),
npt (*numero di punti esterni del grafo di partenza*), nvert (*numero di vertici del grafo di partenza*), indici},
indici = Flatten[grafo(*[[All, 1]]*)]; (*converto a una lista di singoli numeri le coppie del grafo*)
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
indici = Flatten[grafo(*[[All, 1]]*)];
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

(*procedura che restituisce la lista degli impulsi di grafo*)
listaimpulsi[grafo_] := Map[Apply[p, #] &, grafo(*[[All, 1]]*)]
(*procedura che restituisce il grafo in cui la coppia {i, j} viene sostituita da p[i, j]*)
applicaimpulsi[grafo_] := MapAt[Apply[p, #] &, grafo, {All, 1}]

(*procedura che restituisce le equazioni di conservazione dei momenti delle linee interne,
in termini dei momenti esterni*)
generaequazioni[grafo_] := Module[{temp},

indici = Flatten[grafo(*[[All, 1]]*)]; (*converto a una semplice lista di numeri le coppie del grafo,
saranno le etichette di ogni punto del grafo, interno o esterno che sia*)
npt = Max[indici]; (*numero di punti esterni*)
nvert = Min[indici]; (*numero di vertici*)

(*lista degli impulsi p[i, j] di tutte i segmenti del nostro grafo*)
impulsi = listaimpulsi[grafo]; (*applichiamo la "funzione" impulso p a ciascuna linea del grafo*)

(*lista delle equazioni di conservazione del momento per ogni vertice del grafo*)
(*'impulsivertice' \[EGrave] la lista degli impulsi entranti in un vertice*)
equazioni = Table[impulsivertice = Join[Cases[impulsi, p[_, i]], -Cases[impulsi, p[i,_]]
(*qui cambio il segno se l'indice del vertice sta al primo posto,
il meno moltiplica ciascun degli argomenti della lista*)]; eq = Apply[Plus, impulsivertice] == 0
(*questa riga \[EGrave] un'assegnazione: assegno ad 'eq' tutto ci\[OGrave] che sta a sx dell'uguale*); eq (*ritorno della
singola equazione per il vertice i-esimo*), {i, -1, nvert, -1}];

(*voglio rinominare i miei impulsi come p1, p2, ... invece di p[1,-1], p[2,-1], etc...*)
(*il comando '/;' \[EGrave] un if, il comando prende 'i' e verifica che sia maggiore di 0,
se lo \[EGrave] procede della sostituzione facendo cadere la 'j'*)
equazioni = equazioni /. p[i_ /; i>0, j_] :> p[i]; 
(*se avessi scritto solo la freccetta -> sarebbe stata una semplice sostituzione di una certa variabile p[i]
all'interno dell'espressione, non una redefizione della funzione*)

(*a questo punto le incognite rimaste da determinare sono gli impulsi delle linee interne, riconoscibili dal fatto
che hanno ancora due argomenti*)
incognite = Cases[equazioni, p[_, _], Infinity]; (*metto 'Infinity' perch\[EGrave] sono pigro e non ho voglia di cercare
a quale livello devo trovare p[_,_]*)

(*conservazione dei momenti esterni/conservazione globale del momento*)
globale = Rule[p[npt], -Sum[p[k], {k, 1, npt-1}]]; (*'Rule' \[EGrave] un altro modo per scrivere la freccetta ->*)
(*assegna p_n = - p_1 - p_2 - ... - p_{n-1} etc...*)

equazioni = equazioni /. globale;
impulsiinterni = Solve[equazioni, incognite]; (*risolvo 'equazioni' per 'incognite'*)

Return[impulsiinterni]

];

addIndices[grafo_] := Module[{newgrafo},

newgrafo = Table[{grafo[[i]], mu[grafo[[i, 1]]], c[grafo[[i, 1]]], If[grafo[[i, 1]] < 0, Sequence @@ {mu[grafo[[i, 2]]], c[grafo[[i, 2]]]}, Nothing]}, {i, 1, Length[grafo]}]

];

(*procedura che preso un grafo ne costruisce un'espressione data dal prodotto di funzioni di vertice e di linee
interne (propagatori), alle quali assegneremo poi una precisa espressione analitica*)
diagramma[grafo_] := Module[{numeriinterni, newgrafo},

newgrafo = addIndices[grafo];

(*inseriamo in 'regole' le equazioni di conservazione dei momenti interni*)
regole = generaequazioni[grafo];

(*in queste tre righe mi concentro sulle linee interne: 'interne' \[EGrave] una lista che contiene solo le linee
interne del grafo, indicizzate da coppie di numeri e dai loro indici*)
interne = Cases[newgrafo, {{i_ /; i<0, j_ /; j<0}, ___}]; (*prima di applicare gli impulsi*)
numeriinterni = applicaimpulsi[interne] /. regole[[1]]; (*applico gli impulsi e sostituisco quelli delle linee
interne con le leggi di conservazione del momento*)
propagatori = Map[Prop, numeriinterni]; (*applico 'Prop' (oggetto non meglio specificato) ad ogni numero quantico
che decidiamo di associare al nostro propagatore*)
propagatori = Apply[Times, propagatori]; (*moltiplico i propagatori*)

(*vediamo come possiamo estrarre i vertici*)
vertici = Table[linee = applicaimpulsi[newgrafo] (*applico gli impulsi a tutte le righe del grafo*);
lineevertice = Join[Cases[linee, {p[a_, i], rest___} :> {p[a, i], rest}],
Cases[linee, {p[i, b_], rest___} :> {-p[i, b], rest}]]; (*impulsi delle linee che confluiscono nel vertice i-esimo*)
(*se i due indici i & j e sono entrambi negativi, sostituisco p[i, j] con la regola di conservazione ad esso
associata. Se uno dei due \[EGrave] positivo (ad esempio l'indice i) eseguo la sostituzione p[i,j]->p[i]*)
lineevertice = lineevertice /. regole[[1]];
lineevertice = lineevertice /. {p[i_ /; i>0, j_], rest___} :> {p[i], rest};
lineevertice = lineevertice /. {momentum___, mu[j_], c[j_], mu[k_], c[k_]} :> If[j === i,{momentum, mu[j], c[j]}, {momentum, mu[k], c[k]}];
(*definisco una certa funzione V[i] e la associo a 'impulsivertice'*)
If[Length[lineevertice] === 3, vertice = V3[lineevertice], vertice = V4[lineevertice]];
vertice
,
{i, -1, nvert, -1}
];

vertici = Apply[Times, vertici]; (*moltiplico i vertici*)

Return[propagatori*vertici]

];

generadiagrammi[n_, grafo_] := Module[{temp},

grafi = generagrafi[n, grafo]; (*generiamo i grafi*)

diagrammi = Map[diagramma, grafi];

Return[diagrammi]

]
