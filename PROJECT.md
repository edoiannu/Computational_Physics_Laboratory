Computational Physics Laboratory
- Tree-Level Gluon Scattering

```wl
In[]:= (*Lanciamo un numero di kernel corrispondenti al numero di core del computer, utile per eseguire calcolo parallelo di espressioni*)
   CloseKernels[]; 
    LaunchKernels[$ProcessorCount];
```

```wl
In[]:= (*---Formattazioni dell'output---*)
   Format[mu[i_]] := Subscript[\[Mu], i]; (*indici di Lorentz*)
   Format[nu[i_]] := Subscript[\[Nu], i]; 
   Format[s[i_, j_]] := Subscript[s, i, j]; (*variabili di Mandelstam generalizzate*)
   Format[p[a_]] := Subscript[p, a]; (*impulso*)
   Format[e[a_]] := Subscript[\[Epsilon], a]; (*vettore di polarizzazione*)
   Format[c[a_]] := Subscript[c, a]; (*carica di colore*)
   Format[SP[v_, {mu_[i_]}]] := Superscript[v, mu[i]]; (*quadrivettori*)
   Format[SP[expr_ + p___, {mu_[i_]}]] := Superscript[Row[{"(", expr + p, ")"}], mu[i]]; (*quadrivettore somma*)
  (*Format[SP[n,{mu_[i_]}]]:=Superscript[n,mu[i]];*)
   Format[SP[a_, b_]] := a* b; (*prodotti scalari*)
   Format[SP[{mu[a_]}, {mu[b_]}]] := Subscript[g, mu[a], mu[b]]; (*metrica*)
   Format[SP[{nu[a_]}, {nu[b_]}]] := Subscript[g, nu[a], nu[b]]; 
   Format[SP[{mu[a_]}, {nu[b_]}]] := Subscript[g, mu[a], nu[b]]; 
   Format[delta[f_[a_], g_[b_]]] := Subscript[\[Delta], f [a], g[b]]; (*delta*)
   Format[f[f1_[a_], f2_[b_], f3_[c_]]] := Subscript[f, f1[a], f2[b], f3[c]]; (*costanti di struttura*)
   Format[T[i_]] := Superscript[T, i]; (*generatori dell'algebra*)
   Format[T[i_][ j_, k_]] := Subscript[T[i], j, k]; (*in componenti*)
   
   SetAttributes[SP, Orderless]; (*in che ordine sono messi gli indici non ha importanza*)
   SetAttributes[delta, Orderless]; 
   SetAttributes[s, Orderless]; 
   
   h = -4; (*variabile globale per enumerare gli indici sommati*)
```

### Diagrams generations

#### Graphs generation

```wl
In[]:= grafobase = {{1, -1}, {2, -1}, {3, -1}}; (*grafo iniziale*)
   
  (*Aggiunge un vertice a 3 sostituendo una linea con tre nuove*)
 inserisci3[pos_, ext_, int_, grafo_] := Module[{temp, pippo, pos1, pos2}, pos1 = grafo[[pos, 1]]; 
      pos2 = grafo[[pos, 2]]; 
      temp = ReplacePart[grafo, pippo[{ext, int}, {pos1, int}, {pos2, int}], pos]; 
      temp /. {pippo -> Sequence}]; 
   
  (*Applica inserisci3 a tutte le linee del grafo*)
 inserzione3[grafo_] := Module[{nuovigrafi, indici, npt, nvert}, indici = Flatten[grafo]; 
      npt = Max[indici]; 
      nvert = Min[indici]; 
      nuovigrafi = Table[inserisci3[i, npt + 1, nvert - 1, grafo], {i, 1, Length[grafo]}]; 
      Return[nuovigrafi]]; 
   
  (*Aggiunge un punto esterno a un vertice a 3*)
 inserisci4[vertice_, ext_, grafo_] := If[Count[grafo, vertice, Infinity] === 3, Append[grafo, {ext, vertice}], {}]; 
   
  (*Applica inserisci4 a tutti i vertici del grafo*)
 inserzione4[grafo_] := Module[{nuovigrafi, npt, nvert, indici}, indici = Flatten[grafo]; 
      npt = Max[indici]; 
      nvert = Min[indici]; 
      nuovigrafi = Table[inserisci4[i, npt + 1, grafo], {i, -1, nvert, -1}]; 
      Return[nuovigrafi]]; 
   
  (*Costruisce iterativamente tutti i grafi fino a n punti esterni*)
 generagrafi[n_, grafo_] := Module[{temp, listagrafi, nuovigrafi3, nuovigrafi4, indici}, indici = Flatten[grafo]; 
      j = Max[indici]; 
      listagrafi = {grafo}; 
      While[j < n, nuovigrafi3 = Flatten[Map[inserzione3, listagrafi], 1]; 
       nuovigrafi4 = Flatten[Map[inserzione4, listagrafi], 1]; 
       listagrafi = DeleteCases[Join[nuovigrafi3, nuovigrafi4], {}]; 
       j = j + 1;]; 
      Return[listagrafi]]; 
   
  (*---Generazione di tutti i grafi possibili con 4 e 5 linee esterne---*)
 generagrafi[4, grafobase]
 generagrafi[5, grafobase]
```

```wl
Out[]= {{{4, -2}, {1, -2}, {-1, -2}, {2, -1}, {3, -1}}, {{1, -1}, {4, -2}, {2, -2}, {-1, -2}, {3, -1}}, {{1, -1}, {2, -1}, {4, -2}, {3, -2}, {-1, -2}}, {{1, -1}, {2, -1}, {3, -1}, {4, -1}}}
```

```wl
Out[]= {{{5, -3}, {4, -3}, {-2, -3}, {1, -2}, {-1, -2}, {2, -1}, {3, -1}}, {{4, -2}, {5, -3}, {1, -3}, {-2, -3}, {-1, -2}, {2, -1}, {3, -1}}, {{4, -2}, {1, -2}, {5, -3}, {-1, -3}, {-2, -3}, {2, -1}, {3, -1}}, {{4, -2}, {1, -2}, {-1, -2}, {5, -3}, {2, -3}, {-1, -3}, {3, -1}}, {{4, -2}, {1, -2}, {-1, -2}, {2, -1}, {5, -3}, {3, -3}, {-1, -3}}, {{5, -3}, {1, -3}, {-1, -3}, {4, -2}, {2, -2}, {-1, -2}, {3, -1}}, {{1, -1}, {5, -3}, {4, -3}, {-2, -3}, {2, -2}, {-1, -2}, {3, -1}}, {{1, -1}, {4, -2}, {5, -3}, {2, -3}, {-2, -3}, {-1, -2}, {3, -1}}, {{1, -1}, {4, -2}, {2, -2}, {5, -3}, {-1, -3}, {-2, -3}, {3, -1}}, {{1, -1}, {4, -2}, {2, -2}, {-1, -2}, {5, -3}, {3, -3}, {-1, -3}}, {{5, -3}, {1, -3}, {-1, -3}, {2, -1}, {4, -2}, {3, -2}, {-1, -2}}, {{1, -1}, {5, -3}, {2, -3}, {-1, -3}, {4, -2}, {3, -2}, {-1, -2}}, {{1, -1}, {2, -1}, {5, -3}, {4, -3}, {-2, -3}, {3, -2}, {-1, -2}}, {{1, -1}, {2, -1}, {4, -2}, {5, -3}, {3, -3}, {-2, -3}, {-1, -2}}, {{1, -1}, {2, -1}, {4, -2}, {3, -2}, {5, -3}, {-1, -3}, {-2, -3}}, {{5, -2}, {1, -2}, {-1, -2}, {2, -1}, {3, -1}, {4, -1}}, {{1, -1}, {5, -2}, {2, -2}, {-1, -2}, {3, -1}, {4, -1}}, {{1, -1}, {2, -1}, {5, -2}, {3, -2}, {-1, -2}, {4, -1}}, {{1, -1}, {2, -1}, {3, -1}, {5, -2}, {4, -2}, {-1, -2}}, {{4, -2}, {1, -2}, {-1, -2}, {2, -1}, {3, -1}, {5, -1}}, {{4, -2}, {1, -2}, {-1, -2}, {2, -1}, {3, -1}, {5, -2}}, {{1, -1}, {4, -2}, {2, -2}, {-1, -2}, {3, -1}, {5, -1}}, {{1, -1}, {4, -2}, {2, -2}, {-1, -2}, {3, -1}, {5, -2}}, {{1, -1}, {2, -1}, {4, -2}, {3, -2}, {-1, -2}, {5, -1}}, {{1, -1}, {2, -1}, {4, -2}, {3, -2}, {-1, -2}, {5, -2}}}
```

```wl
In[]:= (*Calcolo del numero di grafi per ciascun numero di punti esterni*)next = Range[4, 9]; 
   ngrafi = Table[Length[generagrafi[i, grafobase]], {i, next}]; 
   
  (*Tabella riassuntiva*) 
   Grid[Prepend[Transpose[{next, ngrafi}], {"Punti esterni", "Numero grafi"}], Frame -> All, Background -> {None, {{LightGray}}}, Alignment -> Left] 
   
  (*Preparo i dati per il fit*) 
   data = Transpose[{next, ngrafi}]; 
   xVals = next; 
   yVals = ngrafi; 
   logYVals = Log[yVals]; 
   
  (*Fit lineare:log(y)=log(a)+b x=>y=a Exp[b x]*) 
   logFit = LinearModelFit[Transpose[{xVals, logYVals}], x, x]; 
   
  (*Ricavo i parametri esponenziali*) 
   bFitted = logFit["BestFitParameters"][[2]]; 
   aFitted = Exp[logFit["BestFitParameters"][[1]]]; 
   
  (*Plot con funzione di fit e R^2*) 
   Show[ListPlot[data, PlotStyle -> {Red, PointSize[Medium]}, PlotMarkers -> Automatic, Frame -> True, FrameLabel -> {"Punti esterni", "Numero grafi"}, GridLines -> Automatic, ScalingFunctions -> {"Linear", "Log"}, PlotRange -> All], Plot[aFitted Exp[bFitted x], {x, Min[xVals], Max[xVals]}, PlotStyle -> Blue, ScalingFunctions -> {"Linear", "Log"}], Epilog -> {Inset[Column[{Style["y = " <> ToString[NumberForm[aFitted, {10, 4}]] <> "\[CenterDot]e^(" <> ToString[NumberForm[bFitted, {3, 2}]] <>"\[CenterDot]x)", Medium, Black], Style["R² = " <> ToString[NumberForm[logFit["RSquared"], {3, 3}]],Medium, Black]}], Scaled[{0.5, 0.2}]]}, PlotLabel -> Style["Numero grafi con l'aumentare dei punti esterni", Small, Bold]] 
   
  (*Stampa R^2 separatamente,opzionale*) 
   Print["R² = ", logFit["RSquared"]]; 
   
  (*Pulizia finale*) 
   Clear[next, ngrafi, data, aFitted, bFitted, logFit];
```

| Punti esterni | Numero grafi |
| - | - |
| 4 | 4 |
| 5 | 25 |
| 6 | 220 |
| 7 | 2485 |
| 8 | 34300 |
| 9 | 559405 |

![1ez47ikfvykp0](img/1ez47ikfvykp0.png)

![0661r7gxo4v6n](img/0661r7gxo4v6n.png)

#### Field theory implementation

```wl
In[]:= (*Restituisce la lista degli impulsi associati alle linee del grafo*)listaimpulsi[grafo_] := Map[Apply[p, #] &, grafo] 
   
  (*Sostituisce {i,j} con p[i,j] nel primo elemento di ogni coppia*)
   applicaimpulsi[grafo_] := MapAt[Apply[p, #] &, grafo, {All, 1}] 
   
  (*Genera le equazioni di conservazione del momento per ogni vertice*)
   generaequazioni[grafo_] := Module[{temp}, indici = Flatten[grafo]; 
      npt = Max[indici];(*numero di punti esterni*)nvert = Min[indici];(*numero di vertici*)impulsi = listaimpulsi[grafo]; 
     (*Conservazione del momento per ciascun vertice*) equazioni = Table[impulsivertice = Join[Cases[impulsi, p[_, i]], -Cases[impulsi, p[i, _]]]; 
        eq = Apply[Plus, impulsivertice] == 0, {i, -1, nvert, -1}]; 
     (*Rinomina p[i,j]->p[i] se i>0*) equazioni = equazioni /. p[i_ /; i > 0, j_] :> p[i]; 
     (*Impulsi interni=p[i,j] non rinominati*) incognite = Cases[equazioni, p[_, _], Infinity]; 
     (*Conservazione globale:p_n=-\[CapitalSigma]p_i*) globale = Rule[p[npt], -Sum[p[k], {k, 1, npt - 1}]]; 
      equazioni = equazioni /. globale; 
      impulsiinterni = Solve[equazioni, incognite]; 
      Return[impulsiinterni]]; 
   
  (*Aggiunge indici di Lorentz e colore a ciascuna linea*)
   addIndices[grafo_] := Module[{newgrafo}, newgrafo = Table[{grafo[[i]], mu[grafo[[i, 1]]], c[grafo[[i, 1]]], If[grafo[[i, 1]] < 0, Sequence @@ {mu[grafo[[i, 2]]], c[grafo[[i, 2]]]}, Nothing]}, {i, 1, Length[grafo]}]]; 
   
  (*Costruisce l'espressione associata a un grafo*)
   diagramma[grafo_] := Module[{numeriinterni, newgrafo}, newgrafo = addIndices[grafo]; 
      regole = generaequazioni[grafo]; 
     (*Linee interne del grafo*) interne = Cases[newgrafo, {{i_ /; i < 0, j_ /; j < 0}, ___}]; 
      numeriinterni = applicaimpulsi[interne] /. regole[[1]]; 
      propagatori = Apply[Times, Map[Prop, numeriinterni]]; 
     (*Vertici:calcolo momento+indici per ciascun vertice*) vertici = Table[linee = applicaimpulsi[newgrafo]; 
        lineevertice = Join[Cases[linee, {p[a_, i], rest___} :> {p[a, i], rest}], Cases[linee, {p[i, b_], rest___} :> {-p[i, b], rest}]]; 
        lineevertice = lineevertice /. regole[[1]]; 
        lineevertice = lineevertice /. {p[i_ /; i > 0, j_], rest___} :> {p[i], rest}; 
        lineevertice = lineevertice /. {momentum___, mu[j_], c[j_], mu[k_], c[k_]} :> If[j === i, {momentum, mu[If[k > 0, k, {k, i}]], c[If[k > 0, k, {k, i}]]}, {momentum, mu[{j, i}], c[{j, i}]}];
        vertice = If[Length[lineevertice] === 3, V3[i][lineevertice], V4[i][lineevertice]]; 
        vertice, {i, -1, nvert, -1}]; 
      vertici = Apply[Times, vertici]; 
      Return[propagatori*vertici]]; 
   
  (*Genera tutti i diagrammi con n linee esterne*)
   generadiagrammi[n_, grafo_] := Module[{temp}, grafi = generagrafi[n, grafo]; 
      diagrammi = Map[diagramma, grafi]; 
      Return[diagrammi]]; 
   
  (*Esegui:aggiunta indici+generazione diagrammi*)
   addIndices /@ generagrafi[4, grafobase] 
   generadiagrammi[4, grafobase] 
   
  (*Nota:se una linea collega due punti interni,il suo indice è {interno,vertice}.Questo aiuta a distinguere contrazioni interne vs esterne nel calcolo simbolico.*)
```

![0e2fagk4nggth](img/0e2fagk4nggth.png)

![05u4m9tfwabjo](img/05u4m9tfwabjo.png)

![1406peeln68bd](img/1406peeln68bd.png)

|  |  |  |
| - | - | - |
| -Subscript- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Subscript- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Subscript- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Subscript- |  =  | -Times- |

```wl
In[]:= (*---Definiamo delle regole per effettuare prodotti scalari tra strutture di colore e di Lorentz---*)
   
  (*---Prodotti psedoscalari---*)
   LorentzSP = #  //. {
         SP[a_, {mu_}] SP[b_, {mu_}] :> SP[a, b], 
         SP[a_, {mu_}] SP[a_, {mu_}] :> SP[a, a], 
         SP[{mu_}, {mu_}] :> 4, (*traccia della metrica*)
         SP[n, n] :> 0, (*gauge condition*)
         SP[p[a_], p[a_]] :> 0, (*i gluoni sono particelle massless*)
         SP[p[a_], e[a_]] :> 0  (*polarizzazioni trasverse*) 
        } &; 
    
   (*---Prodotti pseudoscalari tra punti interni---*) 
    LorentzSPinternal = # //. {
         SP[{mu[i_]}, {mu[j_]}] SP[a_, {mu[{i_, j_}]}] SP[b_, {mu[{j_, i_}]}]  :> SP[a, b], 
         SP[{mu[i_]}, {mu[j_]}] SP[{mu[{i_, j_}]}, a_] :> SP[mu[j], a],
         SP[p[a_], p[a_]] :> 0 (*i gluoni sono particelle massless*) 
        } &; 
    
   (*---Linearità e omogeneità delle strutture di Lorentz---*) 
    ExpandLorentz = # //. {
         SP[c_?NumericQ*a_, b_] :> c SP[a, b], 
         SP[a_ + b_, c_] :> SP[a, c] + SP[b, c] 
        } &; 
    
   (*---Operazioni tra fattori di colore---*) 
    colourSP  = # //. {
        (*delta applicata agli indici interni*) 
         delta[c[a_], c[b_]] f[args1___] f[args2___] /; MemberQ[{args1}, c[{a, b}]] && MemberQ[{args2}, c[{b, a}]] && b < a :> Module[{replaced}, 
           replaced = f @@ ReplacePart[{args1}, FirstPosition[{args1}, c[{a, b}]] :> c[h - 1]] f @@ ReplacePart[{args2}, FirstPosition[{args2}, c[{b, a}]] :> c[h = h - 1]] ], 
        (*delta applicata a tutti gli altri casi*) 
         delta[i_, j_]*rest_ :> (rest /. i -> j), 
        (*prodotto tra due delta*) 
         delta[a_, b_] delta[a_, c_] :> delta[b, c] 
        } &;
```

```wl
In[]:= (*Analisi dei costi simbolici:per le prime ampiezze con n = 4, 5 e 6,misuriamo memoria occupata e tempo di esecuzione nei tre passaggi chiave: espansione, contrazione degli indici interni, semplificazione.*)
   
   M5 = ParallelMap[FeynRules, generadiagrammi[5, grafobase]]; 
    M6 = ParallelMap[FeynRules, generadiagrammi[6, grafobase]]; 
    M7 = ParallelMap[FeynRules, generadiagrammi[7, grafobase]]; 
    
    firstamps = {M4[[1]], M5[[1]], M6[[1]], M7[[1]]}; 
    
    Do[
     Print["=== Symbolic processing stages for the first n =",  i + 3, " amplitude ==="]; 
      startsize = ByteCount[firstamps[[i]]]; 
      ti = AbsoluteTime[]; 
      appo = firstamps[[i]] // ExpandLorentz // Expand; 
      tf = AbsoluteTime[]; 
      finalsize = ByteCount[appo]; 
      Print["Time taken to Expand: ", tf - ti, " s"]; 
      Print["Initial and final memory usage: ", startsize, " B -> ", finalsize, " B"]; 
     (*startsize=finalsize;
     ti=AbsoluteTime[];
     appo=appo//colourSP;
     tf=AbsoluteTime[];
     finalsize=ByteCount[appo];
     Print[Time taken to colourSP: ,tf-ti, s];
     Print[Initial and final memory usage: ,startsize,B -> ,finalsize, B];*) 
      startsize = finalsize; 
      ti = AbsoluteTime[]; 
      appo = appo // LorentzSPinternal; 
      tf = AbsoluteTime[]; 
      finalsize = ByteCount[appo]; 
      Print["Time taken to contract internal indices: ", tf - ti, " s"];
      Print["Initial and final memory usage: ", startsize, " B -> ", finalsize, " B"]; 
      startsize = finalsize; 
      ti = AbsoluteTime[]; 
      appo = appo // Simplify; 
      tf = AbsoluteTime[]; 
      finalsize = ByteCount[appo]; 
      Print["Time taken to simplify the expression: ", tf - ti, " s"]; 
      Print["Initial and final memory usage: ", startsize, " B -> ", finalsize, " B"] 
     , {i, 1, Length[firstamps]}] 
    
    Clear[appo, startsize, finalsize, tf, ti]
```

![0boz7m9hdfkps](img/0boz7m9hdfkps.png)

![1swqaifj0vzt7](img/1swqaifj0vzt7.png)

![08yjabzfoqnb9](img/08yjabzfoqnb9.png)

![1wp44lxd7bgpm](img/1wp44lxd7bgpm.png)

![1ewths6jaanjf](img/1ewths6jaanjf.png)

![02ozxwi6lk9mc](img/02ozxwi6lk9mc.png)

![0z3nrpxdxs2jw](img/0z3nrpxdxs2jw.png)

![143pxwg6qlqtr](img/143pxwg6qlqtr.png)

![13hcmmnonp0pg](img/13hcmmnonp0pg.png)

![1v3pztnods21y](img/1v3pztnods21y.png)

![0njhvavcv7pcq](img/0njhvavcv7pcq.png)

![0e2h1k5mdi676](img/0e2h1k5mdi676.png)

![118vjt3131awp](img/118vjt3131awp.png)

![1r68b94tmuio1](img/1r68b94tmuio1.png)

![0mwarbjg2bg0k](img/0mwarbjg2bg0k.png)

![0niwgdqo0v9dn](img/0niwgdqo0v9dn.png)

![1fgvd4io479z2](img/1fgvd4io479z2.png)

![05zxsa4etecbd](img/05zxsa4etecbd.png)

![0l64651u76r1f](img/0l64651u76r1f.png)

![1rlhn4cl8jpih](img/1rlhn4cl8jpih.png)

![1ouo0nfntdwk4](img/1ouo0nfntdwk4.png)

![0lx4825fevdo2](img/0lx4825fevdo2.png)

![0cmpuy8d9f371](img/0cmpuy8d9f371.png)

![1dyk283rijfl4](img/1dyk283rijfl4.png)

![1fyl4ihht1t18](img/1fyl4ihht1t18.png)

![1spri1zz1okyw](img/1spri1zz1okyw.png)

![1uz1lwobcc2wx](img/1uz1lwobcc2wx.png)

![056kmbm7km3xt](img/056kmbm7km3xt.png)

### Ward identity

![049h2dl6pocda](img/049h2dl6pocda.png)

|  |  |  |
| - | - | - |
| -Times- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Times- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Times- |  =  | -Times- |

|  |  |  |
| - | - | - |
| -Times- |  =  | -Times- |

```wl
In[]:= Print[Row[{ SP[p[1], {mu[1]}] Subscript[Subscript[\[ScriptCapitalM], "4"], mu[1]], " = ", (externallegs fourvertex // momcons[4, 1] // colourSP // summedIndices // ExpandLorentz // Expand // LorentzSP) /. {f[e_, c[1], c[4]] f[e_, c[2], c[3]] :> f[e, c[1], c[3]] f[e, c[2], c[4]] - f[e, c[1], c[2]] f[e, c[3], c[4]]} // Simplify}]]
```

|  |  |  |
| - | - | - |
| -Times- |  =  | -Times- |

```wl
In[]:= (*E' facile vedere che le due espressioni si bilanciano perfettamente, essendo una l'opposto dell'altra. E' possibile verificare la identità di Ward anche per nel caso di due momenti p1 e p2 contratti*)
   
   Clear[externallegs]; 
    externallegs = SP[p[1], {mu[1]}] SP[p[2], {mu[2]}] SP[e[3], {mu[3]}] SP[e[4], {mu[4]}]; 
   (*facciamolo un po' più a forza bruta questa volta*) 
    (externallegs (schannel + tchannel + uchannel + fourvertex) // momcons[4, 1] // colourSP // summedIndices // ExpandLorentz // Expand // LorentzSPinternal // LorentzSP) /. {f[e_, c[1], c[4]] f[e_, c[2], c[3]] :> f[e, c[1], c[3]] f[e, c[2], c[4]] - f[e, c[1], c[2]] f[e, c[3], c[4]]} // Simplify
```

```wl
Out[]= 0
```

##### Color factors decomposition 

![0860a79hbllzi](img/0860a79hbllzi.png)

```wl
In[]:= (*---Procedura generale che verifica che le (n-2)! sottostrutture di colore siano tra loro tutto gauge invarianti---*)
   WardIdentity[next_ (*numero di stati di polarizzazione esterni*), mom___ (*momenti da contrarre*)] := Module[{start, stop, M (*contributi all'ampiezza*), colorSubstruct (*sottostrutture di colore indipendenti*), momlist (*lista dei momenti*), momindices (*indici dei momenti da contrarre*), extlegs (*gambe esterne*), result, ti, tf}, start = AbsoluteTime[]; 
       Print["=== Ward Identity Check ==="]; 
       Print["-> Process: 2 -> ", ToString[next - 2]]; 
       momlist = {mom} (*lista dei momenti da contrarre*); 
       momindices = momlist /. {p[i_] :> i} (*indici dei momenti da contrarre*); 
       If[! SubsetQ[Range[next], momindices], MessageDialog["Error: some momenta do not correspond to external legs." <> "Valid range: p[1] to p[" <> ToString[next] <> "]."]; 
        Return[$Failed];]; 
       If[AnyTrue[Tally[momindices], #[[2]] > 1 &], MessageDialog["Error: duplicate momenta detected in contraction list."]; 
        Return[$Failed];]; 
       M = ParallelMap[(# // FeynRules) &, generadiagrammi[next, grafobase]];
       ti = AbsoluteTime[]; 
       M = ParallelMap[(# // ExpandLorentz // Expand // LorentzSPinternal) &, M]; 
       tf = AbsoluteTime[]; 
       Print["Time taken to expand and do internal contractions: ", tf - ti, " s"]; 
       Print["Memory usage after expantion and internal contractions: ", ByteCount[M], " B"]; 
       ti = AbsoluteTime[]; 
       colorSubstruct = indColourSubstructures[M, next]; 
       tf = AbsoluteTime[]; 
       Print["Time taken to extract the indipendent colour sub-amplitudes: ",tf - ti, " s"]; 
       ti = AbsoluteTime[]; 
       extlegs = Product[SP[p[i], {mu[i]}], {i, momindices}]; 
       Do[If[! MemberQ[momindices, i], extlegs = SP[e[i], {mu[i]}] extlegs], {i, 1, next}]; 
       result = ParallelMap[(# // momcons[next, momindices[[1]]] // ExpandLorentz // Expand // LorentzSP) &, extlegs colorSubstruct]; 
       tf = AbsoluteTime[]; 
       Print["Time taken to do external contractions after imposing momentum conservation: ", tf - ti, " s"]; 
       ti = AbsoluteTime[]; 
       Print["Memory usage before finale internal contractions: ", ByteCount[result], " B"]; 
       result = ParallelMap[(# // generalizedMandelstam[next] // sumMandelstam[next] // Simplify) &, result]; 
       tf = AbsoluteTime[]; 
       Print["Time taken to do the final simplification after imposing Maldestam variables conservation: ", tf - ti, " s"]; 
       stop = AbsoluteTime[]; 
       Print["Total evaluation time: ", stop - start, " s"]; 
       Print["Expected result: a list of ", ToString[Factorial[next - 2]], " zeros if the Ward identity is satisfied."]; 
       result]; 
    
    WardIdentity[4, p[1]]
```

```wl
Out[]= "=== Ward Identity Check ==="
```

![0wv9ubvzp8x6o](img/0wv9ubvzp8x6o.png)

![0w8jvv35birj8](img/0w8jvv35birj8.png)

![0wrjbf2dfeael](img/0wrjbf2dfeael.png)

![0unf46wb5swpu](img/0unf46wb5swpu.png)

![0xat8mj3ueod5](img/0xat8mj3ueod5.png)

![0bj15ofc5d874](img/0bj15ofc5d874.png)

![0gyoxmls2dzdn](img/0gyoxmls2dzdn.png)

![1k2xsam30gnx5](img/1k2xsam30gnx5.png)

![012o6evi9b85p](img/012o6evi9b85p.png)

```wl
Out[]= {0, 0}
```

```wl
In[]:= WardIdentity[5, p[1]]
```

```wl
Out[]= "=== Ward Identity Check ==="
```

![0egyzwavpsor5](img/0egyzwavpsor5.png)

![1mdvhpy65rgfw](img/1mdvhpy65rgfw.png)

![0x8tvqkbbxzqi](img/0x8tvqkbbxzqi.png)

![091c8l4f2ni6f](img/091c8l4f2ni6f.png)

![0iasj8k7gbehi](img/0iasj8k7gbehi.png)

![03rf2eygkzadr](img/03rf2eygkzadr.png)

![1ta5xx76p2z5p](img/1ta5xx76p2z5p.png)

![1ab6izg76ilco](img/1ab6izg76ilco.png)

![1esacmzjq7cvw](img/1esacmzjq7cvw.png)

```wl
Out[]= {0, 0, 0, 0, 0, 0}
```

### Un-polarized squared amplitude

![15w4nmqau7rnh](img/15w4nmqau7rnh.png)

#### Independent colour sub-amplitudes

```wl
In[]:= M4 = ParallelMap[(# // ExpandLorentz // Expand // LorentzSPinternal) &, M4];
 M5 = ParallelMap[(# // ExpandLorentz // Expand // LorentzSPinternal) &, M5];
```

##### n = 4

```wl
In[]:= indcolorf4 = newBase[4]; (*base DDM*)
 ti = AbsoluteTime[];
 M4indcolorsub = indColourSubstructures[M4, 4];
 tf = AbsoluteTime[];
 Print["Time taken to extract the colour sub-amplitudes: ", tf - ti, " s"];
 M4indcolorsub = ParallelTable[Coefficient[Total[M4indcolorsub], i], {i, indcolorf4}]; (*isoliamo i solo le sottoampiezze di colore, senza le costanti di struttura*)
 extpolM4indcolorsub = extpolContraction[M4indcolorsub, 4]; (*contraiamo per la somma sulle polarizzazioni esterne*)
 M4indcolorsubDag = complexConjugate[M4indcolorsub]; (*sottoampiezze complesse coniugate*) 
  
 Print["Memory usage of contracted sub-amplitude: ", ByteCount[extpolM4indcolorsub[[1]]], " B"]; 
  
 Print["Memory usage of conjugated sub-amplitude: ", ByteCount[M4indcolorsubDag[[1]]], " B"]; 
  
 Print["Memory usage of their expanded product: ", ByteCount[extpolM4indcolorsub[[1]] M4indcolorsubDag[[1]] // Expand]," B"];
```

![0xr3xoll8qx8m](img/0xr3xoll8qx8m.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![050d6190p9q16](img/050d6190p9q16.png)

![1snywxp5b0aiu](img/1snywxp5b0aiu.png)

![1mt94evsrvko1](img/1mt94evsrvko1.png)

![0hrhj11stx6hs](img/0hrhj11stx6hs.png)

![1nua8aum5253y](img/1nua8aum5253y.png)

![08r19nj6in7cd](img/08r19nj6in7cd.png)

![0avkq6jiobwqi](img/0avkq6jiobwqi.png)

![1vp0yx13a6eqf](img/1vp0yx13a6eqf.png)

![0mw3deoon191c](img/0mw3deoon191c.png)

![0wow4u7bo1fo1](img/0wow4u7bo1fo1.png)

![08jkuqqrculk3](img/08jkuqqrculk3.png)

![1mpiee0zl7qdo](img/1mpiee0zl7qdo.png)

![1f949agksq8pc](img/1f949agksq8pc.png)

![1xce4kzk450f6](img/1xce4kzk450f6.png)

```wl
Out[]= "=== Contraction complete ==="
```

![0fv38bvu211cr](img/0fv38bvu211cr.png)

![18x16kgewu2aq](img/18x16kgewu2aq.png)

![059d4wqppfq41](img/059d4wqppfq41.png)

```wl
In[]:= indcolorf5 = newBase[5];
 ti = AbsoluteTime[];
 M5indcolorsub = indColourSubstructures[M5, 5];
 tf = AbsoluteTime[];
 Print["Time taken to extract the colour sub-amplitudes: ", tf - ti, " s"];
 M5indcolorsub = ParallelTable[Coefficient[Total[M5indcolorsub], i], {i, indcolorf5}];
 extpolM5indcolorsub = extpolContraction[M5indcolorsub, 5];
 M5indcolorsubDag = complexConjugate[M5indcolorsub]; 
  
 Print["Memory usage of contracted sub-amplitude: ", ByteCount[extpolM5indcolorsub[[1]]], " B"]; 
  
 Print["Memory usage of conjugated sub-amplitude: ", ByteCount[M5indcolorsubDag[[1]]], " B"]; 
  
 appo1 = splitInN[List @@ M5indcolorsub[[1]], 4][[1]];
 appo2 = splitInN[List @@ extpolM5indcolorsub[[1]], 4][[1]]; 
  
 Print["Memory usage of their expanded product: ", ByteCount[Total[appo1] Total[appo2] // Expand], " B"]; 
  
 Clear[appo1, appo2];
```

![1h46knhtdaqdk](img/1h46knhtdaqdk.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![0zk50ns8ejfe5](img/0zk50ns8ejfe5.png)

![0iv22v1dob4ws](img/0iv22v1dob4ws.png)

![0c4je5i33bxet](img/0c4je5i33bxet.png)

![0xqq8pqkcfjxz](img/0xqq8pqkcfjxz.png)

![0wbwftmtrrvy9](img/0wbwftmtrrvy9.png)

![1sz1c12ei2x2u](img/1sz1c12ei2x2u.png)

![1i5q6uu95b844](img/1i5q6uu95b844.png)

![052f50fqokp1w](img/052f50fqokp1w.png)

![1xyvhmbnior3t](img/1xyvhmbnior3t.png)

![0egdntvj3axm5](img/0egdntvj3axm5.png)

![0v853ldp14bzk](img/0v853ldp14bzk.png)

![0t16kiq41mbkk](img/0t16kiq41mbkk.png)

![1xjhrtzsgindh](img/1xjhrtzsgindh.png)

![11uey6iouqj5v](img/11uey6iouqj5v.png)

![1jbokra3oy7w4](img/1jbokra3oy7w4.png)

![011m801ixz51p](img/011m801ixz51p.png)

![01ruguhsoxh8g](img/01ruguhsoxh8g.png)

```wl
Out[]= "=== Contraction complete ==="
```

![17j0h16oljona](img/17j0h16oljona.png)

![0v62ysmrfxrr7](img/0v62ysmrfxrr7.png)

![14wvz144y7lke](img/14wvz144y7lke.png)

#### Dependent colour sub-amplitudes

##### n = 4

```wl
In[]:= depcolorf4 = colourFactors[M4, 4]; 
 ti = AbsoluteTime[];
 M4depcolorsub = colourSubstructures[M4, 4]; (*sottoampiezze dipendenti*)
 tf = AbsoluteTime[];
 Print["Time taken to extract the colour sub-amplitudes: ", tf - ti, " s"];
 M4depcolorsub = ParallelTable[Coefficient[Total[M4depcolorsub], i], {i, depcolorf4}];
 extpolM4depcolorsub = extpolContraction[M4depcolorsub, 4];
 M4depcolorsubDag = complexConjugate[M4depcolorsub]; 
  
 Print["Memory usage of contracted sub-amplitude: ", ByteCount[extpolM4depcolorsub[[1]]], " B"]; 
  
 Print["Memory usage of conjugated sub-amplitude: ", ByteCount[M4depcolorsubDag[[1]]], " B"]; 
  
 Print["Memory usage of their expanded product: ", ByteCount[extpolM4depcolorsub[[1]] M4depcolorsubDag[[1]] // Expand]," B"];
```

```wl
Out[]= "=== Extraction of dependent color structures ==="
```

![03zfdp85cl8fg](img/03zfdp85cl8fg.png)

![19wz5xl3vftn9](img/19wz5xl3vftn9.png)

![07saf60p3rjma](img/07saf60p3rjma.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![1xdi6piv853oa](img/1xdi6piv853oa.png)

![0jmntzg34sdu8](img/0jmntzg34sdu8.png)

![0edmdvtje0gmw](img/0edmdvtje0gmw.png)

![1lq14v2dvimuh](img/1lq14v2dvimuh.png)

![0x925334h4vxl](img/0x925334h4vxl.png)

![1spbpwmkne8xl](img/1spbpwmkne8xl.png)

![11xgaz1bgb14z](img/11xgaz1bgb14z.png)

![0oi9qc76l8s1a](img/0oi9qc76l8s1a.png)

![0xk34anpda7ok](img/0xk34anpda7ok.png)

![1rdg9yp8kfig2](img/1rdg9yp8kfig2.png)

![1t5ccbr86ljq2](img/1t5ccbr86ljq2.png)

![1ryzlx1kskijw](img/1ryzlx1kskijw.png)

![0ykwusw9dk9ny](img/0ykwusw9dk9ny.png)

![16fjcd768m0ar](img/16fjcd768m0ar.png)

```wl
Out[]= "=== Contraction complete ==="
```

![1l886iassb52d](img/1l886iassb52d.png)

![1fxp6vn3c7kkt](img/1fxp6vn3c7kkt.png)

![0d1p79icoerkc](img/0d1p79icoerkc.png)

##### n = 5

```wl
In[]:= depcolorf5 = colourFactors[M5, 5];
 ti = AbsoluteTime[];
 M5depcolorsub = colourSubstructures[M5, 5];
 tf = AbsoluteTime[];
 Print["Time taken to extract the colour sub-amplitudes: ", tf - ti, " s"];
 M5depcolorsub = ParallelTable[Coefficient[Total[M5depcolorsub], i], {i, depcolorf5}];
 extpolM5depcolorsub = extpolContraction[M5depcolorsub, 5];
 M5depcolorsubDag = complexConjugate[M5depcolorsub]; 
  
 Print["Memory usage of contracted sub-amplitude: ", ByteCount[extpolM5depcolorsub[[1]]], " B"]; 
  
 Print["Memory usage of conjugated sub-amplitude: ", ByteCount[M5depcolorsubDag[[1]]], " B"]; 
  
 Print["Memory usage of their expanded product: ", ByteCount[extpolM5depcolorsub[[1]] M5depcolorsubDag[[1]] // Expand]," B"];
```

```wl
Out[]= "=== Extraction of dependent color structures ==="
```

![16lpgmof34dtn](img/16lpgmof34dtn.png)

![1n9bedl4igu7a](img/1n9bedl4igu7a.png)

![1ewb0cgu6nvi4](img/1ewb0cgu6nvi4.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![085et8530i4tz](img/085et8530i4tz.png)

![0l9u8w7fjt16z](img/0l9u8w7fjt16z.png)

![1r183mj3mdu29](img/1r183mj3mdu29.png)

![1rympab7jmmtb](img/1rympab7jmmtb.png)

![0qfibollisxpf](img/0qfibollisxpf.png)

![01t6umtan20jo](img/01t6umtan20jo.png)

![0gji3540zwqiz](img/0gji3540zwqiz.png)

![10xzx4xrbu5qu](img/10xzx4xrbu5qu.png)

![1rr61x58j209n](img/1rr61x58j209n.png)

![19dy86rl2idn4](img/19dy86rl2idn4.png)

![0pz0pvegm2iwl](img/0pz0pvegm2iwl.png)

![1mynrxqqr1jmg](img/1mynrxqqr1jmg.png)

![1blr1pysjlzcq](img/1blr1pysjlzcq.png)

![08stq5ugitdug](img/08stq5ugitdug.png)

![1ccp6fauv48tm](img/1ccp6fauv48tm.png)

![0r3vwy1rako0g](img/0r3vwy1rako0g.png)

![1hg9fh451z7gu](img/1hg9fh451z7gu.png)

```wl
Out[]= "=== Contraction complete ==="
```

![1cmjjbbp58hyi](img/1cmjjbbp58hyi.png)

![07vkhccdnqxvb](img/07vkhccdnqxvb.png)

![0ac9ar05dqq84](img/0ac9ar05dqq84.png)

![1in26rf3s9bxb](img/1in26rf3s9bxb.png)

```wl
Out[]= "Computation unpolarized squared amplitude for 2 -> 2 using dependent colour sub-amplitudes..."
```

```wl
Out[]= "=== Extraction of dependent color structures ==="
```

![1f2eaeab005xo](img/1f2eaeab005xo.png)

![1eo50d8phb64f](img/1eo50d8phb64f.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![0fzx71o6rdou7](img/0fzx71o6rdou7.png)

![0mp2i8haq2q6p](img/0mp2i8haq2q6p.png)

![1oycf58suq07k](img/1oycf58suq07k.png)

![07qypyadnvd62](img/07qypyadnvd62.png)

![1ovirwtz9i2ny](img/1ovirwtz9i2ny.png)

![0seq8zso888ht](img/0seq8zso888ht.png)

![0a8he8wczfv1g](img/0a8he8wczfv1g.png)

![14bqmt32s0397](img/14bqmt32s0397.png)

![00w3ykykktvem](img/00w3ykykktvem.png)

![0dhg96dp84uy2](img/0dhg96dp84uy2.png)

![03c5qx7ln4gu9](img/03c5qx7ln4gu9.png)

![1rqeb2xe31hr2](img/1rqeb2xe31hr2.png)

![00acn665xxmgg](img/00acn665xxmgg.png)

![07y6ibt8yb6th](img/07y6ibt8yb6th.png)

```wl
Out[]= "=== Contraction complete ==="
```

![0bixftigyq37b](img/0bixftigyq37b.png)

![11favotk1h2uq](img/11favotk1h2uq.png)

![1le1ux0ux528r](img/1le1ux0ux528r.png)

![14kmb59soxwqq](img/14kmb59soxwqq.png)

![0lyffxl7mp1t8](img/0lyffxl7mp1t8.png)

![0h0q0kppo2xh3](img/0h0q0kppo2xh3.png)

![1be9v754i3z7s](img/1be9v754i3z7s.png)

![1bvmwwstut6q8](img/1bvmwwstut6q8.png)

![0cp912gbyr1o4](img/0cp912gbyr1o4.png)

![14xx3zarc5iss](img/14xx3zarc5iss.png)

![1roy373z05rev](img/1roy373z05rev.png)

![0bw8l700fr3fy](img/0bw8l700fr3fy.png)

![0t1d42t2xll7b](img/0t1d42t2xll7b.png)

![0vj6ysnyiob9v](img/0vj6ysnyiob9v.png)

```wl
In[]:= (result4 - theory4)
```

```wl
Out[]= 0
```

#### gg -> ggg

```wl
In[]:= result5 = UnpolSquaredAmplitude[5];
```

```wl
Out[]= "Computation unpolarized squared amplitude for 2 -> 3 using dependent colour sub-amplitudes..."
```

```wl
Out[]= "=== Extraction of dependent color structures ==="
```

![06akxkk8mr6k7](img/06akxkk8mr6k7.png)

![0ta8qwkyya0kp](img/0ta8qwkyya0kp.png)

```wl
Out[]= "=== External Polarization Contraction ==="
```

![036jvkd0xtcbs](img/036jvkd0xtcbs.png)

![0d84lfpld5mdg](img/0d84lfpld5mdg.png)

![04586jci3jx6e](img/04586jci3jx6e.png)

![0ewh42xmsof7d](img/0ewh42xmsof7d.png)

![153kjjcscrx0u](img/153kjjcscrx0u.png)

![1lvlwfhzuql46](img/1lvlwfhzuql46.png)

![1ommigk7rluq2](img/1ommigk7rluq2.png)

![0rodp8ejh2xkx](img/0rodp8ejh2xkx.png)

![1r5g9ptqq0faa](img/1r5g9ptqq0faa.png)

![04qqjzbei63cm](img/04qqjzbei63cm.png)

![0hhxekzip3kv2](img/0hhxekzip3kv2.png)

![0q8dcsoa4ag78](img/0q8dcsoa4ag78.png)

![0hxphi2zcsb41](img/0hxphi2zcsb41.png)

![1wxruh7n350t8](img/1wxruh7n350t8.png)

![03t4xjfv1lkej](img/03t4xjfv1lkej.png)

![1dl2dbh8tpb6j](img/1dl2dbh8tpb6j.png)

![0ekb7fhvzwxns](img/0ekb7fhvzwxns.png)

```wl
Out[]= "=== Contraction complete ==="
```

![0xjtxrw9mrrpq](img/0xjtxrw9mrrpq.png)

![1agrp23ryfz0d](img/1agrp23ryfz0d.png)

![1mx20aafwyl39](img/1mx20aafwyl39.png)

![01cfqdihsikto](img/01cfqdihsikto.png)

![1aq5x5l8e1sc4](img/1aq5x5l8e1sc4.png)

![08oktk8l28qho](img/08oktk8l28qho.png)

![0ifjde9tdkzmb](img/0ifjde9tdkzmb.png)

![0ol5bgy24rr3w](img/0ol5bgy24rr3w.png)

![18hlq3eg6zdpo](img/18hlq3eg6zdpo.png)

![07d8cbwdazrxw](img/07d8cbwdazrxw.png)

![1cbdp1p4mrwwt](img/1cbdp1p4mrwwt.png)

![1fbzb54nkvjuu](img/1fbzb54nkvjuu.png)

![06tq0xo3na2kr](img/06tq0xo3na2kr.png)

![0jn5r7mo2yqt3](img/0jn5r7mo2yqt3.png)

![1ge58x9mfqszc](img/1ge58x9mfqszc.png)

![0de0f3zxse240](img/0de0f3zxse240.png)

![1r3dejvhsklb1](img/1r3dejvhsklb1.png)

![1qjw1dylg7wie](img/1qjw1dylg7wie.png)

![1gp0qgv19hmmx](img/1gp0qgv19hmmx.png)

![1p1gk6ked4wus](img/1p1gk6ked4wus.png)

![1n5urcvbpu4z9](img/1n5urcvbpu4z9.png)

![1e7w0wv5ihtvp](img/1e7w0wv5ihtvp.png)

![18jimtoj9h5mb](img/18jimtoj9h5mb.png)

![1a3aaax88wiqn](img/1a3aaax88wiqn.png)

![06s1q4v8a1tsb](img/06s1q4v8a1tsb.png)

![1pmi8pt76g6t0](img/1pmi8pt76g6t0.png)

![1xlkg2jbga9k9](img/1xlkg2jbga9k9.png)

![1nrthuuyzuk1q](img/1nrthuuyzuk1q.png)

![0opl5pjvwn062](img/0opl5pjvwn062.png)

![0n79tze0tmokr](img/0n79tze0tmokr.png)

![1kjf2444ono7z](img/1kjf2444ono7z.png)

![13h0me8ijuf29](img/13h0me8ijuf29.png)

![0tgv3ajxoly8h](img/0tgv3ajxoly8h.png)

![07cgo1pzvot43](img/07cgo1pzvot43.png)

![1prnw8mromyeg](img/1prnw8mromyeg.png)

![07j0glfsn80ne](img/07j0glfsn80ne.png)

![0sj04ot1q0nxi](img/0sj04ot1q0nxi.png)

![13ysctqc43jxc](img/13ysctqc43jxc.png)

![079hwj0ro4y17](img/079hwj0ro4y17.png)

![1oxn8fmy4kiaw](img/1oxn8fmy4kiaw.png)

![1w2mqg8anctnc](img/1w2mqg8anctnc.png)

![1qcm1k120b77r](img/1qcm1k120b77r.png)

![0sbypwiznuwmx](img/0sbypwiznuwmx.png)

![10726rcqgqp75](img/10726rcqgqp75.png)

![08j5gpcd9tm98](img/08j5gpcd9tm98.png)

![00vmjx594f5rj](img/00vmjx594f5rj.png)

![12w4qxdsratry](img/12w4qxdsratry.png)

![0lpxrtna61rxp](img/0lpxrtna61rxp.png)

![0weae50y7oty0](img/0weae50y7oty0.png)

![0ttj9c7n9gqyy](img/0ttj9c7n9gqyy.png)

![1dlw8vbgxmr88](img/1dlw8vbgxmr88.png)

![0ijaj0xzk4k6g](img/0ijaj0xzk4k6g.png)

![0mi2igyg13lcb](img/0mi2igyg13lcb.png)

![0l0ijsh7b7xqi](img/0l0ijsh7b7xqi.png)

![0z3v7pqw9igcw](img/0z3v7pqw9igcw.png)

![1c6s9obu7vxd9](img/1c6s9obu7vxd9.png)

![0s5j5524vkypo](img/0s5j5524vkypo.png)

![0q1e4lyj7de18](img/0q1e4lyj7de18.png)

![17fa8phqzn4v9](img/17fa8phqzn4v9.png)

![1r8jwwrou1q0a](img/1r8jwwrou1q0a.png)

![1e9l723n2ud7v](img/1e9l723n2ud7v.png)

![16e61dtw40qep](img/16e61dtw40qep.png)

![0ziainver9gfy](img/0ziainver9gfy.png)

![10qegupewtnf5](img/10qegupewtnf5.png)

![1bvxr5uha21yg](img/1bvxr5uha21yg.png)

![0tjg3syhvnfh7](img/0tjg3syhvnfh7.png)

![1mpq18yg2azjh](img/1mpq18yg2azjh.png)

![01draccgyq5sj](img/01draccgyq5sj.png)

![0exeb47tp0xs3](img/0exeb47tp0xs3.png)

![0x4ddv2dx1z4y](img/0x4ddv2dx1z4y.png)

![080u7byr42ww6](img/080u7byr42ww6.png)

![1121spol42ay3](img/1121spol42ay3.png)

![1o778kdhwidxg](img/1o778kdhwidxg.png)

![1e2n0ssgvh68g](img/1e2n0ssgvh68g.png)

![0cgl5u6097uki](img/0cgl5u6097uki.png)

![1wyzlzwcu4v91](img/1wyzlzwcu4v91.png)

![1gn6z7v7uq6j2](img/1gn6z7v7uq6j2.png)

![0xw2zkfmbskh6](img/0xw2zkfmbskh6.png)

![1kae69wo8gs9h](img/1kae69wo8gs9h.png)

![0bhnsgcaokcq7](img/0bhnsgcaokcq7.png)

![19m53y1vdwtjz](img/19m53y1vdwtjz.png)

![1wslvzsse5oxg](img/1wslvzsse5oxg.png)

![14zye00dbdvn2](img/14zye00dbdvn2.png)

![1efnxlzkuoj8s](img/1efnxlzkuoj8s.png)

![0j95vuq9e8iws](img/0j95vuq9e8iws.png)

![0k8vsao08b0nr](img/0k8vsao08b0nr.png)

![1petgk7bm1a9x](img/1petgk7bm1a9x.png)

![0qtyjwezdr1zl](img/0qtyjwezdr1zl.png)

![1cpzmprap7ftr](img/1cpzmprap7ftr.png)

![0rkfh2f6shq1b](img/0rkfh2f6shq1b.png)

![09g1uxkmzukrw](img/09g1uxkmzukrw.png)

![16s2leqs5eado](img/16s2leqs5eado.png)

![10ujbp9zttuql](img/10ujbp9zttuql.png)

![1q7t6luty4288](img/1q7t6luty4288.png)

![1nhg8tn7pc10c](img/1nhg8tn7pc10c.png)

![06cmcv682dhka](img/06cmcv682dhka.png)

![1xdd0e655bzd9](img/1xdd0e655bzd9.png)

![103m74axgvjm7](img/103m74axgvjm7.png)

![1046i8gl4ox59](img/1046i8gl4ox59.png)

![1mi0ilwslu6i0](img/1mi0ilwslu6i0.png)

![175603aca8ls6](img/175603aca8ls6.png)

![0ho35vb5wk14f](img/0ho35vb5wk14f.png)

![0165uxsacryat](img/0165uxsacryat.png)

![1w6jmk7mr6xbw](img/1w6jmk7mr6xbw.png)

![0thr8vlada1z9](img/0thr8vlada1z9.png)

![0unhptmn65o43](img/0unhptmn65o43.png)

![0pilf42sr1d8v](img/0pilf42sr1d8v.png)

![0zquoig7sv817](img/0zquoig7sv817.png)

![1eyzan0c3az0b](img/1eyzan0c3az0b.png)

![1cotjx8iyrol4](img/1cotjx8iyrol4.png)

```wl
In[]:= result5
```

![1k9lthx8ik9sc](img/1k9lthx8ik9sc.png)

![0wcuops1qen8x](img/0wcuops1qen8x.png)

![0pmm6j5emkshv](img/0pmm6j5emkshv.png)

```wl
In[]:= result5 - theory5
```

```wl
Out[]= 0
```