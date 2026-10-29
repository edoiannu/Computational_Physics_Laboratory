# Computational Physics Laboratory

Questo repository raccoglie un progetto di calcolo simbolico dedicato allo studio delle ampiezze di scattering di gluoni a livello albero nella teoria di Yang-Mills/QCD, implementato in Wolfram Language (Mathematica).

Il lavoro presente in `PROJECT.md` e nel notebook `PROJECT.nb` esplora in modo dettagliato la costruzione di diagrammi, la definizione delle regole di Lorentz e colore, la verifica dell'identità di Ward e il calcolo dell'ampiezza quadrata non polarizzata per processi come:

- `gg -> gg`
- `gg -> ggg`

## Obiettivo del progetto

L'obiettivo è costruire un framework simbolico per:

- generare grafici di Feynman a livello albero;
- applicare conservazione del momento e regole di vertice;
- rappresentare strutture di Lorentz e colore;
- contrarre indici interni ed esterni;
- verificare l'invarianza di gauge tramite l'identità di Ward;
- calcolare ampiezze quadratiche non polarizzate.

In pratica, il progetto mima il flusso tipico di un calcolo perturbativo nello standard model, ma in un ambiente di computer algebra dove la manipolazione simbolica è eseguita direttamente con Wolfram Language.

## Struttura del repository

- `PROJECT.md` — documentazione principale del progetto, scritta come una sequenza di blocchi di codice e spiegazioni; contiene la maggior parte del lavoro e delle analisi.
- `PROJECT.nb` — notebook Wolfram originali con le definizioni e gli esperimenti.
- `REPORT.pdf` — report finale del progetto.
- `img/` — immagini generate durante l'analisi, grafici, diagrammi e output visuali.
- `2g-2g/` — contenuto relativo al canale `2 -> 2`.
- `2g-3g/` — contenuto relativo al canale `2 -> 3`.

## Tecnologie e strumenti

- Wolfram Language / Mathematica
- Kernel parallel in Wolfram per valutazioni distribuite
- Calcolo simbolico per strutture di Feynman, Lorentz e colore

## Requisiti

Per eseguire il codice del progetto è necessario:

- Wolfram Mathematica oppure Wolfram Engine
- un ambiente in grado di eseguire notebook `.nb` o file `.wl`
- eventualmente più core/processori per sfruttare `LaunchKernels[$ProcessorCount]`

## Come usare il progetto

1. Apri `PROJECT.nb` in Wolfram Mathematica.
2. Esegui le celle in ordine, partendo dalle definizioni iniziali.
3. Inizialmente viene preparato il kernel e definite le formattazioni simboliche per variabili come:
   - momenti `p[i]`
   - polarizzazioni `e[i]`
   - indici di Lorentz `mu[i]`
   - indici di colore `c[i]`
4. Successivamente vengono generati i grafi e costruiti i diagrammi.
5. Si applicano le regole per la conservazione del momento, la contrazione di Lorentz e i fattori di colore.
6. Infine si verifica l'identità di Ward e si calcola l'ampiezza quadrata non polarizzata.

## Argomenti principali trattati

### 1. Generazione dei grafi

Il codice costruisce i grafi a tre vertici e a quattro vertici partendo da un grafo iniziale, applicando regole di inserzione iterative fino a raggiungere un numero fissato di punti esterni.

### 2. Definizione delle strutture simboliche

Vengono definite le rappresentazioni per:

- quadrivettori
- metriche
- delta di Kronecker
- generatori del gruppo di colore
- costanti di struttura
- prodotti scalari lorentziani

### 3. Ampiezze di diagramma

La funzione `diagramma[grafo_]` costruisce l'espressione associata a un grafico, combinando:

- propagatori interni
- vertici di QCD
- conservazione del momento
- indici di Lorentz e colore

### 4. Identità di Ward

Il progetto verifica che le ampiezze siano gauge invarianti, applicando contrazioni con momenti esterni e controllando che il risultato sia nullo.

### 5. Fattori di colore e ampiezze indipendenti

Vengono estratte le sottostrutture di colore e valutate le combinazioni indipendenti e dipendenti, con una procedura che prepara la base per il calcolo dell'ampiezza quadrata.

### 6. Ampiezza quadrata non polarizzata

L'ultimo stadio del progetto consiste nel costruire e semplificare il quadrato modulo del modulo dell'ampiezza, usa stato polarizzato e foglie esterne.

## Note importanti

- Questo repository è fortemente orientato alla ricerca e allo studio computazionale, non a un prodotto software standard.
- Il contenuto matematico e simbolico è molto denso e richiede una buona familiarità con QCD, teoria dei campi e notazione di Feynman.
- `PROJECT.md` è il documento di riferimento principale per comprendere il flusso completo del lavoro.

## Risorse principali

- `PROJECT.md`: documentazione completa
- `PROJECT.nb`: notebook eseguibile
- `REPORT.pdf`: report finale

## Licenza

Non è specificata una licenza nel repository. Se vuoi usarlo o riutilizzarlo in un contesto pubblico o accademico, verifica prima la policy del proprietario del repo.

## In breve

Questo progetto è un esempio avanzato di calcolo simbolico in fisica teorica, con implementazione in Wolfram Language di strumenti per la manipolazione di ampiezze di gluoni, regole di colore e Lorentz, verifica di gauge invariance e ottimizzazione di calcoli non polarizzati.
