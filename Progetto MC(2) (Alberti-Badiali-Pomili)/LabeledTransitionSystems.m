(* ::Package:: *)

BeginPackage["LabeledTransitionSystems`"]

Needs["GraphUtilities`"];
Needs["GUIKit`"]
IndiceNodo::usage = "IndiceNodo[x] restituisce l'indice dello stato x, ovvero la parte numerica del stato(ad esempio: IndiceNodo[stato5] restituisce 5)";
LetteraNodo::usage = "LetteraNodo[x] restituisce la parte letterale di x(ad esempio: LetteraNodo[q2] restituisce q)";
CreaLista::usage = "CreaLista[y], dato un lts y formattato come un grafo orientato ed etichettato di mathematica
(ad esempio: {{\"stato0\" -> \"stato1\", \"azione0\"}, {\"stato0\" -> \"stato2\", \"azione2\"}, {\"stato1\" \[Rule] \"stato2\", \"azione1\"}...})
 viene restituita una lista sempre rappresentante l'lts ma formattata in modo differente
(ad esempio: {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\", \"stato2\", \"azione1\"}})";
InserisciIntermedi::usage= "InserisciIntermedi[y1], dato un lts y formattato nello stesso modo di uno restituito funzione CreaLista
(ad esempio: {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\", \"stato2\", \"azione1\"}}),
 viene restituito lo stesso lts ma alla lista vengono aggiunti anche gli stati che non hanno archi uscenti
(ad esempio: {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\"}, {\"stato1\", \"stato2\", \"azione1\"}})";
InserisciUltimo::usage = "InserisciUltimo[y2, lts] date due diverse rappresentazioni dello stesso lts, dove quella con y2 e' la rappresentazione
 di un grafo orientato ed etichettato di mathematica ma con i nodi ordinati(ad esempio: {{\"stato0\" -> \"stato1\", \"azione0\"},
 {\"stato0\" -> \"stato2\", \"azione2\"}, {\"stato1\" \[Rule] \"stato2\", \"azione1\"}...} dove tutti gli archi uscenti dello stato0 vengono
 prima di quelli dello stato1) e dove quello di lts e' la stesso tipo di formattazione data dalla funzione CreaLista
(ad esempio: {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\", \"stato2\", \"azione1\"}}),
 la funzione restituisce l'lts con la stessa formattazione del parametro lts ma, nel caso in cui l'ultimo stato(lo stato con indice maggiore)
 non abbia nodi uscenti allora aggiunge alla lista una sottolista contenente solamente il nome dello stato(ad esempio:
 {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\", \"stato2\", \"azione1\"}, {\"stato2\"}})";
GrafoFormattato::usage = "GrafoFormattato[lts] prende in input un grafo etichettato ed orientato di mathematica,
 lts, ed a lui applica in sequenza le funzioni: IndiciSequenziali, CreaLista, IndiciIntermedi, InserisciUltimo.
 Restituisce quindi una lista del seguente tipo:
 {{\"stato0\", \"stato1\", \"azione0\", \"stato2\", \"azione2\"}, {\"stato1\", \"stato2\", \"azione1\"}, {\"stato2\"}}";
IndiciVerticiSequenziali::usage = "IndiciVerticiSequenziali[lts], prende in input lts, ovvero un grafo etichettato ed
 orientato formattato come un grafo di mathematica e restituisce in output lo stesso grafo ma con i nodi ordinati
 ad esempio: se lts = {{\"stato0\" -> \"stato1\", \"azione0\"}, {\"stato2\" -> \"stato3\", \"azione2\"}, {\"stato1\" \[Rule] \"stato2\", \"azione1\"}}
 allora la funzione restituira':
 {{\"stato0\" -> \"stato1\", \"azione0\"}, {\"stato1\" -> \"stato2\", \"azione1\"}, {\"stato2\" \[Rule] \"stato3\", \"azione2\"}}";
IndiciVerticiOriginali::usage = "IndiciVerticiOriginali[r, ltsx, ltsy], prende in input la relazione r(una lista formattata in questo modo:
 {{q0, q1}, {q1,q2}, ...} con i nomi degli stati nelle coppie uguali a quelli di ltsy), l'ltsx(un qualunque grafo orientato ed etichettato)
 e l'ltsy(rappresentazione di ltsy restituita dalla funzione GrafoFormattato) e restituisce lo stesso tipo di relazione ma con i nomi degli stati
 nelle coppie uguali a quelli di ltsx";
MenuUguale::usage = "MenuUguale[nodo1, nodo2, lts1, lts2], prende in input nodo1(un nodo/stato di lts1), nodo2(un nodo/stato di lts2) e due lts,
 lts1 e lts2(delle liste formattate allo stesse modo di quelle restituite dalla funzione GrafoFormattato rappresentanti due labeled transition systems)
 e restituisce in output True se nodo2 puo' fare le stesse azioni di nodo1, False altrimenti";
ListaNuoveCoppie::usage="ListaNuoveCoppie[nodo1, nodo2, lts1, lts2] prende in input nodo1(un nodo/stato di lts1), nodo2(un nodo/stato di lts2)
 e due lts, lts1 e lts2(delle liste formattate allo stesse modo di quelle restituite dalla funzione GrafoFormattato rappresentanti
 due labeled transition systems) e restituisce la lista delle n-uple di stati da controllare per verificare la relazione di simulazione";
CoppiaGiaPresente::usage="CoppiaGiaPresente[r_,lista_Symbol], prende in input la relazione r(una lista formattata in questo modo:
 {{q0, q1}, {q1,q2}, ...}) e lista che e' una lista che se lunga 2 rappresenta una coppia(come le coppie della lista r) oppure se e' di lunghezza
 maggiore(lunghezza n) allora rappresenta n - 1 coppie formate dal primo elemento della lista piu' l'elemento i-esimo(per 2<=i<=n), la funzione
 cancella da lista tutte le coppie che sono anche presenti in r ";
AndOr::usage="AndOr[lista, ltssimulato, ltssimulante, relazione] prende in input:
lista: Una lista di coppie e di stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante o di n-uple di stati, il primo appartenente ad ltssimulato e l'altro ad ltssimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
La funzione restituisce True se tutti i primi elementi delle coppie in lista possono essere simulate dal secondo elemento della loro coppia
 e se almeno una delle possibili coppie che possono essere formate dalle n-uple(scomponendo la n-upla in coppie in cui il primo elemento
 e' sempre il primo della n-upla mentre il secondo e' uno qualsiasi eccetto il primo della n-upla) per ogni n-upla e' verificata
(ovvero il primo elemento puo' essere simulato dal secondo), altrimenti restituisce False";
F::usage = "F[coppianodi, ltssimulato, ltssimulante, relazione] prende in input:
coppianodi: Una coppia di nodi/stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
La funzione verifica che il primo nodo in coppianodi sia simulabile dal secondo analizzando inanzi tutto che il secondo nodo possa fare le stesse
 azioni del primo ; nel caso in cui questo sia vero coppianodi viene aggiunta a relazione e viene lanciata la funzione AndOr con la lista dei nodi
da verificare perche' coppianodi possa appartenere alla relazione di simulazione, altrimenti viene restituito False. Nel caso in cui la AndOr
 restituisca True viene ritornato True, altrimenti coppianodi viene eliminato da relazione e viene restituito False.";
Simulazione::usage = "Simulazione[ltsSimulato, ltsSimulante] prende in input due liste opportunamente formattate come grafi orientati ed eticchettati
 di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista il cui primo elemento e' True e il secondo e' la
 relazione di simulazione nel caso in cui ltssimulato venga effettivamente simulato da ltssimulante, altrimenti ritorna False.";
SimulationEquivalence::usage = "SimulationEquivalence[lts1, lts2] prende in input due liste opportunamente formattate come grafi orientati ed etichettati
 di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista con le due relazioni di simulazione se lts1 ed lts2
 sono simulation equivalent. Torna una lista con una relazione di simulazione se uno dei due lts e' simulato dall'altro. Oppure torna False nel
 caso in cui nessuno dei due simula l'altro.";
AndOr1::usage = "AndOr1[lista, lts1, lts2, relazione] prende in input:
lista: Una lista di coppie e di stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante o di n-uple di stati, il primo appartenente ad ltssimulato e l'altro ad ltssimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
Restituisce in output True se tutte le n-uple di lunghezza 2 sono coppie che appartengono alla relazione di bisimulazione e se almeno per tutte
 le n-uple di lunghezza maggiore di 2 esiste una coppia (formata dal primo elemento piu' un'altro qualsiasi della n-upla diverso pero' dal primo)
 che appartenga alla relazione di bisimulazione, False altrimenti.";
FB::usage = "FB[coppianodi, lts1, lts2, relazione] prende in input:
coppianodi: Una coppia di nodi/stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
La funzione verifica che il primo nodo in coppianodi abbia lo stesso menu' di mosse del secondo elemento in coppianodi, e viceversa nel caso in
 cui questo sia vero, coppianodi viene aggiunta a relazione e viene lanciata la AndOr1 con la lista dei nodi da verificare perche' coppianodi possa
 n appartenere alla relazione di bisimulazione, viene restituito False. Nel caso in cui la  AndOr1 restituisca True viene ritornato True
 altrimenti, viene eliminato da relazione e viene restituito False.";
Bisimulazione::usage = "Bisimulazione[lts1, lts2]prende in input due liste opportunamente formattate come grafi orientati ed eticchettati
 di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista il cui primo elemento e' True e il secondo e' la
 relazione di bisimulazione nel caso in cui lts1 e lts2 siano bisimili, altrimenti ritorna False.";
MenuDeadlock2::usage = "MenuDeadlock2[nodosimulato, nodosimulante, ltssimulato, ltssimulante] prende in input: due nodi(due stringhe),
 nodosimulato e nodo simulante, appartenenti rispettivamente ai due lts ltssimulato e ltssimulante(che sono due liste opportunamente formattate
 tramite la funzione GrafoFormattato), e restituisce in output: True se sia nodosimulato che nodosimulato sono stati senza archi uscenti nei
 loro rispettivi lts, False altrimenti";
FC::usage = "FC[coppianodi, ltssimulato, ltssimulante, relazione] prende in input:
coppianodi: Una coppia di nodi/stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
La funzione FC lancia funzione F con gli stessi parametri per verificare se ltssimulante simula ltssimulato se questo e il caso la funzione
 verifica se ogni coppia all interno della relazione restituita dalla funzione F rispetta la definizione di complete  simulation verificata dalla
 funzione MenuDeadlock2 restituendo True e la rispetta e False altrimenti";
CompleteSimulation::usage = "CompleteSimulation[ltsSimulato_, ltsSimulante_] prende in input due liste opportunamente formattate come grafi orientati ed etichettati
 di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista il cui primo elemento e' True e il secondo e' la
 relazione di simulazione nel caso in cui ltssimulato venga effettivamente completed simulato da ltssimulante, altrimenti ritorna False.";
CompleteSimulationEquivalence::usage = "CompleteSimulationEquivalence[lts1, lts2] prende in input due liste opportunamente formattate come grafi
 orientati ed eticchettati di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista con le due relazioni
 di simulazione se lts1\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)ed lts2 sono complete simulation equivalent. Torna una lista con una relazione di simulazione se uno dei due lts
 e' complete simulato dall'altro. Oppure torna False nel caso in cui nessuno dei due complete simula l'altro.";
FR::usage = "FR[coppianodi, ltssimulato, ltssimulante, relazione] prende in input:
coppianodi: Una coppia di nodi/stati, il primo appartenente ad ltssimulato ed il secondo ad ltsimulante;
ltssimulato e ltssimulante: Sono due liste formattate opportunamente dalla funzione GrafoFormattato e che rappresentano una l'lts che puo' essere
 simulato(ltssimulato) e l'altra l'lts che lo puo' simulare(ltssimulante);
relazione: Una lista di coppie di nodi che rappresenta la relazione di simulazione.
La funzione FR lancia F con gli stessi parametri per verificare se ltssimulante simula ltssimulato se questo e il caso la funzione verifica
 se ogni coppia all'interno della relazione rispetta la definizione di ready simulation ovvero verifica se il primo n ritorna True in caso
 positivo, False altrimenti";
ReadySimulation::usage = "ReadySimulation[ltsSimulato_, ltsSimulante_] prende in input due liste opportunamente formattate come grafi orientati ed etichettati
 di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista il cui primo elemento e' True e il secondo e' la
 relazione di simulazione nel caso in cui ltssimulato venga effettivamente ready simulato da ltssimulante, altrimenti ritorna False.";
ReadySimulationEquivalence::usage = "ReadySimulationEquivalence[lts1, lts2] prende in input due liste opportunamente formattate come grafi
 orientati ed eticchettati di mathematica, rappresentati due Labeled Transition System e restituisce in output una lista con le due relazioni
 di simulazione se lts1\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)ed lts2 sono ready simulation equivalent. Torna una lista con una relazione di simulazione se uno dei due lts
 e' ready simulato dall'altro. Oppure torna False nel caso in cui nessuno dei due ready simula l'altro.";
Grafica::usage = "Grafica[] avvia una GUI tramite la quale e' possibile eseguire la verifica di simulation equivalence,
 completed simulation equivalence, ready simulation equivalence e bisimulation di due lts caricati da un file opportunamente formattato.";
LTSChecker::usage = "LTSChecker[lts] controlla che il dato contenuto all'interno di lts rispetti la giusta formattazione che serve per essere
 accettato come lts da funzioni come la SimulationEquivalence o la ReadySimulationEquivalence, in caso in cui ci siano problemi restituisce False
 e lancia uno di 3 tipi differenti di errori, altrimenti ritorna True";
Begin["`Private`"]

LTSChecker::error0="Almeno uno degli archi del grafo non e' etichettato";
LTSChecker::error1="Almeno uno degli archi del grafo non e' orientato";
LTSChecker::error2="L'lts deve avere almeno un nodo da cui e' possibile raggiungere tutti gli altri";

LTSChecker[lts_]:=
	Module[{i, ok, vertexlist, maxlength},
	ok = True;
	i = 1;
	While[ i <= Length[lts],
		If[Length[lts[[i]]] != 2, ok = False]
	i++];
	If[ok == False, Message[LTSChecker::error0]; Return[False]];
	i = 1;
	While[ i <= Length[lts],
		If[StringPosition[ToString[lts[[i,1]]], "->"] == {}, ok = False]
	i++];
	If[ok == False, Message[LTSChecker::error1]; Return[False]];
	i = 1;
	vertexlist = Sort[VertexList[lts]];
	maxlength = vertexlist*vertexlist;
	ok = False;
	While[ i <= Length[vertexlist],
		If[vertexlist == Sort[NeighborhoodVertices[lts,vertexlist[[i]]]], ok = True]
	i++];
	If[ok == False, Message[LTSChecker::error2]; Return[False]];
	Return[True]
]

IndiceNodo[x_] := FromDigits[StringDrop[x, 1]](*estrae indice realtivo al vertice*)
LetteraNodo[x_] := StringDrop[x, -(StringLength[x] - 1)]
CreaLista[y_] := 
	Module[{lts,n,temp,listaNodi,a1}, 
		lts = y;
		n = 1;
		temp={};
		listaNodi={};
		While [n <= Length[lts],
			a1 = lts[[n]];
			If[temp == {},
				temp = Join [temp,{a1[[1,1]]},{a1[[1, 2]]}, {a1[[2]]}], 
				If[temp[[1]] == a1[[1, 1]],
					(* then *)
					temp = Join[temp, {a1[[1, 2]]}, {a1[[2]]}],
					(* else *) 
					AppendTo[listaNodi, temp];
					temp = {}; 
					temp = Join[temp, {a1[[1, 1]]}, {a1[[1, 2]]}, {a1[[2]]}]
				] 
			];
			n = n + 1
		];
		AppendTo[listaNodi, temp];
		Return[listaNodi]
	]

InserisciIntermedi[y1_] := 
	Module[{listaNodiApp,n,lung,num,num2},
		listaNodiApp = y1; 
		n = 1;
		lung = Length[listaNodiApp];
		While [n < lung,
			If[(num = IndiceNodo[listaNodiApp[[n, 1]]]) != (num2 = (IndiceNodo[listaNodiApp[[n + 1, 1]]] - 1)),
				(* then *)
				listaNodiApp = Insert[listaNodiApp, {StringJoin[LetteraNodo[listaNodiApp[[1, 1]]], (ToString[IndiceNodo[listaNodiApp[[n, 1]]] + 1])]}, (IndiceNodo[listaNodiApp[[n, 1]]] + 1)];
				lung = lung + 1
			];
			n = n + 1;
		];
		Return[listaNodiApp]	
	]

InserisciUltimo[y2_,lts_] := 
	Module[{listaNodiApp,n,ltsApp}, 
		listaNodiApp = y2;
		ltsApp = lts;
		n = Length[listaNodiApp];
		While[n < Length[VertexList[ltsApp]],
			AppendTo[listaNodiApp, {StringJoin[LetteraNodo[listaNodiApp[[1, 1]]], (ToString[IndiceNodo[listaNodiApp[[n, 1]]] + 1])]}];
			n = n + 1
		];
		Return[listaNodiApp]
	]
	
GrafoFormattato[lts_] := 
 	Module[{lista1, lista2, lista3, ltsordinato},
 		ltsordinato = Sort[lts, IndiceNodo[#1[[1, 1]]] < IndiceNodo[#2[[1, 1]]] &];
  	    ltsordinato = IndiciVerticiSequenziali[ltsordinato];	    
  		lista1 = CreaLista[ltsordinato];
  		lista2 = InserisciIntermedi[lista1];
  		lista3 = InserisciUltimo[lista2, ltsordinato];
  		Return[lista3]
  	]

IndiciVerticiSequenziali[lts_] := 
	Module[{n, ltstemp, letteranodo, listavertici},
		n = 1;
		ltstemp = lts;
		letteranodo = LetteraNodo[lts[[1, 1, 1]]];
		listavertici = Sort[VertexList[lts], IndiceNodo[#1] < IndiceNodo[#2] &];
  		While [ n <= Length[ltstemp],
  			ltstemp[[n, 1, 1]] = StringJoin[letteranodo , IntegerString[Position[listavertici, ltstemp[[n, 1, 1]]]]];
   			ltstemp[[n, 1, 2]] = StringJoin[letteranodo , IntegerString[Position[listavertici, ltstemp[[n, 1, 2]]]]];
   			n++
   		];
   		Return[ltstemp]
  ]

IndiciVerticiOriginali[r_, ltsx_, ltsy_] := 
	Module[{listaverticix, listaverticiy, n, rtemp, letteranodox, letteranodoy},
		If [LetteraNodo[r[[1, 1]]] ==   LetteraNodo[ltsx[[1, 1, 1]]],
   			rtemp = r,
   			rtemp = Map[Reverse, r]
   		];
  		listaverticix = Sort[VertexList[ltsx], IndiceNodo[#1] < IndiceNodo[#2] &];
  		listaverticiy = Sort[VertexList[ltsy], IndiceNodo[#1] < IndiceNodo[#2] &];
  		n = 1;
  		letteranodox = LetteraNodo[rtemp[[1, 1]]];
  		letteranodoy = LetteraNodo[rtemp[[1, 2]]];
  		While [ n <= Length[rtemp],
  			rtemp[[n, 1]] = StringJoin[letteranodox , IntegerString[IndiceNodo[listaverticix[[IndiceNodo[ rtemp[[n, 1]]]]]]]];
   			rtemp[[n, 2]] = StringJoin[letteranodoy , IntegerString[IndiceNodo[listaverticiy[[IndiceNodo[ rtemp[[n, 2]]]]]]]];
   			n++;
   		];
		
	Return[rtemp]
  ]
	
MenuUguale[nodo1_, nodo2_, lts1_, lts2_] := 
	Module[{n, listamossex, indicex, listamossey, indicey}, 
		n = 1;
		listamossex = {};
		indicex = IndiceNodo[nodo1];
		(* con il ciclo while carico in listamossex tutte le label degli archi
		   uscenti dal vertice x *)
		While[ n <= ((Length[lts1[[indicex]]] - 1) / 2),
  			listamossex = Append[listamossex, lts1[[indicex, (n * 2) + 1]]];
  			n++
  		];
  		n = 1;
		listamossey = {};
		indicey = IndiceNodo[nodo2];
		(* con il ciclo while carico in listamossey tutte le label degli archi
		   uscenti dal vertice y *)
		While[ n <= ((Length[lts2[[indicey]]] - 1) / 2),
  			listamossey = Append[listamossey, lts2[[indicey, (n * 2) + 1]]];
  			n++
  		];
  		n = 1;
		(* controlla se la lista delle mosse del vertice x e' un sottoinsieme della lista
			delle mosse del vertice y *)
		If[Sort[Intersection[listamossex, listamossey]] == Sort[DeleteDuplicates[listamossex]], 
			Return [True],
			Return [False]
		]
	]

ListaNuoveCoppie[nodo1_, nodo2_, lts1_, lts2_] := 
	Module[{n,m,lista,indicex,indicey,labelx,listacandidatiy},
		n  = 1;
		lista = {};
		indicex = IndiceNodo[nodo1];
		indicey = IndiceNodo[nodo2];
		(* scandisce la lista degli stati raggiungibili partendo dallo stato x *)
		While[ n <= ((Length[lts1[[indicex]]] - 1) / 2),
 			AppendTo[lista, {lts1[[indicex, n*2]]}];
 			labelx = lts1[[indicex, (n*2) + 1]];
 			listacandidatiy = {};
 			m = 1;
 			(* crea la lista dei candidati di y, ovvero la lista degli stati del grafo y, raggiungibili
 				dallo stato y tramite una mossa equivalente a quella dello stato x *)
 			While[ m <= ((Length[lts2[[indicey]]] - 1) / 2),
  				If[ labelx == lts2[[indicey, (m *2) + 1]],
   					listacandidatiy = Append[listacandidatiy, lts2[[indicey, m*2]]]
   				];
  				m++
  			];
 			m = 1;
 			(* aggiunge alla sottolista di x, che si trova all'interno di "lista", gli elementi 
 				di "listacandidatiy (es: {{x2,y2,y3}, ...} *)  
 			While[ m <= Length[listacandidatiy],
  				lista[[n]] = Append[lista[[n]], listacandidatiy[[m]]];
  				m++
  			];
 			n++
 		];
		Return[lista]	
	]

(* utilizzato per passaggio dei parametri per reference *)
Attributes[CoppiaGiaPresente] = HoldAll;
CoppiaGiaPresente[r_,lista_Symbol] := 
	Module[{n,m}, 
		n = 1;
		While[ n <= Length[lista],
 			m = 1;
 			(* questo ciclo while viene utilizzato perche' gli elementi della 
 				lista "lista" possono essere delle triple/quadruple/etc.; quindi viene fatta 
 				virtualmente una trasformazione da triple/quadruple/et. a coppie e effettuato
 				il controllo con r*) 
 			While[ m < Length[lista[[n]]],
   				If[ MemberQ[r, {lista[[n, 1]], lista[[n, m + 1]]}] == True,
		    		If[ Length[lista[[n]]] == 2,
     					lista = Delete[lista, n];
		     			Break[],
     					Delete[lista[[n]], m + 1]
     				]
     			];
   				m++
   			];
  			n++
  		]
	]

Attributes[CancellaCoppia] = HoldAll;

CancellaCoppia[r_Symbol, x_, y_, lts1_, lts2_, listarimozioniglobale_] := 
 Module[{n = 1, m, listarimozioni = {}, posizionex, listatemp,temp, i},
  While[n <= Length[lts2],
    If[lts2[[n, 1]] == y,
     m = 1;
     While[m <= Length[lts2[[n]]]/2,
       listarimozioni = Append[listarimozioni, lts2[[n, m*2]]];
       m++
      ]];
    n++;
   ];
  n = 1;
  listatemp = listarimozioniglobale;
  While[n <= Length[listarimozioni],
    If[(listarimozioni == {}) || (MemberQ[listarimozioniglobale, 
       listarimozioni[[n]]]),
      listarimozioni = Delete[listarimozioni, n],
      listatemp = Append[listatemp, listarimozioni[[n]]]];
    n++];
  n = 1;
  While[n <= Length[listarimozioni], 
   temp = Position[r, listarimozioni[[n]]];
   If[temp != {},
    posizionex = temp[[1, 1]];
    CancellaCoppia[r, r[[posizionex, 1]], r[[posizionex, 2]], lts1, 
    lts2, listatemp]];
   n++
  ];
  i = Position[r, {x, y}];
  r = Delete[r, i];
  ]
  
(* passa i parametri per reference eccetto il primo *)
Attributes[AndOr] = HoldRest;
AndOr[lista_, ltssimulato_, ltssimulante_, relazione_] := 
	Module[{n, listaor, listaand, m, boolvarand, boolvaror}, 
		n = 1;
		listaor = {};
		listaand = {};
		m = 1;
		boolvarand = False;
		boolvaror = True;
		n = 1;
		(* scandisce la lista delle n-uple *)
		While[n <= Length[lista],
 			m = 1;
 			listaor = {};
 			(* scandisce la n-upla e la divide in coppie che mette nella "listaor" 
 				Es: {{x2,y2,y3}, ...} -> {{x2,y2},{x2,y3},...}*)
 			While[ m < Length[lista[[n]]],
  				AppendTo[listaor, {lista[[n, 1]], lista[[n, m + 1]]}];
  				m++
  			];
 			m = 1;
 			(* scandisce la lista "listaor" e applica F ad ogni suo elemento 
 				Es: {{x2,y2},{x2,y3},...} -> {F{x2,y2},F{x2,y3},...} -> {True,False,...}*)
 			While[ m <= Length[listaor],
 				listaor[[m]] = F[listaor[[m]], ltssimulato, ltssimulante, relazione];
 				m++;
 			];
 			boolvaror = False;
  			m = 1;
  			(* fa l'or logico tra tutti i valori all'interno di listaor 
  				L'or viene fatto quando abbiamo due rami uscenti da uno stesso 
				stato y (che simula il corrispondente stato x del grafox) con label uguali*)
  			While[ m <= Length[listaor],
    			boolvaror = boolvaror || listaor[[m]];
    			m++
    		];
    		(* mette il risulto del while precedente in listaand *)
  			listaand = Append[listaand, boolvaror];
  			n++
  		];
  		n = 1;
		boolvarand = True;
		(* fa l'and logico tra tutti i valori all'interno di listaand 
			L'and viene fatto quando abbiamo due rami uscenti da uno stesso 
			stato y (che simula il corrispondente stato x del grafox) con label diverse *)
		While[n <= Length[listaand],
  			boolvarand = boolvarand && listaand[[n]];
  			n++
  		];
		Return[boolvarand]
	]

	
Attributes[F] = HoldAll;

F[coppianodi_, ltssimulato_, ltssimulante_, relazione_] := 
	Module[{x,y, listacoppie}, 
		x = coppianodi[[1]];
		y = coppianodi[[2]];
		If[MenuUguale[x, y, ltssimulato, ltssimulante] == False,
 			Return[False],
 			AppendTo[relazione, {x, y}];
 			listacoppie = ListaNuoveCoppie[x, y, ltssimulato, ltssimulante];
 			CoppiaGiaPresente[relazione, listacoppie];
 			If[listacoppie == {},
  				Return[True],
  				If [AndOr[listacoppie, ltssimulato, ltssimulante, relazione] == False,
    				CancellaCoppia[r, x, y, ltssimulato, ltssimulante,{y}]; 
    				Return[False],
    				Return[True]
    			];
     		];
 		];
	]
	
Simulazione[ltsSimulato_, ltsSimulante_] := 
	Module[{ltsx, ltsy, r},
		r = {}; 
		If[(LTSChecker[ltsSimulato] == False) || (LTSChecker[ltsSimulante] == False), Return[False]];
		ltsx = GrafoFormattato[ltsSimulato];
		ltsy = GrafoFormattato[ltsSimulante];
		If[F[{ltsx[[1,1]],ltsy[[1,1]]},ltsx,ltsy,r] == False,
			Return [{False}], 
			Return [{True,IndiciVerticiOriginali[r, ltsSimulato, ltsSimulante]}] 
		]
	]
	
SimulationEquivalence[lts1_, lts2_] := 
 	Module[{r0, r1},
		 If[(LTSChecker[lts1] == False) || (LTSChecker[lts2] == False), Return[False]];
 		r0 = {}; 
  	    r1 = {};
   		If [(r0 = Simulazione[lts1,lts2])[[1]] == True,
   			If [(r1 = Simulazione[lts2,lts1])[[1]] == True,
  				Return [{"se",r0,r1}],
  				Return [{"sim0",r0}]
   			],
   			If [(r1 = Simulazione[lts2,lts1])[[1]] == True,
   				Return [{"sim1",r1}],
  				Return [{False}]
   			]
   		]
  	]
  	  
Attributes[AndOr1] = HoldRest;

AndOr1[lista_, lts1_, lts2_, relazione_] := 
 	Module[{n, listaor, listaand, m, boolvarand, boolvaror}, 
  		n = 1;
  		listaor = {};
  		listaand = {};
  		m = 1;
  		boolvarand = False;
  		boolvaror = True;
  		n = 1;
  		(* scandisce la lista delle n-uple *)
  		While[n <= Length[lista],
    		m = 1;
    		listaor = {};
    			(* scandisce la n-upla e la divide in coppie che mette nella "listaor" 
    				Es: {{x2,y2,y3}, ...} -> {{x2,y2},{x2,y3},...}*)
    		While[ m < Length[lista[[n]]],
      			AppendTo[listaor, {lista[[n, 1]], lista[[n, m + 1]]}];
      			m++
      		];
    		m = 1;
    		(* scandisce la lista "listaor" e applica F ad ogni suo elemento 
    			Es: {{x2,y2},{x2,y3},...} -> {F{x2,y2},F{x2,y3},...} -> {True,False,...}*)
    		While[ m <= Length[listaor],
     			listaor[[m]] = FB[listaor[[m]], lts1, lts2, relazione];
     			m++;
     		];
    		boolvaror = False;
     		m = 1;
     			(* fa l'or logico tra tutti i valori all'interno di listaor 
     				L'or viene fatto quando abbiamo due rami uscenti da uno stesso 
   				stato y (che simula il corrispondente stato x del grafox) con label uguali*)
     		While[ m <= Length[listaor],
        		boolvaror = boolvaror || listaor[[m]];
        		m++
        	];
       		(* mette il risulto del while precedente in listaand *)
     		listaand = Append[listaand, boolvaror];
     		n++
     	];
    	n = 1;
  		boolvarand = True;
  		(* fa l'and logico tra tutti i valori all'interno di listaand 
  			L'and viene fatto quando abbiamo due rami uscenti da uno stesso 
  			stato y (che simula il corrispondente stato x del grafox) con label diverse *)
  		While[n <= Length[listaand],
     		boolvarand = boolvarand && listaand[[n]];
     		n++
     	];
  		Return[boolvarand]
  	]
  	
 Attributes[FB] = HoldRest;
 FB[coppianodi_, lts1_, lts2_, relazione_] := 
 	Module[{x, y, listacoppiex, listacoppiey}, 
 		x = coppianodi[[1]];
 		y = coppianodi[[2]];
 		If [(relazione != {}) && (LetteraNodo[x] != LetteraNodo[relazione[[1, 1]]]),
 			relazione = Map[Reverse, relazione]
 		];
 		If[(MenuUguale[x, y, lts1, lts2] == True) && (MenuUguale[y, x, lts2, lts1] == True),
 			If[MemberQ[relazione, {x, y}] == False, 
 				AppendTo[relazione, {x, y}]
 			];
 			listacoppiex = ListaNuoveCoppie[x, y, lts1, lts2];
 			listacoppiey = ListaNuoveCoppie[y, x, lts2, lts1];
 			CoppiaGiaPresente[relazione, listacoppiex];
 			CoppiaGiaPresente[Map[Reverse, relazione], listacoppiey];
 			If[(listacoppiex ==  {}) && (listacoppiey == {}),
 				Return[True],(*then*)
 				If[listacoppiex == {},(*else*)
 					relazione = Map[Reverse, relazione];(*then*)
 					If [AndOr1[listacoppiey, lts2, lts1, relazione] == False,
 						CancellaCoppia[relazione, y, x, lts2, lts1, {x}]; 
 						Return[False],
 						Return[True]
 					],
 					If[listacoppiey == {},(*else*)
 						If [AndOr1[listacoppiex, lts1, lts2, relazione] == False,(*then*)
 							CancellaCoppia[relazione, x, y, lts1, lts2, {y}]; 
 							Return[False],
 							Return[True]
 						],(*else*)
 						If[(AndOr1[listacoppiex, lts1, lts2, relazione] == True),
 							relazione = Map[Reverse, relazione];
 							If[(AndOr1[listacoppiey, lts2, lts1, relazione] == True),
 								Return[True],(*then*)
 								CancellaCoppia[relazione, y, x, lts2, lts1, {x}];
 								Return[False]
 							],
 							CancellaCoppia[relazione, x, y, lts1, lts2, {y}];
 							Return[False]
 						]
 					]
 				]
 			],
 			Return[False]
 		]
 	]
 
 Bisimulazione[lts1_, lts2_] := 
 	Module[{grafox, grafoy, r},
 		r = {};
 		If[(LTSChecker[lts1] == False) || (LTSChecker[lts2] == False), Return[False]];
 		grafox = GrafoFormattato[lts1];
 		grafoy = GrafoFormattato[lts2];
 		If[FB[{grafox[[1, 1]], grafoy[[1, 1]]}, grafox, grafoy, r] == False,
 			Return [{False}], 
   			Return [{"sim0",{True, IndiciVerticiOriginali[r, lts1, lts2]}}] 
   		]
  ]

MenuDeadlock2[nodosimulato_, nodosimulante_, ltssimulato_, ltssimulante_] := 
	Module[{n, m},
		n = 1;
		While[n <= Length[ltssimulato],
			If[(ltssimulato[[n, 1]] == nodosimulato) && (Length[ltssimulato[[n]]] ==   1),
				m = 1;
				While[m <= Length[ltssimulante],
					If[(ltssimulante[[m, 1]] == nodosimulante) && (Length[ltssimulante[[m]]] !=  1),
						Return[False]
					];
					m++
				];
			];
			n++
		];
		Return[True];
  ]
  
Attributes[FC] = HoldAll;
FC[coppianodi_, ltssimulato_, ltssimulante_, relazione_] := 
	Module[{n},
		If[F[{coppianodi[[1]], coppianodi[[2]]}, ltssimulato, ltssimulante, relazione] ==  False,
			Return [False],
			n = 1;
			While[n <= Length[relazione],
				If[MenuDeadlock2[relazione[[n, 1]], relazione[[n, 2]], ltssimulato, ltssimulante] == False,
					If [Count[relazione, {relazione[[n, 1]], _}] > 1,
						relazione = Delete[relazione, Position[relazione, {relazione[[n, 1]], relazione[[n, 2]]}]],
						Return[False]
					];
     			];
    			n++
    		];
   			Return [True]
   		]
  ]

CompleteSimulation[ltsSimulato_, ltsSimulante_] := 
	Module[{grafox, grafoy, r},
		If[(LTSChecker[ltsSimulato] == False) || (LTSChecker[ltsSimulante] == False), Return[False]];
		r = {}; 
  		grafox = GrafoFormattato[ltsSimulato];
  		grafoy = GrafoFormattato[ltsSimulante];
  		If[FC[{grafox[[1, 1]], grafoy[[1, 1]]}, grafox, grafoy, r] == False,
  			Return [{False}], 
  			Return [{True, IndiciVerticiOriginali[r, ltsSimulato, ltsSimulante]}]
  		]
  ]

CompleteSimulationEquivalence[lts1_, lts2_] := 
 	Module[{r0, r1},
		If[(LTSChecker[lts1] == False) || (LTSChecker[lts2] == False), Return[False]];
 		r0 = {}; 
  	    r1 = {}; 
		If [(r0 = CompleteSimulation[lts1,lts2])[[1]] == True,
   			If [(r1 = CompleteSimulation[lts2,lts1])[[1]] == True,
  				Return [{"se",r0,r1}],
  				Return [{"sim0",r0}]
   			],
   			If [(r1 = CompleteSimulation[lts2,lts1])[[1]] == True,
   				Return [{"sim1",r1}],
  				Return [{False}]
   			]
   		]
  ]

Attributes[FR] = HoldAll;
FR[coppianodi_, ltssimulato_, ltssimulante_, relazione_] := 
	Module[{n},
		If[F[{coppianodi[[1]], coppianodi[[2]]}, ltssimulato, ltssimulante, relazione] == False,
			Return [False],
			n = 1;
			While[n <= Length[relazione],
				If[MenuUguale[relazione[[n, 2]], relazione[[n, 1]], ltssimulante, ltssimulato] == False,
     				If [Count[relazione, {relazione[[n, 1]], _}] > 1,
     					relazione = Delete[relazione, Position[relazione, {relazione[[n, 1]], relazione[[n, 2]]}]],
     					Return[False]
     				];
     			];
    			n++
    		];
   			Return [True]
   		]
  ]

ReadySimulation[ltsSimulato_, ltsSimulante_] := 
	Module[{grafox, grafoy, r},
		If[(LTSChecker[ltsSimulato] == False) || (LTSChecker[ltsSimulante] == False), Return[False]];
		r = {};
  		grafox = GrafoFormattato[ltsSimulato];
  		grafoy = GrafoFormattato[ltsSimulante];
  		If[FR[{grafox[[1, 1]], grafoy[[1, 1]]}, grafox, grafoy, r] == False,
  			Return [{False}], 
  			Return [{True, IndiciVerticiOriginali[r, ltsSimulato, ltsSimulante]}] 
   		]
  ]

ReadySimulationEquivalence[lts1_, lts2_] := 
	Module[{r0, r1},
		If[(LTSChecker[lts1] == False) || (LTSChecker[lts2] == False), Return[False]];
		r0 = {}; 
  	  r1 = {}; 
		If [(r0 = ReadySimulation[lts1,lts2])[[1]] == True,
   			If [(r1 = ReadySimulation[lts2,lts1])[[1]] == True,
  				Return [{"se",r0,r1}],
  				Return [{"sim0",r0}]
   			],
   			If [(r1 = ReadySimulation[lts2,lts1])[[1]] == True,
   				Return [{"sim1",r1}],
  				Return [{False}]
   			]
   		]
  ]
    	
Grafica[] := 
	Module[{filegrafi,ref,filepath,returnValue,grafo1,grafo2, nodog1, nodog2, numradio, risp},
		numradio = 0;
		ref = GUIRun[
  			Widget["Frame", {
  				"title" -> "Behaviour equivalences",
    			Widget["Panel", {
      				{{Widget["Label", {"text" -> "Inserire il path del file"}],
        				{Widget["TextField", {"columns" -> 35}, Name -> "txtPath"],
         				Widget["Button", {"text" -> "Sfoglia...", 
           					BindEvent["action", Script[
             						Widget["FileDialog", Name -> "openFileDialog"];
             						SetPropertyValue[{"openFileDialog", "multiSelectionEnabled"}, False];
             						SetPropertyValue[{"openFileDialog", "fileSelectionMode"},PropertyValue[{"openFileDialog", "FILES_ONLY"}]];         			
             						returnValue = InvokeMethod[{"openFileDialog", "showOpenDialog"}, Null];             			
             						If[returnValue === PropertyValue[{"openFileDialog", "APPROVE_OPTION"}], 
              							filepath = PropertyValue[{PropertyValue[{"openFileDialog", "selectedFile"}], "path"}];
              							SetPropertyValue[{"txtPath", "text"}, filepath];
              							(* letture da file*)
              							filegrafi = OpenRead [filepath];
              							grafo1= Read [filegrafi,Expression];
              							grafo2= Read [filegrafi,Expression];
              							(*e visualizzazione dei due grafi*)
              							ref@SetPropertyValue[{"widgetPlotGrafi", "data"} , 
              													ExportString[Rasterize[LayeredGraphPlot[grafo1, Left, VertexLabeling -> True], 
              													ImageSize -> {400, 250}], "GIF"]
              							];
              							ref@SetPropertyValue[{"widgetPlotGrafi2", "data"} , 
              													ExportString[Rasterize[LayeredGraphPlot[grafo2, Left, VertexLabeling -> True], 
              													ImageSize -> {400, 250}], "GIF"]
              							];
              							ref@SetPropertyValue [{"btcompute", "enabled"}, True];
              							Close[filegrafi], Null
              						];
             				]]}
             			]}
        }},
      {{Widget["RadioButton", {"text" -> "Simulation", "selected" -> False, BindEvent ["action",Script[numradio = 2]]},Name -> "sim"],
        Widget["RadioButton", {"text" -> "Complete Simulation", "selected" -> False, BindEvent ["action",Script[numradio = 3]]}, Name -> "csim"],
        Widget["RadioButton", {"text" -> "Ready Simulation", "selected" -> False, BindEvent ["action",Script[numradio = 4]]}, Name -> "rsim"],
        Widget["RadioButton", {"text" -> "Bisimulation", "selected" -> False, BindEvent ["action",Script[numradio = 5]]}, Name -> "bsim"],
        Widget["ButtonGroup", {WidgetReference["te"], WidgetReference["cte"], WidgetReference["sim"],
        						WidgetReference["csim"], WidgetReference["rsim"], WidgetReference["bsim"]}],
        Widget["Button", {"text" -> "Compute", "enabled" -> False, BindEvent ["action", Script[
        	ref@SetPropertyValue [{"lblrisp", "text"},""];
        	ref@SetPropertyValue [{"lblrel1", "text"},""];
        	ref@SetPropertyValue [{"lblrel2", "text"},""];
        	Switch[numradio,
				2, risp = {"\[TildeEqual]", "\[PrecedesTilde]", SimulationEquivalence [grafo1, grafo2]},
				3, risp = {StringJoin["\[TildeEqual]", "c"], StringJoin["\[PrecedesTilde]", "c"], CompleteSimulationEquivalence [grafo1, grafo2]},
				4, risp = {StringJoin["\[TildeEqual]", "rs"], StringJoin["\[PrecedesTilde]", "rs"], ReadySimulationEquivalence [grafo1, grafo2]},
				5, risp = {"\[Tilde]","\[Tilde]",Bisimulazione [grafo1, grafo2]},
				_, Print["Error"]
				];
			
			nodog1 = Sort[VertexList[grafo1], IndiceNodo[#1] < IndiceNodo[#2] &] [[1]];
			nodog2 = Sort[VertexList[grafo2], IndiceNodo[#1] < IndiceNodo[#2] &] [[1]];
			(*Print[risp, " grafo1:", grafo1, " grafo2: ", grafo2, " nodog1: ", nodog1, " nodog2 ", nodog2];*)	
			Switch[risp[[3,1]],
				"se", ref@SetPropertyValue [{"lblrisp", "text"}, StringJoin[nodog1," ",ToString[risp[[1]]]," ",nodog2]];
						ref@SetPropertyValue [{"lblrel1", "text"}, StringJoin["R1= ",ToString[risp[[3,2,2]]]]];
						ref@SetPropertyValue [{"lblrel2", "text"}, StringJoin["R2= ",ToString[risp[[3,3,2]]]]],
				"sim0", ref@SetPropertyValue [{"lblrisp", "text"}, StringJoin[nodog1," ",risp[[2]]," ",nodog2]];
						ref@SetPropertyValue [{"lblrel1", "text"}, StringJoin["R= ",ToString[risp[[3,2,2]]]]],
				"sim1", ref@SetPropertyValue [{"lblrisp", "text"}, StringJoin[nodog2," ",risp[[2]]," ",nodog1]];
						ref@SetPropertyValue [{"lblrel1", "text"}, StringJoin["R= ",ToString[risp[[3,2,2]]]]],
				_, ref@SetPropertyValue [{"lblrisp", "text"}, "False"]
			]
			]] }, Name -> "btcompute"]
        }, WidgetFill[],
       {Widget["ImageLabel", Name -> "widgetPlotGrafi"],
        Widget["ImageLabel", Name -> "widgetPlotGrafi2"]}
       },
      {Widget["Label", {"text" -> "Risultato: "}, Name -> "lblrisultato"],
      Widget["Label", {"text" -> ""}, Name -> "lblrisp"]},
      {Widget["Label", {"text" -> ""}, Name -> "lblrel1"]},
      {Widget["Label", {"text" -> ""}, Name -> "lblrel2"]}
      }
     ]}, Name -> "frameGlobale"],
      IncludedScriptContexts -> {$Context}
     ];
     ref@InvokeMethod[{"frameGlobale", "setSize"} , 640, 640]
	]
End[]
EndPackage[]









