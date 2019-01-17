README PROLOG
Composizione del gruppo: -829584 Davide Pozzoli
			 -830209 Massimo Junior Toselli

Per la gestione delle etichette sfruttiamo il database interno del sistema prolog affermando le label col predicato assert/1.
Finita l'esecuzione del predicato executionloop/2 (all'interno del lmc_run/3) usufruiamo del predicato retractall/1 per svuotare il database dalle label
affermate, così si evitano conflitti con l'esecuzione di altri test.