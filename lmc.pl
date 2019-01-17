%%%% -*- Mode: Prolog -*-

%%%% Nome file: lmc.pl
%%%% Progetto: Little Man Computer
%%%% Progetto realizzato da: Pozzoli Davide e Toselli Massimo


%%% Architettura del LMC

%%% Metodi di supporto
increment_pc(Pc, Pc1) :-
    Pc < 99, !,
    Pc1 is Pc+1.
increment_pc(Pc, Pc1) :-
    Pc = 99, !,
    Pc1 is 0.

find_instruction(0, [X | _], X).
find_instruction(Index, [X | Xs], Out):-
    Index1 is Index-1,
    find_instruction(Index1, Xs, Out).

get_id(X, ID, Xx):-
   X < 100,
   ID = 0.
get_id(X, ID, Xx):-
    ID is X//100,
    Xx is X mod 100.


%%% Add:
%%% Esegui l’istruzione di addizione tra l’accumulatore e il
%%% valore contenuto nella cella indicata da xx
add(Acc1, Acc2, Pc, [M | Ms], In, Out, State) :-
    Acc1 >= 1000,
    increment_pc(Pc, Pc1),
    State = state(Acc2, Pc1, [M | Ms], In, Out, flag).
add(Acc1, Acc2, Pc, [M | Ms], In, Out, State) :-
    Acc1 < 1000,
    increment_pc(Pc, Pc1),
    State = state(Acc2, Pc1, [M | Ms], In, Out, noflag).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, _, NewState) :-
   ID = 1, !,
   find_instruction(Xx, [M | Ms], N),
   Acc1 is Acc+N,
   Acc2 is Acc1 mod 1000,
   add(Acc1, Acc2, Pc, [M | Ms], In, Out, State),
   NewState = State.


%%% Sub:
%%% Esegui l’istruzione di sottrazione tra l’accumulatore e il
%%% valore contenuto nella cella indicata da xx
sub(Acc1, Acc2, Pc, [M | Ms], In, Out, State) :-
    Acc1 < 0,
    increment_pc(Pc, Pc1),
    State = state(Acc2, Pc1, [M | Ms], In, Out, flag).
sub(Acc1, Acc2, Pc, [M | Ms], In, Out, State) :-
    Acc1 >= 0,
    increment_pc(Pc, Pc1),
    State = state(Acc2, Pc1, [M | Ms], In, Out, noflag).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, _, NewState) :-
   ID = 2, !,
   find_instruction(Xx, [M | Ms], N),
   Acc1 is Acc-N,
   Acc2 is Acc1 mod 1000,
   sub(Acc1, Acc2, Pc, [M | Ms], In, Out, State),
   NewState = State.

%%% Store:
%%% Esegue una istruzione di store del valore
%%% dell’accumulatore nella cella indicata da xx
modifica(Acc, 0, [M | Ms], [Acc | Ms]).
modifica(Acc, Xx, [M | Ms], [M | Xs]) :-
   Xx1 is Xx - 1,
   modifica(Acc, Xx1, Ms, Xs).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState)  :-
    ID = 3, !,
    modifica(Acc, Xx, [M | Ms], [X | Xs]),
    increment_pc(Pc, Pc1),
    NewState = state(Acc, Pc1, [X | Xs], In, Out, Flag).

%%% Load:
%%% Esegue una istruzione di load dal valore contenuto nella
%%% cella indicata da xx nell’accumulatore
excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
    ID = 5, !,
    increment_pc(Pc, Pc1),
    find_instruction(Xx, [M | Ms], N),
    NewState = state(N, Pc1, [M | Ms], In, Out, Flag).

%%% Branch:
%%% Esegue una istruzione di branch non condizionale al
%%% valore indicato da xx
excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
    ID = 6, !,
    NewState = state(Acc, Xx, [M | Ms], In, Out, Flag).

%%% Branch If Zero:
%%% Esegue una istruzione di branch condizionale (se
%%% l’accumulatore è zero e non vi è il flag acceso) al valore
%%% indicato da xx.
excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
     ID = 7,
     Acc = 0,
     Flag = noflag, !,
     NewState = state(Acc, Xx, [M | Ms], In, Out, Flag).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
     ID = 7, !,
     increment_pc(Pc, Pc1),
     NewState = state(Acc, Pc1, [M | Ms], In, Out, Flag).

%%% Branch If Positive:
%%% Esegue una istruzione di branch condizionale (se
%%% non vi è il flag acceso) al valore indicato da xx.
excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
     ID = 8,
     Flag = noflag, !,
     NewState = state(Acc, Xx, [M | Ms], In, Out, Flag).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
     ID = 8,
     Flag = flag,
     increment_pc(Pc, Pc1),
     NewState = state(Acc, Pc1, [M | Ms], In, Out, Flag).

%%% Input:
%%% Esegue una istruzione di input
excecute(Acc, Pc, ID, Xx, [M | Ms], [I | Is], Out, Flag, NewState) :-
    ID = 9,
    Xx = 01, !,
    increment_pc(Pc, Pc1),
    NewState = state(I, Pc1, [M | Ms], Is, Out, Flag).

%%% Output:
%%% Esegue una istruzione di output
excecute(Acc, Pc, ID, Xx, [M | Ms], In, [], Flag, NewState) :-
     ID = 9,
     Xx = 02, !,
     increment_pc(Pc, Pc1),
     NewState = state(Acc, Pc1, [M | Ms], In, [Acc], Flag).

excecute(Acc, Pc, ID, Xx, [M | Ms], In, [O | Os], Flag, NewState) :-
     ID = 9,
     Xx = 02,
     append([O | Os], [Acc], Out),
     increment_pc(Pc, Pc1),
     NewState = state(Acc, Pc1, [M | Ms], In, Out, Flag).

%%% Halt:
%%% Esegue una istruzione di halt
excecute(Acc, Pc, ID, Xx, [M | Ms], In, Out, Flag, NewState) :-
   ID = 0, !,
   one_instruction(halted_state(Acc, Pc, [M | Ms], In, Out, Flag), NewState).

%%% Valutazione istruzioni non valide
excecute(Acc, Pc, ID, Xx, [M | Ms], In, [O | Os], Flag, NewState) :-
    fail.

%%% One_Instruction(State, NewState)
%%% ove State e NewState sono stati del LMC rappresentati come descritto
%%% sopra ed il predicato è vero quando l’esecuzione di una singola
%%% istruzione a partire da State porta allo stato NewState.
one_instruction(halted_state(Acc, Pc, Mem, In, Out, Flag), NewState):-
fail.

one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State1):-
    find_instruction(Pc, Mem, Instruction),
    get_id(Instruction, ID, Xx),
    excecute(Acc, Pc, ID, Xx, Mem, In, Out, Flag, State1).

%%% Execution_loop(State, out)
%%% ove State rappresenta  lo  stato  iniziale  del  LMC  e Out la  coda
%%% di output nel momento in cui viene raggiunto uno stato di stop
%%%(e quindi eseguita una istruzione di halt). Il predicato deve fallire
%%% nel caso l’esecuzione termini senza eseguire una istruzione di halt
%%% (ad  esempio  se  si incontra una istruzione non valida).
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), X) :-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                    state(Acc1, Pc1, Mem1, In1, Out1, Flag1)),
    execution_loop(state(Acc1, Pc1, Mem1, In1, Out1, Flag1), X).
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Out):-
    find_instruction(Pc, Mem, Instruction),
    get_id(Instruction, ID, Xx),
    ID= 0, !.

%%% ASSEMBLER

%%% Gestore etichette
assert_label([], N) :- !.
assert_label([I | Ins], N) :-
    dlte_comment(I, I1),
    delete_space([I1], I2),
    I2 \= [""], !,
    check_label(I2, N),
    N1 is N + 1,
    assert_label(Ins, N1).

assert_label([I | Ins], N) :-
    dlte_comment(I, I1),
    delete_space([I1], I2),
    I2 = [""], !,
    assert_label(Ins, N).

check_label([X], N).
check_label([X, Y], N) :-
    string_lower(Y, Y1),
    Y1 = "inp", !,
    atom_string(N, N1),
    string_lower(X, X1),
    assert(label(X1, N1)).
check_label([X, Y], N) :-
    string_lower(Y, Y1),
    Y1 = "out", !,
    atom_string(N, N1),
    string_lower(X, X1),
    assert(label(X1, N1)).
check_label([X, Y], N) :-
    string_lower(Y, Y1),
    Y1 = "hlt", !,
    atom_string(N, N1),
    string_lower(X, X1),
    assert(label(X1, N1)).
check_label([X, Y], N) :-
    string_lower(Y, Y1),
    Y1 = "dat", !,
    atom_string(N, N1),
    string_lower(X, X1),
    assert(label(X1, N1)).
check_label([X, Y], N).
check_label([X, Y, Z], N) :-
    string_lower(X, X1),
    atom_string(N, N1),
    assert(label(X1, N1)).

get_ins([X], [X]).
get_ins([X, Y], [X, Y2]) :-
    string_lower(Y, Y1),
    label(Y1, N),
    Y2 = N.
get_ins([X, Y], [Y]) :-
    string_lower(X, X1),
    label(X1, N).
get_ins([X, Y], [X, Y]).
get_ins([X, Y, Z], [X1, Y1]) :-
    get_ins([Y, Z], [X1, Y1]).

%%% lmc_load(Filename. Mem)
%%% si preoccupa di leggere un file che contiene un codice assembler e
%%% che produce il contenuto “iniziale” della memoria sistema (una lista
%%% di 100 numeri tra 0 e 999).
lmc_load(Filename, Mem):-
    %Leggo il file
    open(Filename, read, File),
    read_string(File,L, I),
    close(File),

    %Mi salvo le singole istruzioni in una lista
    delete_enter(I, Instructions),
    assert_label(Instructions, 0),
    create_mem(Instructions, X),
    fill_memory(X, Mem).

%%% riempe la memoria di 0
fill_memory(Mem, X) :-
    length(Mem, N),
    N1 is 100-N,
    fill_mem(N1, Mem1),
    append(Mem, Mem1, X).
fill_mem(0, []) :- !.
fill_mem(N, [0 | Ms]) :-
    N1 is N-1,
    fill_mem(N1, Ms).

create_mem([],[]).
%Caso stringa vuota(per via del commento)
create_mem([X | Xs], Mem):-
    dlte_comment(X, X1),
    delete_space([X1], ID),
    ID = [""], !,
    create_mem(Xs, Mem).
create_mem([X | Xs], [M | Ms]):-
    dlte_comment(X, X1),
    delete_space([X1], ID),
    get_ins(ID, Ins),
    identify(Ins, Z),
    M=Z,
    create_mem(Xs, Ms).

%%% Identify
%%% Riconosce le stringhe passate in input
%%% trasformandole in numeri per essere inseriti
%%% in memoria

%ID: ADD
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="add", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (100 + Y1).

%ID: SUB
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="sub", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (200 + Y1).

%ID: STA
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="sta", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (300 + Y1).

%ID: LDA
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="lda", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (500 + Y1).

%ID: BRA
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="bra", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (600 + Y1).

%ID: BRZ
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="brz", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (700 + Y1).

%ID: BRP
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="brp", !,
    atom_number(Y, Y1),
    Y1<100,
    Z is (800 + Y1).

%ID: HLT
identify([X], Z):-
    string_lower(X, X1),
    X1 ="hlt", !,
    Z = 000.

%ID: INP
identify([X], Z):-
    string_lower(X, X1),
    X1 ="inp", !,
    Z = 901.

%ID: OUT
identify([X], Z):-
    string_lower(X, X1),
    X1="out", !,
    Z = 902.

%ID: DATxxx
identify([X, Y], Z):-
    string_lower(X, X1),
    X1="dat", !,
    atom_number(Y, Y1),
    Y1<1000,
    Z=Y1.

%ID: DAT
identify([X], Z):-
    string_lower(X, X1),
    X1="dat", !,
    Z=0.

dlte_comment(X, Y):-
    sub_string(X, Before, _, After, "//"), !,
    sub_string(X, 0, Before, _, Y).
dlte_comment(X, X).


delete_enter(X, Y):-
    split_string(X, "\n","", Y).

%Ora tolgo gli spazi dalle singole istruzioni
delete_space(X, Y):-
    dlte_space(X, Z),
    append(Z, Y).

%Chiamata ricorsiva di delete_space, usa due liste
dlte_space([], []).
dlte_space([X | Xs], [Y | Ys]):-
    split_string(X, " ", " ", Y),
    dlte_space(Xs, Ys).

%%% lmc_run(Filename, Input, Output)
%%% si preoccupa di leggere un file che contiene un codice assembler, lo
%%% carica (con lmc_load/2), imposta la coda di input al valore
%%% fornito e produce un output che è il risultato dell’invocazione di
%%% execution_loop/2
lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    execution_loop(state(0, 0, Mem, Input, [], Flag), Output),
    %writeln(Output),
    retractall(label(X, Y)).



%%%% end of file


















