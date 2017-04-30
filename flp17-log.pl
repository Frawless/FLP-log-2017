/** FLP 2015
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz


preklad: swipl -q -g start -o flp17-log -c flp17-log.pl
spusteni: ./flp17-log < input
*/

% Main
start :-
		write("Patnáctka!\n"),
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		parseInput(S),
		createNumbersList(S,F),
		write(F),nl,
		write("Vstup vytvořen, pokračujeme dále...\n"),
		goal(F,X),
		write("Požadovaný výsledek: "),write(X),nl,
		write("Řeším puzzle...\n"),
		!,solvePuzzle(F),
		halt.


%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%parseAtomss if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[X|T]) :- split_lines(Ls,T), split_line(L,H), myConcat(H,X).

/*#########################################################################################################################*/
parseInput(S) :- checkInput(S); write("Neplatný vstup!\n").

% Spravnost vstupu
checkInput([H|T]) :- length(H,L), checkInputField(T,L).

checkInputField([],_) :- !.
checkInputField([A|R],L) :- checkSize(A,L), checkInputField(R,L).

% Velikost jednotlivych seznamu
checkSize(A,L) :- length(A,X), X == L. 

listConcat([],"").
listConcat([H|T],X) :- listConcat(T,H1), atomic_concat(H,H1,X).

% Vytvoreni jednoho seznamu pro kazdy radek
myConcat([],[]). % V pripade, ze chceme [1,2,3,4] odmazat jedny zavorky u X1
myConcat([H|T],X) :- listConcat(H,X1), myConcat(T,X2),append([X1],X2,X).

% Vytvoreni cile
goal([H|T],X) :- createGoal([H|T],I1),!,length(H,L), splitList(I1,L,X).

% Vytvoreni cilove posloupnosti
createGoal([H|T],F):- createGoal(T,X1),!,append(H,X1,X),insert_sort(X,F).
createGoal([H],H).

% Rozdeleni pozadovaneho vysledku
splitList([],_,[]).
splitList(L, S, X) :-
    append(A, B, L),
    length(A, S),
    length(B, _),
	splitList(B,S,A1),!,
	myAppend([A],A1,X).

myAppend(A,[],A).
myAppend(A,B,C) :- append(A,B,C).


% predsort, funkce pro predsort na razeni seznamu
nthcompare(<,[H1|_],[H2|_]) :- H1 < H2.
nthcompare(>,_,_).


% Vytvoreni cisel z atomu
createNumbersList([],[]).					% Momentalni vystup je 1D pole, pro 2D pridat [] k X1 v apppend
createNumbersList([H|T],X) :- parseAtoms(H,X1),!,createNumbersList(T,X2),append([X1],X2,X).
createNumbersList([H],[X]) :- parseAtoms(H,X).

parseAtoms([],[]).
parseAtoms([H|T],X) :- atom_number(H,X1),!, parseAtoms(T,X2), append([X1],X2,X).
parseAtoms([*|T],X) :- !, parseAtoms(T,X2), append([*],X2,X).
parseAtoms([*],[*]).
parseAtoms([H],[X]) :- atom_number(H,X).


% Prevzaty insert sort
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),!,i_sort(T,NAcc,Sorted).
   
insert(*,Y,Z) :- !,append(Y,[*],Z).
insert(Y,*,Z) :- !,append(Y,[*],Z).
insert(Y,[*],[Y,*]).
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).

/*#########################################################################################################################*/
solvePuzzle(L) :- write(L),nl,create1D(L,X,S), write(X).


%create1D([],[],_).
create1D([H|T],X,S) :- write(H),nl, write(T),nl,create1D(T,X1), write("test"),nl,append(H,X1,X), length(H,S).
create1D([H],H).


%Moves
move(H,S,Z) :- 
	write("right"),nl, nth1(I,H,*), I1 is I+1, write(I), nl, I mod S =\= 0, write(I1), nl, delete(H,*,N), nth1(I1,Z,*,N).

move(H,S,Z) :- 
	write("left"),nl, nth1(I,H,*), I1 is I-1, I1 > 0, write(I), nl, I mod S =\= 1, write(I1), nl, delete(H,*,N), nth1(I1,Z,*,N). 

move(H,S,Z) :- 
	write("up"),nl, nth1(I,H,*), I1 is I+S, nth1(I1,H,M), delete(H,M,N1),delete(N1,*,N), nth1(I,Z1,M,N),nth1(I1,Z,*,Z1). 

move(H,S,Z) :- 
	write("right"),nl, nth1(I,H,*), I1 is I-S, I1 > 0, nth1(I1,H,M), delete(H,M,N1),delete(N1,*,N), nth1(I,Z1,M,N),nth1(I1,Z,*,Z1). 


/*#########################################################################################################################*/


/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).



/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
cislo(N,X) :- cislo(N,0,X).
cislo([],F,F).
cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
cislo([],F,F,_).
cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).

% existuje knihovni predikat number_chars(?Number, ?CharList)
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).

