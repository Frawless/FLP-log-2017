/** FLP 2015
Toto je ukazkovy soubor zpracovani vstupu v prologu.
Tento soubor muzete v projektu libovolne pouzit.

autor: Martin Hyrs, ihyrs@fit.vutbr.cz


preklad: swipl -q -g start -o flp17-log -c flp17-log.pl
spusteni: ./flp17-log < input
*/

finalState([1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,x]).

% Main
start :-
		write("Patnáctka!\n"),
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		parseInput(S),
		write(S), nl,
		createNumbersList(S,F),
		write(F),nl,
		write("Čísla vytvořena, pokračujeme dále...\n"),
		%goal(F,X),
		%write(X),
		%nl,
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
parseInput(S) :- checkInput(S), write(S), nl.
parseInput(_) :- write("Neplatný vstup!\n").
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
goal(I,X) :- write(I), nl, createGoal(I,I1),!,predsort(nthcompare,I1,X).

createGoal([H],[X]) :- write("bla"),write(H),nl,insert_sort(H,X).
createGoal([H|T],X) :- write("blo"),write(H),nl,insert_sort(H,X1), createGoal(T,X2), append([X1],X2,X).
createGoal([H|T],X) :- write("blu"),write(H),nl,insert_sort(H,X1), createGoal(T,X2), append(X2,[X1],X).


%nthcompare(<,[_|_],[*|_]).
%nthcompare(<,[*|_],[_|_]).
nthcompare(<,[H1|_],[H2|_]) :- H1 < H2.
nthcompare(>,_,_).


createNumbersList([],[]).
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
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(*,[Y],[Y|*]).
insert(Y,[*],[*|Y]).
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).

/*#########################################################################################################################*/

/* nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].

/* vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")

/* rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
split_line2([],[[]]) :- !.
%pridat novy seznam:
split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


/* pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines2([],[]).
split_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).

/* nacte N radku vstupu, zpracuje, vypise */
start2(N) :-
		prompt(_, ''),
		read_lines2(LL, N),
		split_lines2(LL,S),
		write_lines2(S).


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

