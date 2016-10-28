% William Skagerström, wska@kth.se
% Thony Price, asdasdasd

/*
Regelappliceringar:

premise premise               impel(x,y) →e x,y
assumption assumption         negint(x,y) ¬i x-y
copy(x) copy x                negel(x,y) ¬e x,y
andint(x,y) ∧i x, y           contel(x) ⊥e x
andel1(x) ∧e1 x               negnegint(x) ¬¬i x
andel2(x) ∧e2 x               negnegel(x) ¬¬e x
orint1(x) ∨i1 x               mt(x,y) mt x,y
orint2(x) ∨i2 x               pbc(x,y) pbc x-y
orel(x,y,u,v,w) ∨e x,y–u,v–w  lem lem
impint(x,y) →i x–y

*/

% Importerar en fil och skickar prmisser, mål och bevis.
/*
match([_,X,premise]):-
  member(X,Prems).

match([_,_,assumption]):-
  nl.


match([_,_,copy(X)]):-
  asd.

match([_,_,andel1(X)]):-
  nl.

match([_,_,andel2(X)]):-
  nl.

match([_,_,orint1(X)]):-
  nl.

match([_,_,orint2(X)]):-
  nl.

match([_,_,orel(X,Y,U,V,W)]):-
  nl.

match([_,_,impint(X,Y)]):-
  nl.
*/
match([_,_,Rule], CopyOfRows):-
                    sub_string(Rule,0,5,_,"impel"),
                    sub_string(Rule,6,_,1,X),
                    split_string(X,",","",L),
                    nth0(0,L,A),
                    atom_number(A,A1),
                    nth0(1,L,B),
                    atom_number(B,B1),
                    findRow(CopyOfRows, [A1,Q,_]),
                    findRow(CopyOfRows, [B1,Imp,_]),
                    with_output_to(atom(I),write(Imp)),
                    sub_string(I,0,3,_,"imp"),
                    sub_string(I,4,_,1,Y),
                    split_string(Y,",","",L1),
                    atom_string(Q,Q1),
                    nth0(0,L1,Q1),!.



findRow([],[]).
findRow([H|T],[A1,Q,_]):-
  H = [A1,Q,_];
  findRow(T,[A1,Q,_]).





verify(InputFileName) :-  see(InputFileName),
                          read(Prems), read(Goal), read(Proof),
                          seen,
                          asserta(Prems),
                          valid_proof(Prems, Goal, Proof).

% Main function
valid_proof(Prems,Goal,Proof):-
      valid_ending(Prems,Goal,Proof),
      gtRows(Proof,ProofRows).

gtRows([]).
gtRows([H|T]):-
  [H1|_] = H,
  not(number(H1)),!,
  gtRows(H),
  gtRows(T);
  match(H),
  %write(H),
  nl,
  %[Rownr|_] = H,
  %[_, Text|_] = H,
  %[_,_,Funk|_] = H,
  %Funk(Text),
  gtRows(T).

%rows2([]).
%rows2([H|T]):-
%  rows2(H);
%  rows2(T).

%gtRows([H|T]):-
%  gtRows(H);
%  write(H),
%  nl,
%  gtRows(T).














valid_ending(Prems, Goal, Proof) :-
      % Verifiera att målet finns i bevisets sista rad.
      length(Proof, Last),
      nth1(Last, Proof, Goal1),
      Goal = Goal1.
