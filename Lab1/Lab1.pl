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
match([_,X,premise]):-
  member(X,Prems).

match([_,_,assumption]):-
  

match([_,_,copy(X)]):-
  asd

match([_,_,andel1(X)]):-

match([_,_,andel2(X)]):-

match([_,_,orint1(X)]):-

match([_,_,orint2(X)]):-

match([_,_,orel(X,Y,U,V,W)]):-

match([_,_,impint(X,Y)]):-

match([_,_,impel(X,Y)]):-





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
  write(H),
  nl,
  [Rownr|_] = H,
  [_, Text|_] = H,
  [_,_,Funk|_] = H,
  Funk(Text),

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

match([_,_,f1]):-


match()

match()










valid_ending(Prems, Goal, Proof) :-
      % Verifiera att målet finns i bevisets sista rad.
      length(Proof, Last),
      nth1(Last, Proof, Goal1),
      Goal = Goal1.
