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
verify(InputFileName) :-  see(InputFileName),
                          read(Prems), read(Goal), read(Proof),
                          seen,
                          valid_proof(Prems, Goal, Proof).
                          
% Main function                          
valid_proof(Prems, Goal, Proof) :-
      % Verifiera att målet finns i bevisets sista rad.
      length(Proof, Last),
      nth1(Last, Proof, Goal1),
      Goal = Goal1.