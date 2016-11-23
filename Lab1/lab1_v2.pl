% ~~~ * ~~~ * ~~~ * Info  ~~~ * ~~~ * ~~~ * %

/*
Project:            Lab1 - Logic in computer science
Authors:            Thony Price
                    Wlliam Skagerström
Last revision:      2016-11-19
*/


% ~~~ * ~~~ * ~~~ * Main predicate ~~~ * ~~~ * ~~~ * %


% First predicate of program. Reads a file and saves data to
% variables Prems, Goal and Proof.
verify(InputFileName) :-
                          see(InputFileName),
                          read(Prems), read(Goal), read(Proof),
                          seen,
                          valid_ending(_, Goal, Proof),
                          valid_proof(Prems, Proof, Proof, []), !.
                          /*  valid_proof must send Prems and two
                              copies of proof. One to extract the
                              row we're checking and one to try and
                              bind rows we're looking for. The idea
                              is to let valid_proof bind all rows in
                              proof and each time a row's true "move"
                              it to the empty list. When all items are
                              moved to the other list - proof is valid. */


% ~~~ * ~~~ * ~~~ * Special cases ~~~ * ~~~ * ~~~ * %


% Verifies that last row in proof's equal to goal.
valid_ending(_, Goal, Proof) :-
                          length(Proof, LastIdx),
                          nth1(LastIdx, Proof, LastRow),
                          LastRow = [_,Value,_],
                          Value = Goal.

% Find row and try to bind a row with given number
findRow(_, [], _) :- fail.
findRow(Nr, [[Nr, Value, _] | _], Value).
findRow(Nr, [_ | Tail], Row) :- findRow(Nr, Tail, Row).

% Find first row in a box
find_first_row([H | _], H).

% Find last row in a box
find_last_row([Last | []], Last).
find_last_row([_ | Tail], H) :-
                          find_last_row(Tail, H).

% Get box in which a row exists
find_box(_, [], _) :- fail.
find_box([Nr, Value, _], [[[Nr, Value, _] | BoxTail] | _], [[Nr, Value, _] | BoxTail]).
find_box([Nr, Value, _], [ _ | Tail], Box) :-
                          find_box([Nr, Value, _], Tail, Box).

% Iterate through premises to find a premise
valid_premise([], _) :- fail.
valid_premise(Value, [Value | _]).
valid_premise(Value, [_ | Tail]) :-
                          valid_premise(Value, Tail).


% ~~~ * ~~~ * ~~~ * The "proof predicates" ~~~ * ~~~ * ~~~ * %


% Case: All rows is valid
% If this base case i reached, all rows in Proof is valid.
valid_proof(_,_,[],_) :- !.

% Case: Boxhandeling
% Boxes always starts with an assumption so when found we have two options of binding. First get Boxtail
% and iterate through the rows in box. Second moving box into Done rows and continue verify rows after box
valid_proof(Prems, ProofCopy,[[[Row, Value, assumption] | Boxtail ] | RestRows], Done) :-
                          valid_proof(Prems, ProofCopy, Boxtail, [[Row, Value, assumption] | Done]),
                          valid_proof(Prems, ProofCopy, RestRows, [[[Row, Value, assumption] | Boxtail ] | Done]).

% Case: Premise
% Verifies that a premise in proof exists in the given premises (Prems).
valid_proof(Prems, ProofCopy, [[Row, Value, premise] | RestRows], Done) :-
                          valid_premise(Value, Prems), !,
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, Value, premise] | Done]).

% Case: Implication introduction
valid_proof(Prems, ProofCopy, [[Row, imp(A,B), impint(X,Y)] | RestRows], Done) :-
                          find_box([X, A, _], Done, Box),
                          find_first_row(Box, [X, A, _]),
                          find_last_row(Box, [Y, B, _]),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, imp(A,B), impint(X,Y)] | Done]).

% Case: Implication elimination
valid_proof(Prems, ProofCopy, [[Row, Value, impel(R1,R2)] | RestRows], Done) :-
                          % Note we're looking for the row in already proccesed rows
                          findRow(R1, Done, A),
                          findRow(R2, Done, imp(A, Value)),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, Value, impel(R1,R2)] | Done]).



% Case: Copy
% Make sure that in done-rows we've got the row number which we're copying from
% and it has the same value as the row we're copying to
valid_proof(Prems, ProofCopy, [[Row, X, copy(Some_row)] | RestRows], Done) :-
                          findRow(Some_row,[Some_row, X, _], Done),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, X, copy(Some_row)] | Done]).

% Case: And introduction
% Explonation
valid_proof(Prems, ProofCopy, [[Row, and(A,B), andint(X,Y)] | RestRows], Done) :-
                          findRow(X,Done,A),
                          findRow(Y,Done,B),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, and(A,B), andint(X,Y)] | Done]).


% Case: And deletion 1
% Explonation
valid_proof(Prems, ProofCopy, [[Row, A, andel1(X)] | RestRows], Done) :-
                          findRow(X,Done,and(A,_)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, A, andel1(X)] | Done]).




% Case: And deletion 2
% Explonation
valid_proof(Prems, ProofCopy, [[Row, B, andel2(X)] | RestRows], Done) :-
                          findRow(X,Done,and(_,B)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, B, andel2(X)] | Done]).

% Case: Or introduction 1
% Explonation
valid_proof(Prems, ProofCopy, [[Row, or(A,B), orint1(X)] | RestRows], Done) :-
                          findRow(X,Done,A),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A,B), orint1(X)] | Done]).

% Case: Or introduction 2
% Explonation
valid_proof(Prems, ProofCopy, [[Row, or(A,B), orint2(X)] | RestRows], Done) :-
                          findRow(X,Done,B),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A,B), orint2(X)] | Done]).

% Case: Or elimination
% Explonation

% Case: Negation introduction
% Explonation
valid_proof(Prems, ProofCopy, [[Row, neg(A), negint(X,Y)] | RestRows], Done) :-
                          find_box([X, A, _], Done, Box),
                          find_first_row(Box, [X, A, _]),
                          find_last_row(Box, [Y, cont, _]),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, neg(A), andint(X,Y)] | Done]).

% Case: Negation elimination
% Explonation
valid_proof(Prems, ProofCopy, [[Row, cont, negel(X,Y)] | RestRows], Done) :-
                          findRow(X, Done, A),
                          findRow(Y, Done, neg(A)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, cont, negel(X,Y)] | Done]).

% Case: Contradiction elimination
% Explonation
valid_proof(Prems, ProofCopy, [[Row, Value, contel(X)] | RestRows], Done) :-
                          findRow(X, Done, cont),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, Value, contel(X)] | Done]).

% Case: Double negation introduction
% Explonation
valid_proof(Prems, ProofCopy, [[Row, neg(neg(Value)), negnegint(X)] | RestRows], Done) :-
                          findRow(X, Done, SomeValue),
                          SomeValue = neg(Value),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, neg(neg(Value)), negnegint(X)] | Done]).

% Case: Double negation elimination
% Explonation
valid_proof(Prems, ProofCopy, [[Row, Value, negnegel(X)] | RestRows], Done) :-
                          findRow(X, Done, neg(neg(Value))),
                          valid_proof(Prems, ProofCopy, RestRows,[[Row,Value,negnegel(X)] | Done]).

% Case: Modus tollens
% Explonation
valid_proof(Prems, ProofCopy, [[Row, neg(Value), mt(X,Y)] | RestRows], Done) :-
                            findRow(X,Done, imp(Value, InvalidValue)),
                            findRow(Y,Done, neg(InvalidValue)),
                            valid_proof(Prems, ProofCopy, RestRows, [[Row, neg(Value), mt(X,Y)] | Done]).

% Case: PBC
% Explonation




% Case: LEM
% Explonation
valid_proof(Prems, ProofCopy, [[Row, or(A, B), lem] | RestRows], Done) :-
                          A = neg(B),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A, B), lem] | Done]).

valid_proof(Prems, ProofCopy, [[Row, or(A, B), lem] | RestRows], Done) :-
                          B = neg(A),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A, B), lem] | Done]).


% ~~~ * ~~~ * ~~~ * Rules to apply ~~~ * ~~~ * ~~~ * %


/*
premise premise - Done        impel(x,y) →e x,y - Done
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


% ~~~ * ~~~ * ~~~ * End of program ~~~ * ~~~ * ~~~ * %
