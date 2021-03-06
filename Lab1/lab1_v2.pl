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

find_copy_Row([Nr, Value, Arg], [[Nr, Value, Arg] | _]).
find_copy_Row([Nr, Value, Arg], [_ | Tail]) :-
                      find_copy_Row([Nr, Value, Arg], Tail).

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
% To introduce an impication the rule must refer to a row which is first 
% in a box where the last row in the box's the variable we're introducing implication for
valid_proof(Prems, ProofCopy, [[Row, imp(A,B), impint(X,Y)] | RestRows], Done) :-
                          find_box([X, A, _], Done, Box),
                          find_first_row(Box, [X, A, _]),
                          find_last_row(Box, [Y, B, _]),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, imp(A,B), impint(X,Y)] | Done]).

% Case: Implication elimination
% The value in ths row must exist in an previously used row as an implicated value.
% The value implicating the mentionen one should be apointed by the other row in rule.
valid_proof(Prems, ProofCopy, [[Row, Value, impel(R1,R2)] | RestRows], Done) :-
                          findRow(R1, Done, A),
                          findRow(R2, Done, imp(A, Value)),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, Value, impel(R1,R2)] | Done]).

% Case: Copy
% Make sure that in done-rows we've got the row number which we're copying from
% and it has the same value as the row we're copying to
valid_proof(Prems, ProofCopy, [[Row, X, copy(Some_row)] | RestRows], Done) :-
                          find_box([Row, X, copy(Some_row)], Done, Box),
                          findRow(Some_row,[Some_row, X, _], Box);
                          find_copy_Row([Some_row, X, premise], Done),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, X, copy(Some_row)] | Done]).

% Case: And introduction
% Make sure that the value on the rows we are and-ing have the correct values.
valid_proof(Prems, ProofCopy, [[Row, and(A,B), andint(X,Y)] | RestRows], Done) :-
                          findRow(X,Done,A),
                          findRow(Y,Done,B),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, and(A,B), andint(X,Y)] | Done]).

% Case: And deletion 1
% Checks if A in the and is the same one that is presented.
valid_proof(Prems, ProofCopy, [[Row, A, andel1(X)] | RestRows], Done) :-
                          findRow(X,Done,and(A,_)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, A, andel1(X)] | Done]).

% Case: And deletion 2
% Checks if B in the and is the same one that is presented.
valid_proof(Prems, ProofCopy, [[Row, B, andel2(X)] | RestRows], Done) :-
                          findRow(X,Done,and(_,B)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, B, andel2(X)] | Done]).

% Case: Or introduction 1
% Checks if A is on row X.
valid_proof(Prems, ProofCopy, [[Row, or(A,B), orint1(X)] | RestRows], Done) :-
                          findRow(X,Done,A),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A,B), orint1(X)] | Done]).

% Case: Or introduction 2
% Checks if B is on row X.
valid_proof(Prems, ProofCopy, [[Row, or(A,B), orint2(X)] | RestRows], Done) :-
                          findRow(X,Done,B),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, or(A,B), orint2(X)] | Done]).

% Case: Or elimination
% Check that there exists a row with an or containing two values. Both of these
% must also be present as first lines in two separate boxes and both of those 
% boxes should end with the value given on the row using orel
valid_proof(Prems, ProofCopy, [[Row, Value, orel(R1, R2, R4, R3, R5)] | RestRows], Done) :-
                          findRow(R1, Done, or(C, D)),
                          find_box([R2, C, _], Done, Box1),
                          find_box([R3, D, _], Done, Box2),
                          find_first_row(Box1, [R2, C, _]),
                          find_last_row(Box1, [R4, Value, _]),
                          find_first_row(Box2, [R3, D, _]),
                          find_last_row(Box2, [R5, Value, _]),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, Value, orel(R1, R2, R4, R3, R5)] | Done]).

% Case: Negation introduction
% Checks if there is a box in which begins with A, and ens with a contradiction.
valid_proof(Prems, ProofCopy, [[Row, neg(A), negint(X,Y)] | RestRows], Done) :-
                          find_box([X, A, _], Done, Box),
                          find_first_row(Box, [X, A, _]),
                          find_last_row(Box, [Y, cont, _]),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, neg(A), andint(X,Y)] | Done]).

% Case: Negation elimination
% Checks if there is A and neg(A), which is a requirement for a contradiction.
valid_proof(Prems, ProofCopy, [[Row, cont, negel(X,Y)] | RestRows], Done) :-
                          findRow(X, Done, A),
                          findRow(Y, Done, neg(A)),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, cont, negel(X,Y)] | Done]).

% Case: Contradiction elimination
% Checks to make sure that there is a contradiction on the specified row.
valid_proof(Prems, ProofCopy, [[Row, Value, contel(X)] | RestRows], Done) :-
                          findRow(X, Done, cont),
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, Value, contel(X)] | Done]).

% Case: Double negation introduction
% Makes sure that the value on X is in fact already a neg(Value), then doublenegates it.
valid_proof(Prems, ProofCopy, [[Row, neg(neg(Value)), negnegint(X)] | RestRows], Done) :-
                          findRow(X, Done, SomeValue),
                          SomeValue = Value,
                          valid_proof(Prems,ProofCopy,RestRows,[[Row, neg(neg(Value)), negnegint(X)] | Done]).

% Case: Double negation elimination
% Checks if the row contains a doubleneg(Value).
valid_proof(Prems, ProofCopy, [[Row, Value, negnegel(X)] | RestRows], Done) :-
                          findRow(X, Done, neg(neg(Value))),
                          valid_proof(Prems, ProofCopy, RestRows,[[Row,Value,negnegel(X)] | Done]).

% Case: Modus tollens
% Checks if row X has an implication(A->B), and that there is a neg(B) on row Y.
valid_proof(Prems, ProofCopy, [[Row, neg(Value), mt(X,Y)] | RestRows], Done) :-
                          findRow(X,Done, imp(Value, InvalidValue)),
                          findRow(Y,Done, neg(InvalidValue)),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, neg(Value), mt(X,Y)] | Done]).

% Case: PBC
% Checks for a box which begins with a neg(Value) and ends with a contradiction, thus resulting in just value.
valid_proof(Prems, ProofCopy, [[Row, Value, pbc(X,Y)] | RestRows], Done) :-
                          find_box([X, neg(Value), _], Done, Box),
                          find_first_row(Box, [X, neg(Value), _]),
                          find_last_row(Box, [Y, cont, _]),
                          valid_proof(Prems, ProofCopy, RestRows, [[Row, Value, pbc(X,Y)] | Done]).




% Case: LEM
% A or B has to be the neg() countpart of the other.
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
