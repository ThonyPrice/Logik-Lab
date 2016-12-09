% ~~~ * Info  * ~~~%

/*
Project:            Lab2 - Logic in computer science
Authors:            Thony Price
                    Wlliam Skagerström
Last revision:      2016-12-09
*/

% ~~~ * Main predicate * ~~~ %

% Programskelett
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).
%   check(T, L, S, U, F)
%       T - The transitions in form of adjacency lists
%       L - The labeling
%       S - Current state
%       U - Currently recorded states
%       F - CTL Formula to check.

% Should evaluate to true iff the sequent below is valid. %
% (T,L), S |-   F
%             U

% Literals
% check(_, L, S, [], X)      :- ...
% check(_, L, S, [], neg(X)) :- ...
% check(T, L, S, [], and(F,G)) :- ...

% And ✓
% Or  ✓
% AX  ✓
% EX
% AG
% EG
% EF
% AF

% ~~~ * Check predicates * ~~~ %

% And ->  Evaluate both expressions.
check(Transitions, Labels, State, [], and(F1, F2)) :-
      check(Transitions, Labels, State, [], F1),
      check(Transitions, Labels, State, [], F2).
      
% Or  ->  Either of the expressions evaluates True, try both w cuts
check(Transitions, Labels, State, [], or(F, _)) :-
      check(Transitions, Labels, State, [], F), !.
check(Transitions, Labels, State, [], or(_, F)) :-
      check(Transitions, Labels, State, [], F), !.

% Ax  ->  "Along all paths". First get all paths from
%         current state. Then evaluate all paths.
check(Transitions, Labels, State, [], ax(F)) :-
      allPaths(Transitions, State, Paths),
      evalAll(Transitions, Labels, Paths, U, F).

% Ex  ->  "There exists a path". First get all paths from
%         current state. Then check if one if the paths 
%         evaluates as True using member. 

% Ag  ->  "All future states (Globally)".

% Eg  ->  "Exists Globally" (?).

% Ef  ->  "Exists some Future state".

% Af  ->  "For All Future state".

% ~~~ * Help predicates * ~~~ %

% Iterates through all transitions and binds the adjacent states 
% to the variable "Paths".
allPaths([[State|Paths]|_], State, Paths) :- !.
allPaths([_|T], State, Paths) :- allPaths(T, State, Paths).

% Iterate through all states in list and check each one
evalAll(_,_,[],_,_).
evalAll(Transitions, Labels, [State|States], U, F) :-
      check(Transitions, Labels, State, U, F), !,
      evalAll(Transitions, Labels, States, U, F).
