% ~~~ * Info  * ~~~%

/*
Project:            Lab2 - Logic in computer science
Authors:            Thony Price
                    Wlliam Skagerström
Last revision:      2016-12-09
*/

% ~~~ * Main predicate * ~~~ %

% Provided code
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).
%   check(T, L, S, U, F)
%       T - The transitions in form of adjacency lists
%       L - The Labeling
%       S - Current State
%       U - Currently recorded states (Used)
%       F - CTL Formula to check.

% ~~~ * Check predicates * ~~~ %

% And ->  Evaluate both expressions. Both must evaluate
%         as True for the proof to hold. 
check(Transitions, Labels, State, [], and(F1, F2)) :-
          check(Transitions, Labels, State, [], F1),
          check(Transitions, Labels, State, [], F2).

% Or  ->  Either of the expressions evaluates True, try both.
%         If first fails suppress backtracking w cuts.
check(Transitions, Labels, State, [], or(F, _)) :-
          check(Transitions, Labels, State, [], F), !.
check(Transitions, Labels, State, [], or(_, F)) :-
          check(Transitions, Labels, State, [], F), !.

% Neg ->  Ensure that adjacent states not disproves neg(F)
check(_, Labels, State, [], neg(F)) :-
          allPaths(Labels, State, Paths),
          not(member(F, Paths)).

% Ax  ->  "All neXt states". First get all paths from
%         current state (using allPaths). Then evaluate 
%         all paths (using evalAll).
check(Transitions, Labels, State, Used, ax(F)) :-
          allPaths(Transitions, State, Paths),
          evalAll(Transitions, Labels, Paths, Used, F).

% Ex  ->  "There Exists a neXt state". First get all paths from
%         current state. Then check if one if the paths
%         evaluates as True using member to let prolog bind
%         with each adjacent state and check each one.
check(Transitions, Labels, State, Used, ex(F)) :-
          allPaths(Transitions, State, Paths),
          member(X, Paths),
          check(Transitions, Labels, X, Used, F).

% Ag  ->  "Along all paths, (Globally)". We need to check along all
%         the path for every possible path. In case of cycles we must
%         keep track of states we've visited by appending them into Used
%         and return tru if it's already evaluated. 
%         Because all next states, ax, is already defined it can be
%         repetitively to verify that the proof holds globally.
check(_, _, State, Used, ag(_)) :-
          member(State, Used).
check(Transitions, Labels, State, Used, ag(F)) :-
          check(Transitions, Labels, State, [], F),
          check(Transitions, Labels, State, [State|Used], ax(ag(F))).

% Eg  ->  "Exists Globally". See comments for pred. ag. This implies
%         the same but needs only be valid for at least one of the
%         adjacent states. Therefore we can find allPaths and
%         using member evaluate each until we find one that holds.
%         If we encounter a state we already been at eg evals as True.
check(_, _, State, Used, eg(_)) :-
          member(State, Used), !.
check(Transitions, Labels, State, Used, eg(F)) :-
          check(Transitions, Labels, State, [], F),
          allPaths(Transitions, State, Paths),
          member(X, Paths),
          check(Transitions, Labels, X, [State|Used], eg(F)).

% Af  ->  Again we can reuse ax to evaluate allPaths. It's
%         necessary to make sure states aren't member of visited 
%         states. 
check(Transitions, Labels, State, U, af(F)) :-
          not(member(State, U)),
          check(Transitions, Labels, State, [], F).
check(Transitions, Labels, State, U, af(F)) :-
          not(member(State, U)),
          check(Transitions, Labels, State, [State|U], ax(af(F))).
          
% Ef  ->  As af but using member. 
check(Transitions, Labels, State, Used, ef(F)) :-
          not(member(State, Used)),
          check(Transitions, Labels, State, [], F).
check(Transitions, Labels, State, Used, ef(F)) :-
          not(member(State, Used)),
          allPaths(Transitions, State, Paths),
          member(X, Paths),
          check(Transitions, Labels, X, [State|Used], ef(F)).

% ~~~ * Help predicates * ~~~ %

% This makes sure that the atom, F, is present in the state. 
% Calling allPaths w Labels returns all variables in the state. 
% Make sure F's one of them
check(_, Labels, State, [], F) :-
          allPaths(Labels, State, Paths),
          member(F, Paths).

% Iterates through all transitions and binds the adjacent states
% to the variable "Paths".
allPaths([[State, Paths]|_], State, Paths) :- !.
allPaths([_|T], State, Paths) :- allPaths(T, State, Paths).

% Iterate through all states in list and check each one
evalAll(_,_,[],_,_).
evalAll(Transitions, Labels, [State|States], U, F) :-
          check(Transitions, Labels, State, U, F), !,
          evalAll(Transitions, Labels, States, U, F).
          
          