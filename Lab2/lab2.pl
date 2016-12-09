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
%check(_, L, S, [], X)      :- ...
%check(_, L, S, [], neg(X)) :- ...
% And
%check(T, L, S, [], and(F,G)) :- ...
% Or
% AX
% EX
% AG
% EG
% EF
% AF
