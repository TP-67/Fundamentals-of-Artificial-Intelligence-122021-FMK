% facts
% define distances between different locations
dist(a, t1, 5).
dist(a, t2, 13).

dist(a, r1, 8).
dist(a, r2, 10).

dist(t1, m, 3).
dist(t1, n, 9).
dist(t2, m, 8).
dist(t2, n, 5).

dist(r1, m, 5).
dist(r1, n, 6).
dist(r2, m, 5).
dist(r2, n, 4).

dist(t1, s1, 9).
dist(t1, s2, 4).
dist(t1, s3, 5).
dist(t2, s1, 5).
dist(t2, s2, 7).
dist(t2, s3, 6).

dist(s1, m, 6).
dist(s1, n, 1).
dist(s2, m, 2).
dist(s2, n, 3).
dist(s3, m, 2).
dist(s3, n, 5).

% define different items and their classes
cargo(book, daily).
cargo(toothbrush, daily).
cargo(wine, liquid).
cargo(milk, liquid).
cargo(offer, docs).
cargo(idcard, docs).
cargo(fish, ref).
cargo(steak, ref).

% rules
% a function that defines a transport method for a certain class of
% items
transport(X, Y) :-
    cargo(X, Z),
    (Z == daily ->
        Y = atsd;
        (Z == liquid ->
            Y = atd;
            (Z == docs ->
                Y = atd;
                (Z == ref ->
                    Y = ard; Y = false)))).

% functions that calculate distances for transporting liquid and
% important documents
atdDist1(X, Y) :- dist(a, t1, Z1), dist(t1, X, Z2), Y is Z1 + Z2.
atdDist2(X, Y) :- dist(a, t2, Z1), dist(t2, X, Z2), Y is Z1 + Z2.

% functions that calculate distances for transporting fresh food
% (needs refrigeratoin storage)
ardDist1(X, Y) :- dist(a, r1, Z1), dist(r1, X, Z2), Y is Z1 + Z2.
ardDist2(X, Y) :- dist(a, r2, Z1), dist(r2, X, Z2), Y is Z1 + Z2.

% functions that calculate distances for transporting daily supplies
atsdDist1(X, Y) :- dist(a, t1, Z1), dist(t1, s1, Z2), dist(s1, X, Z3), Y is Z1 + Z2 + Z3.
atsdDist2(X, Y) :- dist(a, t1, Z4), dist(t1, s2, Z5), dist(s2, X, Z6), Y is Z4 + Z5 + Z6.
atsdDist3(X, Y) :- dist(a, t1, Z7), dist(t1, s3, Z8), dist(s3, X, Z9), Y is Z7 + Z8 + Z9.
atsdDist4(X, Y) :- dist(a, t2, Z10), dist(t2, s1, Z11), dist(s1, X, Z12), Y is Z10 + Z11 + Z12.
atsdDist5(X, Y) :- dist(a, t2, Z13), dist(t2, s2, Z14), dist(s2, X, Z15), Y is Z13 + Z14 + Z15.
atsdDist6(X, Y) :- dist(a, t2, Z16), dist(t2, s3, Z17), dist(s3, X, Z18), Y is Z16 + Z17 + Z18.

% find the max element in a list along with its index
:- use_module(library(clpfd)).
maximum_at(Zs, Max, Pos) :-
    maplist(#>=(Max), Zs),
    nth0(Pos, Zs, Max).

% find the min element in a list along with its index
% :- use_module(library(clpfd)).
minimum_at(Zs, Min, Pos) :-
    maplist(#=<(Min), Zs),
    nth0(Pos, Zs, Min).

% functions that calculate minmum distance for transporting different
% classes of items to a designated residential area
atdMinDist(X, Y, D) :- atdDist1(X, Y1), atdDist2(X, Y2), minimum_at([Y1, Y2], Y, D).

ardMinDist(X, Y, D) :- ardDist1(X, Y1), ardDist2(X, Y2), minimum_at([Y1, Y2], Y, D).

atsdMinDist(X, Y, D) :- atsdDist1(X, Y1), atsdDist2(X, Y2), atsdDist3(X, Y3), atsdDist4(X, Y4), atsdDist5(X, Y5), atsdDist6(X, Y6), minimum_at([Y1, Y2, Y3, Y4, Y5, Y6], Y, D).

% return the recommended route and distance for transporting an input
% item to a designated residential area
minDist(C, X, Y, S) :-
    transport(C, M),
    (M == atd ->
    atdMinDist(X, Y, D), getByIndex(["A -> T1 -> Des", "A -> T2 -> Des"], D, S);
        (M == ard ->
        ardMinDist(X, Y, D), getByIndex(["A -> R1 -> Des", "A -> R2 -> Des"], D, S);
            (M == atsd ->
            atsdMinDist(X, Y, D), getByIndex(["A -> T1 -> S1 -> Des", "A -> T1 -> S2 -> Des", "A -> T1 -> S3 -> Des", "A -> T2 -> S1 -> Des", "A -> T2 -> S2 -> Des", "A -> T2 -> S3 -> Des"], D, S); Y = false, S = false))).

% get index of a given element in a list
getByIndex([X], 0, X).
getByIndex([H|_], 0, H).
getByIndex([_|T], I, E) :- NewIndex is I-1, getByIndex(T, NewIndex, E).

% return classes of different items
whatType(X) :- cargo(X, Y),
    format('~w will be shipped as ~w', [X, Y]), nl.

% final output of our system
routeRecom(I, X) :- minDist(I, X, Y, D), whatType(I),
    format('Distance to your home: ~w', [Y]), nl,
    format('Route recommendation: ~w', [D]).

userInteract() :-
    write('Please type-in your goods: '),
    read(X), nl,
    write('Please type-in your address: '),
    read(Y), nl,
    routeRecom(X, Y).
