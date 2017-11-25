:- use_module(library(clpfd)).
:- use_module(library(lists)).

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

% First solve it to set parameters

%Groups is passed as a list of sizes. The size of the Groups list is the amount of groups

%First with 1 group of 2
staticredis(Seats, Groups):-
	%variables
	Groups = [Group],
	Group = [G1, G2],
	flatten(Groups, List),
	domain(List, 1, Seats),
	all_distinct(List),
	
	%restrictions
	G1 #= G2+1, G1 #= G2-1,
	
	labeling([], List).