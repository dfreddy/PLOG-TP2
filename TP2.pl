:- use_module(library(clpfd)).
:- use_module(library(lists)).

% First solve it to set parameters
%Groups is passed as a list of sizes. The size of the Groups list is the amount of groups
%In the future Groups could be a list of lists, each member of the list being the initial ticket placement



%First with 1 group of 2
staticRedis(Seats, Group):-
	%variables
	Group = [T1, T2],
	
	domain(Group, 1, Seats),
	all_distinct(Group),
	
	%group_seating
	abs(T1-T2) #= 1,
	
	labeling([], Group).
	
	
%Now with a group of N members
%A distancia entre cada membro de um grupo e outro do mesmo grupo deve ser sempre menor que o numero de membros no grupo
nRedis(Seats, N, Group):-
	length(Group, N),
	domain(Group, 1, Seats),
	group_seating(Group, N),
	labeling([], Group).
	
group_seating([], _).
group_seating([M | G], N):-
	seat_in_group(M, G, N),
	group_seating(G, N).
	
seat_in_group(_, [], _).
seat_in_group(M, [GM | G], N):-
	seat_condition(M, GM, N),
	seat_in_group(M, G, N).
	
seat_condition(M, GM, N):-
	M #\= GM,
	abs(M-GM) #< N.