:- use_module(library(clpfd)).
:- use_module(library(lists)).

% First solve it to set parameters
%Groups is passed as a list of sizes. The size of the Groups list is the amount of groups
%In the future Groups could be a list of lists, each member of the list being the initial ticket placement


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%First with 1 group of 2
staticRedis(Seats, Group):-
	%variables
	Group = [T1, T2],
	
	domain(Group, 1, Seats),
	all_distinct(Group),
	
	%group_seating
	abs(T1-T2) #= 1,
	
	labeling([], Group).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Now with a given initial seating for the group
%Must choose the most optimal, least seat changing option
%abs(sumlist(Option) - sumlist(Initial)) must be as small as possible
%Iterate using a starting Difference=1 and increment it each failed try
%%%%Vars and VT are used for debugging %%%%%%%%%%%%%%%%
%%%%+Seats, +Initial, +Limit, -Group, -Vars, -VT %%%%%%%%%%%%%%%%
%Test with groupRedis(5, [1,3,5], 1, Group, Vars, VT)
%and groupRedis(7, [1,4,7], 1, Group, Vars, VT
groupRedis(Seats, Initial, Limit, Group, Vars, VT):-
	%Variables
	length(Initial, GroupLength),
	length(Group, GroupLength),
	domain(Group, 1, Seats),
	%sumlist(Initial, InitialTotal),
	
	find_optimal(Group, GroupLength, Initial, Vars, VT, Limit),
	labeling([], Group).

%Finds a possible seating for the group. If it doesnt have minimal seat changes when compared to the initial seating, redo with a less optimal option
find_optimal(Group, GroupLength, Initial, Vars, VT, Limit):-
	group_seating(Group, GroupLength),
	
	variation_list(Group, Initial, Vars),
	optimal_var(Vars, Limit, VT).
	
	%sumlist(Group, GroupTotal),
	%abs(GroupTotal - InitialTotal) #> K.
	%K1 is K+1,
	%find_optimal(Group, GroupLength, InitialTotal, K1).
	
%Using total group sums to check for optimals.  Wrong.
%group_sum([], 0).
%group_sum([G| Group], Sum):-
%		group_sum(Group, NewSum),
%		G+Sum #= NewSum.
	
%check_for_optimal(Group, InitialTotal, K):-
%	sum(Group, #= , GroupSum),
%	(
%	abs(InitialTotal - GroupSum) #< K;
%	K1 is K+1,
%	check_for_optimal(Group, InitialTotal, K1)
%	).


%New way to check for optimals: member variation.
%First get all member variation in  a list Vars and assign variations binarily
variation_list([], [], []).
variation_list([G|Group], [I|Initial], [V|Vars]):-
	(abs(G-I) #= 0,
	V #= 0;
	V #= 1
	),
	variation_list(Group, Initial, Vars).
	
optimal_var([], _, 0).
optimal_var(Vars, K, VT):-
	sum(Vars, #=, VT), !,
	VT #<= K;
	K1 is K+1,
	optimal_var(Vars, K1, VT).