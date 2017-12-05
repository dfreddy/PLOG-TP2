:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

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
%Returns the best possible solution
%%%%Vars and VT are used for debugging %%%%%%%%%%%%%%%%
%%%%+Seats, +Initial, -Group, -Vars, -VT %%%%%%%%%%%%%%%%
%Test with groupRedis(5, [1,3,5], Group, Vars, VT).
%and groupRedis(7, [1,4,7], Group, Vars, VT).
groupRedis(Seats, Initial, Group, Vars, VT):-
	%Variables
	length(Initial, GroupLength),
	length(Group, GroupLength),
	domain(Group, 1, Seats),
	%sumlist(Initial, InitialTotal),
	
	find_optimal(Group, GroupLength, Initial, Vars, VT),
	
	minimize(labeling([all], Group),VT).

%Finds a possible seating for the group. If it doesnt have minimal seat changes when compared to the initial seating, redo with a less optimal option
find_optimal(Group, GroupLength, Initial, Vars, VT):-
	group_seating(Group, GroupLength),
	
	variation_list(Group, Initial, Vars),
	sum(Vars, #= , VT).
	
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
	(abs(G-I) #\= 0 #<=> V), %ou (V #= abs(G-I))
	variation_list(Group, Initial, Vars).

%%%%%%%%%%%%% READ ME PLS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% READ ME PLS %%%%%%%%%%%%%%%%%%%%%%
% READ ME ----> Com esta implementacao, tenta-se obter o menor numero de mudancas, mas com (V #= abs(G-I)) obtem-se a mudanca mais pequena possivel. 
% Ex. Grupo com lugares iniciais [1,99] - com (abs(G-I) #\= 0 #<=> V) obtem-se [1,2] / [98,99]
%										- com (V #= abs(G-I)) obtem-se [49,50] (duas mudancas, mas mais pequenas)
% Qual destas implementacoes é a melhor???
%%%%%%%%%%%%% READ ME PLS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% READ ME PLS %%%%%%%%%%%%%%%%%%%%%%



%optimal_var([], _, 0).
%optimal_var(Vars, K, VT):-
%	sum(Vars, #=, VT), !,
%	VT #<= K;
%	K1 is K+1,
%	optimal_var(Vars, K1, VT).
	
% TRY iterating over find_optimal on the main() and incrementing K there, instead of inside the find_optimal() cycle
% APPEND/2 flattens listoflists1 into list2
% para o tratamento de varios grupos em vez de só um, TRY append(ListOfGroups, ListOfEveryone), labeling([], ListOfEveryone), mas return ListOfGroups
% TRY minimize(labeling([], Group), VT) para ter o resultado com o minimo de variaçao, em vez de incrementar o K no optimal_var()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Now with 2 groups of 4
%Must choose the most optimal, least seat changing option
%Returns the best possible solution
%%%%Vars and VT are used for debugging %%%%%%%%%%%%%%%%
%%%%+Seats, +Initials, -Groups, -Vars, -VT %%%%%%%%%%%%%%%%
%Test with multipleGroupRedis(20,[[1,3,5,7],[9,10,13,11]], Groups, Vars, VT).

multipleGroupRedis(Seats, Initials, Groups, Vars, VT) :-
	Groups = [G1, G2],
	length(G1, 4),
	length(G2, 4),
	domain(G1, 1, Seats),
	domain(G2, 1, Seats),
	find_optimal_groups(Initials, Groups, Vars, VTList, SeatsTaken),
	all_distinct(SeatsTaken),
	sum(VTList, #= , VT),

	append(Groups,GTotal),
	minimize(labeling([],GTotal),VT).

find_optimal_groups(_, [], [], [], []).
find_optimal_groups([Initial|IRest], [Group|GRest], [Var|VRest], [VT|VTRest], SeatsTaken) :-
	length(Initial,GroupLength),
	find_optimal(Group, GroupLength, Initial, Var, VT),
	append(RestOfSeatsTaken, Group, SeatsTaken),
	find_optimal_groups(IRest, GRest, VRest, VTRest, RestOfSeatsTaken).

%TODO - Gerar Seats e Initials aleatoriamente e com dimensoes variadas

% 1st- Find optimal for a group - Done
% 2nd- Check if seats not already taken (member of a list) - Done
% 3rd- Add to seats taken (append) - Done
% 4th- Increment variation of group to total variation - Done

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Now for many groups with many people
%Must choose the most optimal, least seat changing option
%Returns the best possible solution
%%%%Vars and VT are used for debugging %%%%%%%%%%%%%%%%
%%%%+Seats, +Initials, -Groups, -Vars, -VT %%%%%%%%%%%%%%%%
%Test with manyGroupRedis(20,[[1,3,5,7,15],[9,10,13],[20,2,14],[15,16]], Groups, Vars, VT).

manyGroupRedis(Seats, Initials, Groups, Vars, VT) :-
	length(Initials, GroupNum),
	length(Groups, GroupNum),
	initialize(Groups, Initials, Seats),
	find_optimal_groups(Initials, Groups, Vars, VTList, SeatsTaken),
	all_distinct(SeatsTaken),
	sum(VTList, #= , VT),

	append(Groups,FlattenedGroups),
	minimize(labeling([],FlattenedGroups),VT).

initialize([],[],_).
initialize([Group|GRest], [Initial|IRest], Seats) :-
	length(Initial, GroupSize),
	length(Group, GroupSize),
	domain(Group, 1, Seats),
	initialize(GRest, IRest, Seats).

%TODO: visualizacao da plateia.
%Ex: 77777*&&&&&////!!
%	 99931111111||||||
%	 00005555588888822
%	 44477777.....6666
%

manyGroupsRandomized(Groups, MaxSeats, MaxGroupElem/*,Vars,VT*/) :-
	random(10,MaxSeats,Seats),
	random(2,MaxGroupElem,MaxGroups),
	geraGruposAleatorios(MaxGroups,Seats,Initials,0),
	print(Initials),
	manyGroupRedis(Seats, Initials, Groups, _ , _ /*,Vars, VT*/).

geraGrupo(_,TamGrupo,[],TamGrupo) :- !.
geraGrupo(Seats,TamGrupo,[Seat|Rest],TamAtual) :-
	random(1,Seats,Seat),
	NovoTamAtual is TamAtual + 1,
	geraGrupo(Seats,TamGrupo,Rest,NovoTamAtual).

geraGruposAleatorios(_,Seats,[],Seats).
geraGruposAleatorios(MaxGroups,Seats,[Grupo|Rest],CurrentSeats):-
	Seats - CurrentSeats < MaxGroups,
	ElementosGrupo is Seats-CurrentSeats,
	geraGrupo(Seats,ElementosGrupo,Grupo,0),
	NewCurrentSeats is CurrentSeats + ElementosGrupo,
	geraGruposAleatorios(MaxGroups,Seats,Rest,NewCurrentSeats).
geraGruposAleatorios(MaxGroups,Seats,[Grupo|Rest],CurrentSeats):-
	random(2,MaxGroups,ElementosGrupo),
	geraGrupo(Seats,ElementosGrupo,Grupo,0),
	NewCurrentSeats is CurrentSeats + ElementosGrupo,
	geraGruposAleatorios(MaxGroups,Seats,Rest,NewCurrentSeats).

