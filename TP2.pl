:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

find_optimal(Group, GroupLength, Initial, ChairsPerRow, Vars, VT):-
	minimum(MinSeat,Group),
	maximum(MaxSeat,Group),
	(MaxSeat - MinSeat) #= (GroupLength - 1), 

	(MaxSeat mod ChairsPerRow) #> (MinSeat mod ChairsPerRow), %Must be on the same row - This implementation means that a group mustnt be bigger than a row

	variation_list(Group, Initial, Vars),
	sum(Vars, #= , VT).
	
variation_list([], [], []).
variation_list([G|Group], [I|Initial], [V|Vars]):-
	(abs(G-I) #\= 0 #<=> V), %ou (V #= abs(G-I))
	variation_list(Group, Initial, Vars).

find_optimal_groups(_, [], _ , [], []).
find_optimal_groups([Initial|IRest], [Group|GRest], ChairsPerRow, [Var|VRest], [VT|VTRest]) :-
	length(Initial,GroupLength),
	find_optimal(Group, GroupLength, Initial, ChairsPerRow, Var, VT),
	find_optimal_groups(IRest, GRest, ChairsPerRow, VRest, VTRest).

manyGroupRedis(Seats, ChairsPerRow, Initials, Groups, Vars, VT) :-
	length(Initials, GroupNum),
	length(Groups, GroupNum),
	initialize(Groups, Initials, Seats),
	find_optimal_groups(Initials, Groups, ChairsPerRow, Vars, VTList),
	append(Groups,FlattenedGroups),
	all_distinct(FlattenedGroups),
	sum(VTList, #= , VT),

	%print(FlattenedGroups),nl, %Must be uninstantiated before labeling
	%print(VT),nl,
	%print(Groups),nl,
	%print(VTList),nl,
	labeling([minimize(VT),time_out(20000,_)],FlattenedGroups).

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

manyGroupsRandomized(Groups, MaxSeats, MaxGroups, ChairsPerRow) :-
	ChairsPerRow > MaxGroups, !,
	random(MaxGroups,MaxSeats,TicketNumber),
	geraGruposAleatorios(MaxGroups, MaxSeats,TicketNumber, Initials, 0), !,
	print(Initials), nl,
	manyGroupRedis(MaxSeats, ChairsPerRow, Initials, Groups, _ , _).
manyGroupsRandomized(_,_,_,_) :-
    print('Error: Groups mustnt be larger than a row of chairs'), nl, fail.

geraGrupo(_,TamGrupo,[],TamGrupo) :- !.
geraGrupo(Seats,TamGrupo,[Seat|Rest],TamAtual) :-
	random(1,Seats,Seat),
	NovoTamAtual is TamAtual + 1,
	geraGrupo(Seats,TamGrupo,Rest,NovoTamAtual).

geraGruposAleatorios(_,_,TicketNumber,[],TicketNumber).
geraGruposAleatorios(MaxGroups,Seats,TicketNumber,[Grupo|Rest],CurrentSeats):-
	TicketNumber - CurrentSeats < MaxGroups, !,
	ElementosGrupo is TicketNumber-CurrentSeats,
	geraGrupo(Seats,ElementosGrupo,Grupo,0),
	NewCurrentSeats is CurrentSeats + ElementosGrupo,
	geraGruposAleatorios(MaxGroups,Seats,TicketNumber,Rest,NewCurrentSeats).
geraGruposAleatorios(MaxGroups,Seats,TicketNumber,[Grupo|Rest],CurrentSeats):-
	random(2,MaxGroups,ElementosGrupo),
	geraGrupo(Seats,ElementosGrupo,Grupo,0),
	NewCurrentSeats is CurrentSeats + ElementosGrupo,
	geraGruposAleatorios(MaxGroups,Seats,TicketNumber,Rest,NewCurrentSeats).

