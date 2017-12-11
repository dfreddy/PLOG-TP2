:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
	
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

find_optimal(Group, GroupLength, Initial, Vars, VT):-
	group_seating(Group, GroupLength),
	
	variation_list(Group, Initial, Vars),
	sum(Vars, #= , VT).
	
variation_list([], [], []).
variation_list([G|Group], [I|Initial], [V|Vars]):-
	(abs(G-I) #\= 0 #<=> V), %ou (V #= abs(G-I))
	variation_list(Group, Initial, Vars).

find_optimal_groups(_, [], [], []).
find_optimal_groups([Initial|IRest], [Group|GRest], [Var|VRest], [VT|VTRest]) :-
	length(Initial,GroupLength),
	find_optimal(Group, GroupLength, Initial, Var, VT),
	find_optimal_groups(IRest, GRest, VRest, VTRest).

manyGroupRedis(Seats, Initials, Groups, Vars, VT) :-
	length(Initials, GroupNum),
	length(Groups, GroupNum),
	initialize(Groups, Initials, Seats),
	find_optimal_groups(Initials, Groups, Vars, VTList),
	append(Groups,FlattenedGroups),
	all_distinct(FlattenedGroups),
	sum(VTList, #= , VT),

	%print(FlattenedGroups),nl %Uninstantiated before labeling
	%print(VT),nl
	labeling([minimize(VT),time_out(20000,_),first_fail],FlattenedGroups),
	output(1, Seats, Groups, []).

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

writeOutput([]).
writeOutput([Seat|List]):-
	write(Seat), output(List).
	
isFromGroupNumber(Counter, [], _, _):- fail.
isFromGroupNumber(Counter, [Group|Groups], N, Number):-
	member(Counter, Group), Number = N;
	N1 is N+1, isFromGroupNumber(Counter, Groups, N1, Number).
	
output(_, 0, _, OutputList):- nl, writeOutput(OutputList).
output(Counter, Seats, Groups, OutputList):-
	(
		isFromGroupNumber(Counter, Groups, 1, Number), append(OutputList, [Number], NewList);
		append(OutputList, ['-'], NewList)
	),	
	NewCounter is Counter+1, NewSeats is Seats-1,
	output(NewCounter, NewSeats, Groups, NewList).

% Visualização da plateia numa só linha %
% Exemplo: manyGroupRedis(40,[[1,3,5,7,15],[9,10,13],[20,2,14],[15,16], [40, 30,35,27]], Groups, Vars, VT). %
% Resultado: 11111--222-33344----------5555---------- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

manyGroupsRandomized(Groups, MaxSeats, MaxGroups) :-
	random(MaxGroups,MaxSeats,TicketNumber),
	print(TicketNumber), nl,
	geraGruposAleatorios(MaxGroups, MaxSeats,TicketNumber, Initials, 0), !,
	print(Initials), nl,
	manyGroupRedis(MaxSeats, Initials, Groups, _ , _).

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

