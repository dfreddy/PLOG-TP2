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
	labeling([minimize(VT),time_out(20000,_)],FlattenedGroups),
	outputseats(1, ChairsPerRow, Seats, Groups, []).

initialize([],[],_).
initialize([Group|GRest], [Initial|IRest], Seats) :-
	length(Initial, GroupSize),
	length(Group, GroupSize),
	domain(Group, 1, Seats),
	initialize(GRest, IRest, Seats).

writeOutput(_, _, []).
writeOutput(RowCounter, ChairsPerRow, [Seat|List]):-
	(
		RowCounter == ChairsPerRow, nl, write(Seat), writeOutput(0, ChairsPerRow, List)
	);
	write(Seat), NewRowCounter is RowCounter+1, writeOutput(NewRowCounter, ChairsPerRow, List).
	
isFromGroupNumber(Counter, [], _, _):- fail.
isFromGroupNumber(Counter, [Group|Groups], N, Number):-
	member(Counter, Group), Number = N;
	N1 is N+1, isFromGroupNumber(Counter, Groups, N1, Number).
	
outputseats(_, ChairsPerRow, 0, _, OutputList):- nl, writeOutput(0, ChairsPerRow, OutputList).
outputseats(Counter, ChairsPerRow, Seats, Groups, OutputList):-
	(
		isFromGroupNumber(Counter, Groups, 1, Number), append(OutputList, [Number], NewList);
		append(OutputList, ['-'], NewList)
	),	
	NewCounter is Counter+1, NewSeats is Seats-1,
	outputseats(NewCounter, ChairsPerRow, NewSeats, Groups, NewList).

% Visualização da plateia numa só linha %
% Exemplo: manyGroupRedis(40,[[1,3,5,7,15],[9,10,13],[20,2,14],[15,16], [40, 30,35,27]], Groups, Vars, VT). %
% Resultado: 11111--222-33344----------5555---------- %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

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

