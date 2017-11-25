:- use_module(library(clpfd)).

%4
puzzle([S,E,N,D,M,O,R,Y]):-
	domain([S,E,N,D,M,O,R,Y], 0, 9),
	S #> 0, M #> 0,
	all_different([S,E,N,D,M,O,R,Y]),
	S*1000 + E*100 + N*10 + D +
	M*1000 + O*100 + R *10 + E #=
	M*10000 + O*1000 + N*100 + E*10 + Y,
	labeling([], [S,E,N,D,M,O,R,Y]).
	
%fechadura
fechadura([A,B,C]):-
	domain([A, B, C], 1, 50),
	A1 in 0..5, A2 in 0..9,
	B1 in 0..5, B2 in 0..9,
	B #= A*2,
	C #= B+10,
	A+B #> 10,
	A #= A1*10 + A2,
	(
		A1 mod 2 #= 0 #/\ A2 mod 2 #\= 0 #\/
		A1 mod 2 #\= 0 #/\ A2 mod 2 #=0
	),
	B #= B1*10 + B2,
	B1 mod 2 #= B2 mod 2,
	labeling([], [A,B,C]).
	
%5
forte([S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12]):-
	domain([S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12], 0, 5),
	S1+S2+S3+S4 #= 5,
	S4+S5+S6+S7 #= 5,
	S7+S8+S9+S10 #= 5,
	S10+S11+S12+S1 #= 5,
	sum([S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12], #= , 12),
	labeling([], [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12]).