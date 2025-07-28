x_generator4( N ) :-
	x_generator4_loop(
		[ [[9 ,6 ,7] ,[4 ,0 ,1] ,[2 ,8 ,3] ,[5]]
		, [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
		, [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
		, [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
		, [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
		, [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
		, [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]]
		, [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]]
		, [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]]
		, [[3] ,[5] ,[6 ,0 ,7] ,[2] ,[4 ,1] ,[8 ,9]] ], 0, N ).
x_generator4_loop( [], C, C ).
x_generator4_loop( [T|TS], C, N ) :-
	generator4( T ),
	C1 is C + 1,
	x_generator4_loop( TS , C1 , N ).
x_generator4_loop( [_|TS], C, N ) :-
	x_generator4_loop( TS , C, N ).

generator4( Y ) :-
    magic_prime_generator( Y ).
%    matcher( Primes, Y ).


secret_generator( Y ) :-
    matcher( Y ).



matcher( X ) :-
    match_generator( [0,1,2,3,4,5,6,7,8,9], [], X).



candidate_validator(N, A):-
    generator4(N),
    
    subset(N, A).

match_generator([], C, C).
match_generator(Digits, C, S):-
    bagof(P, (candidate_validator(P,Digits)), Q),
    member(N, Q),
    append(C, [N], C1),
    last(C1, N),
    subtract(Digits, N, New_digits),
    match_generator(New_digits, C1, S).


% MAGIC PRIME GENERATION

magic_prime_generator( X ) :-
    candidate_generator( 0, Y ),
    member(X, Y).

candidate_generator( 9871, XS ) :-
    last(XS, [9,8,7,1]).
candidate_generator( 0, [[2]|XS] ) :-
    candidate_generator( 3, XS ). 
candidate_generator( Counter, [New_prime_digits|XS] ):-
    Counter <9999,
    is_magic_prime(Counter),
    digits( Counter, New_prime_digits ),
    New_counter is Counter + 2,
    candidate_generator( New_counter, XS ).
candidate_generator( Counter, XS ):-
    Counter < 9999,
	\+ is_magic_prime(Counter),
    New_counter is Counter + 2,
    candidate_generator( New_counter, XS ).




is_magic_prime( X ) :-
    X >= 2, 
    digits( X, X_digits ),
    nub(X_digits, X_digits_nub),
	X_digits_nub = X_digits,
    Root is floor(sqrt(X)),
    prime_checker( X, Root, 2 ).

prime_checker( X, _, _ ):-
    X = 2,
	true.
prime_checker( X, Root, Counter ):-
    ( Counter > Root -> true ;
    X mod Counter =\= 0,
    New_counter is Counter + 1,
    prime_checker( X, Root, New_counter )).


digits(X, [X]) :-
    X<10.

digits(X, W):-
    X>=10,
    div_mod(X, 10, D, M),
	digits(D, R),
    append( R, [M], W).

div_mod( A, B, D, M ) :-
    D is A div B,
    M is A mod B.

nub([], []).
nub([X|XS], [X|Y]):-
    \+ member(X, XS),
    nub(XS, Y).
nub([X|XS], Y):-
    member(X, XS),
    nub(XS, Y).
                
total_append( [], YS, YS ).
total_append( [X], YS, QS):-
    append( X, YS, QS ),
	total_append( [], QS, _ ).
total_append( [X|XS], YS, ZS):-
    append( X, YS, QS ),
	total_append( XS, QS, ZS ).
