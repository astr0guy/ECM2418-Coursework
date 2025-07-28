x_generator3( N ) :-
    x_generator3_loop( 
        [ 1024,
          9409,
          23716,
          51529,
          123904,
          185761,
          868624,
          962361,
          982081,
          1000000], 0, N ).
x_generator3_loop( [], C, C ).
x_generator3_loop( [T|TS], C, N ) :-
    generator3( T ),
    C1 is C + 1,
    x_generator3_loop( TS, C1, N ).
x_generator3_loop( [_|TS], C, N ) :-
    x_generator3_loop( TS, C, N ).

x_tester3( N ) :-
    x_tester3_loop( [ 123056, 128036, 139076, 142076 , 148056, 159076, 173096, 189036 , 193056, 198076 ], 0, N ).
x_tester3_loop( [], C, C ). 
x_tester3_loop( [T|TS], C, N ) :-
    tester3( T ),
    C1 is C + 1,
    x_tester3_loop( TS, C1, N ). 
x_tester3_loop( [_|TS], C, N ) :-
    x_tester3_loop( TS, C, N ).

generator3(X) :-	
    inrange(31,1000, W),
    X is W ^ 2.      
        
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

inrange(P, Q, P) :- 
    P =< Q.
inrange(P, Q, W):-
    P1 is P + 1,
    inrange(P1, Q, W).

nub([], []).
nub([X|XS], [X|Y]):-
    \+ member(X, XS),
    nub(XS, Y).
nub([X|XS], Y):-
    member(X, XS),
    nub(XS, Y).

tester3(X):-
    digits( X, X_digits ),
    member( 0 , X_digits),
    nub( X_digits, X_nub),
    X_digits = X_nub,
	length(X_digits, Len),
    X_last_pos is Len - 1,
    X_second_last_pos is Len - 2,
    nth0(X_last_pos, X_digits, X_last),
    nth0(X_second_last_pos, X_digits, X_second_last),
    nth0(0, X_digits, X_first),
    nth0(1, X_digits, X_second),
    nth0(2, X_digits, X_third),
    X_last =:= Len,
    0 =\= X_second,
    0 =\= X_third,
    0 =\= X_second_last,
    1 =:= X_second_last mod 2,
    0 =:= X_second mod X_first,
    0 =:= X_third mod X_first,
    0 =:= X_second_last mod X_first.