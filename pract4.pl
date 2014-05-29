%Árboles
esArbol(nil).
esArbol(t(_,I,D)) :- esArbol(I),esArbol(D).

inserta(N,nil,t(N,nil,nil)).
inserta(N,t(R,I,D),t(R,NewI,D)) :- N =< R, inserta(N,I,NewI).
inserta(N,t(R,I,D),t(R,I,NewD)) :- N > R, inserta(N,D,NewD).

%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).

% Regresa los vertices de un arbol
lista_vertices(nil,[]).
lista_vertices(t(R,I,D),[R|L]) :- lista_vertices(I,IL), lista_vertices(D,DL), pega(IL,DL,L). 

% Arbol de busqueda binaria
arbBinBusqAux([],A,A).
arbBinBusqAux([H|R], A, T) :- inserta(H,A,N), arbBinBusqAux(R,N,T).
arbBinBusq(L,T) :- arbBinBusqAux(L,nil,T).

%preOrden
preOrden(nil, []).
preOrden(t(R,nil,nil), [R]).
preOrden(t(R,I,D),[R|RS]) :- lista_vertices(I,IL), lista_vertices(D,DL), pega(IL,DL,RS).

%inOrden
inOrden(nil, []).
inOrden(t(R,I,D),L) :- inOrden(I,IL), inOrden(D,DL), pega(IL,[R],L1), pega(L1,DL,L).

%postOrden
postOrden(nil,[]).
postOrden(t(R,I,D),L) :- postOrden(I,IL), postOrden(D,DL), pega(IL,DL,L1), pega(L1,[R],L).

%hojas
hojas(nil, []).
hojas(t(R,nil,nil), [R]).
hojas(t(_,I,D), L) :- hojas(I, HI), hojas(D,HD), pega(HI,HD, L).

%numHojas
numHojas(T,N) :- hojas(T, L), length(L, N).

%internos
internos(nil, []).
internos(t(_,nil,nil), []).
internos(t(R,I,D), L) :- internos(I, II), internos(D, ID), pega([R], II, L1), pega(L1,ID, L).

%numNodInternos
numNodInternos(T,N) :- internos(T, L), length(L, N).


%drop
drop(0,L,L).
drop(1,[_|[]], []).
drop(N,[_|T], R) :- X is N - 1, drop(X,T,R).

%take
take(0,_,[]).
take(1,[H|_],[H]).
take(N,[H|T],R) :- X is N - 1, take(X, T, R1), pega([H], R1, R).

%veces
veces(_,[],0).
veces(X,[X|T],R) :- veces(X,T,R1), R is R1 + 1.
veces(X,[_|T],R) :- veces(X,T, R).

%get
get(0,[H|_],H).
get(N,[_|T],X) :- M is N - 1, get(M, T, X).

% Aritmetica
expr(A) --> s, nat(B,_), s, "-", s, expr(C), s, {A is B-C}.
expr(A) --> s, nat(B,_), {A is B}, s.
nat(X,1) --> dig(X).
nat(X,T) --> dig(A), nat(B,S), {T is 10*S, X is A*T + B}.
dig(X) --> [D], {"0" =< D, D =< "9", X is D - "0"}.
s --> "" | " ", s.
