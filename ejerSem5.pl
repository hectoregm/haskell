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

%Autómatas 
/*start(0).
t(0, a, 1).
t(0, b, 2).
t(1, a, 2).
t(1, b, 1).
t(2, a, 2).
t(2, b, 2).
final(1).

accept(S) :- start(I), path(I, S).

path(S,[]) :- final(S).
path(S,[H|T]) :- t(S,H,X) , path(X,T). */

% automata que decodifica cadenas de digitos binarios de longitud par,
% con la siguiente definicion:
% 00 = a
% 01 = b
% 10 = c
% 11 = d

start(0).
t(0, 0, [], 1).
t(0, 1, [], 2).
t(1, 0, a, 0).
t(1, 1, b, 0).
t(2, 0, c, 0).
t(2, 1, d, 0).

mealy(In, Out) :- start(I), execute(I, In, Out).
execute(_, [], []).
execute(S, [H|T], Y) :- t(S, H, [], N), execute(N, T, Y).
execute(S, [H|T], [X|Y]) :- t(S, H, X, N), execute(N, T, Y).
