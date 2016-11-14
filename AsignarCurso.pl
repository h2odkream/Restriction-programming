:- use_module(library(clpfd)).

init(0,Period,[]). %la posicion del cero equvale a la candtidad de periodos
init(N,Period,[H|T]):-
    length(H,Period),
    H ins 0..1,
    NewN is N-1,
    init(NewN,Period,T).

%la suma de cada lista debe ser igual n, siendo n el numero de salones
invertir1(N,H):- transpose(H,NewH), maximum_n(N,NewH).
maximum_n(_,[]).
maximum_n(N,[H|T]):-
    sum(H,#=<,N),
    maximum_n(N,T).

%Devuelve la posicion de un elemento dado
my_nth(_, [], _, []).
my_nth(N, [H|B], H, [N|R]):-
    NewN is N + 1,
    my_nth(NewN, B, H, R).
my_nth(N, [X|B], Y, R):-
    X #\= Y,
    NewN is N + 1,
    my_nth(NewN, B, Y, R).

maximum([],[]).
maximum([H1|T1],[H|T]):-
    sum(H,#=,H1),
    maximum(T1,T).

%Devuelve los cursos que tienen estudiante en comun
%el Kcomienza desde uno
conflict(_,[],[]).
conflict(K,[H|T],[[K|Result]|NewResult]):-
        NewK is K +1, my_nth(1, H, 1, Result),
       conflict(NewK,T,NewResult).

%Devuelve el elemento(curso) que esta en una posicion de una lista
elemento([],N,[]).
elemento([X|N],1,X).
elemento([X|R],N,S):- M is N - 1, elemento(R,M,S).

%Travel_Matrix junto a travel devuelven las listas que equivale a cada curso
travel_Matrix(_,[],[]).
travel_Matrix(H,[H1|T],[R|Rr]):- travel(H,H1,R), travel_Matrix(H,T,Rr).

travel(_,[],[]).
travel(H,[H1|T1], [Elemento|R]):- elemento(H,H1,Elemento), travel(H,T1,R).

%Restriccion de que cada leccion de un curso que tiene estudiante en comun con otro no se puede ver en un mismo periodo
student_same_course([]).
student_same_course([H|T]):- transpose(H,NewH),suma(NewH), student_same_course(T).
suma([]).
suma([H|T]):- sum(H,#=<,1), suma(T).

compare([],[]).
compare([H|T],[H1|T1]):-compare_Matriz(H,H1), compare(T,T1).

compare_Matriz([],[]).
compare_Matriz([0|T],[H1|T1]):- H1#=0, compare_Matriz(T,T1).
compare_Matriz([1|T],[_|T1]):- compare_Matriz(T,T1).

asignar_curso(Period,Course,N,Lesson,Matriz):-
    Matriz_Disponibilidad=
				     [[0,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0,0],
				      [1,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,1,1,1],
				      [0,0,0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,1,1],
				      [0,0,1,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0,0,0],
                                      [1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0]],
    Matriz_Conflicto=
                     [[0,1,0,0,1],
					  [1,0,0,1,0],
                      [0,0,0,0,1],
                      [0,1,0,0,1],
                      [1,0,1,1,0]],



    init(Course,Period,Matriz),
	conflict(1,Matriz_Conflicto,R),
    transpose(Matriz,Mt),
    maximum_n(N,Mt),
    compare(Matriz_Disponibilidad,Matriz),
    travel_Matrix(Matriz,R,Result),
    student_same_course(Result),
    maximum(Lesson,Matriz),
    flatten(Matriz, M), labeling([ffc],M).

asignar_curso(20,5,2,[6,10,14,6,4],Resultado).
