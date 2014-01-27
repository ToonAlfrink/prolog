prepend([],L,L).
prepend([H|T],L2,[H|L3]) :- prepend(T,L2,L3).

prefix(E,[],[]).
prefix(E,[H|T],[H1|L]) :- 
        prepend([E],H,H1),
        prefix(E,T,L).

append([X],L1,L2) :- 
        last(L2,X),
        prepend(L1,[X],L2).

append([H|T],L1,L2) :- 
        append([H],L1,X),
        append(T,X,L2).

gray(0,[[]]).
gray(N,L) :-
        N>0,
        Minus is N - 1,
        gray(Minus,Half),
        prefix(0,Half,Half1),
        prefix(1,Half,Half2),
        reverse(Half2,Half2a),
        prepend(Half1,Half2a,L).

% difference/3: finds the first element in list 1 that differs from the element with the same index in list 2.
difference([Head1|EqualTail],[Head2|EqualTail],Head2).
difference([EqualHead|Tail1],[EqualHead|Tail2],Out) :- difference(Tail1,Tail2,Out).

isEqual(A,A).

% turns/2: turns a gray code sequence into a turtle sequence
turns([Element],[]).
turns([First,Next|GrayTail],[TurnHead,fd|TurnTail]) :-
        difference(First,Next,ChangedDigit),
        (isEqual(ChangedDigit,1) -> isEqual(TurnHead,lt);true),
        (isEqual(ChangedDigit,0) -> isEqual(TurnHead,rt);true),
        prepend([Next],GrayTail,NextTail),
        turns(NextTail,TurnTail).
        

n_elements([],Total,Total).
n_elements([Head|Tail],Sum,Total) :-
        Count is Sum + 1,
        n_elements(Tail,Count,Total).


% diffindex/4: gets the index of the bit that changes
diffindex([Head1|EqualTail],[Head2|EqualTail],Schijfnr,Schijfnr).
diffindex([EqualHead|Tail1],[EqualHead|Tail2],Teller,Schijfnr) :- 
        Plus1 is Teller + 1,
        diffindex(Tail1,Tail2,Plus1,Schijfnr).

% diffindex/3: wrapper for diffindex/4
diffindex(L1,L2,Schijf) :- diffindex(L1,L2,0,Schijf).

        
towers(P,N,GraySeq,Vorige,SpelSituaties,Verschil) :-
        !.