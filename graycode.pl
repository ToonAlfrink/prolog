% isEqual/2: true if first argument matches second argument
isEqual(A,A).

% prepend/3: concatenates lists
prepend([],L,L).
prepend([H|T],L2,[H|L3]) :- prepend(T,L2,L3).

% prefix/3: prefixes an element to each lists in a list of lists
prefix(E,[],[]).
prefix(E,[H|T],[H1|L]) :- 
        isEqual([E|H],H1),
        prefix(E,T,L).

% gray/2: gets the gray code of a given bit length
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

% turns/2: turns a gray code sequence into a turtle sequence
turns([Element],[]).
turns([First,Next|GrayTail],[TurnHead,fd|TurnTail]) :-
        difference(First,Next,ChangedDigit),
        (isEqual(ChangedDigit,1) -> isEqual(TurnHead,lt);true),
        (isEqual(ChangedDigit,0) -> isEqual(TurnHead,rt);true),
        turns([Next|GrayTail],TurnTail).
        
% getdisc/3: gets the index of the bit that changes, which corresponds with the disc to move
getdisc([Head1|EqualTail],[Head2|EqualTail],Out) :- length(EqualTail,X),Out is X + 1.
getdisc([EqualHead|Tail1],[EqualHead|Tail2],Out) :- getdisc(Tail1,Tail2,Out).

% move/4 with disc 1, which needs special treatment
move(P,1,[L1,L2,L3],New) :-
        (isEqual(P,0) -> %even parity: smallest disc goes left-mid-right
            ((isEqual(L1,[1|T1]) -> isEqual(New,[T1,[1|L2],L3]);true),
            (isEqual(L2,[1|T2]) -> isEqual(New,[L1,T2,[1|L3]]);true),
            (isEqual(L3,[1|T3]) -> isEqual(New,[[1|L1],L2,T3]);true));true),
        (isEqual(P,1) -> %odd parity: smallest disc goes left-right-mid
            ((isEqual(L1,[1|T1]) -> isEqual(New,[T1,L2,[1|L3]]);true),
            (isEqual(L2,[1|T2]) -> isEqual(New,[[1|L1],T2,L3]);true),
            (isEqual(L3,[1|T3]) -> isEqual(New,[L1,[1|L2],T3]);true));true).

% move/4 with other discs
move(P,Schijf,[L1,L2,L3],New) :-
        update(Schijf,L1,L1a),
        update(Schijf,L2,L2a),
        update(Schijf,L3,L3a),
        isEqual(New,[L1a,L2a,L3a]).

% update/3: remove the disc if it's there, add it if it fits on the stack
update(Schijf, LijstIn, LijstUit) :-
        (isEqual(LijstIn,[Schijf|Tail]) -> isEqual(LijstUit, Tail));
        ((isEqual(LijstIn,[]) ; (isEqual(LijstIn,[Head|Tail]), Schijf<Head)) ->
            isEqual(LijstUit,[Schijf|LijstIn]) ;
            isEqual(LijstIn,LijstUit)).

% towers/2: wrapper for towers/4.
towers(N,[[Left,[],[]]|L]) :-
        gray(N,GraySeq),
        P is N mod 2,
        findall(X,between(1,N,X),Left),
        towers(P,GraySeq,[Left,[],[]],L).

% towers/4: gets the steps taken in a towers of hanoi game.
towers(P,[LastGray],Vorige,[]).
towers(P,[GrayHead,NextGray|GraySeq],Vorige,[SpelHead|SpelSituaties]) :-
        getdisc(GrayHead,NextGray,Schijf),
        move(P,Schijf,Vorige,SpelHead),
        towers(P,[NextGray|GraySeq],SpelHead,SpelSituaties).