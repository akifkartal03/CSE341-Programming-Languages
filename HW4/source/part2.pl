% Author: Akif Kartal
% 171044098

% knowledge base
flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).
%-------------------------
flight(edirne,edremit).
flight(edremit,erzincan).
%-------------------------
flight(izmir,isparta).
flight(isparta,burdur).
%-------------------------
flight(konya,antalya).
%-------------------------
flight(gaziantep,antalya).
%-------------------------
flight(ankara,konya).
flight(ankara,van).
%------------------------
flight(rize,van).
%-------------------------
distance(istanbul, izmir, 329).
distance(istanbul, antalya, 483).
distance(istanbul, gaziantep, 848).
distance(istanbul, ankara, 352).
distance(istanbul, van, 1263).
distance(istanbul, rize, 968).
distance(edirne,edremit, 915).
distance(edremit,erzincan, 1044).
distance(izmir,isparta, 309).
distance(isparta,burdur, 25).
distance(gaziantep, antalya, 593).
distance(konya, antalya, 193).
distance(ankara, konya, 228).
distance(ankara, van, 921).
distance(rize, van, 373).
%---------------------------------------

% rules
addList([],L,L).
addList(Element,L,[Element|L]).

member(X,[X|_]).
member(X,[_|Tail]):- member(X,Tail).

sroute(A,B,D) :-
       travel(A,B,D).

travel(A,B,D) :-
       flight(A,B),
       distance(A,B,D),!.

travel(A,B,Visited,Path,D) :-
       flight(A,C),
       C \== B,
       \+member(C,Visited),
       distance(A,C,Old),
       travel(C,B,[C|Visited],Path,D2),
       D is Old+ D2.
