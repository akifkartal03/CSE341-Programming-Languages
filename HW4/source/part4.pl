% Author: Akif Kartal
% 171044098

%------4.1----------------
element(E, [E|_]).
element(E, [_|S]):- element(E, S).
%----------------------------------

%-----4.2----------------------

get_union([], Y, Y).
get_union([E|T], K, [E|Y]):-	
	get_union(T, K, Y).

union(S1, S2, S3):-
	get_union(S1, S2, Y),
	equivalent(S3, Y).
%----------------------------------
%----------4.3---------------------

intersect(S1,S2,S3) :- intersectHelper(S1,S2,X), equivalent(X,S3).
intersectHelper([], _, []).
intersectHelper([E|S1], S2, [E|S3]):- element(E, S2), !, intersectHelper(S1, S2, S3).
intersectHelper([_|S1], S2, S3):- intersectHelper(S1, S2, S3).

%----------------------------------
%----------4.4---------------------
equivalent(S1, S2) :- equivalentHelper(S1,S2), equivalentHelper(S2,S1).
equivalentHelper([],_).
equivalentHelper([E|S1],S2):- element(E,S2), equivalentHelper(S1,S2).
