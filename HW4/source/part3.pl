% Author: Akif Kartal
% 171044098

% knowledge base

when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).
%-----------------------
where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).
%------------------------
enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).
%---------------------------
%----3.1----------------------
schedule(S, P, T) :- enroll(S, C),where(C,P), when(C, T).
%----3.2----------------------
usage(P, T) :- where(X,P), when(X,T).
%----3.3----------------------
conflict(X, Y) :- not(X==Y),where(X,A), where(Y,B) , A == B.
conflict(X, Y) :- not(X==Y),when(X,A), when(Y,B) , A == B.
%----3.4----------------------
meet(X,Y) :- not(X==Y), enroll(X,A), enroll(Y, B), A == B.
