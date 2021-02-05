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
% rules
route(X, Y) :- flight(X, Y).
route(X, Y) :- flight(X, Z),route(Z, Y).
