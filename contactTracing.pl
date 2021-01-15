#include 'knowledgeBase.pl'.

%START ------FUNCTION HELPER ---------------------------------

%max and min implementations
max(X,Y,X) :- X > Y.
max(X,Y,Y) :- X =< Y.
min(X,Y,Y) :- X > Y.
min(X,Y,X) :- X =< Y.

%member implementation
member(X,[X|_]).
member(X,[Y|T]) :- member(X,T).

%duplicate removal
% An empty list is a set.
set([], []).

% Put the head in the result,
% remove all occurrences of the head from the tail,
% make a set out of that.
set([H|T], [H|T1]) :- 
    remv(H, T, T2),
    set(T2, T1).

% Removing anything from an empty list yields an empty list.
remv(_, [], []).

% If the head is the element we want to remove,
% do not keep the head and
% remove the element from the tail to get the new list.
remv(X, [X|T], T1) :- remv(X, T, T1).

% If the head is NOT the element we want to remove,
% keep the head and
% remove the element from the tail to get the new tail.
remv(X, [H|T], [H|T1]) :-
    X \= H,
    remv(X, T, T1).

% append
append([], L, L).
append([H|T], L2, [H|L3]) :- append(T,L2,L3).
append(H, L, [H|L]).

%END------FUNCTION HELPER ---------------------------------

%main program: generate a list of people who could have been in contact with the infected person in the past 2 weeks.
/*
potential :- not (p & q) :- not p or not q
critial :- p & q :- not(not p or not q)
*/

%check if this student has been infected
already_infected(Infected) :- infected(Infected, _).

%check if potential student has high temp and existing medical conditions
critical_health_status(Potential):-
    high_temperature(Potential),
    at_risk(Potential).

% close contact conditions
close_contact(Infected, PersonX):- infected(Infected, _), time_greater_than_fifteen(Infected, PersonX). 
close_contact(Infected, PersonX):- infected(Infected, _), in_same_class(Infected, PersonX). 
close_contact(Infected, PersonX):- infected(Infected, _), not commuter(Infected),  not commuter(PersonX), roommates(Infected, PersonX).
close_contact(Infected, PersonX):- infected(Infected, _), went_to_infected_event(Infected, PersonX, Event).

%get roommates of infected students 
roommates2(X, Y) :- roommates1(X, Y).
roommates2(X, Y) :- roommates1(Y, X).
roommates(X, Y)  :- roommatesHelper(X, Y, [X]).
roommatesHelper(X, Y, Seen) :-  
    roommates2(X, Y),
    not member(Y, Seen).
roommatesHelper(X, Y, Seen) :-
    roommates2(X, Z),
    not member(Z, Seen),
    roommatesHelper(Z, Y, [Z | Seen]).

% check students temperature
high_temperature(Student) :- 
    student_temp(Student, Temp),
    Temp >= 100. %in fahrenheit

% check if student is at risk due to existing medical conditions
at_risk(Student) :- high_cholesterol(Ls), member(Student, Ls).
at_risk(Student) :- heart_conditions(Ls), member(Student, Ls).
at_risk(Student) :- cancer(Ls), member(Student, Ls).
at_risk(Student) :- diabetes(Ls), member(Student, Ls).

%check if person X and Y are in the same class
in_same_class(X, Y):- person_in_class(X, Z), person_in_class(Y, Z), X \= Y.
%check if person X and Y are in the same building
in_same_building(X, Y):- in_same_building(X, Z), in_same_building(Y, Z), X \= Y.
%check if person X and Y are live in the same dorm or are roomates
on_campus(X) :- not commuter(X).

%check if two people were in the in the same building for more than 15 minutes (900 seconds)
time_greater_than_fifteen(PersonX, PersonY) :-
    person_in_buildings(PersonX, BuildingID, XstartT, XendT), 
    person_in_buildings(PersonY, BuildingID, YstartT, YendT),
    max(XstartT, YstartT, MaxStart),
    min(XendT, YendT, MinEnd),
    max_contact_time(MaxTime), %900 seconds
    PersonX \= PersonY,  
    MinEnd - MaxStart >= MaxTime. %900 is equivalent to 15mins
%p maybe true :- not -p (no evidence that p is not true therefore p is maybe true)
-time_greater_than_fifteen(PersonX, PersonY) :-
    person_in_buildings(PersonX, BuildingID, XstartT, XendT), 
    person_in_buildings(PersonY, BuildingID, YstartT, YendT),
    max(XstartT, YstartT, MaxStart),
    min(XendT, YendT, MinEnd),
    max_contact_time(MaxTime), %900 seconds
    PersonX \= PersonY,  
    MinEnd - MaxStart < MaxTime. %900 is equivalent to 15mins   

%conditions for potential_infected_students to hold:
    % 1) student Potential is not already infected
    % 2) student Potential must be in close contact with Infected
    % 3) student Potential may not have high temp
    % 4) student Potential may not have any existing medical conditions
potential_infected_student(Infected, Potential) :-
    not already_infected(Potential), 
    close_contact(Infected, Potential), 
    not critical_health_status(Potential).
potential_infected_students(Infected, Result) :- 
    infected(Infected, _),
    findall(X, potential_infected_student(Infected, X), Xs), 
    set(Xs, Result).

%conditions for critical_potential_infected_student to hold:
    % 1) student Potential is not already infected
    % 2) student Potential must be in close contact with Infected
    % 3) student Potential has high temp
    % 4) student Potential does have existing medical conditions
critical_potential_infected_student(Infected, Potential) :-
    not already_infected(Potential),
    close_contact(Infected, Potential),
    critical_health_status(Potential).
critical_potential_infected_students(Infected, Result) :- 
    infected(Infected, _),
    findall(X, critical_potential_infected_student(Infected, X), Xs), 
    set(Xs, Result).

%check if a potentially infected student went to an infected event
went_to_infected_event(Potential, Event) :- 
    attended_event(Potential, Event), 
    infected_event(Event, _, _, _), 
    not already_infected(Potential).

%get list of students who went to infected events (except the infected)
went_to_infected_events(Result, Event) :-
    findall(X, went_to_infected_event(X, Event), Xs),
    set(Xs, Result).

%get list of infected events that the infected students went to
list_of_infected_events(Result) :-
    findall(X, infected_event(X, _, _, _), Xs),
    set(Xs, Result).

%get list of roommates of the infected students
find_all_roommates(Infected, Result) :-
    findall(X, roommates(Infected, X), Xs), 
    set(Xs, Result).

%conditions for potential_infected_locations to hold:
    % 1) student X must be in close contact with Infected
    % 2) student X has high temp
    % 3) student X does not have existing medical conditions
potential_infected_location(Infected, Location) :-
    person_in_buildings(Infected, Location, _, _).
potential_infected_location(Infected, Location) :-
    infected_event(Infected_Event, Location, _, _).
potential_infected_locations(Infected, Result) :- 
    findall(X, potential_infected_location(Infected, X), Xs), 
    set(Xs, Result).

%get list of classmates of the infected students
find_all_classmates(Infected, Result) :-
    findall(X, in_same_class(Infected, X), Xs), 
    set(Xs, Result).

%get list of students with high temperatures 
get_students_with_high_temperatures(Result) :-
    findall(Student, high_temperature(Student), Xs),
    set(Xs, Result).

%get list of students who are at high risk
get_students_at_high_risk(Result) :-
    findall(Student, at_risk(Student), Xs),
    set(Xs, Result).

%SAMPLE QUERY: ?- findall(X, went_to_infected_event(X, Y), Xs).

