
%UNIX TIMESTAMP CONVERTER: https://www.unixtimestamp.com/index.php

%DEFINE CONSTANTS
max_contact_time(900). %900 secs = 15mins

%START ------ SAMPLE KNOWLEDGE BASE ---------------------------------------------------------
%List of infected students and time of covid contraction
infected(matt, 1610657119). %01/14/2021 @ 8:45pm (UTC) check locations between 01/01/2021 - 01/14/2021

%List of in-person events that the infected student went to in the last 2 weeks
%infected_event(Name_Of_Event, StartT, EndT) %StartT and EndT denotes that start and end time of an event
infected_event(hackathon, 1609848000, 1609934400).   %01/05/2021 @ 12:00pm (UTC)- 01/06/2021 @ 12:00pm (UTC)
infected_event(luncheon, 1610280000, 1610283600).    %01/10/2021 @ 12:00pm (UTC) - 01/10/2021 @ 1:00pm (UTC)
infected_event(career_fair, 1610013600, 1610031600). %01/07/2021 @ 10:00am (UTC) - 01/07/2021 @ 3:00pm (UTC)

%List of people that attended all events
attended_event(matt, hackathon).
attended_event(matt, luncheon).
attended_event(matt, career_fair).
attended_event(nguyen, hackathon).
attended_event(jack, hackathon).
attended_event(anusha, hackathon).
attended_event(jack, career_fair).


%list of potentially infected classmates (TO DO: replace with actual courses)
person_in_class(matt, 1).
person_in_class(jack, 1).
person_in_class(nguyen, 2).
person_in_class(matt, 2).

%Commuter or Roommate
commuter(troy).
commuter(tea).
commuter(eve).
commuter(jade).

roommates1(jack, matt).
roommates1(jack, nguyen).
roommates1(matt, anusha).
roommates1(anusha, jerry).

%TODO - Add more data
%List of each student highest temperature (Fahrenheit) in the last 2 weeks 
student_temp(matt, 110).
student_temp(jack, 98).
student_temp(anusha, 98).
student_temp(nguyen, 98).
student_temp(tom, 110).
student_temp(jerry, 110).

%medical history 
high_cholesterol([nguyen, jack, sam]).
heart_conditions([anusha, sam, matt, terry]).
diabetes([tom, jerry, maria]).
cancer([eve, ai, phuong, theresa]).

% person_in_buildings(Person, BuildingID, StartT, EndT). StartT & EndT in unix timestamps 
% this list is updated frequently based on sign-ins and sign-outs based on card swipe
person_in_buildings(matt, 1, 1610651386, 1610652386).
person_in_buildings(jack, 1, 1610651486, 1610652486).

%END ------ SAMPLE KNOWLEDGE BASE ---------------------------------------------------------

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
set([],[]).
set([H|T],[H|Out]) :- not member(H,T), set(T,Out).
set([H|T],Out) :- member(H,T), set(T,Out).

%END------FUNCTION HELPER ---------------------------------

%main program: generate a list of people who could have been in contact with the infected person in the past 2 weeks.
%conditions for potential_infected_student to hold:
    % 1) student X must be in close contact with Infected
    % 2) student X may not have high temp
    % 3) student X may not have any existing medical conditions
potential_infected_student(Infected, Potential) :- infected(Infected, _), get_close_contact(Infected, Potential), not high_temperature(Potential), not at_risk(Potential).
%conditions for critical_potential_infected_student to hold:
    % 1) student X must be in close contact with Infected
    % 2) student X has high temp
    % 3) student X does not have existing medical conditions
critical_potential_infected_student(Infected, Potential) :- infected(Infected, _), get_close_contact(Infected, Potential), high_temperature(Potential), at_risk(Potential).
%conditions for potential_infected_locations to hold:
    % 1) student X must be in close contact with Infected
    % 2) student X has high temp
    % 3) student X does not have existing medical conditions
potential_infected_locations(Infected, Potential, Location) :-
    close_contact(Infected, Potential),
    time_greater_than_fifteen(Infected, Potential),
    person_in_buildings(Infected, Location, _, _),
    person_in_buildings(Potential, Location, _, _),
    Infected \= Potential.
/*
%setof(Y, jack^roommates1(jack, Y), Result)
% check for students that have been in close contact with the Infected student
close_contact(Infected, Potential) :- close_conact_helper(Infected, Potential, [Infected]). 
close_contact_helper(Infected, Potential, Seen) :-
    close_contact1(Infected, Potential),
    not member(Potential, Seen),
    close_contact_helper(Infected, Potential, [Potential|Seen]). 
*/
% close contact conditions
close_contact(Infected, PersonX):- time_greater_than_fifteen(Infected, PersonX). 
close_contact(Infected, PersonX):- in_same_class(Infected, PersonX). 
close_contact(Infected, PersonX):- not commuter(Infected),  not commuter(PersonX), roommates(Infected, PersonX).
close_contact(Infected, PersonX):- went_to_event(Infected, PersonX, Event).

get_close_contacts(PersonX, Result) :- findall(X, close_contact(PersonX, X), Xs), set(Xs, Result).


% check student's temperature
high_temperature(Student) :- 
    student_temp(Student, Temp),
    Temp >= 100. %in fahrenheit

% check if student is at risk due to existing medical conditions
at_risk(Student) :- high_cholesterol(Ls), member(Student, Ls).
at_risk(Student) :- heart_conditions(Ls), member(Student, Ls).
at_risk(Student) :- cancer(Ls), member(Student, Ls).
at_risk(Student) :- diabetes(Ls), member(Student, Ls).

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
% SAMPLE QUERY: ?- findall(X, roommates(matt, X), Xs).

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

%check if two people went to the same event
went_to_infected_event(PersonX, Event) :- attended_event(PersonX, Event), infected_event(Event, _, _), infected(Infected, _), PersonX \= Infected.
%get all people that went to infected events
%SAMPLE QUERY: ?- findall(X, went_to_infected_event(X, Y), Xs).



/*
sample queries:
1) List infected student's classmates
    ?- in_same_class(name_of_infected, Student).

*/