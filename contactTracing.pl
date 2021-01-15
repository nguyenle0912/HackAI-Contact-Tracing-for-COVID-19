%DEFINE CONSTANTS
max_contact_time(900).

%START ------ Sample Database ---------------------------------------------------------
infected(matt).

person_in_class(matt, 1).
person_in_class(jack, 1).
person_in_class(nguyen, 2).
person_in_class(matt, 2).

commuter(britney).
commuter(tea).
roommates1(jack, nguyen).
roommates1(jack, matt).
roommates1(matt, anusha).

/* person_in_buildings(Person, BuildingID, StartT, EndT). StartT & EndT in unix timestamps 
    timestamps are updated frequently based on card sign-ins
*/
person_in_buildings(matt, 1, 1610651386, 1610652386).
person_in_buildings(jack, 1, 1610651486, 1610652486).

%infected(InfectedPerson, Time when they find out that they are tested positive)
infected(matt, 1610657119).

%END ------ Sample Database ---------------------------------------------------------


%main program: generate a list of potential infected students
potential_infected_student(Infected, Potential) :- close_contact(Infected, Potential); 



roommates(X, Y) :- roommates1(Y, X).
roommates(X, Y):- roommates1(X, Y).
roommates(X, Y) :- roommates1(X, Z), roommates(Z, Y), Z \= Y, Z \= X, X \= Y.
% ?- findall(X, roommates(matt, X), Xs).

%max and min implementations
max(X,Y,X) :- X > Y.
max(X,Y,Y) :- X =< Y.
min(X,Y,Y) :- X > Y.
min(X,Y,X) :- X =< Y.

%check if person X and Y are in the same class
in_same_class(X, Y):- person_in_class(X, Z), person_in_class(Y, Z), X \= Y.
%check if person X and Y are in the same building
in_same_building(X, Y):- in_same_building(X, Z), in_same_building(Y, Z), X \= Y.
%check if person X and Y are live in the same dorm or are roomates
on_campus(X) :- not commuter(X).
%check if person X and Y are related



%generate a list of people who could have been in contact with the infected person in the past 2 weeks.


%check if two people were in the in building for more than max_contact_time minute
time_greater_than_fifteen(PersonX, PersonY) :-
    person_in_buildings(PersonX, BuildingID, XstartT, XendT), 
    person_in_buildings(PersonY, BuildingID, YstartT, YendT),
    max(XstartT, YstartT, MaxStart),
    min(XendT, YendT, MinEnd),
    max_contact_time(MaxTime),
    PersonX \= PersonY,  
    MinEnd - MaxStart >= MaxTime. %900 is equivalent to 15mins
%p maybe true :- not -p (no evidence that p is not true therefore p is maybe true)
-time_greater_than_fifteen(PersonX, PersonY) :-
    person_in_buildings(PersonX, BuildingID, XstartT, XendT), 
    person_in_buildings(PersonY, BuildingID, YstartT, YendT),
    max(XstartT, YstartT, MaxStart),
    min(XendT, YendT, MinEnd),
    max_contact_time(MaxTime),
    PersonX \= PersonY,  
    MinEnd - MaxStart < MaxTime. %900 is equivalent to 15mins   

% close contact: within 6ft for time >= 15mins
close_contact(Infected, PersonX):- time_greater_than_fifteen(Infected, PersonX). 
close_contact(Infected, PersonX):- in_same_class(Infected, PersonX). 
close_contact(Infected, PersonX):- roommates(Infected, PersonX), not commuter(Infected),  not commuter(PersonX).

/*
sample queries:
1) List infected student's classmates
    ?- in_same_class(name_of_infected, Student).

*/