
%UNIX TIMESTAMP CONVERTER: https://www.unixtimestamp.com/index.php

%DEFINE CONSTANTS
max_contact_time(900). %900 secs = 15mins

%START ------ SAMPLE KNOWLEDGE BASE ---------------------------------------------------------
%List of infected students and time of covid contraction
infected(matt, 1610657119). %01/14/2021 @ 8:45pm (UTC) check locations between 01/01/2021 - 01/14/2021
infected(jack, 1610280000). %01/10/2021 @ 12:00pm (UTC)


% ?- infected(X, _).

%List of in-person events that the infected student went to in the last 2 weeks
%infected_event(Name_Of_Event, StartT, EndT) %StartT and EndT denotes that start and end time of an event
infected_event(hackathon, ecsw, 1609848000, 1609934400).   %01/05/2021 @ 12:00pm (UTC)- 01/06/2021 @ 12:00pm (UTC)
infected_event(luncheon, jsom, 1610280000, 1610283600).    %01/10/2021 @ 12:00pm (UTC) - 01/10/2021 @ 1:00pm (UTC)
infected_event(career_fair, visitor_center, 1610013600, 1610031600). %01/07/2021 @ 10:00am (UTC) - 01/07/2021 @ 3:00pm (UTC)
infected_event(career_fair, science_building, 1610013600, 1610031600). %01/07/2021 @ 10:00am (UTC) - 01/07/2021 @ 3:00pm (UTC)

%List of people that attended all events
%---attended_event(Name, Event_Name, Building) 
attended_event(matt, hackathon).
attended_event(matt, luncheon).
attended_event(matt, career_fair).
attended_event(nguyen, hackathon).
attended_event(jack, hackathon).
attended_event(anusha, hackathon).
attended_event(jack, career_fair).


%list of potentially infected classmates (TO DO: replace with actual courses)
person_in_class(matt, cs4337).
person_in_class(jack, cs3354).
person_in_class(nguyen, cs4337).
person_in_class(matt, cs2337).
person_in_class(eve, cs1333).
person_in_class(anusha, cs2337).

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
person_in_buildings(matt, science_building, 1610651386, 1610652386).
person_in_buildings(jack, ecss, 1610651486, 1610652486).
person_in_buildings(nguyen, ecsw, 1610651386, 1610652386).
person_in_buildings(tea, jsom, 1610651486, 1610652486).
person_in_buildings(anusha, ecss, 1610651386, 1610652386).
person_in_buildings(eve, science_building, 1610651486, 1610652486).

%END ------ SAMPLE KNOWLEDGE BASE ---------------------------------------------------------