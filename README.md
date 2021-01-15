# HackAI-Contact-Tracing-for-COVID-19

## Inspiration
Contact tracing is a process used by public health ministries to help stop the spread of infectious disease, such as COVID-19, within a community. Once a person is positive for coronavirus, it is very important to identify other people who may have been infected by the patients diagnosed. To identify infected people, the authorities follow the activity of patients diagnosed in the last 14 days. 

## What it does
Our application takes in input about UTD students such as their roommates, classes, existing medical conditions, body temperature, and public events they've been to and allows for users to make various queries to find students at UTD that may have been infected by another UTD student that tested positive for COVID-19 in the last 14 days. 

## Sample Queries
1) Get list of potentially infected students
```
?- get_all_potential_infected_students(Result).
```
2) Get list of potentially infected students that are at risk and should be prioritized
```
?- get_all_critical_students(Result).
```
3) Get list of people that went to infected events who are not already infected
```
?- went_to_infected_events(Result, Event).
```
4) Get list of events that that the infected students went to in the past 2 weeks
```
?- list_of_infected_events(Result).
```
5) Find all roommates of infected students
```
?- find_all_roommates(Infected, Result).
```
6) Get list of potentially infected locations
```
?- potential_infected_locations(Infected, Location).
```
7) Get list of infected student's classmates
```
?- find_all_classmates(Infected, Result).
```
8) Get list of students swith high temperatures
```
?- get_students_with_high_temperatures(Result).
```
9) Get list of students who are at risk
```
?- get_students_at_high_risk(Result).
```

---
## How we built it
We used s(CASP) and Prolog for defining rules used to determine potentially infected students and if they are critical. 

## Challenges we ran into
There was a bit of learning curve since all of us had only had minimal exposure to Prolog and s(CASP). There were a couple of times when our program would go into an infinite loop. We had to be creative and come up with more complex functions to prevent that from happening. Another challenge that we faced was eliminating duplicates in the output list such that it does not output duplicates of potentially infected names or locations.

## Accomplishments that we're proud of
We're proud to have developed our skills in Prolog and logic programing. We're also proud we created an application that is very relevant to current real world situations. 

## What we learned
We learned how to make more complex functions in Prolog and how s(CASP) works. We also learned a lot about the techniques for contact tracing used by authorities as we tried to mirror their logic in our application. 

## What's next for Contact Tracer
Instead of using hard coded inputs, we can use actual data collected through a survey sent to students. We can also develop a user-interface for our application so it can be easier and less confusing for users to make queries. 


