:- dynamic dutch/1.
:- dynamic documents/2.
:- dynamic work/2.
:- dynamic vaccinated/3.

start:-
    write("Hello World!"), nl.

transport_sector(Name):-
    (work(Name, transportOfGoods); work(Name, containerShips); work(Name, bulkCarriers);
    work(Name, fishingBoats)) -> writeln("You are recognized as working in the Transport Sector.");
    writeln("I'm sorry, unfortunately you are not recognized as working in the Transport Sector.").

transport_questions(Name):-
    writeln("What field in the Transport Sector are you working in?"),
    format("1 - Transport of Goods~n2 - Container Ships~n3 - Bulk Carriers~n4 - Fishing Boats~n5 - Others~n6 - Exit~n"),
    read(X),
    (
     X = 1 -> assertz(work(Name, transportOfGoods));
     X = 2 -> assertz(work(Name, containerShips));
     X = 3 -> assertz(work(Name, bulkCarriers));
     X = 4 -> assertz(work(Name, fishingBoats));
     X = 5 -> writeln("");
     X = 6 -> ofw(Name);
     writeln("Invalid Input!"), transport_questions(Name)
    ), transport_sector(Name).

energy_sector(Name):-
   (work(Name, oil); work(Name, gas); work(Name, wind);
    work(Name, energyCompany)) -> write("You are recognized as working in the energy sector.");
    write("I'm sorry, unfortunately you are not recognized as working in the energy sector.").

energy_questions(Name):-
    writeln("What field in the energy sector are you working in?"),
    writeln("1 - Oil platform"),
    writeln("2 - Gas platform"),
    writeln("3 - Offshore wind farms"),
    writeln("4 - Offshore company that provides energy services"),
    writeln("5 - None of the above"),
    writeln("6 - Exit"),
    write("Choice: "), read(X),
    (
    X = 1 -> assertz(work(Name, oil));
    X = 2 -> assertz(work(Name, gas));
    X = 3 -> assertz(work(Name, wind));
    X = 4 -> assertz(work(Name, energyCompany));
    X = 5 -> write("");
    X = 6 -> ofw(Name);
    writeln("Invalid input"), energy_questions(Name)
    ), energy_sector(Name).

essential_worker(Name):-
    (work(Name, careWorker); work(Name, doctor); work(Name, nurse)) -> write("You are considered an Essential Worker.");
    write("You are unfortunately not an essential worker.").

essential_worker_questions(Name) :-
    writeln("What is your profession?"),
    format("1 - Care Worker~n2 - Doctor~n3 - Nurse~n4 - Others~n5 - Exit~n"),
    read(X),
    (
     X = 1, assertz(work(Name, careWorker));
     X = 2, assertz(work(Name, doctor));
     X = 3, assertz(work(Name, nurse));
     X = 4, writeln("");
     X = 5, ofw(Name);
     writeln("Invalid Input!"), essential_worker_questions(Name)
    ), essential_worker(Name).

ofw(Name):-
    writeln("What sector are you working in?"),
    writeln("1 - Essential Worker"),
    writeln("2 - Energy Sector"),
    writeln("3 - Transport Sector"),
    writeln("4 - Exit"),
    write("Choice: "), read(X),
    (
    X = 1 -> essential_worker_questions(Name);
    X = 2 -> energy_questions(Name);
    X = 3 -> transport_questions(Name);
    X = 4 -> write("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid input"), ofw(Name)
    ).

valid_vaccination(X, Y, Z) :-
    (vaccinated(X, Y, Z),
     (Z = pfizer; Z = moderna; Z = astrazeneca), Y > 14, writeln("Your Vaccination is Valid");
     (Z = johnsonAndJohnson), Y > 28, writeln("Your Vaccination is Valid");
     writeln("Your Vaccination is unfortunately invalid.")
    ).

vaccine_questions(X) :-
    writeln("What vaccine were you vaccinated with? "),
    format("1 - Pfizer~n2 - Moderna~n3 - Astrazeneca~n4 - Johnson & Johnson~n5 - Others~n"),
    read(Z),
    write("How many days ago did you get vaccinated? "), read(Y),
    (
     Z = 1 -> assertz(vaccinated(X, Y, pfizer)), valid_vaccination(X, Y, pfizer);
     Z = 2 -> assertz(vaccinated(X, Y, moderna)), valid_vaccination(X, Y, moderna);
     Z = 3 -> assertz(vaccinated(X, Y, astrazeneca)), valid_vaccination(X, Y, astrazeneca);
     Z = 4 -> assertz(vaccinated(X, Y, johnsonAndJohnson)), valid_vaccination(X, Y, johnsonAndJohnson);
     Z = 5 -> valid_vaccination(X, Y, others);
     writeln("Invalid input!"), nl, vaccine_questions(X)
     ).

tourist(Name) :-
    write("Are you fully vaccinated? "), read(X),
    (
     X = yes, vaccine_questions(Name);
     X = no, write("Your Vaccination is unfortunately invalid.");
     writeln("Invalid Input!"), tourist(Name)
     ).

returning_resident(Name):-
    (documents(Name, residentVisa), not(dutch(Name))) -> writeln("You are considered a Returning Resident");
    writeln("You are unfortunately not considered a Returning Resident").

resident(Name):-
    write("Do you have a resident visa? "), read(Visa),
    write("Are you a Dutch Citizen? "), read(Citizen),
    (Visa = yes -> assertz(documents(Name, residentVisa)); Visa = no -> writeln("")),
    (Citizen = yes -> assertz(dutch(Name)); Citizen = no -> writeln("")), returning_resident(Name).

purpose_of_travel(Name):-
    write("Hello "), write(Name), write(" what is your purpose of travel?"), nl,
    writeln("1 - I am a Dutch citizen"),
    writeln("2 - I am a non-Dutch resident of the Netherlands"),
    writeln("3 - I am an Overseas Filipino Worker in the Netherlands"),
    writeln("4 - I am on a business trip"),
    writeln("5 - I am a tourist"),
    writeln("6 - Exit"),
    write("Choice: "), read(X),
    (
    X = 2 -> resident(Name);
    X = 3 -> ofw(Name);
    X = 5 -> tourist(Name);
    X = 6 -> write("Exiting system...");
    writeln("Invalid input"), purpose_of_travel(Name)
    ).

basic_information(Name):-
    write("What is your name? (Enclose in quotes) "), nl, read(Name).

consultation:-
    basic_information(Name), nl,
    purpose_of_travel(Name).

