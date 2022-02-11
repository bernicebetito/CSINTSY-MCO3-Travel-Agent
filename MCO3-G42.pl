:- dynamic traveller/3.
:- dynamic purpose/2.
:- dynamic dutch/1.
:- dynamic documents/2.
:- dynamic work/2.
:- dynamic workSector/2.
:- dynamic essentialWorker/1.
:- dynamic business_purpose/2.
:- dynamic validBusiness/1.
:- dynamic vaccinated/3.
:- dynamic validVaccination/1.
:- dynamic returningResident/1.

start:-
    write("Hello! Enter consultation. to start."), nl.

returning_resident(Name):-
    (documents(Name, residentVisa), not(dutch(Name))) -> assertz(returningResident(Name)),
    writeln("You are considered a Returning Resident");
    writeln("You are unfortunately not considered a Returning Resident").

resident(Name):-
    write("Do you have a resident visa? "), read(Visa),
    write("Are you a Dutch Citizen? "), read(Citizen),
    (Visa = yes -> assertz(documents(Name, residentVisa)); Visa = no -> writeln("")),
    (Citizen = yes -> assertz(dutch(Name)); Citizen = no -> writeln("")), returning_resident(Name).

essential_worker(Name):-
    (work(Name, careWorker); work(Name, doctor); work(Name, nurse)) -> assertz(essentialWorker(Name)),
    write("You are considered an Essential Worker.");
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
     X = 5, write("Returning to previous question"), ofw(Name);
     writeln("Invalid Input!"), essential_worker_questions(Name)
    ), essential_worker(Name).

transport_sector(Name):-
    (work(Name, transportOfGoods); work(Name, containerShips); work(Name, bulkCarriers);
    work(Name, fishingBoats)) -> assertz(workSector(Name, transport)),
    writeln("You are recognized as working in the Transport Sector.");
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
     X = 6 -> write("Returning to previous question"), ofw(Name);
     writeln("Invalid Input!"), transport_questions(Name)
    ), transport_sector(Name).

energy_sector(Name):-
   (work(Name, oil); work(Name, gas); work(Name, wind);
    work(Name, energyCompany)) -> assertz(workSector(Name, energy)),
    write("You are recognized as working in the energy sector.");
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
    X = 6 -> write("Returning to previous question"), ofw(Name);
    writeln("Invalid input"), energy_questions(Name)
    ), energy_sector(Name).

seafarer_questions(Name) :-
    format("~nSelect what fits your situation:~n"),
    format("1 - In possession of sefarer's record book (not on commercial yachts and pleasure crafts)~n"),
    format("2 - On a commercial vessel with a length of 24 meters or more~n3 - Others~n4 - Exit~n"), read(X),
    (
    (X = 1; X = 2) -> assertz(workSector(Name, seafarer));
    X = 3 -> write("");
    X = 4 -> write("Returning to previous question"), ofw(Name);
    writeln("Invalid input"), seafarer_questions(Name)
    ).

journalist(Name) :-
    workSector(Name, journalist) -> writeln("You are considered a journalist.");
    writeln("You are unfortunately not considered a journalist.").

journalist_questions(Name) :-
    format("~nAre you a journalist engaged in topical news reporting requiring immediate physical presence?~n"), read(X),
    (
    X = yes -> assertz(workSector(Name, journalist));
    X = no -> write("");
    writeln("Invalid Input!"), journalist_questions(Name)
    ), journalist(Name).

cultural(Name) :-
    (work(Name, culturalInvited); work(Name, culturalPhysical); work(Name, culturalFinance)) -> assertz(workSector(Name, cultural)),
    writeln("You are considered as working in the Cultural and Creative Sector.");
    writeln("You are unfortunately not considered as working in the Cultural and Creative Sector.").

cultural_questions(Name) :-
    format("~nSelect what fits your situation:~n"),
    writeln("1 - Invited by the Dutch cultural or creative organisation concerned to engage in work for which you will be compensated"),
    writeln("2 - Your physical presence and participation is necessary for the overall execution of the activity in question"),
    writeln("3 - The income generated by the activity contributes substantially to the financial situation of the organisation concerned"),
    format("4 - Others~n5 - Exit~n"), read(X),
    (
    X = 1 -> assertz(work(Name, culturalInvited));
    X = 2 -> assertz(work(Name, culturalPhysical));
    X = 3 -> assertz(work(Name, culturalFinance));
    X = 4 -> write("");
    X = 5 -> ofw(Name)
    ), cultural(Name).

ofw(Name):-
    writeln("What sector are you working in?"),
    writeln("1  - Essential Worker"),
    writeln("2  - Cross-Border Commuter"),
    writeln("3  - Seasonal Worker"),
    writeln("4  - Transport Sector"),
    writeln("5  - Energy Sector"),
    writeln("6  - Urgent Technical Assist"),
    writeln("7  - Flight Crew"),
    writeln("8  - Seafarer"),
    writeln("9  - Diplomat"),
    writeln("10 - State Worker (In possession of service passport and need to travel)"),
    writeln("11 - Armed Forces"),
    writeln("12 - International / Humanitarian Organization Worker"),
    writeln("13 - Journalist"),
    writeln("14 - Elite Athlete"),
    writeln("15 - Researcher"),
    writeln("16 - Cultural and Creative Sector"),
    writeln("17 - Exit"),
    write("Choice: "), read(X),
    (
    X = 1 -> essential_worker_questions(Name);
    X = 2 -> assertz(workSector(Name, crossBorder));
    X = 3 -> essential_worker_questions(Name);
    X = 4 -> transport_questions(Name);
    X = 5 -> energy_questions(Name);
    X = 6 -> assertz(workSector(Name, urgentTech));
    X = 7 -> assertz(workSector(Name, flightCrew));
    X = 8 -> seafarer_questions(Name);
    X = 9 -> assertz(workSector(Name, diplomat));
    X = 10 -> assertz(workSector(Name, stateWorker));
    X = 11 -> assertz(workSector(Name, armedForce));
    X = 12 -> assertz(workSector(Name, orgWorker));
    X = 13 -> journalist_questions(Name);
    X = 14 -> assertz(workSector(Name, eliteAthlete));
    X = 15 -> write("Researcher");
    X = 16 -> cultural_questions(Name);
    X = 17 -> write("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid input"), ofw(Name)
    ).

business(X) :-
    (business_purpose(X, urgent); business_purpose(X, physical); business_purpose(X, economy); business_purpose(X, invite);
     business_purpose(X, investment); business_purpose(X, organization)) -> assertz(validBusiness(X)), assertz(purpose(X, business)),
     writeln("Your business is considered valid");
     writeln("Your business is unfortunately considered invalid.").

business_invitation(Name) :-
    writeln("Has the travel date been confirmed? "),
    format("1 - Yes~n2 - No~n"), read(X),
    writeln("Does the invitation show the following conditions?"),
    format("The trip relates to significant confirmed or potential direct foreign investment in the Netherlands which, within 3 years, will lead to:~n"),
    format("-> Creation of at least five new jobs or~n-> Investment of at least €500,000 in the Netherlands"),
    format("~n1 - Yes~n2 - No~n3 - Exit"), read(Y),
    (
    (X = 1, Y = 1) -> assertz(business_purpose(Name, invite));
    Y = 3 -> write("Returning to previous question"), business_questions(Name);
    writeln("Invalid Input!"), business_invitation(Name)
    ).

business_organization(Name) :-
    writeln("What kind of organization?"),
    format("1 - Has at least 10 FTEs and/or annual turnover of at least €2 Million~n"),
    format("2 - Smaller organization invovled in socially relevant ground-breaking contributions to research, innovation, sustainability, or public health~n"),
    format("3 - Others~n4 - Exit~n"), read(X),
    (
    (X = 1; X = 2) -> assertz(business_purpose(Name, organization));
    X = 4 -> write("Returning to previous question"), business_questions(Name);
    writeln("Invalid Input!"), business_organization(Name)
    ).

business_questions(Name) :-
    format("~nPlease select your Official Business Reason for travelling:~n"),
    format("1 - Urgent trip which cannot be postponed~n2 - Physical presence is required / Digital alternatives are not feasible~n"),
    format("3 - Vitally Important for the Dutch economy~n4 - Invite from a Dutch private or public sector party~n"),
    format("5 - Trip is related to potential direct foreign investment in Netherlands contributing to any of the following:~n"),
    format("  -> Strengthening the Dutch Innovation Capacity~n  -> Making the Dutch Economy Sustainable~n  -> Further Digitalization of the Dutch Economy~n"),
    format("6 - Of major importance to a specific organization based in Netherlands~n7 - Others~n8 - Exit~n"), read(X),
    (
    X = 1 -> assertz(business_purpose(Name, urgent));
    X = 2 -> assertz(business_purpose(Name, physical));
    X = 3 -> assertz(business_purpose(Name, economy));
    X = 4 -> business_invitation(Name);
    X = 5 -> assertz(business_purpose(Name, investment));
    X = 6 -> business_organization(Name);
    X = 8 -> write("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid Input!"), business_questions(Name)
    ), business(Name).

valid_vaccination(X, Y, Z) :-
    (vaccinated(X, Y, Z), (((Z = pfizer; Z = moderna; Z = astrazeneca), Y > 14);
                          (Z = johnsonAndJohnson), Y > 28) -> assertz(validVaccination(X)),
     writeln("Your Vaccination is Valid");
     writeln("Your Vaccination is unfortunately invalid.")
    ).

vaccine_questions(X) :-
    format("~nWhat vaccine were you vaccinated with?~n"),
    format("1 - Pfizer~n2 - Moderna~n3 - Astrazeneca~n4 - Johnson & Johnson~n5 - Others~n6 - Exit~n"),
    read(Z),
    write("How many days ago did you get vaccinated? "), read(Y),
    (
    Z = 1 -> assertz(vaccinated(X, Y, pfizer)), valid_vaccination(X, Y, pfizer);
    Z = 2 -> assertz(vaccinated(X, Y, moderna)), valid_vaccination(X, Y, moderna);
    Z = 3 -> assertz(vaccinated(X, Y, astrazeneca)), valid_vaccination(X, Y, astrazeneca);
    Z = 4 -> assertz(vaccinated(X, Y, johnsonAndJohnson)), valid_vaccination(X, Y, johnsonAndJohnson);
    Z = 5 -> write("Returning to previous question"), valid_vaccination(X, Y, others);
    writeln("Invalid input!"), nl, vaccine_questions(X)
    ).

tourist(Name) :-
    format("~nAre you fully vaccinated?~n"),
    format("1 - Yes~n2 - No~n3 - Exit"), read(X),
    (
    X = 1 -> vaccine_questions(Name);
    X = 2 -> valid_vaccination(Name, 0, others);
    X = 3 -> write("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid Input!"), tourist(Name)
    ).

documents_list(Name) :-
  format("~nRequired Documents~n"),
  purpose(Name, tourist) -> (
    writeln("* Vaccine Declaration Form"),
    writeln("   - Must be accomplished and can be found at https://www.government.nl/topics/coronavirus-covid-19/documents/publications/2021/07/01/vaccine-declaration-covid-19"),
    writeln("* Proof of Vaccination"),
    writeln("   - Can be either paper or digital certificate issued by a country taking part in the EU Digital Covid Certificate System"),
    writeln("* Proof of Return Journey"),
    writeln("   - Return ticket issued by airline, bus company, or railway company"),
    writeln("* Visa"),
    writeln("* Negative Test Result"),
    writeln("   - Too many to mention")
  );
  purpose(Name, business) -> (
    writeln("* Vaccine Declaration Form"),
    writeln("   - Must be accomplished and can be found at https://www.government.nl/topics/coronavirus-covid-19/documents/publications/2021/07/01/vaccine-declaration-covid-19"),
    writeln("* Proof of Vaccination"),
    writeln("   - Can be either paper or digital certificate issued by a country taking part in the EU Digital Covid Certificate System"),
    writeln("* Proof of Return Journey"),
    writeln("   - Return ticket issued by airline, bus company, or railway company"),
    writeln("* Visa"),
    writeln("* Negative Test Result"),
    writeln("   - Too many to mention"),
    writeln("* Diplomatic note"),
    writeln("   -  Issued by a Dutch embassy stating that you fall under an exemption to the entry ban"),
    writeln("* Hotel Booking")
  ).

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
    X = 4 -> business_questions(Name);
    X = 5 -> tourist(Name), assertz(purpose(Name, tourist));
    X = 6 -> write("Exiting system...");
    writeln("Invalid input"), purpose_of_travel(Name)
    ), documents_list(Name).

basic_information(Name):-
    write("What is your name? (Enclose in quotes) "), nl, read(Name),
    write("What is your age? (Enclose in quotes) "), nl, read(Age),
    write("What is your Nationality? (Enclose in quotes) "), nl, read(Citizenship),
    assertz(traveller(Name, Age, Citizenship)).

consultation:-
    basic_information(Name), nl,
    purpose_of_travel(Name).
