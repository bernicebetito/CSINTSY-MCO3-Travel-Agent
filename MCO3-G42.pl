:- dynamic traveller/3.
:- dynamic purpose/2.
:- dynamic transportation/2.
:- dynamic airline/2.
:- dynamic dutch/1.
:- dynamic european/1.
:- dynamic euroResident/1.
:- dynamic documents/2.
:- dynamic work/2.
:- dynamic workSector/2.
:- dynamic essentialWorker/1.
:- dynamic research_job/2.
:- dynamic business_purpose/2.
:- dynamic validBusiness/1.
:- dynamic vaccinated/3.
:- dynamic validVaccination/1.
:- dynamic returningResident/1.

start:-
    write("Hello! Enter consultation. to start."), nl.

citizen(Name) :-
    (dutch(Name); european(Name)) -> (assertz(purpose(Name, citizen)), writeln("You are considered a valid citizen."));
    writeln("You are unfortunately not considered a valid citizen.").

citizen_questions(Name) :-
    nl, writeln("Select what best fits your situation:"),
    format("1 - Dutch Citizen~n2 - Citizen of any other EU or Schengen Area Country~n3 - Others~n4 - Exit~n"),
    read(X),
    (
     X = 1 -> assertz(dutch(Name));
     X = 2 -> assertz(european(Name));
     X = 3 -> write("");
     X = 4 -> (writeln("Returning to previous question"), purpose_of_travel(Name));
     writeln("Invalid Input!"), citizen_questions(Name)
    ), citizen(Name).

returning_resident(Name) :-
    ((euroResident(Name); documents(Name, residentPermit); documents(Name, longStayVisa); documents(Name, notifImmigration)),
    not(dutch(Name))) -> assertz(returningResident(Name)), assertz(purpose(Name, resident)), writeln("You are considered a Returning Resident");
    writeln("You are unfortunately not considered a Returning Resident").

resident_questions(Name) :-
    format("~nSelect what best fits your situation:~n"),
    writeln("1 - National or resident of one of the following:"),
    format("  - Norway~n  - Iceland~n  - Switzerland~n  - Liechtenstein~n  - Monaco~n  - Andorra~n  - San Marino~n  - Vatican City~n"),
    writeln("2 - National of a non-EU country and have a residence card or residence permit in accordance with Directive 2003/109/EEC"),
    writeln("3 - National of a non-EU country and derive your right of residence from other EU directives or the national law of a Schengen Area Country"),
    format("4 - Long-stay visa holder~n5 - In possession of a letter of notification from the Immigration and Naturalisation Service for long-term stay~n"),
    format("6 - In possession of a valid residence permit for the Netherlands~n7 - Others~n8 - Exit~n"), read(X),
    (
    X = 1 -> assertz(euroResident(Name));
    X = 2 -> assertz(documents(Name, residentPermit));
    X = 3 -> assertz(euroResident(Name));
    X = 4 -> assertz(documents(Name, longStayVisa));
    X = 5 -> assertz(documents(Name, notifImmigration));
    X = 6 -> assertz(documents(Name, residentPermit));
    X = 7 -> write("");
    X = 8 -> writeln("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid Input!"), resident_questions(Name)
    ), returning_resident(Name).

essential_worker(Name) :-
    (work(Name, careWorker); work(Name, doctor); work(Name, nurse)) -> (assertz(essentialWorker(Name)), assertz(purpose(Name, ofw)),
    writeln("You are considered an Essential Worker."));
    writeln("You are unfortunately not an essential worker.").

essential_worker_questions(Name) :-
    nl, writeln("What is your profession?"),
    format("1 - Care Worker~n2 - Doctor~n3 - Nurse~n4 - Others~n5 - Exit~n"),
    read(X),
    (
     X = 1 -> assertz(work(Name, careWorker));
     X = 2 -> assertz(work(Name, doctor));
     X = 3 -> assertz(work(Name, nurse));
     X = 4 -> writeln("");
     X = 5 -> writeln("Returning to previous question"), ofw(Name);
     writeln("Invalid Input!"), essential_worker_questions(Name)
    ), essential_worker(Name).

transport_sector(Name) :-
    (work(Name, transportOfGoods); work(Name, containerShips); work(Name, bulkCarriers); work(Name, fishingBoats))
    -> assertz(workSector(Name, transport)), assertz(purpose(Name, ofw)),
    writeln("You are recognized as working in the Transport Sector.");
    writeln("I'm sorry, unfortunately you are not recognized as working in the Transport Sector.").

transport_questions(Name) :-
    nl, writeln("What field in the Transport Sector are you working in?"),
    format("1 - Transport of Goods~n2 - Container Ships~n3 - Bulk Carriers~n4 - Fishing Boats~n5 - Others~n6 - Exit~n"),
    read(X),
    (
     X = 1 -> assertz(work(Name, transportOfGoods));
     X = 2 -> assertz(work(Name, containerShips));
     X = 3 -> assertz(work(Name, bulkCarriers));
     X = 4 -> assertz(work(Name, fishingBoats));
     X = 5 -> writeln("");
     X = 6 -> writeln("Returning to previous question"), ofw(Name);
     writeln("Invalid Input!"), transport_questions(Name)
    ), transport_sector(Name).

energy_sector(Name) :-
   (work(Name, oil); work(Name, gas); work(Name, wind); work(Name, energyCompany)) ->
   assertz(workSector(Name, energy)), assertz(purpose(Name, ofw)),
    writeln("You are recognized as working in the energy sector.");
    writeln("I'm sorry, unfortunately you are not recognized as working in the energy sector.").

energy_questions(Name) :-
    nl, writeln("What field in the energy sector are you working in?"),
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
    X = 6 -> writeln("Returning to previous question"), ofw(Name);
    writeln("Invalid input"), energy_questions(Name)
    ), energy_sector(Name).

seafarer(Name) :-
    (work(Name, recordBookSeafarer); work(Name, commercialVesselSeafarer)) ->
    (assertz(workSector(Name, seafarer)), assertz(purpose(Name, ofw)),writeln("You are considered a valid seafarer."));
    writeln("You are unfortunately not considered a valid seafarer.").

seafarer_questions(Name) :-
    format("~nSelect what best fits your situation:~n"),
    format("1 - In possession of sefarer's record book (not on commercial yachts and pleasure crafts)~n"),
    format("2 - On a commercial vessel with a length of 24 meters or more~n3 - Others~n4 - Exit~n"), read(X),
    (
    X = 1 -> assertz(work(Name, recordBookSeafarer));
    X = 2 -> assertz(work(Name, commercialVesselSeafarer));
    X = 3 -> assertz(work(Name, othersSeafarer));
    X = 4 -> writeln("Returning to previous question"), ofw(Name);
    writeln("Invalid input"), seafarer_questions(Name)
    ), seafarer(Name).

journalist(Name) :-
    work(Name, urgentJournalist) -> (assertz(workSector(Name, journalist)), assertz(purpose(Name, ofw)), writeln("You are considered a valid journalist."));
    writeln("You are unfortunately not considered a valid journalist.").

journalist_questions(Name) :-
    format("~nSelect which best fits your situation:~n"),
    format("1 - A journalist engaged in topical news reporting requiring immediate physical presence~n"),
    format("2 - Others~n3 - Exit~n"), read(X),
    (
    X = 1 -> assertz(work(Name, urgentJournalist));
    X = 2 -> assertz(work(Name, othersJournalist));
    X = 3 -> writeln("Returning to previous question"), ofw(Name);
    writeln("Invalid Input!"), journalist_questions(Name)
    ), journalist(Name).

research(Name) :-
    (work(Name, essentialResearch)) ->
    assertz(workSector(Name, research)), assertz(purpose(Name, ofw)),
    writeln("You are considered as a valid researcher.");
    writeln("You are unfortunately not considered as a valid researcher.").

researcher_questions(Name) :-
    format("~nDoes your research meet the criteria necessary to qualify as essential to the economy and society?~n"),
    format("1 - Yes~n2 - No~n3 - Exit~n"), read(X),
    (
    X = 1 -> assertz(work(Name, essentialResearch));
    X = 2 -> assertz(work(Name, nonEssentialResearch));
    X = 3 -> ofw(Name)
    ), (X < 3) -> (
    format("~nHow long are you staying for?~n"), read(Y), assertz(research_job(Name, Y))
    ), research(Name).

cultural(Name) :-
    (work(Name, culturalInvited); work(Name, culturalPhysical); work(Name, culturalFinance)) ->
    assertz(workSector(Name, cultural)), assertz(purpose(Name, ofw)),
    writeln("You are considered as working in the Cultural and Creative Sector.");
    writeln("You are unfortunately not considered as working in the Cultural and Creative Sector.").

cultural_questions(Name) :-
    format("~nSelect what best fits your situation:~n"),
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

ofw(Name) :-
    nl, writeln("What sector are you working in?"),
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
    writeln("17 - Others"),
    writeln("18 - Exit"),
    write("Choice: "), read(X),
    (
    X = 1 -> essential_worker_questions(Name);
    X = 2 -> assertz(workSector(Name, crossBorder)), assertz(purpose(Name, ofw));
    X = 3 -> essential_worker_questions(Name);
    X = 4 -> transport_questions(Name);
    X = 5 -> energy_questions(Name);
    X = 6 -> assertz(workSector(Name, urgentTech)), assertz(purpose(Name, ofw));
    X = 7 -> assertz(workSector(Name, flightCrew)), assertz(purpose(Name, ofw));
    X = 8 -> seafarer_questions(Name);
    X = 9 -> assertz(workSector(Name, diplomat)), assertz(purpose(Name, ofw));
    X = 10 -> assertz(workSector(Name, stateWorker)), assertz(purpose(Name, ofw));
    X = 11 -> assertz(workSector(Name, armedForce)), assertz(purpose(Name, ofw));
    X = 12 -> assertz(workSector(Name, orgWorker)), assertz(purpose(Name, ofw));
    X = 13 -> journalist_questions(Name);
    X = 14 -> assertz(workSector(Name, eliteAthlete)), assertz(purpose(Name, ofw));
    X = 15 -> researcher_questions(Name);
    X = 16 -> cultural_questions(Name);
    X = 17 -> assertz(workSector(Name, others));
    X = 18 -> writeln("Returning to previous question"), purpose_of_travel(Name);
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
    Y = 3 -> writeln("Returning to previous question"), business_questions(Name);
    writeln("Invalid Input!"), business_invitation(Name)
    ).

business_organization(Name) :-
    writeln("What kind of organization?"),
    format("1 - Has at least 10 FTEs and/or annual turnover of at least €2 Million~n"),
    format("2 - Smaller organization invovled in socially relevant ground-breaking contributions to research, innovation, sustainability, or public health~n"),
    format("3 - Others~n4 - Exit~n"), read(X),
    (
    (X = 1; X = 2) -> assertz(business_purpose(Name, organization));
    X = 4 -> writeln("Returning to previous question"), business_questions(Name);
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
    X = 7 -> write("");
    X = 8 -> writeln("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid Input!"), business_questions(Name)
    ), business(Name).

valid_vaccination(X, Y, Z) :-
    ((vaccinated(X, Y, Z), (((Z = pfizer; Z = moderna; Z = astrazeneca), Y > 14); (Z = johnsonAndJohnson), Y > 28)) ->
    (assertz(validVaccination(X)), assertz(purpose(X, tourist)), writeln("Your Vaccination is Valid"));
    writeln("Your Vaccination is unfortunately invalid.")
    ).

vaccine_questions(X) :-
    format("~nWhat vaccine were you vaccinated with?~n"),
    format("1 - Pfizer~n2 - Moderna~n3 - Astrazeneca~n4 - Johnson & Johnson~n5 - Others~n6 - Exit~n"),
    read(Z),
    write("How many days ago did you get vaccinated? "), read(Y),
    (
    Z = 1 -> (assertz(vaccinated(X, Y, pfizer)), valid_vaccination(X, Y, pfizer));
    Z = 2 -> (assertz(vaccinated(X, Y, moderna)), valid_vaccination(X, Y, moderna));
    Z = 3 -> (assertz(vaccinated(X, Y, astrazeneca)), valid_vaccination(X, Y, astrazeneca));
    Z = 4 -> (assertz(vaccinated(X, Y, johnsonAndJohnson)), valid_vaccination(X, Y, johnsonAndJohnson));
    Z = 5 -> (assertz(vaccinated(X, Y, others)), valid_vaccination(X, Y, others));
    Z = 6 -> (writeln("Returning to previous question"), tourist(X));
    writeln("Invalid input!"), nl, vaccine_questions(X)
    ).

tourist(Name) :-
    format("~nAre you fully vaccinated?~n"),
    format("1 - Yes~n2 - No~n3 - Exit~n"), read(X),
    (
    X = 1 -> vaccine_questions(Name);
    X = 2 -> valid_vaccination(Name, 0, others);
    X = 3 -> writeln("Returning to previous question"), purpose_of_travel(Name);
    writeln("Invalid Input!"), tourist(Name)
    ).

documents_list(Document) :-
  (
  Document = healthDeclaration -> (
    writeln("* Health Declaration Form"),
    writeln("   - Must be accomplished and can be found at https://www.government.nl/topics/coronavirus-covid-19/documents/publications/2021/07/20/covid-19-and-flying-health-declaration-form"),
    writeln("   - Form can be printed or digitally filled in"),
    writeln("       = If you prefer to print, print 2 copies and fill up both"),
    writeln("       = If you prefer digital, fill up the PDF before the flight")
  );
  Document = healthDeclarationAirline -> (
    writeln("* Health Declaration Form"),
    writeln("   - Part of your online check-in procedures with the airline")
  );
  Document = vaccineDeclaration -> (
    writeln("* Vaccine Declaration Form"),
    writeln("   - Must be accomplished and can be found at https://www.government.nl/topics/coronavirus-covid-19/documents/publications/2021/07/01/vaccine-declaration-covid-19")
  );
  Document = vaccineProof -> (
    writeln("* Proof of Vaccination"),
    writeln("   - Can be either paper or digital certificate issued by a country taking part in the EU Digital Covid Certificate System")
  );
  Document = returnJourney -> (
    writeln("* Proof of Return Journey"),
    writeln("   - Return ticket issued by airline, bus company, or railway company")
  );
  Document = visa -> (
    writeln("* Visa")
  );
  Document = negativeResult -> (
    writeln("* Negative Test Result"),
    writeln("   - From NAAT (PCR) test taken no more than 24 hours before departure"),
    writeln("   - From a NAAT (PCR) test taken no more than 48 hours before departure and a negative result from an antigen test taken no more than 24 hours before departure.")
  );
  Document = euroResidentProof -> (
    writeln("* Proof of Nationality / Residence")
  );
  Document = residentPermit -> (
    writeln("* Resident Permit")
  );
  Document = longStay -> (
    writeln("* Long Stay Visa")
  );
  Document = notifImmigration -> (
    writeln("* Letter of Notification"),
    writeln("   -  From the Immigration and Naturalisation Service")
  );
  Document = diplomaticNote -> (
    writeln("* Diplomatic note"),
    writeln("   -  Issued by a Dutch embassy stating that you fall under an exemption to the entry ban")
  );
  Document = hotelBooking -> (
    writeln("* Hotel Booking")
  );
  Document = ipc -> (
  writeln("* Any of the following:"),
  writeln("   - International Press Card"),
  writeln("   - Note verbale and a National Press Card or Card from the Media Organisation you work for")
  );
  Document = athleteInvite -> (
    writeln("* Any of the following:"),
    writeln("   - Note Verbale"),
    writeln("   - Letter of invitation from the Netherlands Olympic Committee (NOC*NSF) or the Royal Netherlands Football Association (KNVB)")
  );
  Document = athleteProof -> (
    writeln("* Valid proof of participation in an international sporting event, tournament, or match at the highest level"),
    writeln("   - Officially recognised by an international sports federation with which the Netherlands is affiliated")
  );
  Document = researcherLetter -> (
    writeln("* A letter from the Netherlands Enterprise Agency (RVO)"),
    writeln("   - Recommending your admission to the Netherlands under the exemption for researchers."),
    writeln("   - A printout of a digital copy of the letter may be presented, provided that the signed original remains available for verification.")
  );
  Document = culturalInvite -> (
    writeln("* Letter of Invitation"),
    writeln("   - Can be digital, provided that the signed original remains available for verification.")
  );
  Document = culturalEntry -> (
    writeln("* Signed Entry Statement"),
    writeln("   - Can be digital, provided that the signed original remains available for verification.")
  );
  write("")
  ).

traveller_documents(Name) :-
  format("~nRequired Documents~n"),
  (
  purpose(Name, citizen) -> (
  (((traveller(Name, Y, _), (Y > 11)), transportation(Name, airplane)) ->
  (not(airline(Name, klm); airline(Name, corendon); airline(Name, tui);
        airline(Name, transavia); airline(Name, easyjet)) ->
  documents_list(healthDeclaration); documents_list(healthDeclarationAirline)); write("")),
  documents_list(vaccineDeclaration), documents_list(vaccineProof),
  documents_list(returnJourney), documents_list(negativeResult)
  );
  purpose(Name, resident) -> (
  (((traveller(Name, Y, _), (Y > 11)), transportation(Name, airplane)) ->
  (not(airline(Name, klm); airline(Name, corendon); airline(Name, tui);
        airline(Name, transavia); airline(Name, easyjet)) ->
  documents_list(healthDeclaration); documents_list(healthDeclarationAirline)); write("")),
  documents_list(vaccineDeclaration), documents_list(vaccineProof),
  documents_list(returnJourney), documents_list(visa), documents_list(negativeResult),
  (euroResident(Name) -> documents_list(euroResidentProof); write("")),
  (documents(Name, residentPermit) -> documents_list(residentPermit); write("")),
  (documents(Name, longStayVisa) -> documents_list(longStay); write("")),
  (documents(Name, notifImmigration) -> documents_list(notifImmigration); write(""))
  );
  purpose(Name, ofw) -> (
  (((traveller(Name, Y, _), (Y > 11)), transportation(Name, airplane)) ->
  (not(airline(Name, klm); airline(Name, corendon); airline(Name, tui);
        airline(Name, transavia); airline(Name, easyjet)) ->
  documents_list(healthDeclaration); documents_list(healthDeclarationAirline)); write("")),
  documents_list(vaccineDeclaration), documents_list(vaccineProof),
  documents_list(returnJourney), documents_list(visa), (
  (not(workSector(Name, energy)), not(workSector(Name, tranport)),
  not(workSector(Name, flightCrew)), not(workSector(Name, seafarer))) -> documents_list(negativeResult);
  write("")
  ),
  (workSector(Name, journalist) -> documents_list(ipc); write("")),
  (workSector(Name, eliteAthlete) -> (documents_list(athleteInvite), documents_list(athleteProof)); write("")),
  (workSector(Name, research) -> (documents_list(researcherLetter)); write("")),
  (workSector(Name, cultural) -> (documents_list(culturalInvite), documents_list(culturalEntry)); write(""))
  );
  purpose(Name, business) -> (
  (((traveller(Name, Y, _), (Y > 11)), transportation(Name, airplane)) ->
  (not(airline(Name, klm); airline(Name, corendon); airline(Name, tui);
        airline(Name, transavia); airline(Name, easyjet)) ->
  documents_list(healthDeclaration); documents_list(healthDeclarationAirline)); write("")),
  documents_list(vaccineDeclaration), documents_list(vaccineProof),
  documents_list(returnJourney), documents_list(visa), documents_list(negativeResult),
  documents_list(diplomaticNote), documents_list(hotelBooking)
  );
  purpose(Name, tourist) -> (
  (((traveller(Name, Y, _), (Y > 11)), transportation(Name, airplane)) ->
  (not(airline(Name, klm); airline(Name, corendon); airline(Name, tui);
        airline(Name, transavia); airline(Name, easyjet)) ->
  documents_list(healthDeclaration); documents_list(healthDeclarationAirline)); write("")),
  documents_list(vaccineDeclaration), documents_list(vaccineProof),
  documents_list(returnJourney), documents_list(visa), documents_list(negativeResult)
  )), documents_procedure(Name).

documents_procedure(Name) :-
    (purpose(Name, citizen); purpose(Name, resident); purpose(Name, ofw); purpose(Name, business); purpose(Name, tourist)) -> (
    format("~nSelect an Option:~n1 - Required Documents~n2 - Procedure~n3 - Exit~n"), read(X),
    (
    X = 1 -> traveller_documents(Name);
    X = 3 -> writeln("Thank you for consulting!");
    writeln("Invalid Input!"), documents_procedure(Name)
    )
    );
    writeln("You are unfortunately not allowed to travel to the Netherlands.").

airline_question(Name) :-
    transportation(Name, airplane) -> (
    format("~nSelect your airline:~n"),
    format("1 - KLM~n2 - Corendon~n3 - TUI~n4 - Transavia~n5 - easyJet~n6 - Others~n7 - Exit~n"), read(X),
    (
    X = 1 -> assertz(airline(Name, klm));
    X = 2 -> assertz(airline(Name, corendon));
    X = 3 -> assertz(airline(Name, tui));
    X = 4 -> assertz(airline(Name, transavia));
    X = 5 -> assertz(airline(Name, easyjet));
    X = 6 -> assertz(airline(Name, others));
    X = 7 -> ((transportation(Name, airline) -> retract(transportation(Name, airline)); write("")),
    writeln("Returning to previous question"), mode_of_transportation(Name));
    writeln("Invalid input!"), airline_question(Name)
    )
    ); writeln("You are not recorded as travelling through an airplane.").

mode_of_transportation(Name) :-
    (purpose(Name, citizen); purpose(Name, resident); purpose(Name, ofw); purpose(Name, business); purpose(Name, tourist)) -> (
    format("~nCongratulations! You are allowed to travel to the Netherlands."),
    format("~nBefore proceeding, kindly select what your mode of transportation is:~n"),
    format("1 - Airplane~n2 - Boat~n3 - Others~n"),
    read(X),
    (
    X = 1 -> (assertz(transportation(Name, airplane)), airline_question(Name));
    X = 2 -> assertz(transportation(Name, boat));
    X = 3 -> assertz(transportation(Name, others));
    writeln("Invalid input!"), mode_of_transportation(Name)
    ), documents_procedure(Name)
    );
    writeln("You are unfortunately not allowed to travel to the Netherlands.").

purpose_of_travel(Name) :-
    nl, write("Hello "), write(Name), write("! What is your purpose of travel?"), nl,
    writeln("1 - I am a citizen of Netherlands or another European or Schengen Area Country"),
    writeln("2 - I am a non-Dutch resident of the Netherlands or another European or Schengen Area Country"),
    writeln("3 - I am an Overseas Filipino Worker in the Netherlands"),
    writeln("4 - I am on a business trip"),
    writeln("5 - I am a tourist"),
    writeln("6 - Exit"),
    write("Choice: "), read(X),
    (
    X = 1 -> (citizen_questions(Name), mode_of_transportation(Name));
    X = 2 -> (resident_questions(Name), mode_of_transportation(Name));
    X = 3 -> (ofw(Name), mode_of_transportation(Name));
    X = 4 -> (business_questions(Name), mode_of_transportation(Name));
    X = 5 -> (tourist(Name), mode_of_transportation(Name));
    X = 6 -> write("Exiting system...");
    writeln("Invalid input!"), purpose_of_travel(Name)
    ).

basic_information(Name) :-
    write("What is your name? (Enclose in quotes) "), nl, read(Name),
    write("What is your age? (Enclose in quotes) "), nl, read(Age),
    write("What is your Nationality? (Enclose in quotes) "), nl, read(Citizenship),
    (
    (Citizenship = dutch; Citizenship = 'Dutch'; Citizenship = "Dutch") -> assertz(dutch(Name));
    write("")
    ), assertz(traveller(Name, Age, Citizenship)).

consultation:-
    basic_information(Name), nl,
    purpose_of_travel(Name).
