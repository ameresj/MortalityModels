*******************************************************************************
**************************	  READ ME	  *****************************
*******************************************************************************

Her finder du en beskrivelse af indeholdet af det kommunale dashboard,
herunder beskrivelser af tabellerne med tilhørende variabelnavne (søjlenavne).


Forkortelser:
_______________________________________________________________________________
TCD = Test Center Danmark (Statens Serum Institut)
KMA = Klinisk Mikrobiologisk Afdeling (hospitaler)


Generel struktur:
_______________________________________________________________________________
Rækkerne i filerne er som udgangspunkt opdelt efter relevante 
parametre, eksempelvis aldersgruppering eller tidsopdeling. 
Der opdeles generelt efter variablen i første søjle. 
Enkelte tabeller kan have rækker, som afviger fra dette mønster. 
I disse tabeller specificeres dette i "Noter" under tabellens 
variabelbeskrivelse. Filerne er kommaseparerede.


Opdateringer:
_______________________________________________________________________________
Filerne bliver opdateret hver dag. I den forbindelse kan tidsserier også 
ændre sig tilbage i tiden, hvis nyere data foreligger. Derfor anbefales det 
altid at benytte det senest tilgængelige data og for så vidt muligt, 
ikke at gemme filer og lave tidsserier på basis af gamle filer.


Typer af tests:
_______________________________________________________________________________
Filerne baseres primært på PCR-opgørelser, medmindre andet er angivet. 
PCR-tests og antigentests bruges til hhv. at bekræfte eller mistænke covid-19-
smitte under et aktivt sygdomsforløb. 
Begreberne "PCR-prøver" og "PCR-test" betyder det samme og refererer til
antallet af podninger. Det samme gælder for begreberne "antigentest" og 
"antigenprøver".
Data indeholder ikke serologitest, som er den test, der udføres, 
når man skal undersøge, om raske mennesker tidligere har haft covid-19.

01_Noegletal
-------------------------------------------------------------------------------
Dette er den daglige opgørelse af antallet af bekræftede tilfælde, døde,
overståede infektioner, indlæggelser, PCR-testede personer, PCR-prøver og antigenprøver,
samt ændringen siden i går, opgjort pr. region og pr. køn. 
De kummulerede opgørelser udgør det samlede antal observerede siden pandemiens
start i Danmark, hvor antallet siden i går udgør de daglige opgørelser.
Bemærk, at denne opgørelsesmetode benytter sig af svardatoer
og ikke prøvedatoer. Derfor vil disse to opgørelser afvige fra hinanden. 


IndberetningDato			: Dato for opgørelsen
Totalt antal tests			: Kumulerede antal PCR-prøver
Antal testede personer		: Kumulerede antal PCR-testede personer
Antal bekræftede tilfælde		: Kumulerede antal bekræftede tilælde
Antal overstået infektion		: Kumulerede antal overståede infektioner	
Antal døde			: Kumulerede antal døde
Antal nye bekræftede tilfælde		: Antallet af bekræftede tilfælde siden i går	
Antal nye døde			: Antallet af døde siden i går
Antal nye overstået infektion		: Antallet af personer med overstået infektion siden i går
Antal indlagte i dag med covid-19	: Antallet af indlagte
Antal indlagt i dag og 	i respirator	: Antallet af indlagte i respirator
Antal indlagt i dag på 	intensiv		: Antallet af indlagte på intensiv i respirator
Antal nye indlæggelser		: Antallet af nye indlæggelser siden i går
Ændring i antal indlæggelser		: Ændringen i antallet af indlagte siden i går	
Ændring i antal i respirator		: Ændringen i antallet af indlagte på intensiv i respirator siden i går
Ændring i antal på intensiv		: Ændringen i antallet af indlagte på intensiv siden i går
Førstegangstestede			: Antallet af PCR-testede personer siden i går
Antal prøver siden sidst		: Antallet af PCR-prøver siden i går	


02_indlaeggelser_pr_dag_kummulativt
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af indlagte pr. dag kummulativt.

Dato					: Dato for indlæggelse
Indlæggelser kumuleret			: Antallet af indlagte


03_indlæggelser_pr_aldersgrp
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af indlagte pr. aldersgruppe.

Aldersgruppe			: Den aldersgruppe en person tilhørte ved
				  prøvetagningen
Indlæggelser			: Antallet af indlagte


04_bekraeftede_tilfaelde_doed_pr_aldersgrp
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde og døde pr.
aldersgruppe.

Aldersgruppe			: Den aldersgruppe en person tilhørte ved prøvetagningen
Bekræftede tilfælde			: Antallet af bekræftede tilælde
Døde				: Antallet af døde



05_nye_indlaeggelser_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af nye indlæggelser pr. dag.

Indlæggelsesdato		: Datoen for indlæggelse
Indlæggelser		: Antallet af nye indlæggelser


06_doed_pr_dag_kumuleret
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af døde pr. dag kummuleret

Dato			: Dato for dødsfald registreret i det Dødsårsagsregisteret
Kummuleret antal døde	: Kummulerede antal døde siden pandemiens start


07_bekraeftede_tilfaelde_pr_dag_pr_kommune
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde pr. dag pr.
kommune.

Kommune			: Bopælskommunekode (kommune man boede i ved prøvetagningen)
Dato			: Dato for dødsfald registreret i Dødsårsagsregisteret
Bekræftede tilfælde		: Antallet af bekræftede tilfælde


08_Proever_pr_uge_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af PCR-prøver pr. uge pr. region. Disse opgørelser 
inkludere kun opgørelser fra det offentlige.

uge			: Uger
Region Hovedstaden	: Antallet af PCR-prøver
Region Midtjylland		: Antallet af PCR-prøver	
Region Nordjylland		: Antallet af PCR-prøver	
Region Sjælland	  	: Antallet af PCR-prøver
Region Syddanmark		: Antallet af PCR-prøver
Statens Serum Institut	: Antallet af PCR-prøver
Test center Danmark		: Antallet af PCR-prøver
Sundhedsspor		: Antallet af PCR-prøver	
Samfundsspor		: Antallet af PCR-prøver
Total			: Det samlede antal PCR-prøver


09_tilfaelde_aldersgrp_kommuner
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde pr. aldersgruppe 
pr. kommune.

Kommune			: Bopælskommunekode (kommune man boede i ved prøvetagning)	
Aldersgruppe		: Den aldersgruppe en person tilhørte ved prøvetagningen
Bekræftede tilfælde		: Antallet af bekræftede tilælde
Dagsdato			: Prøvetagnings dato


10_kort_pr_kommune
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde, bekræftede tilfælde
de seneste 7 dage, PCR-testede de seneste 7 dage samt positivprocenten de seneste
7 dag.

Kommune					: Bopælskommunekode (kommune man boede i ved prøvetagning)
Bekræftede tilfælde de seneste 7 dage		: Antallet af bekræftede tilfælde de seneste 7 dage
Testede de seneste 7 dage			: Antallet af PCR-testede personer de seneste 7 dage 	
Positivprocent de seneste 7 dage		: (antal covid-19-bekræftede personer/antallet af PCR-testede personer) *100 for de seneste 7 dage


11_Mistænkte_tilfælde_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af mistænkte tilfælde (læs mere om opgørelsesmetoden her: https://covid19.ssi.dk/datakilder-og-definitioner)
Proevedato			: Dato for prøvetagningen
Kommune: 			: Kommunekode
Mistænkte positive antigen tests	: Antal personer med en positiv antigentest

