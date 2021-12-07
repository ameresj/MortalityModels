*******************************************************************************
***************			READ ME	  	  *****************************
*******************************************************************************

Her finder du en beskrivelse af indeholdet af det regionale dashboard, herunder 
beskrivelser af tabellerne med tilhørende variabelnavne (søjlenavne).


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


Filerne:
_______________________________________________________________________________


01_Noegle_tal
-------------------------------------------------------------------------------
Dette er den daglige opgørelse af antallet af bekræftede tilfælde, døde,
overståede infektioner, indlæggelser, PCR-testede personer, PCR-prøver og 
antigenprøver, samt ændringen siden i går, opgjort pr. region og pr. køn. 
De kumulerede opgørelser udgør det samlede antal observerede siden 
pandemiens start i Danmark, hvor antallet siden i går udgør de daglige opgørelser.
Bemærk, at denne opgørelsesmetode benytter sig af svardatoer og ikke 
prøvedatoer. Derfor vil disse to opgørelser afvige fra hinanden. 
NA, i <Region> variablen, repræsenterer alle danske personer uden en tilknyttet 
bopælsregion. 


Dato					: Dato for opgørelsen
Region					: Bopælsregion (region man boede i ved prøvetagning)
Køn						: Mænd og kvinder
Bekræftede tilfælde			: Kumulerede antal bekræftede tilfælde
Døde					: Kumulerede antal døde
Overstået infektion			: Kumulerede antal personer med overstået infektion
Indlæggelser				: Kumulerede antal indlæggelser
Testede personer			: Kumulerede antal PCR-testede personer
Ændring i antal 
	bekræftede tilfælde		: Antallet af nye bekræftede tilfælde siden i går
Ændring i antal døde			: Antallet af nye dødsfald siden i går 
Ændring i antal 		
	overstået infektion		: Antallet af personer med overstået infektion siden i går
Ændring i antal indlagte		: Antallet af indlagte siden i går.
Ændring i antallet af 
	testede personer		: Antallet af PCR-testede personer siden i går	
Antallet af prøver			: Kumulerede antal PCR-prøver
Ændring i antallet af 
	prøver				: Antallet af nye PCR-prøver siden i går


02_Hospitalsbelaegning
-------------------------------------------------------------------------------
Dette er den daglige opgørelse for den covid-19-relaterede hospitalsbelægning,
opgjort pr. region.
Bemærk, at der er forskel mellem antallet af personer indlagt siden i går, opgjort i 
filen: <01_Noegletal>, og ændringen i antallet af indlagte. Ændringen er den reelle 
ændring observeret, hvor antallet af indlæggelser siden i går udgør det antal nye 
indlæggelser siden i går.

Dato					: Dato for prøvetagning og eventuel indlæggelsesdag
Region					: Indlæggelsesregion
Indlagte				: Antallet af personer indlagt
Heraf indlagte på 
	intensiv i
	respirator			: Antallet af personer der er indlagt på intensiv og ligger i respirator 

Heraf indlagte på 		 	 
	intensiv			: Antallet af personer indlagt på intensiv
Ændring i antal indlagte		: Ændringen i antallet af indlagte siden i går	
Ændring i antal indlagte på intensiv	: Ændringen i antallet af indlagte på intensiv siden i går
Ændring i antal indlagte i respirator 	: Ændringen i antallet af indlagte på intensiv i respirator
				  siden i går


03_bekraeftede tilfaelde_doede_indlagte_pr_dag_pr_koen
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde og døde pr. dag
fordelt på bopælsregioner og køn.

Region					: Bopælsregion (region man boede i ved prøvetagning)		
Dato					: Dato for prøvetagning og evntuel indlæggelsesdag
Køn						: Mænd og kvinder 
Bekræftede tilfælde			: Antallet af bekræftede tilælde
Døde					: Antallet af døde
Indlæggelser				: Antallet af indlæggelser
Kumuleret antal døde			: Alle dødsfald fra pandemiens start til den aktuelle dag
Kumuleret antal 		
	bekræftede tilfælde		: Alle bekræftede tilfælde fra pandemiens start til den aktuelle dag
Kumuleret antal 		 	
	indlæggelser			: Alle indlæggelser fra pandemiens start til den aktuelle dag



04_indlagte_pr_alders_grp_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af indlagte pr. aldersgruppe pr. region.
Bemærk at blanke felter under variablen <Region> udgør de danskere,
som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved indlæggelsen)
Aldersgruppe				: Den aldersgruppe en person tilhørte ved
						  prøvetagningen
Indlæggelser				: Kumulerede antal indlæggelser



05_bekraeftede_tilfaelde_doede_pr_region_pr_alders_grp
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde og døde pr. region 
og pr. aldergruppe. Bemærk at blanke felter under variablen <Region> 
udgør de danskere, som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved prøvetagningen)
Aldersgruppe				: Den aldersgruppe en person tilhørte ved
						  prøvetagningen
Bekræftede tilfælde			: Kumulerede antal bekræftede tilælde
Døde					: Kumulerede antal døde



06_nye_indlaeggelser_pr_region_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af nye indlæggelser pr. region pr. dag.

Region					: Bopælsregion (region man boede i ved indlæggelsen)		
Dato					: Dato for indlæggelsen
Indlæggelser				: Antallet af indlæggelser på en given dag i en given
						  region


07_antal_doede_pr_dag_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af døde pr. region pr. dag. 
Bemærk at blanke felter under variablen <Region> udgør de danskere, 
som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved prøvetagningen)
Dato					: Dato for dødsfald registreret Dødsårsagsregisteret
Kumulerede antal døde			: Kumulerede antal døde siden pandemiens start



08_bekraeftede_tilfaelde_pr_dag_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde pr. region pr.
dag siden pandemiens start.

Region					: Bopælsregion (region man boede i ved prøvetagningen)
Dato					: Dato for dødsfald registreret i Dødsårsagsregisteret
Bekræftede tilfælde			: Antallet af bekræftede tilælde


09_bekraeftede_tilfaelde_pr_PCR_test_region_pr_uge
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfælde pr. uge pr. region
opgjort på den region, hvor PCR-testen blev taget.

Uge					: Den uge hvor testen blev taget
TCD Region Hovedstaden			: Alle positive PCR-test i region Hovedstaden testet af TCD
Region Hovedstaden			: Alle positive PCR-test i region Hovedstaden testet af KMA
TCD Region Sjælland			: Alle positive PCR-test i region Sjælland testet af TCD
Region Sjælland				: Alle positive PCR-test i region Sjælland testet af KMA
TCD Region Nordjylland			: Alle positive PCR-test i region Nordjylland testet af TCD
Region Nordjylland			: Alle positive PCR-test i region Nordjylland testet af KMA
TCD Region Syddanmark			: Alle positive PCR-test i region Syddanmark testet af TCD
Region Syddanmark			: Alle positive PCR-test i region Syddanmark testet af KMA
TCD_Region Midtjylland			: Alle positive PCR-test i region Midtjylland testet af TCD
Region Midtjylland			: Alle positive PCR-test i region Midtjylland testet af KMA
Sundhedsspor				: Alle positive PCR-test registreret af KMA
Samfundsspor				: Alle positive PCR-test registreret af TCD
Total					: Det samlede antal bekræftede positive PCR-test.




10_testede_pr_uge_pr_samfundsspor_opgjort_paa_bopaelsregion
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af PCR-testede pr. uge pr. region opgjort på
bopælsregion.

Uge				: Den uge hvor PCR-testen blev foretaget.
Region Hovedstaden		: Alle PCR-testede i region Hovedstaden testet af KMA
Region Sjælland			: Alle PCR-testede i region Sjælland testet af KMA
Region Nordjylland		: Alle PCR-testede i region Nordjylland testet af KMA
Region Syddanmark		: Alle PCR-testede i region Syddanmark testet af KMA
Region Midtjylland		: Alle PCR-testede i region Midtjylland testet af KMA



11_noegletal_pr_region_pr_aldersgruppe
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfælde, døde, indlagte, herunder på intensiv 
afdeling opgjort pr. region og pr. aldersgruppe.


Region					: Bopælsregion (region man boede i ved prøvetagningen)
Aldersgruppe				: Den  aldersgruppe en person tilhørte ved
						  prøvetagningen
Bekræftede tilfælde			: Antallet af bekræftede tilfælde
Døde					: Antallet af døde
Indlagte på intensiv
	afdeling			: Antallet af patienter indlagt på intensiv afdeling
Indlæggelser				: Antallet af indlagte
	


12_noegletal_pr_region_pr_aldersgruppe_de_seneste_7_dage
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfælde, døde, indlagte,
herunder på intensiv afdeling opgjort pr. region og pr. aldersgruppe de seneste 7 dage.

Region					: Bopælsregion (region man boede i ved prøvetagningen)
Aldersgruppe				: Den aldersgruppe en person tilhørte ved
						  prøvetagningen
Bekræftede tilfælde			: Antallet af bekræftede tilfælde
Døde					: Antallet af døde
Indlagte på intensiv
	afdeling			: Antallet af patienter indlagt på intensiv afdeling
Indlæggelser				: Antallet af indlagte


13_regionale_kort
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfælde, incidensen, bekræftede
tilfælde de seneste 7 dage, incidensen de seneste 7 dage, PCR-testede personer,
PCR-testede pr. 100.000 borgere, PCR-testede de seneste 7 dage, PCR-testede pr. 
100.000 borgere de seneste 7 dage, samt positivprocenten de seneste 7 dage, opgjort pr. region. 
Læs mere om opgørelsesmetoden for de seneste 7 dage under "Definitioner og datakilder" på 
https://covid19.ssi.dk/datakilder-og-definitioner.



Region					: Bopælsregion (region man boede i ved prøvetagningen)
Bekræftede tilfælde			: Antallet af bekræftede tilfælde siden pandemiens start
Incidensen				: Antallet af bekræftede tilfælde pr. 100.000 borgere
Bekræftede tilfælde
	de seneste 7 dage		: Antallet af bekræftede tilfælde de seneste 7 dage
Incidensen de seneste
	7 dage				: Antallet af bekræftede tilfælde pr. 100.000 borgere de seneste 7 dage
Testede					: Antallet af PCR-testede personer siden pandemiens start
Testede pr. 100.000 borgere		: Antallet af PCR-testede pr. 100.000 borgere
Testede de seneste 		
	7 dage				: Antallet af PCR-testede personer de seneste 7 dage 
Testede pr. 100.000 borgere 
	de seneste 7 dage		: Antallet af PCR-testede pr. 100.000 borgere de seneste 7 dage
Positivprocent 			 
	de seneste 7 dage		: (antal covid-19-bekræftede personer/antallet af PCR-testede personer) *100 for de seneste 7 dage


14_Testede_pr_test_region_pr_uge
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af PCR-testede pr. region pr. uge, samt det samlede antal PCR-testede 
i hhv. samfundssporet (TCD) og sundhedssporet (KMA).

Uge					: Den uge hvor PCR-testen blev taget
TCD Region Hovedstaden			: Alle PCR-test i region Hovedstaden foretaget af TCD
Region Hovedstaden			: Alle PCR-test i region Hovedstaden foretaget af KMA
TCD Region Sjælland			: Alle PCR-test i region Sjælland foretaget af TCD
Region Sjælland				: Alle PCR-test i region Sjælland foretaget af KMA
TCD Region Nordjylland			: Alle PCR-test i region Nordjylland foretaget af TCD
Region Nordjylland			: Alle PCR-test i region Nordjylland foretaget af KMA
TCD Region Syddanmark			: Alle PCR-test i region Syddanmark foretaget af TCD
Region Syddanmark			: Alle PCR-test i region Syddanmark foretaget af KMA
TCD_Region Midtjylland			: Alle PCR-test i region Midtjylland foretaget af TCD
Region Midtjylland			: Alle PCR-test i region Midtjylland foretaget af KMA
Sundhedsspor				: Alle PCR-test registreret af KMA
Samfundsspor				: Alle PCR-test registreret af TCD
Total					: Det samlede antal PCR-test



15_Indlagte_pr_region_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af indlagte på en given dag fordelt på 
regioner. 

Dato					: Dato for indlæggelse 
Region					: Regionen hvor patienten er indlagt
Indlagte				: Antallet af indlæggelser


16_Mistaenkte_pos_pr_Region_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af mistænkte tilfælde (se definitionen her: https://covid19.ssi.dk/datakilder-og-definitioner)
Proevedato			:Dato for prøvetagningen
Region				:Region
Mistænkte positive antigen tests	:Antal personer med en positiv antigen test, som hverken tidligere har fået en positiv PCR-test, eller en positiv/negativ PCR-test samme dag eller dagen efter.




