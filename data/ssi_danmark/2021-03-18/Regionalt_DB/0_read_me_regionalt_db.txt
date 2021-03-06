*******************************************************************************
**************************		  READ ME 		  *****************************
*******************************************************************************

Her finder du en beskrivelse af indeholdet af det regionale dashboard.
Du vil her finde en beskrivelse af tabellerne med deres tilhørende
variable navne (søjlenavne).



Forkortelser:
_______________________________________________________________________________
TCD = Test Center Danmark (hvide telte)
KMA = Klinisk microbiologisk afdeling (hospitaler)



Generel struktur:
_______________________________________________________________________________
Rækkerne i filerne er som udgangspunkt stratificeringer efter relevante 
parametre, eksempelvis aldersgruppering eller tidsopdeling. 
Der stratificeres generelt efter variablen i første søjle. 
Enkelte tabeller kan have rækker, som afviger fra dette mønster. 
I disse tabeller specificeres dette i "Noter" under tabellens 
variabelbeskrivelse. Filerne er komma-separerede.


Opdateringer:
_______________________________________________________________________________
Filerne bliver opdateret hver dag og i denne forbindelse kan tidsserier også 
ændre sig tilbage i tiden, hvis nyere data foreligger. Derfor anbefales det 
altid at benytte det senest tilgængelige data og for så vidt muligt, 
ikke at gemme filer og lave tidsserier på basis af gamle filer.


Typer af tests:
_______________________________________________________________________________
Filerne bygger på nuværende tidspunkt både på PCR- og antigentest. 
Både PCR- og antigentests, bruges til at påvise covid-19-smitte under et aktivt 
sygdomsforløb. 
Data indeholder ikke serologitest, som er den test, der udføres, 
når man skal undersøge, om raske mennesker tidligere har haft covid-19.


Filerne:
_______________________________________________________________________________


01_Noegle_tal
-------------------------------------------------------------------------------
Dette er den daglige opgørelse af antallet af bekræftede tilfælde, døde,
overståede infektioner, indlæggelser, testede personer og prøver,
samt ændringen siden i går, opgjort pr. region og pr. køn. 
De kummulerede opgørelser udgøre det samlede antal observerede siden pandemi
start i danmark, hvor antallet siden i går udgøre de daglige opgørelser.
Bemærk venligst her at denne opgørelses metode benytter sig af svar datoer
og ikke prøve datoer, hvorfor disse to opgørelses metoder vil afvige 
fra hindanden. Der benyttes to forskellige opgørelses metoder, da prøvedatoen
anses for mest retvisende. Svar datoen benyttes derimod til at give den best
mulige formodning på en given dag. NA, i <Region> variablen, representerer
alle danske personer uden en tilknyttet bopælsregion. 


Dato					: Dato for opgørelsen
Region					: Bopælsregion (region man boede i ved prøvetagning)
Køn						: Køn, herunder mænd og kvinder
Bekræftede tilfælde		: Kummulerede antallet af bekræftede tilælde
Døde					: Kummulerede antallet af døde
Overstået infektion		: Kummulerede antallet af overståede infektioner
Indlæggelser			: Kummulerede antallet af indlæggelser
Testede personer		: Kummulerede antallet af testede personer
Ændring i antal 
	bekræftede tilfælde	: Antallet af bekræftede tilfælde siden i går
Ændring i antal døde	: Antallet af døde siden i går 
Ændring i antal 		
	overstået infektion	: Antallet af overståede infektioner siden i går
Ændring i antal indlagte: Antallet af indlagte siden i går.
Ændring i antallet af 
	testede personer	: Antallet af testede personer siden i går	
Antallet af prøver		: Kummulerede antallet af prøver
Ændring i antallet af 
	prøver				: Antallet af prøver siden i går


02_Hospitalsbelaegning
-------------------------------------------------------------------------------
Dette er den daglige opgørelse for den covid-19-relatered hospitalsbelægning,
opgjort pr. region.
Bemærk venligst her at der er forskel mellem antallet af personer indlagt 
siden i går, opgjort i filen: <01_Neogletal>, og ændringen i antallet af
indlagte. Ændringen er den reele ændring observeret, hvor antallet af 
indlæggelser siden i går udgør det antal nye indlæggelser observeret 
siden i går.

Dato					: Dato for prøvetagning og evntuel indlæggelses dag
Region					: Indlæggelses region
Indlagte				: Antallet af personer indlagt
Heraf indlagte på 
	intensiv i
	repsiratoir			: Antallet af personer indlagt på intesiv 
Heraf indlagte på 		  og ligger i respirator
	intensiv			: Antallet af personer indlagt på intesiv
Ændring i antal indlagte: Ændringen i antallet af indlagte siden i går	
Ændring i antal 
	indlagte på intensiv
	i respirator		: Ændringen i antallet af indlagte på intensiv, som er
Ændring i antal indlate   i respirator, siden i går
	på intensiv			: Ændringen i antallet af indlagte på intensiv
						  siden i går


03_bekraeftedetilfaelde_doede_indlagte_pr_dag_pr_koen
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde, døde, pr. dag
fordelt på bopælsregioner og køn.

Region					: Bopælsregion (region man boede i ved prøvetagning)		
Dato					: Dato for prøvetagning og evntuel indlæggelses dag
Køn						: Køn, herunder mænd og kvinder 
Bekræftede tilfælde		: Antallet af bekræftede tilælde
Døde					: Antallet af døde
Indlæggelser			: Antallet af indlæggelser
Kummuleret antal døde	: Alle dødsfald fra pandemi start til den given dag
Kummuleret antal 		
	bekræftede tilfælde	: Alle bekræftede tilfælde fra pandemi start til den 
Kummuleret antal 		  given dag.
	indlæggelser		: Alle indlæggelser fra pandemi start til den given dag



04_indlagte_pr_alders_grp_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af indlagte pr aldersgruppe pr. region.
Bemærk venligst at blanke felter under variablen <Region> udgøre de danskere,
som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved indlæggelse)
Aldersgruppe			: Den 10 års aldersgruppe en person tilhørte ved
						  prøvetagning
Indlæggelser			: Kummulerede antallet af indlæggelser



05_bekraeftede_tilfaelde_doede_pr_region_pr_alders_grp
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde og døde pr. region 
og pr. aldergruppe. Bemærk venligst at blanke felter under variablen <Region> 
udgøre de danskere, som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved prøvetagning)
Aldersgruppe			: Den 10 års aldersgruppe en person tilhørte ved
						  prøvetagning
Bekræftede tilfælde		: Kummulerede antallet af bekræftede tilælde
Døde					: Kummulerede antallet af døde



06_nye_indlaeggelser_pr_region_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af nye indlæggelser pr. region pr. dag

Region					: Bopælsregion (region man boede i ved indlæggelse)		
Dato					: Dato for indlæggelse
Indlæggelser			: Antallet af indlæggelser den given dag i den given
						  region


07_antal_doede_pr_dag_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af døde pr. region pr. dag. 
Bemærk venligst at blanke felter under variablen <Region> udgøre de danskere, 
som ikke har en tildelt bopælsregion.

Region					: Bopælsregion (region man boede i ved prøvetagning)
Dato					: Dato for dødsfald registreret i det 
						  dansk dødsårsags register
Kummuleret antal døde	: Kummulerede antal døde siden pandemi start.



08_bekraeftede_tilfaelde_pr_dag_pr_region
-------------------------------------------------------------------------------
Dette er en opgørelse over antallet af bekræftede tilfælde pr. region pr.
dag siden pandemi start.

Region					: Bopælsregion (region man boede i ved prøvetagning)
Dato					: Dato for dødsfald registreret i det 
Bekræftede tilfælde		: Antallet af bekræftede tilælde


09_bekraeftede_tilfaelde_pr_test_region_pr_uge
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfælde pr. uge pr. region
opgjort på den region hvorpå testen blev foretaget.

Uge						: Den uge hvor testen blev foretaget.
TCD Region Hovedstaden	: Alle positive test i region Hovedstaden testet af TCD
Region Hovedstaden		: Alle positive test i region Hovedstaden testet af KMA
TCD Region Sjælland		: Alle positive test i region Sjælland testet af TCD
Region Sjælland			: Alle positive test i region Sjælland testet af KMA
TCD Region Nordjylland	: Alle positive test i region Nordjylland testet af TCD
Region Nordjylland		: Alle positive test i region Nordjylland testet af KMA
TCD Region Syddanmark	: Alle positive test i region Syddanmark testet af TCD
Region Syddanmark		: Alle positive test i region Syddanmark testet af KMA
TCD_Region Midtjylland	: Alle positive test i region Midtjylland testet af TCD
Region Midtjylland		: Alle positive test i region Midtjylland testet af KMA
Sundhedsspor			: Alle positive test registreret af KMA
Samfundsspor			: Alle positive test registreret af TCD
Total					: Det samlede antal  registrede positve test.




10_testede_pr_uge_pr_samfundsspor_opgjort_paa_bopaelsregion
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af testede pr. uge pr. region opgjort på
bopælsregion.

Uge						: Den uge hvor testen blev foretaget.
Region Hovedstaden		: Alle testede i region Hovedstaden testet af KMA
Region Sjælland			: Alle testede i region Sjælland testet af KMA
Region Nordjylland		: Alle testede i region Nordjylland testet af KMA
Region Syddanmark		: Alle testede i region Syddanmark testet af KMA
Region Midtjylland		: Alle testede i region Midtjylland testet af KMA



11_noegletal_pr_region_pr_aldersgruppe
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfædle, døde, indlagte,
indlagte på intensiv afdeling pr. region pr. aldersgruppe


Region					: Bopælsregion (region man boede i ved prøvetagning)
Aldersgruppe			: Den 10 års aldersgruppe en person tilhørte ved
						  prøvetagning
Bekræftede tilfælde		: Antallet af bekræftede tilælde
Døde					: Antallet af døde
Indlagte på intensiv
	afdeling			: Antallet af patienter indlagt på intensiv afdeling.
Indlæggelser			: Antallet af indlagte.



12_noegletal_pr_region_pr_aldersgruppe_de_seneste_7_dage
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfædle, døde, indlagte,
indlagte på intensiv afdeling pr. region pr. aldersgruppe de seneste 7 dage.

Region					: Bopælsregion (region man boede i ved prøvetagning)
Aldersgruppe			: Den 10 års aldersgruppe en person tilhørte ved
						  prøvetagning
Bekræftede tilfælde		: Antallet af bekræftede tilælde
Døde					: Antallet af døde
Indlagte på intensiv
	afdeling			: Antallet af patienter indlagt på intensiv afdeling.
Indlæggelser			: Antallet af indlagte.


13_regionale_kort
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af bekræftede tilfæde, incidensen, bekræftede
tilfælde de seneste 7 dage, incidensen de seneste 7 dage, testede,
test incidensen (antallet af testede i en given region pr. 100.000), testede de
seneste 7 dage, test incidensen de seneste 7 dage, samt positivprocenten de
seneste 7 dage, opgjort pr. region. Læs venligst informationsteksten vedr. 
opgørelsesmetoden for de seneste 7 dage.


Region					: Bopælsregion (region man boede i ved prøvetagning)
Bekræftede tilfælde		: Antallet af bekræftede tilfælde sidne pandemi start
Incidensen				: Antallet af bekræftede tilfædle pr. 100.000 borgere
Bekræftede tilfælde
	de seneste 7 dage	: Antallet af bekræftede tilfælde de seneste 7 dage
Incidensen de seneste
	7 dage				: Antallet af bekræftede tilfælde pr. 100.000 borgere de seneste 7 dage
Testede					: Antallet af testede personer siden pandemi start
Test incidens			:
Testede de seneste 		
	7 dage				: Antallet af testede personer de seneste 7 dage 
Test incidens de 	
	seneste 7 dage		: Antallet af test pr. 100.000 borgere 
Positivprocent 			  de seneste 7 dage
	de seneste 7 dage	: Procenvise antal covid-19 bekræftede personer
						  over antallet af testede personer de senste 7 dage


14_testede_pr_test_region_pr_uge
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af antallet af testede pr. test region 
pr. uge, samt det samlede antal tested ved samfunds- og sunhedsspor.

Uge						: Den uge hvor testen blev foretaget.
TCD Region Hovedstaden	: Alle test i region Hovedstaden testet af TCD
Region Hovedstaden		: Alle test i region Hovedstaden testet af KMA
TCD Region Sjælland		: Alle test i region Sjælland testet af TCD
Region Sjælland			: Alle test i region Sjælland testet af KMA
TCD Region Nordjylland	: Alle test i region Nordjylland testet af TCD
Region Nordjylland		: Alle test i region Nordjylland testet af KMA
TCD Region Syddanmark	: Alle test i region Syddanmark testet af TCD
Region Syddanmark		: Alle test i region Syddanmark testet af KMA
TCD_Region Midtjylland	: Alle test i region Midtjylland testet af TCD
Region Midtjylland		: Alle test i region Midtjylland testet af KMA
Sundhedsspor			: Alle test registreret af KMA
Samfundsspor			: Alle test registreret af TCD
Total					: Det samlede antal test.



15_Indlagte_pr_region_pr_dag
-------------------------------------------------------------------------------
Dette er en opgørelse af antallet af indlagte på en given dag fordelt på 
indlæggelses regioner. Dvs.

Dato					: Dato for indlæggelse 
Region					: Regionen hvor petienten er indlagt
Indlagte				: Antallet af indlæggelser


