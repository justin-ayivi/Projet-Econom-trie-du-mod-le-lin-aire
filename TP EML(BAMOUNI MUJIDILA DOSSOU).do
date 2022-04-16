********************************************************************************
* TITRE      : Projet Econométrie du modèle lineaire 2019-2020                 *
* ANNEE      : 2019 -2020                                                      *
* PERIODE    : 20 JANVIER - 10 FEVRIER                                         *
* ECOLE      : ENSAE - SENEGAL                                                 *
* AUTEURS    : BAMOUNI CHRISTOPHE--- DOSSOU AUDINET--- MUJIDILA SYLAVIN        *
* PROFESSEUR : M. Idrissa DIAGNE                                               *
********************************************************************************
***************** Préliminaire de l'etude **************************************

*** Répertoire de travail
cd "C:\Users\Christophe J-B B\Desktop\TP EML\DOSSIER DE TRAVAIL"

******************** Creation de notre base  ***********************************
************
use "Individus_echantillon_réduit.dta", clear
*creation de la variable indentifiant menanges
tostring a07b, gen(id1)
tostring a08 , gen(id2)
gen Iden=id2+"_"+id1
saveold "tmpindividus.dta",replace
sort Iden

************
use "Ménage_Complet_echantillon_réduit", clear
*creation de la variable indentifiant menanges
tostring a07b, gen(id1)
tostring a08 , gen(id2)
gen Iden=id2+"_"+id1
sort Iden
saveold "tmpmenages.dta",replace

************
**On fusion les deux bases
use "tmpindividus.dta", clear
merge m:1 Iden using tmpmenages.dta
keep if _merge==3
drop _merge
************
label var Iden "Identifiant de l'individu"
saveold "BaseComplete.dta", replace
************
****************** Installation des packages ***********************************
ssc install labutil
ssc install vselect

****************** Nettoyage des labels des variables **************************
use "BaseComplete.dta", clear
	clear
	unicode analyze BaseComplete.dta
	unicode encoding set latin1
	unicode translate BaseComplete.dta
	use "BaseComplete.dta"
*******************

******************** Depense d'education individuelles *************************
** remplacer les valeurs manquantes par 0
foreach x in a b c d e f g h {
	mvencode c13`x'_2, mv(0)
}
** Ramener toutes les depenses qui etaient en mois ou semaine en depense annuelle
gen frais_inscription = c13a_2
gen frais_scolarite = c13b_2*10
gen frais_livre_fourniture = c13c_2
gen frais_uniforme_tenue = c13d_2
gen frais_nourriture = c13e_2*43
gen frais_transport = c13f_2*10
gen frais_repetiteur = c13g_2*10
gen frais_divers = c13h_2*10

** Calcul de la variable depense
#delimit ;
egen Depense_Education = rowtotal(
									frais_inscription
									frais_scolarite
									frais_livre_fourniture
									frais_uniforme_tenue
									frais_nourriture
									frais_transport
									frais_repetiteur
									frais_divers
								 );
#delimit cr
** Labelisation de la variable
label var Depense_Education "depense d'education individuelle"

** variables explicatives
rename a01 Region
rename b1 Lien_CM
rename b2 Sexe
rename b3 Age
rename b4 Situation_Matrimoniale
rename e29f Nivean_Instruction
rename milieu Milieu_Residence
rename d1 Situation_Handicap
rename Deptot Depense_Totale_Menage
rename taille Taille_Menage
rename nivinst_cm Niveau_Instruction_CM
rename c9 Classe_Actuelle
rename c10 Type_Ecole

recode Type_Ecole (5 9=6)

*creation de la variable cycle d'ecole frequenté
gen Cycle=.
replace Cycle= 1 if  Classe_Actuelle==1
replace Cycle= 2 if  Classe_Actuelle>=2 & Classe_Actuelle<=7
replace Cycle= 3 if  Classe_Actuelle>=8 & Classe_Actuelle<=14
replace Cycle= 4 if  Classe_Actuelle>=15 & Classe_Actuelle<=20
replace Cycle= 5 if  Classe_Actuelle==21 & Classe_Actuelle==99

#delimit ;
label define cycleLab 1 "Préscolaire" 
					  2 "Primaire" 
					  3 "Secondaire" 
					  4 "Superieur" 
					  5 "Ne sait pas";
#delimit cr
label value Cycle cycleLab
label var Cycle "Cycle d'enseignement"
*********
save "BaseComplete.dta",replace

*****************************************
**recuperation de l'age du CM
keep if Lien_CM==1
keep Iden Age
rename Age Age_CM
sort Iden
save "baseAgeCm.dta", replace
***********
use "BaseComplete.dta",clear
sort Iden
merge m:1 Iden using baseAgeCm.dta
keep if _merge==3
drop _merge

label var Age_CM "Age du CM"
******************************************
******** selection des variables de la base

keep Iden Cycle Type_Ecole Age_CM Niveau_Instruction_CM Depense_Education  Depense_Totale_Menage Region Lien_CM Sexe Age Situation_Matrimoniale  Situation_Handicap Taille_Menage Milieu_Residence poids3_a

******************
***** On supprime ceux qui ont une depense non nulle
drop if Depense_Education==0
saveold baseTravail.dta, replace

********************* Appurement de la base *********************
*********
use "baseTravail.dta", clear
gen lnDepeduc= log(Depense_Education)
gen lnDepTotal= log(Depense_Totale_Menage)
label var lnDepTotal "Logarythme des dépenses totales"
label var lnDepeduc "Logarythme des dépenses d'education"
********* variabilité
*tri d'une variable
sort Cycle
gen indice=_n
label var indice " numero d'observation"

scatter Depense_Education indice // pour voir la variabilité
scatter lnDepeduc indice
graph box lnDepeduc
***********
scatter Depense_Totale_Menage indice  // pour voir la variabilité
scatter lnDepTotal indice 

********* Situation_Handicap
tab Situation_Handicap
recode Situation_Handicap (9=2) // imputer les "non déclarer" aux sans handicap
hilo Depense_Education

* recodage de la situation matrimoniale en deux modalités compte tenu de la structure
tab Situation_Matrimoniale
recode Situation_Matrimoniale (1 2 3 4 5 7 8 9 = 20) (6=10)
label define lab_Sit_Mat 10 "Célibatire" 20 "Non Célibatire"
label values Situation_Matrimoniale lab_Sit_Mat

* recodage du lien de parenté
tab Lien_CM [iw=poids3_a], nolab
recode Lien_CM (1 2 5 8 9 12 13 = 50) (3 = 10 ) (10 = 20) (6 = 30) (11 = 40)
label define lab_lien 10 "Fils/Filles" 20 "Petit-Enfant" 30 "Neveu/Nièce" ///
						40 "Autres parents" 50 "Autres"
label values Lien_CM lab_lien

*appurement de lavariable Type_Ecole frequentée
recode  Type_Ecole (3 4=3 )(6=4) //Affectation de cieux de l'ecole PRIVEE CATHOLIQUE ET CEUX DE L'ECOLE PRIVEE LAIC à ecole privé 
label define typeLab 1 "PUBLIC FRANCAIS" 2 " ECOLE FRANCO ARABE" 3 "ECOLE PRIVEE" 4 "AUTRE"
label value Type_Ecole typeLab
 
*Appurement de nivea d'instruction du CM
recode Niveau_Instruction_CM (4 3=3) (5=4) // affectation de moyen et secondaire en niveau sécondaire
label define NiveauLab 1 " Sans instruction" 2 "Primaire" 3 " Secondaire" 4 " Supérieur"
label value Niveau_Instruction_CM NiveauLab

******dichotomisation des variables
gen Feminin=(Sexe==2)
label define labfeminin 1 "FEMININ" 0 "MASCULIN"
label values Feminin labfeminin
label var Feminin "Est de sexe Feminin"

gen Niv_CM=(Niveau_Instruction_CM==1)
label define labniv_cm 1 "Aucun" 0 "Primaire et plus"
label values Niv_CM labniv_cm
label var Niv_CM "Nivean d'instruction du CM"

gen Fils_Filles=(Lien_CM==10)
label define labfils_fille 1 "Fils/Filles" 0 "Autre parenté"
label values Fils_Filles labfils_fille
label var Fils_Filles "Est fils ou filles du CM"

gen Milieu=.
replace Milieu =1 if Region ==1
replace Milieu =2 if Milieu_Residence==1 & Region!=1
replace Milieu =3 if Milieu_Residence ==2 & Region!=1

label var Milieu "Milieu de résidence"
#delimit;
label define labmilieu 
						1 "Dakar"
						2 "Autre urbain"
						3 "Rural";
#delimit cr
label values Milieu labmilieu

saveold baseTravail.dta, replace


********************************************************************************
********************************************************************************
***********************    REPONSES AUX QUESTIONS     **************************
use "baseTravail.dta", clear
			
********************************************************************************
***  1-) Démarche pour réduire le nombre de variables à 7
****selection des regresseurs
vselect lnDepeduc Cycle Type_Ecole Age_CM Niveau_Instruction_CM   lnDepTotal Milieu Lien_CM Sexe Age Situation_Matrimoniale  Situation_Handicap Taille_Menage, best 


********************************************************************************
***   2-) Description des variables du modèle
foreach var of varlist  Region Lien_CM Sexe Age Type_Ecole Type_Ecole Situation_Handicap Milieu_Residence Cycle{
				 tab `var'
}
				   	   
foreach var of varlist  Taille_Menage Depense_Totale_Menage Depense_Education lnDepeduc lnDepTotal Age_CM{
			sum `var'
}

********************************************************************************
***   3-) Régression MCO
** Nous faisons d'abord le diagnostic avant la regression finale.

reg lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles  

*****************************************************************************
	*DIAGNOSTIC DU MODELE AVANT LA REGRESSION
*****************************************************************************
***** residu studentisé
predict rstd, rstudent

****** visualisation des outliers
*graph twoway scatter rstd indice, yline(-2 2)
*pour indexer les individus sur le graphe
graph twoway scatter rstd indice, yline(-2 2) 
count if abs(rstd)>=2 & abs(rstd)<.  

* Pour predire les hii de Pregibom (1981) on met en option hat prediction des hii 
******
predict hii,hat
graph twoway scatter hii indice, yline(0.5)

**Examinons maintenant l'effet de levier pour identifier les observations qui auront une grande influence potentielle sur les estimations des coefficients de régression

***** levier
predict lev, leverage
graph twoway scatter lev indice, yline(0.012)

*Généralement, un point avec un effet de levier supérieur à (2k + 2) / n doit être soigneusement examiné. Ici k est le nombre de prédicteurs et n est le nombre d'observations.

count if lev > 0.01 & lev<.
lvr2plot

**Passons maintenant aux mesures globales d'influence, en particulier au D et aux DFITS de Cook. Ces mesures combinent à la fois des informations sur le résidu et l'effet de levier. Cook’s D et DFITS sont très similaires, sauf qu'ils évoluent différemment, mais ils nous donnent des réponses similaires.

*La valeur la plus basse que le D du cuisinier peut supposer est zéro, et plus le D du cuisinier est élevé, plus le point est influent. Le point de coupure de la convention est 4 / n. 
*Nous pouvons répertorier toute observation au-dessus du point de coupure en procédant comme suit. Nous voyons que le Cook’s D pour DC est de loin le plus grand.

predict dcook, cooksd
label var dcook "cook's statistique"
graph twoway scatter dcook indice, yline(0.008)

*Voyons maintenant les DFITS. Le point de coupure pour DFITS est de 2 * sqrt (k / n). Les DFITS peuvent être positifs ou négatifs, avec des nombres proches de zéro correspondant aux points avec une influence faible ou nulle. 

predict dfit, dfits
graph twoway scatter dfit indice, yline(-0.3 0.3)

*****comptage des outliers et influents
count if hii >0.5 & hii<.
count if abs(rstd)>=2 & abs(rstd)<.
count if lev > 0.1 & lev<.
count if dcook >0.4 & dcook<.
count if abs(dfit) > 0.5 & dfit<.

*****suppression des outliers et influents
drop if hii >0.5 & hii<.
drop if abs(rstd)>2 & abs(rstd)<.
drop if lev > 0.1 & lev<.
drop if dcook >0.1 & dcook<.
drop if abs(dfit) > 0.5 & dfit<.

********************************************************************************
**********             FIN DU DIAGNOSTIC                          **************
********************************************************************************
********************************************************************************

*******nouvelle regression apres diagnostic
reg lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles  [aw=poids3_a]  

***  Test de Ramsey: d'omission de variables importantes
estat ovtest

*** Avec l'option robust
reg lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles  [aw=poids3_a] , robust 

********************************************************************************
***   4-) Diagnostic graphique de la régression

*Récupération des résidus
predict residu, residual

************* TEST DE NORMALITE DES ERREURS
kdensity residu, normal
*******
*Courbe superposée de l'histogramme et de la densité des densité ainsi que
*la courbe de la loi normale
*******
histogram residu, kdensity  normal

*Le Q-Q plot et le P-P plot
*******
pnorm residu //P-P plot
qnorm residu //Q-Q plot

*******test de normalité
sktest residu

********************************************************************************
***   5-) Testons l'hypothèse d'hétéroscédasticité
************ Tests graphiques
rvfplot 
************ Tests non graphiques
** IMPLEMENTATION

***Test de White
capture program drop white
program define white
quietly regress  lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles ,notable noheader
*on recupère les residus de la premiere  regression
predict  resid,residual
gen resid2 = resid^2 ,
*on regresse le carré des residus sur l'ensemble variables explicatives et leurs interaction deux à deux
quietly regress resid2 c.lnDepTotal##(c.lnDepTotal i.Type_Ecole i.Milieu i.Fils_Filles i.Feminin c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu)  i.Type_Ecol##(i.Type_Ecole i.Milieu i.Fils_Filles i.Feminin c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu)  i.Milieu##( i.Milieu i.Fils_Filles i.Feminin c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu)  i.Fils_Fille##(i.Fils_Filles i.Feminin c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu) i.Feminin##( i.Feminin c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu) c.Age##c.Age##(c.Age##c.Age i.Cycle i.Type_Ecole#i.Milieu) i.Cycle##( i.Cycle i.Type_Ecole#i.Milieu)  i.Type_Ecole#i.Milieu#(i.Type_Ecole#i.Milieu) ,notable noheader
display "Test d'heroscedasciticite"
display "Ho:Homoscedaciticité contre H1:Heteroscedasciticité"
display  "{hline 21}{c TT}{hline 29}" 
*on construit la staistique du test
scalar LM=e(N)* e(r2) 
display "la statistique de white calculée est:" ,LM
display  "{hline 21}{c TT}{hline 29}"
*la p_valeur R2 de la deuxieme regression de carré des résidus  
scalar p_valeur= 1-chi2(e(df_m),e(N)* e(r2))
display "la p_valeur est: " ,p_valeur
display  "{hline 21}{c TT}{hline 29}"
*la p_valeur R2 de la deuxieme regression de carré des résidus   
end
 white


***Test de Breusch-Pagan

capture program drop BreushPagan
*test de breusch pagan
program define BreushPagan
regress lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles ,notable noheader
*on recure le residus de la pss gi Depense_Totale_Menage i.Region i.Lien_CM i.Sexe Age i.Situation_Maremiere  regression
predict  residus,residual
gen gi = residus^2*e(N)/e(rss)
regress gi i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles ,notable noheader
*la statistique du test est c'est la moitié de la somme de carré
 display "la statistique du test de breusch-pagan est:"  e(mss)/2
 display "la p_valeur est: " , 1-chi2(e(df_m),e(N)* e(r2))

*La p_valeur

end
BreushPagan


********************************************************************************
***   6-) Evaluons l'hypothèse de multicolinéarité

***  On refait la régression 
quietly reg lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age##c.Age i.Feminin i.Fils_Filles  [aw=poids3_a]  // quietly pour ne pas afficher le tableau de regression

***  On test
vif // ou collin (si installé) 
*****
********************************************************************************
***   7-) Evaluation de l’affirmation : "l'effet de l'âge sur les dépenses 
***       d’éducation est plus important chez les filles que chez les garçons"
****** ON regresse avec interaction

reg lnDepeduc i.Type_Ecole##i.Milieu lnDepTotal ib2.Cycle  c.Age#c.Age c.Age##i.Feminin i.Fils_Filles  [aw=poids3_a]  


                   **************************************************
				   *                                                *
				   *                 FIN DU TP                      *
				   *                                                *
				   **************************************************