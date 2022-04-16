
/********************************************************************************************/

									TP ECONOMETRIE DU MODELE LINEAIRE 
									FAIT PAR : AYIVI KOSSIVI JUSTIN
											   BALDE ALPHA MAMADOU
											   MPAYO MPANI RICHARD

/********************************************************************************************/


merge m:1 Ident_MEN using Ménage_Complet_echantillon_réduit.dta // pour fusionner les deux bases en se basant sur l'ID du menage comme clé primaire de l'autre base

keep if estlourd==1 // on garde pour les individus seulement qui ont été soumis au questionnaire lourd

recode c13a_2 (miss=0), gen (rc13a_2)
recode c13c_2 (miss=0), gen (rc13c_2)
recode c13d_2 (miss=0), gen (rc13d_2)
recode c13b_2 (miss=0), gen (rc13b_2)
gen a_rc13b_2 = rc13b_2*12 //on multiplie par 12 c3b_2 parce que est mensuelle
recode c13e_2 (miss=0), gen (rc13e_2)
gen a_rc13e_2 = rc13e_2*52 //on multiplie par 52 c3e_2 parce que est hebdoma..
recode c13f_2 (miss=0), gen (rc13f_2)
gen a_rc13f_2 = rc13f_2*12 //on multiplie par 12 c3f_2 parce que est mensuelle
recode c13g_2 (miss=0), gen (rc13g_2)
gen a_rc13g_2 = rc13g_2*12 //on multiplie par 12 c3g_2 parce que est mensuelle
recode c13h_2 (miss=0), gen (rc13h_2)
gen a_rc13h_2 = rc13h_2*12 //on multiplie par 12 c3h_2 parce que est mensuelle

*CALCUL DE LA VARIABLE
gen deptotedu = rc13a_2 + rc13c_2 + rc13d_2 + a_rc13b_2 + a_rc13e_2 + a_rc13f_2 + a_rc13g_2 + a_rc13h_2

******************************************************************************
	*SUPPRESSION DES OBSERVATIONS AYANT UNE DEPENSE D'EDUCATION NULLE
	* CETTE SUPPRESSION EST JUSTIFIEE POUR POUVOIR APPLIQUER LE LOG
******************************************************************************
drop if deptotedu == 0
****************************************************************************
	*GENERONS LA LOG DEPENSE D'EDUCATION ANNUELLE
*****************************************************************************
gen l_deptotedu= ln(deptotedu)
****************************************************************************
	*GENERONS LES VARIABLES A PARTIR DES MODALITES DES VARIABLES QUALITATIVES 
		*POUR APPLIQUER LA REGRESSION PAS A PAS DE SELECTION DES VARIABLES
*****************************************************************************

//nivinst_cm (niveau d'instruction cm // n'existe pas non plus
gen nivinst_cm_1=nivinst_cm==1
gen nivinst_cm_2=nivinst_cm==2
gen nivinst_cm_3=nivinst_cm==3
gen nivinst_cm_4=nivinst_cm==4
gen nivinst_cm_5=nivinst_cm==5

//etatmat_cm (굡t matrimonial)
gen etatmat_cm_1=etatmat_cm==1
gen etatmat_cm_2=etatmat_cm==2
gen etatmat_cm_3=etatmat_cm==3
gen etatmat_cm_4=etatmat_cm==4
gen etatmat_cm_5=etatmat_cm==5

//b1 (lien de parent驍
gen b1_1=b1==1
gen b1_2=b1==2
gen b1_3=b1==3
gen b1_4=b1==4
gen b1_5=b1==5
gen b1_6=b1==6
gen b1_7=b1==7
gen b1_8=b1==8
gen b1_9=b1==9
gen b1_10=b1==10
gen b1_11=b1==11
gen b1_12=b1==12
gen b1_13=b1==13

//b4 (situation matri_individu)
gen b4_1=b4==1
gen b4_2=b4==2
gen b4_3=b4==3
gen b4_4=b4==4
gen b4_5=b4==5
gen b4_6=b4==6
gen b4_7=b4==7
gen b4_8=b4==8
gen b4_9=b4==9

//nivinst (niveau d'instruction)

gen nivinst_1=nivinst==1
gen nivinst_2=nivinst==2
gen nivinst_3=nivinst==3
gen nivinst_4=nivinst==4
gen nivinst_5=nivinst==5

//d1 (handicap)
gen d1_1=d1==1
gen d1_2=d1==2
gen d1_3=d1==9

//milieu de r괩dence
recode milieu (2=0), gen(r_milieu)

//a01 (rꨩon)
gen region_1=a01==1
gen region_2=a01==2
gen region_3=a01==3
gen region_4=a01==4
gen region_5=a01==5
gen region_6=a01==6
gen region_7=a01==7
gen region_8=a01==8
gen region_9=a01==9
gen region_10=a01==10
gen region_11=a01==11
gen region_12=a01==12
gen region_13=a01==13
gen region_14=a01==14

//b7b(ethnie
gen ethnie_1=b7b==1
gen ethnie_2=b7b==2
gen ethnie_3=b7b==3
gen ethnie_4=b7b==4
gen ethnie_5=b7b==5
gen ethnie_6=b7b==6
gen ethnie_7=b7b==7
gen ethnie_8=b7b==8
gen ethnie_9=b7b==9
gen ethnie_10=b7b==10
gen ethnie_11=b7b==11
gen ethnie_12=b7b==12
gen ethnie_13=b7b==13

//occupation CM 
gen occupation_cm_1=occupation_cm==1
gen occupation_cm_2=occupation_cm==2
gen occupation_cm_3=occupation_cm==3

//sexe_cm
recode sexe_cm (2=0), gen(r_sexe_cm)

//b1 sexe
recode b2 (2=0), gen(r_sexe_b2)
label define sexe_label 0 "Feminin" 1 "Masculin"
label values r_sexe_b2 sexe_label 
gen age_2=b3*b3

*****************************************************************************
	*REGRESSION PAS A PAS DE SELECTION DES VARIABLES
	*A PARTIR DE LA COMMANDE vselect 
*****************************************************************************
 *** j'ai enlevé r_c10_2 r_c10_3 ce sont les types d'établissement, il faut les chercher et les ajouter à la regression

ssc install vselect  

 vselect l_deptotedu taille b3 Deptot nivinst_cm_2 nivinst_cm_3 nivinst_cm_4 nivinst_cm_5 etatmat_cm_1 etatmat_cm_2 etatmat_cm_3 etatmat_cm_4 etatmat_cm_5 b1_2 b1_3 b1_4 b1_5 b1_6 b1_7 b1_8 b1_9 b1_10 b1_11 b4_2 b4_3 b4_4 b4_5 b4_6 b4_7 b4_8 b4_9 nivinst_2 nivinst_3 nivinst_4 nivinst_5 d1_2 d1_3 r_milieu region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 region_10 region_11 region_12 region_13 region_14 r_sexe_cm r_sexe_b2 ethnie_2 ethnie_3 ethnie_4 ethnie_5 ethnie_6 ethnie_7 ethnie_8 ethnie_9 ethnie_10 ethnie_11 ethnie_12 ethnie_13 occupation_cm_2 occupation_cm_3 [pweight=poids3_a], forward bic


******************************************************************************
	*MATRICE DE CORRELATION DES VARIABLES
******************************************************************************
corr Deptot taille c10 b3

pwcorr Deptot taille c10 b3, sig
 
*****************************************************************************
	*REGRESSION SANS L'OPTION robust
*****************************************************************************
reg l_deptotedu Deptot taille i.c10 i.nivinst r_sexe_b2 i.a01 b3 age_2 [pweight=poids3_a]

*********************************************************************************
	*REGRESSION AVEC L'OPTION robust pour corriger la matrice de var-cov
*****************************************************************************
reg l_deptotedu Deptot taille i.c10 i.nivinst r_sexe_b2 i.a01 b3 age_2 [pweight=poids3_a],robust

*********************************************************************************
	*Resume des variables qualitatives et quantitatives
	
*********************************************************************************

summarize  l_deptotedu Deptot taille b3 

graph hbox Deptot

graph box Deptot, over(nivinst)

tab1 a01 etatmat_cm c10 nivinst r_sexe_b2

graph pie, over(r_sexe_b2) plabel(_all percent, size(*1.5) color(white))

graph pie, over(a01) plabel(_all percent) title("Proportion de dépenses d'éducation au niveau des régions")

tab a01

tab c10

 graph pie, over(c10) plabel(_all name, color(white))

tab b3

sum b3

graph hbox b3

hist taille,percent kdensity fcolor("243 243 255") lcolor("8 8 190")

hist Deptot,percent kdensity fcolor("243 243 255") lcolor("8 8 190")

*****************************************************************************
	*DIAGNOSTIC DE LA REGRESSION
*****************************************************************************
		*Rꤵp곡tion des r괩dus
		predict residu, res
		*Courbe superposꥠde l'histogramme et de la densit顤es densit顡insi que
		*la courbe de la loi normale
		histogram residu, kdensity  normal
		*Le Q-Q plot et le P-P plot
		pnorm residu //P-P plot
		qnorm residu//Q-Q plot
		
*******************************************************************************
	*TEST GRAPHIQUE DE DETECTION D'HETEROSCEDASTICITE
*******************************************************************************
rvfplot, yline(0)

*******************************************************************************
	* CALCUL DU VIF
*******************************************************************************
vif

*******************************************************************************
	*Evaluons l'affirmation : "l'effet de l'㨥 sur les d걥nses d'ꥵcation est plus important
    *chez les filles que chez les garcons": TEST DE SHOW
********************************************************************************
gen b3_d= b3*r_sexe_b
reg l_deptotedu Deptot taille i.c10 i.nivinst i.a01 b3 b3_d r_sexe_b age_2 [pweight=poids3_a]

test _b[r_sexe_b]=0, accum

reg l_deptotedu Deptot taille i.c10 i.nivinst i.a01 b3 age_2 [pweight=poids3_a] if r_sexe_b==1

reg l_deptotedu Deptot taille i.c10 i.nivinst i.a01 b3 age_2 [pweight=poids3_a] if r_sexe_b==0

reg l_deptotedu Deptot taille i.c10 i.nivinst i.a01 b3 r_sexe_b age_2 [pweight=poids3_a]



******************************************************************************
	*TEST D'HETEROSCEDASTICITE: Test de White
******************************************************************************
predict residus,r
gen resid_2= residus^2
gen deptotmen_2= deptotmen^2
gen taille_2= taille^2
gen c10_2= c10^2
gen nivinst2= nivinst^2
gen b2_2= b2^2
gen a01_2= a01^2
gen deptotmen_taille = deptotmen*taille
gen taille_c10 = taille*c10
gen c10_nivinst = c10*nivinst
gen nivinst_b2 = nivinst*b2
gen b2_a01 = b2*a01
gen a01_b3 = a01*b3
gen deptotmen_c10= deptotmen*c10
gen deptotmen_nivinst = deptotmen*nivinst
gen deptotmen_b2 = deptotmen*b2
gen deptotmen_a01 = deptotmen*a01
gen deptotmen_b3 = deptotmen*b3
gen taille_nivinst = taille*nivinst
gen taille_b2 = taille*b2
gen taille_a01 = taille*a01
gen taille_b3 = taille*b3
gen c10_b2 = c10*b2
gen c10_a01 = c10*a01
gen c10_b3 = c10*b3
gen nivinst_a01 = nivinst*a01
gen nivinst_b3 = nivinst*b3
gen b2_b3 = b2*b3
reg resid_2 deptotmen taille b3 age_2 deptotmen_2 taille_2 deptotmen_taille deptotmen_b3 taille_b3 c10 nivinst b2 a01  c10_2 nivinst2 b2_2 a01_2 c10_b2 c10_a01 c10_b3 nivinst_a01 nivinst_b3 b2_b3 taille_c10 c10_nivinst nivinst_b2 b2_a01 a01_b3 deptotmen_c10 deptotmen_nivinst deptotmen_b2 deptotmen_a01 taille_nivinst taille_b2 taille_a01
scalar N_obs = e(N)
scalar R_2=e(r2)
scalar fisher=N_obs*R_2
display fisher


*************************************************************************************************
	*CONSRUCTION DU TABLEAU DE LA PART DES DEPENSE D'ACHAT DE LIVRE DANS LA DEPENSE MOYENNE INDIVIDUELLE 
	* D'EDUCATION
	
mean(deptotedu) //La depense moyenne 
gen deptotlivre_deptotE_pr = c13c_2/deptotedu*100 // Creation de la variable deptotlivre_deptotE_pr
sum deptotlivre_deptotE
tabstat deptotlivre_deptotE
total deptotedu c13a_2 c13b_2 c13c_2 c13d_2 c13e_2 c13f_2 c13g_2 c13h_2
mean deptotlivre_deptotE
tab deptotlivre_deptotE_pr
total rc13a_2 rc13b_2 rc13c_2 rc13d_2 rc13e_2 rc13f_2 rc13g_2 rc13h_2
mean rc13a_2 rc13b_2 rc13c_2 rc13d_2 rc13e_2 rc13f_2 rc13g_2 rc13h_2

*********************************************************************************

		*************************************************
		*					FIN							*
		*************************************************