*##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
*##--------------------------------------------------------------------------------#
*##                                                                                #
*##Statistical Analysis for LungIMPACT                                             #
*##                                                                                #
*##Statistician. Lesley Smith                                                      #
*##                                                                                #
*##Stata 19.5                                                                      #
*##                                                                                #
*##--------------------------------------------------------------------------------#
*##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
*##--------------------------------------------------------------------------------#


*load data 
use LungIMPACT_v1.dta, clear


*************************************************
*Descriptive analysis 
*************************************************
tab AI

*outcomes - based on updated final data 
tab1 CT_indicator_final cancer_indicator_final

*by AI
tab CT_indicator_final AI
tab cancer_indicator_final AI

*summarise number of patients and number of CXRs
tab site first_cxr

*summarise CXRs dates over sites
summdate date_CXR
summdate date_CXR if site==1
summdate date_CXR if site==2
summdate date_CXR if site==3
summdate date_CXR if site==4
summdate date_CXR if site==5

*number of AI abnormal CXRs in each arm
gen ai_normal=1 if ai_nodule==0 & ai_cavity==0 &  ai_mediastinal==0 &  ai_hilar==0 &  ai_plueral==0 &  ai_opacity==0 &  ai_cardiomegaly==0 &  ai_pneumothorax==0 &  ai_other==0  
replace ai_normal=2 if ai_nodule==1 | ai_cavity==1 |ai_mediastinal==1 | ai_hilar==1 | ai_plueral==1 | ai_opacity==1 | ai_cardiomegaly==1 | ai_pneumothorax==1 | ai_other==1 
label define ainormlab 1 "AI normal" 2 "AI abnormal"
label values ai_normal ainormlab
tab ai_normal, m 

tab ai_normal AI
tab ai_normal AI, col

*TABLE 1 - use dtable include age, sex, site and quarter by AI prioritisation 
dtable, by(AI) ///
	sample(, statistic(frequency) place(seplabels)) sformat("(N=%s)" frequency ) ///
	continuous(age, statistics(mean sd) ) ///
	factor(sex site quarter) ///
	nformat(%9.1f mean sd) ///
	title(Table 1. Study population by AI prioritisation) ///
	note("Values are mean (SD) or n (%)") 
collect style cell var[age 2.sex 5.site 6.quarter], border( bottom, width(1))
collect preview
collect export "~Table1_DescStudyPop.docx", replace	

*TABLE  - outcomes by AI prioritisation  (primary and secondary)
tab CT_indicator_final AI
tab cancer_indicator_final AI

*Primary outcomes - time to CT and time to lung cancer 
*secondary outcomes - time to  2WW and time to cancer treatment starting 
dtable if first_cxr==1, by(AI, nototal) ///
    sample(, statistic(frequency) place(seplabels)) sformat("(N=%s)" frequency ) ///
	continuous(time_CXR_CT_days_final time_CXR_LCa_final, statistics(n med q1 q3) ) ///
	continuous(time_CXR_TWW_days_final time_CXR_can_treat_final, statistics(n med q1 q3) ) ///
	nformat(%9.1f median q1 q3) ///
	title(Table 2a1. Time outcomes by AI prioritisation) ///
	note("Values are N, median, Q25 and Q75")
collect preview
collect export "~Table_DescTimeOutcomes.docx", replace	
*time to CT if <14 day and if CT referral= yes 
dtable if first_cxr==1 &  CTlt14days==1, by(AI, nototal) ///
    sample(, statistic(frequency) place(seplabels)) sformat("(N=%s)" frequency ) ///
	continuous(time_CXR_CT_days_final, statistics(n med q1 q3) ) ///
	nformat(%9.1f median q1 q3) ///
	title(Table 2a1. Time outcomes by AI prioritisation) ///
	note("Values are N, median, Q25 and Q75" "Time to CT where CT <14 days CXR")
collect preview
collect export "~Table_DescTimeCTlt14days.docx", replace

dtable if first_cxr==1 &  CT_referral==1, by(AI, nototal) ///
    sample(, statistic(frequency) place(seplabels)) sformat("(N=%s)" frequency ) ///
	continuous(time_CXR_CT_days_final, statistics(n med q1 q3) ) ///
	nformat(%9.1f median q1 q3) ///
	title(Table 2a1. Time outcomes by AI prioritisation) ///
	note("Values are N, median, Q25 and Q75" "Time to CT where CT referral")
collect preview
collect export "~Table_DescTimeCTreferral.docx", replace

*Tables for 2WW referral and cancer diagnosis 
tab cancer_indicator_final AI if first_cxr==1, m col
tab TWW_indicator_final AI if first_cxr==1, m col

dtable if first_cxr==1, by(AI, nototal) ///
	sample(, statistic(frequency) place(seplabels)) sformat("(N=%s)" frequency ) ///
	factor(cancer_indicator_final TWW_indicator_final) ///
	title(Table 2b. Categorical outcomes by AI prioritisation) ///
	note("Values are n (%)")
collect preview
collect export "~Table_DescCategoricalOutcomes1.docx", replace	


*********************************************
*Primary outcome - time to CT 
*********************************************
*Skewed distribution so use logs
bysort AI: summ log_time_CT
bysort AI: summ time_CXR_CT_days_final, d

graph box time_CXR_CT_days_final, over(AI)
graph box time_CXR_CT_days_final, over(AI) over(site)
graph box time_CXR_CT_days_final, over(AI) over(quarter)

* t-test log transformmed times
ttest log_time_CT, by(AI)
* equivalent in regression model
regress log_time_CT i.AI
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_CT i.AI, eform(exp(Coef.))


*Time to CT sensitivity analysis 
*Time to CT restricted to Cts within 14 days  
tab CTlt14days AI
* t-test log transformmed times
ttest log_time_CT if CTlt14days==1, by(AI)
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_CT i.AI if CTlt14days==1, eform(exp(Coef.))

*Time to CT restricted to those with CT referral
tab CT_indicator_final AI if CT_referral==1
* t-test log transformmed times
ttest log_time_CT if CT_referral==1, by(AI)
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_CT i.AI if CT_referral==1, eform(exp(Coef.))

*stratify analysis by site and quarter 
bysort site: regress log_time_CT i.AI , eform(exp(Coef.))
bysort quarter: regress log_time_CT i.AI , eform(exp(Coef.))


*********************************************
*primary outcome - time to lung cancer diagnosis 
*********************************************
*Skewed distribution so use logs
bysort AI: summ log_time_LCa, d
bysort AI: summ time_CXR_LCa_final, d

graph box time_CXR_LCa_final, over(AI)
graph box time_CXR_LCa_final, over(AI) over(site)
graph box time_CXR_LCa_final, over(AI) over(quarter)

* t-test log transformmed times
ttest log_time_LCa, by(AI)
* equivalent in regression model
regress log_time_LCa i.AI
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_LCa i.AI, eform(exp(Coef.))

*stratify analysis by site and quarter 
tab cancer_indicator_final site
tab cancer_indicator_final quarter
bysort site: regress log_time_LCa i.AI , eform(exp(Coef.))
bysort quarter: regress log_time_LCa i.AI , eform(exp(Coef.))


*********************************************
*Secondary outcomes 
*********************************************

*outcome - time to 2WW referral 
tab TWW_indicator_final, m 
bysort AI: summ log_time_TWW
bysort AI: summ time_CXR_TWW_days_final, d 

* t-test log transformmed times
ttest log_time_TWW, by(AI)
* equivalent in regression model
regress log_time_TWW i.AI
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_TWW i.AI, eform(exp(Coef.))

*outcome - time to treatment starting
bysort AI: summ log_time_treat
bysort AI: summ time_CXR_can_treat_final, d

* t-test log transformmed times
ttest log_time_treat, by(AI)
* equivalent in regression model
regress log_time_treat i.AI
*use exponentaited coeffs to get ratio of geometic means 
regress log_time_treat i.AI, eform(exp(Coef.))

*outcome - number of urgernt referrals 
tab TWW_indicator_final 
tab TWW_indicator_final AI if first_cxr==1, m chi col

*outcome - lung cancer incidence
tab cancer_indicator_final
tab cancer_indicator_final AI if first_cxr==1, chi col m

*outcome - lung cancer stage 
*stage recorded for 517/558 cancers
tab stage if cancer_indicator_final==1, m
tab stage if cancer_indicator_final==1

tab stage AI if cancer_indicator_final==1, m
tab stage4 AI if cancer_indicator_final==1, col chi

*agreement analysis 
kap rad_nodule ai_nodule, tab
kap rad_cavity ai_cavity, tab
kap rad_mediastinal ai_mediastinal, tab
kap rad_hilar ai_hilar, tab 
kap rad_plueral ai_plueral, tab
kap rad_opacity ai_opacity, tab
kap rad_cardiomegaly ai_cardiomegaly, tab
kap rad_pneumothorax ai_pneumothorax, tab
kap rad_other ai_other, tab

*table of all outcomes (TP, FP, FN, TN) - numbers only collect in one table
table outcome_nodule, statistic(frequency) 
table outcome_cavity, append statistic(frequency) 
table outcome_mediatinal, append statistic(frequency) 
table outcome_hilar, append statistic(frequency) 
table outcome_pleural, append  statistic(frequency) 
table outcome_opacity, append  statistic(frequency) 
table outcome_cardiomegaly, append statistic(frequency)
table outcome_pneumothorax, append  statistic(frequency) 
table outcome_other, append statistic(frequency)
collect remap outcome_cavity = outcome_nodule 
collect remap outcome_mediatinal = outcome_nodule
collect remap outcome_hilar = outcome_nodule
collect remap outcome_pleural = outcome_nodule 
collect remap outcome_opacity = outcome_nodule
collect remap outcome_cardiomegaly = outcome_nodule
collect remap outcome_pneumothorax = outcome_nodule 
collect remap outcome_other = outcome_nodule
collect style header outcome_nodule, title(hide)
collect style header cmdset, title(hide)
collect label levels cmdset 1 "Nodule", modify
collect label levels cmdset 2 "Cavity", modify
collect label levels cmdset 3 "Mediastinal", modify
collect label levels cmdset 4 "Hilar", modify
collect label levels cmdset 5 "Pleural", modify
collect label levels cmdset 6 "Opacity", modify
collect label levels cmdset 7 "Cardiomegaly", modify
collect label levels cmdset 8 "Pneumothorax", modify
collect label levels cmdset 9 "Other", modify
collect label levels statcmd 1 "N" 2 "%", modify
collect style cell statcmd[2], warn nformat(%4.1f)
collect layout (outcome_nodule outcome_cavity outcome_mediatinal outcome_hilar outcome_pleural outcome_opacity  outcome_cardiomegaly outcome_pneumothorax outcome_other) (cmdset#statcmd)
collect export "~Table_OutcomeAgreement.xlsx", replace	

*For all outcomes the number of cancers diagnosed in each groups
table outcome_nodule if cancer_indicator_final==1, statistic(frequency)
table outcome_cavity if cancer_indicator_final==1, append statistic(frequency) 
table outcome_mediatinal if cancer_indicator_final==1, append statistic(frequency)  
table outcome_hilar if cancer_indicator_final==1, append statistic(frequency)  
table outcome_pleural if cancer_indicator_final==1, append  statistic(frequency) 
table outcome_opacity if cancer_indicator_final==1, append  statistic(frequency) 
table outcome_cardiomegaly if cancer_indicator_final==1, append statistic(frequency)
table outcome_pneumothorax if cancer_indicator_final==1, append  statistic(frequency)  
table outcome_other if cancer_indicator_final==1, append statistic(frequency) 
collect remap outcome_cavity = outcome_nodule 
collect remap outcome_mediatinal = outcome_nodule
collect remap outcome_hilar = outcome_nodule
collect remap outcome_pleural = outcome_nodule 
collect remap outcome_opacity = outcome_nodule
collect remap outcome_cardiomegaly = outcome_nodule
collect remap outcome_pneumothorax = outcome_nodule 
collect remap outcome_other = outcome_nodule
collect style header outcome_nodule, title(hide)
collect style header cmdset, title(hide)
collect label levels cmdset 1 "Nodule", modify
collect label levels cmdset 2 "Cavity", modify
collect label levels cmdset 3 "Mediastinal", modify
collect label levels cmdset 4 "Hilar", modify
collect label levels cmdset 5 "Pleural", modify
collect label levels cmdset 6 "Opacity", modify
collect label levels cmdset 7 "Cardiomegaly", modify
collect label levels cmdset 8 "Pneumothorax", modify
collect label levels cmdset 9 "Other", modify
collect label levels statcmd 1 "N" 2 "%", modify
collect style cell statcmd[2], warn nformat(%4.1f)
collect layout (outcome_nodule outcome_cavity outcome_mediatinal outcome_hilar outcome_pleural outcome_opacity  outcome_cardiomegaly outcome_pneumothorax outcome_other) (cmdset#statcmd)
collect export "~Table_OutcomeAgreementLCa.xlsx", replace	

*tabulate suggsted outcome Y/N 
dtable i.sout_noaction i.sout_lungca i.sout_noduleCT i.sout_otherca i.sout_respreview i.sout_secondarycare i.sout_primarycare
collect export "~Table_SuggestedOutcome.docx", replace	

*numbers of cancers by outcome
tab sout_noaction cancer_indicator_final if sout_noaction!=., m
tab sout_lungca cancer_indicator_final  if sout_lungca!=., m 
tab sout_noduleCT cancer_indicator_final  if sout_noduleCT!=., m
tab sout_otherca cancer_indicator_final  if sout_otherca!=., m 
tab sout_respreview cancer_indicator_final  if sout_respreview!=., m
tab sout_secondarycare cancer_indicator_final  if sout_secondarycare!=., m
tab sout_primarycare cancer_indicator_final  if sout_primarycare!=., m

****************************************************************
*Time to CT by Rad/AI normal/abnornal 

*gen indicator for AI normal/abnormal
gen ai_normal=1 if ai_nodule==0 & ai_cavity==0 &  ai_mediastinal==0 &  ai_hilar==0 &  ai_plueral==0 &  ai_opacity==0 &  ai_cardiomegaly==0 &  ai_pneumothorax==0 &  ai_other==0  
replace ai_normal=2 if ai_nodule==1 | ai_cavity==1 |ai_mediastinal==1 | ai_hilar==1 | ai_plueral==1 | ai_opacity==1 | ai_cardiomegaly==1 | ai_pneumothorax==1 | ai_other==1 
label define ainormlab 1 "AI normal" 2 "AI abnormal"
label values ai_normal ainormlab
tab ai_normal, m

*gen indicator for red report normal/abnormal
gen rad_normal=1 if rad_nodule==0 & rad_cavity==0 &  rad_mediastinal==0 &  rad_hilar==0 &  rad_plueral==0 &  rad_opacity==0 &  rad_cardiomegaly==0 &  rad_pneumothorax==0 &  rad_other==0  
replace rad_normal=2 if rad_nodule==1 | rad_cavity==1 |rad_mediastinal==1 | rad_hilar==1 | rad_plueral==1 | rad_opacity==1 | rad_cardiomegaly==1 | rad_pneumothorax==1 | rad_other==1 
label define radnormlab 1 "Rad normal" 2 "Rad abnormal"
label values rad_normal radnormlab
tab rad_normal, m

*gen indicator for AI and rad report normal/abnormal
gen radai_agree=1 if rad_normal==1 & ai_normal==1  
replace radai_agree=2 if rad_normal==1 & ai_normal==2
replace radai_agree=3 if rad_normal==2 & ai_normal==1
replace radai_agree=4 if rad_normal==2 & ai_normal==2
label define radailab 1 "Rad & AI norm" 2 "Rad norm AI abnorm" 3 "Rad abnorm AI norm" 4 "Rad & AI abnorm"
label values radai_agree radailab
tab radai_agree 
tab ai_normal rad_normal
 
*time to CT for those diagnosed with cancer
bysort radai_agree: summ time_CXR_CT_days_final if cancer_indicator_final==1, d

graph box time_CXR_CT_days_final if cancer_indicator_final==1, over(radai_agree) ///
	title("Time to CT for those diagnosed with cancer" "by radiologist and AI report normal/abnormal") ytitle("Days")

*model with logged times
regress log_time_CT ib4.radai_agree if cancer_indicator_final==1, eform(exp(Coef.))

*time to cancer diagnosis for those diagnosed with cancer
bysort radai_agree: summ time_CXR_LCa_final if cancer_indicator_final==1, d

graph box time_CXR_LCa_final if cancer_indicator_final==1, over(radai_agree) ///
	title("Time to cancer diagnosis for those diagnosed with cancer" "by radiologist and AI report normal/abnormal") ytitle("Days")

	*model with logged times
regress log_time_LCa ib4.radai_agree if cancer_indicator_final==1, eform(exp(Coef.))
