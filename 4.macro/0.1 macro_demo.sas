/* ************************************************************** */
/* Analysis 2: demographic information (table one) 		  	  	  */
/* ************************************************************** */


/*clean the codes file: delete the empty title lines*/
proc sql;
	create table sasfiles.codesmnd as
	select * from (select * from sasfiles.codesmnd) where not missing(Dx);
quit;

/*create a backup*/
data sasfiles.Demo_;
	set sasfiles.Demo;
	where onset_date between "&studydate_st."d and "&studydate_ed."d and (dod >= onset_date or missing(dod));
run;

/*age*/
data sasfiles.Demo_;
set sasfiles.Demo_;
age=(onset_date - dob)/365.25;
run;

/*get the treatment grouping*/
%get_expo();

/*proc sql;*/
/*select count(id) from sasfiles.Demo_ where exposure =1;*/
/*quit;*/

/*get subtypes of the MND*/
%get_subtype(5);

/*count of past hx dx needed*/
proc sql noprint;
	select count(dx) into :nohxdx from sasfiles.codesmnd where Dx is not NULL;
quit;

/*%put no. of variables = &nohxdx.;*/
/*%PUT _user_;*/
%get_past_hx(&nohxdx.)

/*count of prescription needed*/
proc sql noprint;
	select count(Name) into :nohxrx from sasfiles.codesrx where Name is not NULL;
quit;
/*%PUT &nohxrx.;*/
%get_past_rx(&nohxrx.)

/*add cci*/
data sasfiles.Demo_;
	set sasfiles.Demo_;
	dx_dm_com01=0;
	if dx_dm_com0=1 and dx_dm_com1=0 then dx_dm_com01=1;
	dx_liver_mild01=0;
	if dx_liver_mild=1& dx_liver_modsev = 0 then dx_liver_mild01 = 1;
	score_cci = dx_mi+dx_chf+dx_pvd+dx_cbd+dx_copd+dx_dementia+dx_paralysis+dx_dm_com01+dx_dm_com1*2+dx_crf*2+dx_liver_mild01+
					dx_liver_modsev*3+dx_ulcers+dx_ra+dx_aids*6+dx_cancer*2+dx_cancer_mets*6;
	if age >= 50 & age < 60 then score_cci= score_cci+1;
	if age >= 60 & age < 70 then score_cci= score_cci+2;
	if age >= 70 & age < 80 then score_cci= score_cci+3;
	if age >= 80 then score_cci= score_cci+4;
run;

/* create table one*/
%table1(in_for_table1=sasfiles.Demo_,
		treatment_var=exposure,
		continuous_var_list = age score_cci,
		categorical_var_list = sex subtype_als subtype_pma subtype_pbp subtype_pls subtype_others dx_arf	dx_et	dx_pd	dx_depre	dx_mi	dx_pvd	dx_cbd	dx_copd	dx_dementia	dx_paralysis	dx_dm_com0	dx_dm_com1	dx_crf	dx_liver_mild	dx_liver_modsev	dx_ulcers	dx_ra	dx_aids	dx_cancer	dx_cancer_mets	dx_stroke_embo	dx_asthma	dx_neoplasms	dx_infection_resp	dx_infection_viral	dx_smoking	dx_migraine	dx_ramsayhunt	dx_chf	dx_htn	dx_dm	dx_stroke_isch	rx_ras	rx_bb	rx_ccb	rx_diuretic	rx_nitrates	rx_lipid	rx_insulin	rx_dm	rx_af	rx_inotrope	rx_oac	rx_pac	rx_apt	rx_fibhemo	rx_hormone	rx_steroid	rx_antidepressant	rx_ivig	rx_nsaid	rx_gout	rx_epilepsy	rx_antiviral	rx_antibiotics,
		out_table1=table1)

proc export data=table1
    outfile="&outdir/mnd.xlsx"
    dbms=xlsx
    replace;
    sheet="table 1 demo";
run;
