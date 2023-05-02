/****************************************************************************
*  Macro name : motor neurone disease                                       *
*  Version    : 1.00                                                        *
*  Author     : FAN Min		                                                *
*  Date : 26JUL2022                                                         *
*  ------------------------------------------------------------------------ *
*  Revisions :   Versions     Date       Author                             *
*                                                                           *
*                 0.91       15JAN2023   FAN Min							*
*                 Creation of the macro for shrinking date interval         *
*                 0.9        13JAN2023 	 FAN Min                            *
*    		      SCCS model data cleanning									*
*				  0.8	     09JAN2023 	 FAN Min							*
*				  AFT model finished. 4 digiits difference with R results   *
*				  0.7	     31DEC2022   FAN Min							*
*				  Finished Cox regression. fixed. Same resutls with R       *
*                 0.6        01NOV2022   FAN Min                            *
*                 Add function for obtain past rx                           *
*                 0.5        31OCT2022   FAN Min		                    *
*                 Description of the dataset for table one                  *
*				  0.1		 11AUG2022   FAN Min							*
*				  Macro for reading raw dataset and get past Dx hx			*
*  ------------------------------------------------------------------------ *
*  Description : Macro to create dataset for case series analysis           *
*  ------------------------------------------------------------------------ *
*  INPUT - parameters                                                       *
*                                                                           *
*     data     :  Input dataset                                             *
*     pid      :  subject ID number                                         * 
*     dob_raw  :  Date of birth                                             *
*     events   :  Dates for events                                          *
*     vacc     :  Dates for vaccinations                                    *
*     startst  :  Study start                                               *
*     endst    :  Study end                                                 *
*     covars   :  Covariates to be put in output dataset                    *
*     overlap  :  Allow overlapping risk intervals (Default=N)              *
*                                                                           *
*  INPUT - macros variables (other than standards)                          *
*                                                                           *
*     agerange : e.g., 0 730 (in days)                                      *
*     risk     : risk periods after vaccination, e.g., 0 6 7 14             *
*                 (first period from 0 to 6 days                            *
*                 and a second period from 7 to 14 days)                    *
*     age      : age cutpoints for age covariates in model,                 *
*                 e.g., 90 180 270 360                                      *
*     season   : season cutpoints for season covariates in model,           *
*                 e.g., 31MAR 30JUN 30SEP 31DEC                             *
*                                                                           *
*  INPUT - datasets from lib_data                                           *
*                                                                           *
*                                                                           *
*  INPUT - datasets from work                                               *
*                                                                           *
*  ------------------------------------------------------------------------ *
*  OUTPUT - macros variables                                                *
*                                                                           *
*                                                                           *
*  OUTPUT datasets in  work                                                 *
*                                                                           *
*     wk_sccs                                                               *
*     This dataset contains a class variable for age and indicator variables*
*     for the different risk periods:                                       *
*       RISK = 1 if the interval lies in a risk period                      *
*       RISKVi = 1 if the interval lies in a risk period after dose i       *
*       RISKRj = 1 if the interval lies in the jth risk period over all     *
*                doses                                                      *
*       RISKk = 1 if the interval lies in the kth risk period (each risk    *
*                 period after each dose has a separate indicator variable) *
*       When overlap=Y, overlapping intervals are possible and these        *
*       variables should be used with caution                               *
*  ------------------------------------------------------------------------ *
*  UPDATE - table                                                           *
*                                                                           *
****************************************************************************/

/*define codes system and excel folder in your study sites*/
%let sysdx=icd9;
%let sysrx=atc;
%let studydate_st=01jan1994;
%let studydate_ed=31dec2018;
%let sccs_obs_st=24aug2001;
%let sccs_obs_ed=31dec2018;
%let wd = C:/Users/LabPCSLi03/Desktop/mnd project;

options dlcreatedir; /* Create folders */
/* working path for my projects */
%let rc = %sysfunc(dlgcdir("&wd."));



filename dx "./1.raw data/dx.xlsx";
filename ip "./1.raw data/ip.xlsx";
filename rx "./1.raw data/rx.xlsx";
filename demo "./1.raw data/demo.xlsx";
filename codesmnd "./1.raw data/codes_mnd.xlsx";

/* Define macro and output directories */
libname sasfiles "./2.sas data";
libname output "./3.output";

%LET macdir = C:\Users\LabPCSLi03\Desktop\sas macro\macro;
%LET outdir = &wd/3.output;
%LET indir =  C:\Users\LabPCSLi03\Desktop\sas macro\input;

/* Read in macros */
%INCLUDE "0.macro_mnd.sas";
%INCLUDE "0.Table 1s.sas";
%INCLUDE "&macdir\sccs.sas";
%INCLUDE "&macdir\poisreg.sas";
%INCLUDE "&macdir\element.sas";
%INCLUDE "&macdir\macro_sccs.sas";

/*SCCS*/
OPTIONS nodate nonumber;

/* Data reading */
%reading(filename=demo,out=demo)
%reading(filename=dx,out=dx)
%reading(filename=ip,out=ip)
%reading(filename=rx,out=rx)

%reading(filename=codesmnd,out=codesmnd)
%reading(filename=codesmnd,sheet=hx,out=hx)
%reading(filename=codesmnd,sheet=subtype,out=subtype)
%reading(filename=codesmnd,sheet=rx,out=codesrx)

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
    outfile="3.output/mnd.xlsx"
    dbms=xlsx
    replace;
    sheet="table 1 demo";
run;

/*coxph analysis - time dependent exposure survival analysis*/
proc sql;
	create table dt4cox as 
	(select a.id as id format = $8., /*onset_date,b.date_rx_st, b.date_rx_end,*/
		   case /* start of drug*/
			   when b.date_rx_st-a.onset_date<0 then 0
			   when b.date_rx_st-a.onset_date>=0 then b.date_rx_st-a.onset_date
		   end as strx, 
		   case /* end of drug*/
		   	   when b.date_rx_st < a.onset_date then 0
		   	   when b.date_rx_st >= a.onset_date and b.date_rx_end - a.onset_date <0 then 0
			   when b.date_rx_st >= a.onset_date and b.date_rx_end - a.onset_date >=0 then min(min("&studydate_ed."d,a.dod)-a.onset_date,b.date_rx_end-a.onset_date+1)
		   end as edrx,
		   0 as astart,
		   case
		   when min("&studydate_ed."d, a.dod) - a.onset_date=0 then 1
		   else min("&studydate_ed."d, a.dod) - a.onset_date 
		   end as aend from 
		   (select id, onset_date, exposure,  dod from sasfiles.Demo_) as a 
			left join ttt as b on a.id=b.id 
			where b.date_rx_st >= a.onset_date or b.date_rx_st=.) order by id, strx;
quit;

proc sort nodupkey  data=dt4cox;
by _all_;
run;

data dt4cox;
	set dt4cox;
	by id;
		seq_id+1;
		if first.id then seq_id=1;
run;

/*proc sort data=dt4cox;*/
/*	by id seq_id;*/
/*run;*/

data abc_drug (keep= id strx edrx riluzole);
set dt4cox;
if not edrx=0
then riluzole=1;
else riluzole=0;
run;

proc transpose data = dt4cox out = dt4cox(Drop = _NAME_ seq_id) ;
    by id seq_id;
	var strx edrx astart aend;
run;

proc sort data=dt4cox nodup;
	by id col1;
run;

data dt4cox;
set dt4cox(rename=(col1=tstop));
by id;
tstart = lag1(tstop);
if first.id then tstart = .;
run;

data dt4cox;
retain id tstart tstop;
set dt4cox (where=( not missing(tstart)));
run;

proc sql;
	create table dt4cox as
	select x.id, x.tstart, x.tstop, 
	case
	when y.riluzole=0 then 0
    when y.riluzole=. then 0
	when y.riluzole=1 then 1 
	end as riluzole from 
	(select * from dt4cox) as x left join (select * from abc_drug) as y on x.id = y.id and x.tstart =  y.strx;
quit;
proc sort data=dt4cox nodup;
	by _all_;
run;

proc sql;
	create table dt4cox1 as
	select y.id, x.tstart, x.tstop, x.riluzole, 
	x.tstop-x.tstart as time,
	y.* from 
	(select *,dod-onset_date as aend from sasfiles.Demo_) as y left join 
	(select * from dt4cox) as x 
	 on x.id=y.id;
quit;

data dt4cox1;
set dt4cox1;
if tstart=. then tstart=0;
if tstop=. then tstop=min("&studydate_ed."d, dod)-onset_date;
if riluzole=. then riluzole=0;
if tstop=0 then tstop=1;
if time=. then time=tstop-tstart;
if aend >= tstart and aend<=tstop then endpt=1;
else endpt=0;
run;

proc export data=dt4cox1
    outfile="C:/Users/LabPCSLi03/Desktop/mnd project/test.xlsx"
    dbms=xlsx
    replace;
    sheet="data4cox";
run;

/*time dependent exposure analysis on Cox regression */
proc phreg data=dt4cox1;
 id id time endpt;
 class sex(ref='F') dx_htn(ref="0") dx_depre(ref="0") dx_pd(ref="0")  / param=glm;
 model (tstart,tstop)*endpt(0)=riluzole sex dx_htn dx_depre dx_pd score_cci / ties=efron;
 output out=obs ;
run;

/*accelarate failure time model for sensitivity analysis*/
data dt4aft; set dt4cox1;run;
proc sort data=dt4aft;
by descending sex descending dx_htn descending dx_depre descending dx_pd; 
run;

proc lifereg data=dt4aft order=data;
 class sex dx_htn dx_depre dx_pd ;
 model time*endpt(0)=riluzole sex dx_htn dx_depre dx_pd score_cci / dist=weibull noscale ;
 output out=obs_aft;
run;

/* ************************************************************** */
/* Analysis 4: SCCCS analysis between hospitalization and rilozle */
/* ************************************************************** */

/*data cleanning*/

%shrink(source=RX,Remove_IP_Rx=TRUE,st=date_rx_st,end=date_rx_end,out=shrankrx,gap=1);
%shrink(source=IP,st=date_adm, end=date_dsg,out=shrankip,gap=7)

/*keep ae information*/
proc sql;
	create table shrankip as
	select a.id,a.date_adm,a.date_dsg,b.ae from 
		(select * from shrankip) as a left join 
		(select id, date_adm, sum(ae)>0 as ae from sasfiles.ip group by id,date_adm) as b on a.id=b.id and a.date_adm=b.date_adm;
quit;

proc sql;
create table sccs_dt as
	select distinct * from 
	(select * from shrankrx) as a left join 
	(select id, min(date_rx_st) format=yymmdd10. as earliest_rx_date 
		from shrankrx group by id) as b on a.id=b.id 
	left join (select * from sasfiles.Demo) as c on b.id = c.id 
	left join (select distinct * from shrankip) as d on c.id = d.id
	where date_dsg <= "&sccs_obs_ed."d and date_dsg <>.  ; 
quit;


proc sort data=sasfiles.dx out=sasfiles.dx;
    by id ref_date;
run;

data dx_ip;
set sasfiles.dx;
if setting="I" then output;
run;

data collapse_dx;
	length codes_all $30;
   do until (last.ref_date);
      set dx_ip;
        by id ref_date;
      codes_all=catx(',',codes,codes_all);
   end;
   drop codes;
run;

proc sql;
	create table sccs_dt as
	select a.id, a.date_rx_st, a.date_rx_end, a.earliest_rx_date, a.sex, a.dob, a.dod, a.onset_date, 
		 a.date_adm, a.date_dsg,
		 a.ae, b.codes from 
	(select * from sccs_dt) as a left join
	(select id, ref_date, codes_all as codes from collapse_dx where setting="I") as b on a.id=b.id and a.date_dsg=b.ref_date;
quit;

proc sql;
    create table sccs_dt as 
	(select id, sex, dob, dod, onset_date, date_rx_st, date_rx_end as date_rx_ed, date_dsg, date_adm, 
			obst as obst,
			obed as obed,
			date_adm as event,
			date_dsg as endevent,
			date_rx_st as drug_st,
			date_rx_end as drug_ed from
	(select *, 
			 max(min(a, b),"&sccs_obs_st."d) format=mmddyy10. as obst,
		     min(min(onset_date, earliest_rx_date) + 365*2, "&sccs_obs_ed"d,dod) format = mmddyy10. as obed from
	(select *, onset_date - 365 format=mmddyy10. as a, earliest_rx_date - 365 format=mmddyy10. as b from sccs_dt)));
quit;

/*keep records within range of observation*/
proc sql;
	create table sccs_dt as
	select * from sccs_dt where drug_st>=obst & drug_st<= obed & event >=obst & event <= obed;
quit;


/* Define name of batch */
%LET batch=MND;


%deduptrans(data=sccs_dt,out=sccs_dt_event,var=event,prefix=event)
%deduptrans(data=sccs_dt,out=sccs_dt_drugst,var=drug_st,prefix=drug_st)
%deduptrans(data=sccs_dt,out=sccs_dt_druged,var=drug_ed,prefix=drug_ed)
/* Transform data to one line per subject */


proc sql;
create table sccs_dt_e_r as
	select * from 
	(select * from sccs_dt_event) as a left join
	(select * from sccs_dt_drugst) as b on a.id=b.id and a.obst=b.obst and a.obed=b.obed left join
	(select * from sccs_dt_druged) as c on a.id=c.id and a.obst=c.obst and a.obed=c.obed;
quit;

/*Delete the seperated temperal datasets*/
proc sql;
   drop table work.sccs_dt_event;
   drop table work.sccs_dt_drugst;
   drop table work.sccs_dt_druged;
quit;

/* Agerange defines the age range for each subject
In this example, the observation period goes from 366 (inclusive) to 730 (inclusive) 
days */
%LET agerange=;

/* Define risk periods */
%LET risk= -30 0 1 30 31 60 61 90 91 120 121 150 151 180 181 99999999;

/* Define age categories */
%LET age= 0 7300 10950 14600 18250 21900 25550 29200 32850;

/* No adjustment for seasonality is used in this example */
%LET season= 01MAR 01MAY 01SEP 01NOV;

/* No semiparametric analysis is done */
%LET semi=N;

/**/
/**/
/*%macro ob_col_names;*/
/*	proc sql noprint;*/
/*	select name into : vars_event separated by ' '*/
/*	from dictionary.columns*/
/*	where LIBNAME = upcase('work')*/
/*	and MEMNAME = upcase('sccs_dt_e_r')*/
/*	and upcase(name) like 'EVENT_%';*/
/*	quit;*/
/**/
/*	proc sql noprint;*/
/*	select name into : vars_rx_s separated by ' '*/
/*	from dictionary.columns*/
/*	where LIBNAME = upcase('work')*/
/*	and MEMNAME = upcase('sccs_dt_e_r')*/
/*	and upcase(name) like 'DRUG_ST%'*/
/*	where count(*)>1;*/
/*	quit;*/
/**/
/*	proc sql noprint ;*/
/*	select name into : vars_rx_e separated by ' '*/
/*	from dictionary.columns*/
/*	where LIBNAME = upcase('work')*/
/*	and MEMNAME = upcase('sccs_dt_e_r')*/
/*	and upcase(name) like 'DRUG_ED%';*/
/*	quit;*/
/**/
/*		*/
/*	proc sql noprint ;*/
/*	select name into : vars_rx_s_new separated by ' '*/
/*	from dictionary.columns*/
/*	where LIBNAME = upcase('work')*/
/*	and MEMNAME = upcase('sccs_dt_e_r')*/
/*	and upcase(name) like 'DRUG_ST_%'*/
/*	and LENGTH(COMPRESS(name)) = 10;*/
/*	quit;*/
/**/
/*	proc sql noprint ;*/
/*	select name into : vars_rx_e_new separated by ' '*/
/*	from dictionary.columns*/
/*	where LIBNAME = upcase('work')*/
/*	and MEMNAME = upcase('sccs_dt_e_r')*/
/*	and upcase(name) like 'DRUG_ED%'*/
/*	and LENGTH(COMPRESS(name)) = 10;*/
/*	quit;*/
/*%mend ob_col_names;*/
/*%ob_col_names;*/
/**/
/*%put &vars_event;*/
/*%put &vars_rx_s;*/
/*%put &vars_rx_e;*/
/*%put &risk;*/
/**/
/*%put &vars_rx_s_new;*/
/*%put &vars_rx_e_new;*/

 
/*%scan(&vars_rx_s,3);*/

/*cut those prescripiton period into desired risk periods*/
/*%shiftwd(rawdata=sccs_dt_e_r,fromvar= drug_st1,endvar=drug_ed1,wdays= &risk)*/
/*%shiftwd(rawdata=sccs_dt_e_r,fromvar= drug_st2,endvar=drug_ed2,wdays= &risk)*/
/*%shiftwd(rawdata=sccs_dt_e_r,fromvar= drug_st3,endvar=drug_ed3,wdays= &risk)*/
/*%shiftwd(rawdata=sccs_dt_e_r,fromvar= drug_st4,endvar=drug_ed4,wdays= &risk)*/
/*%shiftwd(rawdata=sccs_dt_e_r,fromvar= drug_st5,endvar=drug_ed5,wdays= &risk)*/
%ob_col_names(DRUG_ST%);
%let vars_rx_s = &vars_temp;
%ob_col_names(DRUG_ED%);
%let vars_rx_e = &vars_temp;
%ob_col_names(EVENT%);
%let vars_event = &vars_temp;
%put &vars_event;

%cut_risks;

%restructure_risk_periods;


/*resturcture event dataset*/
data sccs_dt_event;
	set sccs_dt_e_r(keep = id obst obed dob &vars_event);
run;

proc sort data=sccs_dt_event nodup;
	by id obst obed dob &vars_event;
run;
/**/
/*proc transpose data=sccs_dt_event out=sccs_dt_event (drop = _NAME_ rename=(COL1=event));*/
/*	   var &vars_event;*/
/*	   by id obst obed dob;*/
/*run;*/
/**/
/*proc sort data=sccs_dt_event nodup;*/
/*	by id obst obed dob event;*/
/*	where event is not null;*/
/*run;*/

/*merge rx st/ed and rx event*/

data sccs_sted_event;
merge sccs_dt_event sccs_dt_st_ed_out ;
by id obst obed dob;
run;

proc sort data = sccs_sted_event nodup;
	by id obst obed &vars_event;
run;


%ob_col_names(DRUG_ST%,dataset=SCCS_STED_EVENT);
%let vars_rx_s = &vars_temp;
%ob_col_names(DRUG_ED%,dataset=SCCS_STED_EVENT);
%let vars_rx_e = &vars_temp;
%ob_col_names(EVENT%,dataset=SCCS_STED_EVENT);
%let vars_event = &vars_temp;

%sccs(data=sccs_sted_event, 
              pid=id,
              dob_raw=dob, events=&vars_event, 
              start_risk = &vars_rx_s, 
              end_risk = &vars_rx_e, 
              treatments_ind = 1 8,
              treatments_names = riluzole,
              startst=obst, endst=obed);



ods output estimates=modelestimates;
proc genmod data = wk_sccs;
Class risk_riluzole_1(ref = "0") risk_riluzole_2(ref = "0") risk_riluzole_3(ref = "0") 
	  risk_riluzole_4(ref = "0") risk_riluzole_5(ref = "0") risk_riluzole_6(ref = "0") 
	  risk_riluzole_7(ref = "0") risk_riluzole_8(ref = "0") age(ref = "1") id season(ref = "0") / order = data;/*param=ref ref=first*/
Model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 
			 risk_riluzole_4 risk_riluzole_5 risk_riluzole_6 
			 risk_riluzole_7 risk_riluzole_8 age season /dist=poisson link=log offset=l_off;
/*repeated subject=id;*/
/* Use the estimate statement to get predicted values and their confidence intervals */
Estimate "strx_30b" risk_riluzole_1 1 -1 / exp ;
Estimate "strx_0a" risk_riluzole_2  1 -1 / exp ;
Estimate "strx_30a" risk_riluzole_3 1 -1 / exp ;
Estimate "strx_60a" risk_riluzole_4 1 -1 / exp ;
Estimate "strx_90a" risk_riluzole_5 1 -1 / exp ;
Estimate "strx_120a" risk_riluzole_6 1 -1 / exp ;
Estimate "strx_150a" risk_riluzole_7 1 -1 / exp ;
Estimate "strx_180a" risk_riluzole_8 1 -1 / exp ;
run;
ods output close;

proc export data=modelestimates
    outfile="&outdir/mnd.xlsx"
    dbms=xlsx 
	replace;
	sheet="primary_analysis";
run;






ods excel file="output.xlsx";
/*model estimation*/
ods excel options(sheet_name='Primary analysis');



ods excel close;
/* ?? ODS Excel ?? */
ods _all_ close;









ods listing close;
ods trace on;
proc genmod data = wk_sccs;
Class risk_riluzole_1(ref = "0") risk_riluzole_2(ref = "0") risk_riluzole_3(ref = "0") 
	  risk_riluzole_4(ref = "0") risk_riluzole_5(ref = "0") risk_riluzole_6(ref = "0") 
	  risk_riluzole_7(ref = "0") risk_riluzole_8(ref = "0") age id season / order = data;/*param=ref ref=first*/
Model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 
			 risk_riluzole_4 risk_riluzole_5 risk_riluzole_6 
			 risk_riluzole_7 risk_riluzole_8 age season /dist=poisson link=log offset=l_off;
/*repeated subject=id;*/
/* Use the estimate statement to get predicted values and their confidence intervals */
Estimate "strx_30b" risk_riluzole_1 1 -1 / exp ;
Estimate "strx_0a" risk_riluzole_2  1 -1 / exp ;
Estimate "strx_30a" risk_riluzole_3 1 -1 / exp ;
Estimate "strx_60a" risk_riluzole_4 1 -1 / exp ;
Estimate "strx_90a" risk_riluzole_5 1 -1 / exp ;
Estimate "strx_120a" risk_riluzole_6 1 -1 / exp ;
Estimate "strx_150a" risk_riluzole_7 1 -1 / exp ;
Estimate "strx_180a" risk_riluzole_8 1 -1 / exp ;
run;
ods trace off;
ods listing;






proc genmod data = wk_sccs;
Class risk_riluzole_1(ref = "0") risk_riluzole_2(ref = "0") risk_riluzole_3(ref = "0") 
	  risk_riluzole_4(ref = "0") risk_riluzole_5(ref = "0") risk_riluzole_6(ref = "0") 
	  risk_riluzole_7(ref = "0") risk_riluzole_8(ref = "0") age id season / order = data;/*param=ref ref=first*/
Model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 
			 risk_riluzole_4 risk_riluzole_5 risk_riluzole_6 
			 risk_riluzole_7 risk_riluzole_8 age season /dist=poisson link=log offset=l_off;
/*repeated subject=id;*/
			 run;



%macro extract_sccs_multiple_outcome;
	%let end = %sysfunc(countw(&vars_event));
    %DO abc=1 %TO &end;
        %sccs(data=sccs_sted_event, 
              pid=id,
              dob_raw=dob, events=event&abc, 
              start_risk = &vars_rx_s, 
              end_risk = &vars_rx_e, 
              treatments_ind = 1 8,
              treatments_names = riluzole,
              startst=obst, endst=obed, outdata=wk_sccs&abc);
    %END;
%mend extract_sccs_multiple_outcome;

%extract_sccs_multiple_outcome;

/* Create a dataset with the table names */
proc sql noprint;
  select memname into : name_sccs_dt separated by ' ' 
  from dictionary.tables
  where LIBNAME = upcase('work')
  and upcase(MEMNAME) like "WK_SCCS%" 
  and upcase(MEMNAME) <> "WK_SCCS";
quit;
DATA combined_sccs_dt;
SET &name_sccs_dt;
RUN;
proc sql noprint;
  select memname into : name_sccs_dt separated by ',' 
  from dictionary.tables
  where LIBNAME = upcase('work')
  and upcase(MEMNAME) like "WK_SCCS%" 
  and upcase(MEMNAME) <> "WK_SCCS";
quit;
proc sql;
	drop table &name_sccs_dt;
quit;

proc sort data=combined_sccs_dt;
by id start stop ;
run;

proc logistic data=combined_sccs_dt;
  strata id; /* this specifies the matching variable */
  class id age season;
  model nevt(ref="0") = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
		     risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8 age season l_off;
  ods output OddsRatios=ORs; 
run;


proc genmod data = combined_sccs_dt;
Class risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
		     risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8 age id season / order = data ;/*param=ref ref=first*/
Model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
		     risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8 age season /dist=poisson link=log offset=l_off;
repeated subject=id;
/* Use the estimate statement to get predicted values and their confidence intervals */
Estimate "strx_30b" risk_riluzole_1 1 -1 / exp ;
Estimate "strx_0a" risk_riluzole_2  1 -1 / exp ;
Estimate "strx_30a" risk_riluzole_3 1 -1 / exp ;
Estimate "strx_60a" risk_riluzole_4 1 -1 / exp ;
Estimate "strx_90a" risk_riluzole_5 1 -1 / exp ;
Estimate "strx_120a" risk_riluzole_6 1 -1 / exp ;
Estimate "strx_150a" risk_riluzole_7 1 -1 / exp ;
Estimate "strx_180a" risk_riluzole_8 1 -1 / exp ;
run;

proc genmod data = wk_sccs;
  class risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
        risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8 age id season / order=data ; /*param=ref ref=first*/
  model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
               risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8 age season / dist=poisson link=log offset=l_off;
  estimate 'RR Risk Riluzole 1' risk_riluzole_1 1 -1 / exp ;  /* Create estimates of adjusted RR for each level of risk_riluzole */
  estimate 'RR Risk Riluzole 2' risk_riluzole_2  1 -1 / exp ;
  estimate 'RR Risk Riluzole 3' risk_riluzole_3  1 -1 / exp ;
  estimate 'RR Risk Riluzole 4' risk_riluzole_4  1 -1 / exp ;
  estimate 'RR Risk Riluzole 5' risk_riluzole_5  1 -1 / exp ;
  estimate 'RR Risk Riluzole 6' risk_riluzole_6  1 -1 / exp ;
  estimate 'RR Risk Riluzole 7' risk_riluzole_7  1 -1 / exp ;
  estimate 'RR Risk Riluzole 8' risk_riluzole_8  1 -1 / exp ;
run;





/* Load the data */
data mydata;
   input exposure_count predictor1 predictor2 predictor3;
   datalines;
   22 201 134 0
   17 89 88 1
   33 350 211 0
   10 100 75 1
   40 450 311 0
   8 77 70 1
   27 270 160 0
   12 110 85 1
   34 400 250 0
   15 130 90 1
   ;
run;

/* Run Poisson regression using PROC GENMOD */
proc genmod data=mydata;
   model exposure_count = predictor1 predictor2 predictor3 / dist=poisson link=log;
   /* Use ESTIMATE statement to obtain exponentiated parameter estimates and their confidence intervals */
   estimate 'predictor1 exp(beta)' predictor1  1 -1 / exp;
   estimate 'predictor2 exp(beta)' predictor2  1 -1 / exp;
   estimate 'predictor3 exp(beta)' predictor3  1 -1 / exp;
run;







proc logistic data=wk_sccs;
strata id;
model nevt = &vars_risk age season;
run


lsmestimate riskR1 "abc" 1 -1 /  exp cl;

ods output parameterestimates=outdataset;
Estimate "strx_30b" riskR1 1 -1 / exp ;
Estimate "strx_0a" riskR2  1 -1 / exp ;
Estimate "strx_30a" riskR3 1 -1 / exp ;
Estimate "strx_60a" riskR4 1 -1 / exp ;
Estimate "strx_90a" riskR5 1 -1 / exp ;
Estimate "strx_120a" riskR6 1 -1 / exp ;
Estimate "strx_150a" riskR7 1 -1 / exp ;
Estimate "strx_180a" riskR8 1 -1 / exp ;

proc genmod data = wk_sccs;
class &vars_risk age id season / order = data param=ref ref=first;
Model nevt = &vars_risk age season /dist=poisson link=LOG offset=offset;
REPEATED SUBJECT=id/type=Exchangeable;
Ods output parameterestimates=outdataset;
Estimate "SCCS" riskR1;
run;


%poisreg(data=wk_sccs,y=nevt
		,covar=int risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 risk_riluzole_4
        risk_riluzole_5 risk_riluzole_6 risk_riluzole_7 risk_riluzole_8
		,class=age season
		,offset=offset,prntyn=Y,elim=id
        ,title="SCCS primary analysis for MND");


%poisreg(data=wk_sccs,y=nevt,covar=int riskR1
		,class=age
        ,offset=offset,prntyn=Y,elim=id
        ,title="SCCS primary analysis for MND");


data macro_vars;
     set SASHELP.VMACRO;
     where scope = 'GLOBAL';
     if substr(name, 1, 1) = '_' then delete;
     if substr(name, 1, 3) in ('SAS', 'SQL', 'SYS') then delete;
     drop scope offset;
run;
