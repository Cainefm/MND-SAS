
/* ************************************************************** */
/* Analysis 4: SCCS analysis between hospitalization and rilozle  */
/* ************************************************************** */

/*SCCS setting*/
OPTIONS nodate nonumber;

%MACRO run_sccs(title=MND, ae=F, icd_defined = .,collapse=F);

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
	
/*	for subgroup analysis*/
	%if &ae eq T %then %do;
		data shrankip;
			set shrankip;
			where ae=1;
		run;
	%end;

	/*merge the prescription and demographic information together*/
	proc sql;
	create table sccs_dt as
		select distinct * from 
		(select * from shrankrx) as a left join 
		(select id, min(date_rx_st) format=yymmdd10. as earliest_rx_date 
			from shrankrx group by id) as b on a.id=b.id 
		left join (select * from sasfiles.Demo) as c on b.id = c.id 
		left join (select distinct * from shrankip) as d on c.id = d.id
		where date_dsg <= "&sccs_obs_ed."d and date_dsg ^=.  ; 
	quit;

	proc sort data=sasfiles.dx out=sasfiles.dx;
	    by id ref_date;
	run;

	/*keep only the Inpatient records*/
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



	/*primary analysis*/
	proc sql;
	create table sccs_dt as
		select a.id, a.date_rx_st, a.date_rx_end, a.earliest_rx_date, a.sex, a.dob, a.dod, a.onset_date, 
			 a.date_adm, a.date_dsg,
			 a.ae, b.codes from 
		(select * from sccs_dt) as a left join
		(select id, ref_date, codes_all as codes from collapse_dx where setting="I") as b on a.id=b.id and a.date_dsg=b.ref_date;
	quit;

	/*	for subgroup analysis */
	proc sql;
	%if &icd_defined ne . %then %do;
	create table sccs_dt as
		select * from sccs_dt where id in (
			select distinct id from collapse_dx where PRXMATCH(&icd_defined., codes_all ) > 0
		);
	%end;
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
	%LET batch=&title;

	/*dedpulicate the recrods and split the dataset into event, rxst, and rxed*/
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

	/* Agerange defines the age range for each subject*/
	%LET agerange=;
	/* Define risk periods */
	
	%IF &collapse=T %THEN %DO;
		%LET risk = -30 0 1 60 61 120 121 180 181 9999999999;
	%END;

	%IF &collapse=F %THEN %DO;
		%LET risk = -30 0 1 30 31 60 61 90 91 120 121 150 151 180 181 99999999;
	%END;


	/* Define age categories */
	%macro generate_sequence(start=, end=);
	    %local i;
	    %let sequence = 0; 
	    %do i = &start %to &end;
	        %let sequence = &sequence. %eval(&i*3650); 
	    %end;
	    &sequence.;
	%mend generate_sequence;

	proc sql noprint;
		select CEIL(min(obst-dob)/3650) into: age_min from sccs_dt;
	quit;

	proc sql noprint;
		select CEIL(max(obst-dob)/3650) into: age_max from sccs_dt;
	quit;

	%LET age = %generate_sequence(start=&age_min, end=&age_max);
/*	%LET age= 0 7300 10950 14600 18250 21900 25550 29200 32850;*/
	/* No adjustment for seasonality is used in this example */
	%LET season= 01MAR 01MAY 01SEP 01NOV;
	/* No semiparametric analysis is done */
	%LET semi=N;

	%ob_col_names(DRUG_ST%); /*obtain the prescription start column names*/
	%let vars_rx_s = &vars_temp;
	%ob_col_names(DRUG_ED%); /*obtain the prescription end column names*/
	%let vars_rx_e = &vars_temp;
	%ob_col_names(EVENT%);   /*obtain the column names of event*/
	%let vars_event = &vars_temp;
	%put &vars_event;

	/*cut the risk period*/
	%cut_risks;
	/*merge the same risk period into one same column*/
	%restructure_risk_periods;


	/*resturcture event dataset*/
	data sccs_dt_event;
		set sccs_dt_e_r(keep = id obst obed dob &vars_event);
	run;

	proc sort data=sccs_dt_event nodup;
		by id obst obed dob &vars_event;
	run;


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

	%IF &collapse=F %THEN %DO;
		
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
		ods output close;
		
		%obt_sccs();

		proc export data=sccs_est
		    outfile="&outdir/mnd.xlsx"
		    dbms=xlsx 
			replace;
			sheet="&batch";
		run;
	%END;

	%IF &collapse=T %THEN %DO;
		
		%sccs(data=sccs_sted_event, 
	              pid=id,
	              dob_raw=dob, events=&vars_event, 
	              start_risk = &vars_rx_s, 
	              end_risk = &vars_rx_e, 
	              treatments_ind = 1 5,
	              treatments_names = riluzole,
	              startst=obst, endst=obed);

		ods output estimates=modelestimates;
		proc genmod data = wk_sccs;
		Class risk_riluzole_1(ref = "0") risk_riluzole_2(ref = "0") risk_riluzole_3(ref = "0") 
			  risk_riluzole_4(ref = "0") risk_riluzole_5(ref = "0") age id season / order = data;/*param=ref ref=first*/
		Model nevt = risk_riluzole_1 risk_riluzole_2 risk_riluzole_3 
					 risk_riluzole_4 risk_riluzole_5 age season /dist=poisson link=log offset=l_off;
		/*repeated subject=id;*/
		/* Use the estimate statement to get predicted values and their confidence intervals */
		Estimate "strx_30b" risk_riluzole_1 1 -1 / exp ;
		Estimate "strx_0a" risk_riluzole_2  1 -1 / exp ;
		Estimate "strx_60a" risk_riluzole_3 1 -1 / exp ;
		Estimate "strx_120a" risk_riluzole_4 1 -1 / exp ;
		Estimate "strx_180a" risk_riluzole_5 1 -1 / exp ;
		run;
		ods output close;
		
		%obt_sccs_collapse();

		proc export data=sccs_est
		    outfile="&outdir/mnd.xlsx"
		    dbms=xlsx 
			replace;
			sheet="&batch";
		run;
	%END;

%MEND run_sccs;


%MACRO obt_sccs;
	/* Create a new table with the sums */
	proc sql;
	    create table sum_personcounts as 
	    select sum(nevt * risk_riluzole_1) as strx_30b,
	           sum(nevt * risk_riluzole_2) as strx_0a,
	           sum(nevt * risk_riluzole_3) as strx_30a,
	           sum(nevt * risk_riluzole_4) as strx_60a,
	           sum(nevt * risk_riluzole_5) as strx_90a,
	           sum(nevt * risk_riluzole_6) as strx_120a,
	           sum(nevt * risk_riluzole_7) as strx_150a,
	           sum(nevt * risk_riluzole_8) as strx_180a,
			   sum(nevt * (risk_riluzole-1)*-1) as baseline
	    from wk_sccs ;
	quit;

	/* Transpose the table to get the columns as rows */
	proc transpose data=sum_personcounts out=sum_personcounts (rename=(_name_=risk_period col1=no_people));
	    var strx_30b strx_0a strx_30a strx_60a strx_90a strx_120a strx_150a strx_180a baseline;
		label risk_period='risk period'; 
	run;

	/*add a column of total event number by id*/
	proc sql;
	    create table wk_sccs as
	    select *, sum(nevt) as nevt_total
	    from wk_sccs
	    group by id;
	quit;

	proc sql;
	    create table sum_py as
	    select 
	        sum(offset*risk_riluzole_1*nevt_total)/365.25 as strx_30b,
			sum(offset*risk_riluzole_2*nevt_total)/365.25 as strx_0a,
			sum(offset*risk_riluzole_3*nevt_total)/365.25 as strx_30a,
			sum(offset*risk_riluzole_4*nevt_total)/365.25 as strx_60a,
			sum(offset*risk_riluzole_5*nevt_total)/365.25 as strx_90a,
			sum(offset*risk_riluzole_6*nevt_total)/365.25 as strx_120a,
			sum(offset*risk_riluzole_7*nevt_total)/365.25 as strx_150a,
			sum(offset*risk_riluzole_8*nevt_total)/365.25 as strx_180a,
			sum(offset*(risk_riluzole-1)*-1  *nevt_total)/365.25 as baseline
	    from wk_sccs;
	quit;
	/* Transpose the table to get the columns as rows */
	proc transpose data=sum_py out=sum_py (rename=(_name_=risk_period col1=no_py) ) ;
	    var strx_30b strx_0a strx_30a strx_60a strx_90a strx_120a strx_150a strx_180a baseline;
		label risk_period='risk period'; 
	run;

	PROC SQL;
	   CREATE TABLE sccs_py AS
	   SELECT *
	   FROM sum_personcounts
	   LEFT JOIN sum_py
	   ON sum_personcounts.risk_period = sum_py.risk_period;
	QUIT;

	/*clean estimation*/
	data modelestimates;
		set modelestimates;
		pvalue = lag(ProbChiSq);
		format pvalue PVALUE6.4;
	run;

	data modelestimates;
		set modelestimates;
		if pvalue = '_' then pvalue=.;
		if missing(pvalue )then pvalue=.;
	run;

	proc sql;
		create table temp_est as
		select substr(label, index(label, '(') + 1, length(label) - index(label, '(') - 1) as label, 
				LBetaEstimate, LBetaLowerCL,LBetaUpperCL,pvalue from
		modelestimates where not missing(pvalue);
	run;

	proc sql;
		create table sccs_est as
		select * 
	    from sccs_py 
	    left join (
	        select LBetaEstimate, LBetaLowerCL, LBetaUpperCL, pvalue 
	        from temp_est
	    ) on sccs_py.risk_period = temp_est.label
		ORDER BY
	      CASE sccs_py.risk_period 
	         WHEN 'strx_30b' THEN 1
	         WHEN 'strx_0a' THEN 2
	         WHEN 'strx_30a' THEN 3
			 WHEN 'strx_60a' THEN 4
	         WHEN 'strx_90a' THEN 5
	         WHEN 'strx_120a' THEN 6
	         WHEN 'strx_150a' THEN 7
			 WHEN 'strx_180a' THEN 8
	         ELSE 9
		END;
	quit;

	proc sql;
		drop table sum_personcounts;
		drop table sum_py;
		drop table sccs_py;
		drop table temp_est;
	quit;
%MEND;


%MACRO obt_sccs_collapse;
	/* Create a new table with the sums */
	proc sql;
	    create table sum_personcounts as 
	    select sum(nevt * risk_riluzole_1) as strx_30b,
	           sum(nevt * risk_riluzole_2) as strx_0a,
	           sum(nevt * risk_riluzole_3) as strx_60a,
	           sum(nevt * risk_riluzole_4) as strx_120a,
	           sum(nevt * risk_riluzole_5) as strx_180a,
			   sum(nevt * (risk_riluzole-1)*-1) as baseline
	    from wk_sccs ;
	quit;

	/* Transpose the table to get the columns as rows */
	proc transpose data=sum_personcounts out=sum_personcounts (rename=(_name_=risk_period col1=no_people));
	    var strx_30b strx_0a strx_60a strx_120a strx_180a baseline;
		label risk_period='risk period'; 
	run;

	/*add a column of total event number by id*/
	proc sql;
	    create table wk_sccs as
	    select *, sum(nevt) as nevt_total
	    from wk_sccs
	    group by id;
	quit;

	proc sql;
	    create table sum_py as
	    select 
	        sum(offset*risk_riluzole_1*nevt_total)/365.25 as strx_30b,
			sum(offset*risk_riluzole_2*nevt_total)/365.25 as strx_0a,
			sum(offset*risk_riluzole_3*nevt_total)/365.25 as strx_60a,
			sum(offset*risk_riluzole_4*nevt_total)/365.25 as strx_120a,
			sum(offset*risk_riluzole_5*nevt_total)/365.25 as strx_180a,
			sum(offset*(risk_riluzole-1)*-1  *nevt_total)/365.25 as baseline
	    from wk_sccs;
	quit;
	/* Transpose the table to get the columns as rows */
	proc transpose data=sum_py out=sum_py (rename=(_name_=risk_period col1=no_py) ) ;
	    var strx_30b strx_0a strx_60a strx_120a strx_180a baseline;
		label risk_period='risk period'; 
	run;

	PROC SQL;
	   CREATE TABLE sccs_py AS
	   SELECT *
	   FROM sum_personcounts
	   LEFT JOIN sum_py
	   ON sum_personcounts.risk_period = sum_py.risk_period;
	QUIT;

	/*clean estimation*/
	data modelestimates;
	set modelestimates;
	pvalue = lag(ProbChiSq);
	format pvalue PVALUE6.4;
	run;

	data modelestimates;
	set modelestimates;
	if pvalue = '_' then pvalue=.;
	if missing(pvalue )then pvalue=.;
	run;

	proc sql;
		create table temp_est as
		select substr(label, index(label, '(') + 1, length(label) - index(label, '(') - 1) as label, 
				LBetaEstimate, LBetaLowerCL,LBetaUpperCL,pvalue from
		modelestimates where not missing(pvalue);
	run;

	proc sql;
		create table sccs_est as
		select * 
	    from sccs_py 
	    left join (
	        select LBetaEstimate, LBetaLowerCL, LBetaUpperCL, pvalue 
	        from temp_est
	    ) on sccs_py.risk_period = temp_est.label
		ORDER BY
	      CASE sccs_py.risk_period 
	         WHEN 'strx_30b' THEN 1
	         WHEN 'strx_0a' THEN 2
			 WHEN 'strx_60a' THEN 4
	         WHEN 'strx_120a' THEN 6
			 WHEN 'strx_180a' THEN 8
	         ELSE 9
		END;
	quit;


%MEND;
