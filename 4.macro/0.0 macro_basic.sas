
/*reading raw file with excel format*/
%macro reading(filename=,out=,sheet=);
	/* %put "reading ======"; */
	%if %sysfunc(exist(&out.)) %then %do;
		%put "===============================";
		%put "already have the dataset, plz check dir of sas data";
		%put "===============================";
	%end;
	%else %do;
		%if &sheet. =  %then %do;
			proc import datafile = &filename.
				out = sasfiles.&out. dbms=xlsx REPLACE;
			run;
		%end;
		%else %do;
			proc import datafile = &filename.
				out = sasfiles.&out. dbms=xlsx REPLACE;
				sheet=&sheet.;
			run;
		%end;
		%end;
%mend reading;



/*check the past history from the new merged dx dataset*/
%macro gen_pt_hx(nrow);
/*select colname from */
	proc sql noprint;
		select "dx_"||Dx into :colname from (select Dx, monotonic() AS num_row from sasfiles.codesmnd) where num_row=&nrow.;
	quit;
/*select patients with target past hx*/
	proc sql noprint;
		create table sasfiles.temp_mnd_pt_hx as 
		select distinct id,
						1 as &colname from 
		(select * from sasfiles.pthx where prxmatch((select cats("/",icd9,"/") from (select *, monotonic() AS num_row from sasfiles.codesmnd) where num_row=&nrow.),trim(codes))>0);
	quit;
/*merge back information*/
	proc sql;
		create table sasfiles.Demo_ as
		select * from sasfiles.Demo_ as x left join sasfiles.temp_mnd_pt_hx as y
		on x.id = y.id;
	quit;
/*filling the blank*/
	proc sql; 
		update sasfiles.Demo_
		set &colname = 0 where &colname is NULL;
	quit;
/*delete the temp dataset*/
	proc delete data=sasfiles.temp_mnd_pt_hx; run;
%mend gen_pt_hx;

/*obtain all past disease past history*/
%macro get_past_hx(a);
   /*Generate database with all dx occuring before index date*/
	proc sql;
	  create table sasfiles.pthx as
	  select * from (select id,onset_date from sasfiles.demo) as a, (select id, codes, ref_date from sasfiles.dx) as b
	  where a.id=b.id & ref_date < onset_date;
	quit;
   %let i = 1;
   %do %while (&i <&a+1);
      %gen_pt_hx(&i);
	  %put The No &i. / &a. covariate; 
      %let i=%eval(&i+1);
   %end;
   proc sql;
   	drop table sasfiles.pthx;
   quit;
%mend get_past_hx;

/*check the past medical records*/
%macro gen_pt_rx(nrow);
/*select colname from */
	proc sql noprint;
		select Name into :colname from (select Name, monotonic() AS num_row from sasfiles.codesrx) where num_row=&nrow.;
	quit;
/*select patients with target past hx*/
	proc sql noprint;
		create table sasfiles.temp_mnd_pt_rx as 
		select distinct id,
						1 as &colname from 
		/*pick all the records with defined icd9 codes*/
		(select * from sasfiles.ptrx where prxmatch((select "/"||trim(BNF)||"/" from (select *, monotonic() AS num_row from sasfiles.codesrx) where num_row=&nrow.),trim(codes))>0); 		
	quit;
/*merge back information*/
	proc sql;
		create table sasfiles.Demo_ as
		select * from sasfiles.Demo_ as x left join sasfiles.temp_mnd_pt_rx as y
		on x.id = y.id;
	quit;
/*filling the blank*/
	proc sql; 
		update sasfiles.Demo_
		set &colname = 0 where &colname is NULL;
	quit;
/*delete the temp dataset*/
	proc delete data=sasfiles.temp_mnd_pt_rx; run;
%mend gen_pt_rx;

/*obtain all past prescription records*/
%macro get_past_rx(a);
	/*create all rx information with master sheet*/
	proc sql noprint;
	  create table sasfiles.ptrx as
	  select * from (select id,onset_date from sasfiles.demo) as a, 
					(select id, codes, date_rx_st from sasfiles.rx) as b
	  where a.id=b.id & date_rx_st < onset_date & date_rx_st > onset_date - 90;
	quit;
   %let i = 1;
   %do %while (&i <&a+1);
      %gen_pt_rx(&i);
	  %put The No &i. / &a. drugs hx; 
      %let i=%eval(&i+1);
   %end;
   proc sql;
   	drop table sasfiles.ptrx;
	quit;
%mend get_past_rx;

/*subtype of the MND*/
%macro gen_subtype(nrow);
/*select colname from */
	proc sql noprint;
		select "subtype_"||abbr into :colname from (select abbr, monotonic() AS num_row from sasfiles.subtype) where 
		num_row=&nrow.;
	quit;
/*select patients with target subtype of MND*/
	proc sql noprint;
		create table sasfiles.temp_dxmnd as 
		select distinct id,
						1 as &colname from 
		(select * from sasfiles.dxmnd where prxmatch((select cats("/",icd9,"/") from (select *, monotonic() AS num_row from sasfiles.subtype) where num_row=&nrow.),trim(codes))>0);
	quit;
/*put back information*/
	proc sql;
		create table sasfiles.Demo_ as
		select * from sasfiles.Demo_ as x left join sasfiles.temp_dxmnd as y
		on x.id = y.id;
	quit;
/*filling the blank*/
	proc sql; 
		update sasfiles.Demo_
		set &colname = 0 where &colname is NULL;
	quit;
/*delete the temp dataset*/
	proc delete data=sasfiles.temp_dxmnd; run;
%mend gen_subtype;

/*obtain all past disease past history*/
%macro get_subtype(a);
   proc sql;
	  create table sasfiles.dxmnd as
	  select * from (select id,onset_date from sasfiles.demo) as a, (select id, codes, ref_date from sasfiles.dx) as b
	  where a.id=b.id & ref_date = onset_date;
   quit;
   %let i = 1;
   %do %while (&i <&a+1);
      %gen_subtype(&i);
	  %put The No &i. / &a. subtype; 
      %let i=%eval(&i+1);
   %end;
   proc sql;
		drop table sasfiles.dxmnd;
   quit;
%mend get_subtype;

/*exposure*/
%macro get_expo();
/*	*/
/*	proc sql;*/
/*	create table t as (select * from sasfiles.rx where upcase(drug_name) CONTAINS "RILUZOLE" or upcase(drug_name) CONTAINS "RILUTEK");*/
/*	quit;*/
/**/
/*	proc sort data=t; */
/*		by id date_rx_st date_rx_end; */
/*	run;*/
/**/
/*	data tt;*/
/*	  set t;*/
/*	  by id;*/
/*	  retain max_step lag_ed lag_dif lag_dif_in sum_dif;*/
/*	  	  if first.id then max_step= date_rx_end;*/
/*		  else max_step = max(date_rx_end,max_step);*/
/*		  lag_ed = lag1(max_step);*/
/*		  if first.id then lag_ed = .;*/
/*	  lag_dif = date_rx_st - lag_ed;*/
/*	  lag_dif_in = lag_dif >1;*/
/*	  if _n_=1 then sum_dif = 0;*/
/*	  else sum_dif = sum_dif + lag_dif_in ;*/
/*	  FORMAT max_step lag_ed MMDDYY10.;*/
/*	run;*/
/**/
/*	proc sql;*/
/*		create table ttt as */
/*		select id, min(date_rx_st) as date_rx_st format=yymmdd10., max(max_step) format=yymmdd10. as date_rx_end from tt group by id,sum_dif;*/
/*	quit;*/
	%shrink(source=RX,Remove_IP_Rx=FALSE,st=date_rx_st,end=date_rx_end,out=ttt,gap=1);

	proc sql;
		create table sasfiles.Demo_ as
		select * from sasfiles.Demo_ as x left join 
			(SELECT distinct x.id, 1 as exposure
				FROM (SELECT id, date_rx_st as date_rx_st format=mmddyy10.
				      FROM ttt
				      WHERE date_rx_st <>.
				      GROUP BY id) as x
						LEFT JOIN (SELECT id, onset_date, min(dod, obs_deadline) format=mmddyy10. as obs_deadline from
										(SELECT id, onset_date, dod, "&studydate_ed."d format=mmddyy10. as obs_deadline FROM sasfiles.Demo_)) as y
			ON x.id = y.id where x.date_rx_st >= y.onset_date & x.date_rx_st < y.obs_deadline) as y 
		on x.id = y.id;
		update sasfiles.Demo_ /* update all empty*/
		set exposure = 0 where exposure =.;
		/*drop table ttt;*/
	run;


%mend get_expo;


%macro shrink(source=, Remove_IP_Rx=TRUE,st=,end=,out=,gap=1);
	
	%if &source.=IP and &Remove_IP_Rx. ne %then %do;
		proc sql;
			create table &out. as (select * from sasfiles.ip);
		quit;
		%end;
	%if &source.=RX and &Remove_IP_Rx.=FALSE %then %do;
		proc sql;
			create table &out. as (select * from sasfiles.rx where (upcase(drug_name) CONTAINS "RILUZOLE" or upcase(drug_name) CONTAINS "RILUTEK" ));
		quit;
	   %end;
	%if &source.=RX %then %do;
		proc sql;
			create table &out. as (select * from sasfiles.rx where (upcase(drug_name) CONTAINS "RILUZOLE" or upcase(drug_name) CONTAINS "RILUTEK" ) and setting <> "I");
		quit;
	   %end;

	proc sort data=&out.; 
		by id &st. &end.; 
	run;

	data &out.;
	  set &out.;
	  by id;
	  retain max_step lag_ed lag_dif lag_dif_in sum_dif;
	  	  if first.id then max_step= &end.;
		  else max_step = max(&end.,max_step);
		  lag_ed = lag1(max_step);
		  if first.id then lag_ed = .;
	  lag_dif = &st. - lag_ed;
	  lag_dif_in = lag_dif >&gap.;
	  if _n_=1 then sum_dif = 0;
	  else sum_dif = sum_dif + lag_dif_in ;
	  FORMAT max_step lag_ed MMDDYY10.;
	run;

	proc sql;
		create table &out. as 
		select distinct id, min(&st.) as &st. format=yymmdd10., max(max_step) format=yymmdd10. as &end. from &out. 
		where &st. <>. and &end. <>. group by id,sum_dif;
	quit;
%mend shrink;



%macro deduptrans(data=, out= ,var=,prefix=);
data &out;
keep &var id obst obed dob;
set &data;
run;
proc sort data=&out out=&out nodupkey;
    by _all_;
run;
PROC TRANSPOSE DATA=&out OUT=&out PREFIX=&prefix;
  Var &var ;
  BY id obst obed dob;
RUN;
%mend deduptrans;


%macro shiftwd (rawdata=,fromvar=,endvar=,wdays=);
   %GLOBAL nb_risk;
   
   %element(lstvar=&wdays , pref = risk);
   %LET nb_risk = %EVAL(&nb_risk/2);

   DATA &rawdata;
   SET &rawdata;
   %put &nb_risk;
   %DO i=1 %TO &nb_risk;
/*   %put &i;*/
/*	 %put risk&ib;*/
     %LET ib = %EVAL(&i*2-1);
	 %LET ie = %EVAL(&i*2);
/*	 Same date of rx st and end;*/
	 IF (&endvar=&fromvar) THEN DO;
		&&fromvar._&i = &fromvar; 
		&&endvar._&i = &endvar; 
	 END;

	 IF (&endvar-&fromvar+1 < &&risk&ib) THEN DO; 
		  &&fromvar._&i = .; 
		  &&endvar._&i = .; 
	 END;

	 IF (&endvar-&fromvar+1 >= &&risk&ib) THEN DO;
		 IF (&endvar - &fromvar+1 <= &&risk&ie) THEN DO;
	  		  &&fromvar._&i = &fromvar + &&risk&ib  -1;
			  &&endvar._&i = &endvar;
		 END;
		 ELSE IF (&endvar -&fromvar+1 > &&risk&ie) THEN DO;
/*			 	  IF (&fromvar + &&risk&ie > &endvar) THEN DO;*/
				      &&fromvar._&i = &fromvar + &&risk&ib -1;
/*		     		  &&endvar._&i = &endvar;	*/
					  &&endvar._&i = &fromvar + &&risk&ie -1;
/*				  END;*/
/*				  ELSE DO;*/
/*			  		  &&fromvar._&i = &fromvar + &&risk&ib;*/
/*					  &&endvar._&i = &fromvar + &&risk&ie;*/
/*		          END;*/
		 END;
	  END;
	  	 
	  format &&fromvar._&i yymmdd10.;
	  format &&endvar._&i yymmdd10.;
   %END;
   RUN;
%mend shiftwd;

/*cut the rx period into 8 risk period*/
%macro cut_risks;
%local j;
%do j=1 %to %sysfunc(countw(&vars_rx_s));
	%shiftwd(rawdata=sccs_dt_e_r,fromvar= %scan(&vars_rx_s,&j),endvar=%scan(&vars_rx_e,&j),wdays= &risk);
   	%put %scan(&vars_rx_s,&j);
%end;
%mend cut_risks;


/*obtain the column names according to keywords;*/
%macro ob_col_names(keywords,dataset=sccs_dt_e_r);
	%symdel vars_temp;
	%global vars_temp;
	proc sql noprint;
	select name into : vars_temp separated by ' '
	from dictionary.columns
	where LIBNAME = %upcase('work')
	and MEMNAME = %upcase("&dataset")
	and upcase(name) like "&keywords" escape '\';
	quit;
%mend ob_col_names(keywords,dataset);

/*loop to restruction the risk period into one; cos the previous risk cutting expand each rx into 8 risk period (8 column* rx); now restructure it*/
%macro restructure_risk_periods;
%local j;
%do j= 1 %to %EVAL(%sysfunc(countw(&risk))/2);

	%ob_col_names(DRUG_ST__&j);
	%let vars_rx_s_new=&vars_temp;
/*	%ob_col_names(%upcase(%scan(DRUG_ED__&j)));*/
	%ob_col_names(DRUG_ED__&j);
	%let vars_rx_e_new=&vars_temp;
/*	%ob_col_names(EVENT%);*/
/*	%let vars_event = &vars_temp;*/
/*	%put &vars_rx_s_new;*/
	%rs_rp_4_rx(&j);
	%if &j =1 %then %do;
		data sccs_dt_st_ed_out;
			set sccs_dt_st_ed;
		run;
	%end;
	%else %do;
		data sccs_dt_st_ed_out;
			merge sccs_dt_st_ed_out sccs_dt_st_ed;
			by id obst obed dob;
		run;
	%end;
	%if &j = %EVAL(%sysfunc(countw(&risk))/2) %then %do;
		proc sql;
		drop table work.sccs_dt_st_ed;
		quit;
	%end;
%end;
%mend restructure_risk_periods;



%macro rs_rp_4_rx(i);
	/*restructure_risk_periods_within_each_rx*/
	data sccs_dt_e_r_st;
	set sccs_dt_e_r (keep =id obst obed dob &vars_rx_s_new);
	run;

	data sccs_dt_e_r_ed;
	set sccs_dt_e_r (keep =id obst obed dob &vars_rx_e_new);
	run;
	
	%local new_name_s;
	%local new_name_e;
	%let new_name_s = drug_st_&j;
	%let new_name_e = drug_ed_&j;
/*	%let new_name_e = %scan(&vars_rx_e_new,1,"_")_%scan(&vars_rx_e_new,2,"_");*/

	proc transpose data=sccs_dt_e_r_st out=sccs_dt_e_r_st_long (drop = _NAME_ rename=(COL1=&new_name_s));
	   var &vars_rx_s_new;
	/*   id id obst obed dob ;*/
	   by id obst obed dob;
	run;

	proc transpose data=sccs_dt_e_r_ed out=sccs_dt_e_r_ed_long (drop=_NAME_ rename=(COL1=&new_name_e));
	   var &vars_rx_e_new;
	   by id obst obed dob;
	run;

	data sccs_dt_st_ed;
	  merge sccs_dt_e_r_st_long sccs_dt_e_r_ed_long;
	  by id obst obed dob ; 
	run;

	data sccs_dt_st_ed;
	set sccs_dt_st_ed;
	where &new_name_s is not null and &new_name_e is not null;
	run;

	proc sort data = sccs_dt_st_ed out = sccs_dt_st_ed;
	by id obst obed &new_name_s;
	run;

	/*Delete the temperal datasets*/
	proc sql;
	   drop table work.sccs_dt_e_r_st_long;
	   drop table work.sccs_dt_e_r_ed_long;
	   drop table work.sccs_dt_e_r_st;
	   drop table work.sccs_dt_e_r_ed;	   
	quit;

%mend rs_rp_4_rx(vars_rx_s_new,vars_rx_e_new);
