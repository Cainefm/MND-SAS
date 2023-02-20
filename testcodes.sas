proc sql;
	select Name into :colname from (select Name, monotonic() AS num_row from sasfiles.codesrx) where num_row=1;
quit;
%put &colname;
%put &nofvariables;

proc sql;
		create table sasfiles.temp_mnd_pt_hx as 
		select distinct id,
						1 as &colname from 
		/*pick all the records with defined icd9 codes*/
		(select * from sasfiles.pthx where prxmatch((select "/"||trim(BNF)||"/" from (select *, monotonic() AS num_row from sasfiles.codesrx) where num_row=1),codes)>0); 		
quit;


proc sql;
	select count(id) from sasfiles.Demo_ where dx_depre=1;
quit;



proc sql;
		select * from sasfiles.pthx where prxmatch((select "/"||icd9||"/" from (select *, monotonic() AS num_row from sasfiles.codesmnd) where num_row=1),codes)>0;
quit;

proc sql;
	select count(id) from sasfiles.Demo_ where dx_arf =1;
run;


/*create dataset*/
data original_data;
    input day $ sales fm;
    datalines;
1 14 20
2 19 50
2 22 90
3 20 50
3 16 40
4 26 30
5 40 44
5 43 15
5 29 15
5 30 15
6 35
7 33
;
run;

/*create new dataset that shows lagged values of sales*/
proc sort data=new_data;
by day;
run;
data new_data;
    set original_data;
	by day; 
		lagfm = lag(fm);
		if first.day then lagfm=0;
	    lag1_sales = lagfm-sales;
run;

/*view new dataset*/
proc print data=new_data;



proc sql;
  select count(distinct id) from sasfiles.Demo_ where exposure =1;
quit;
proc sql;
  select * from t where id ="10080107";
quit;

proc sql;
  select * from sasfiles.Demo_ where id ="10080107";
quit;

proc sql;
SELECT *
FROM (SELECT id, date_rx_st as date_rx_st format=mmddyy10.
			      FROM t
			      WHERE date_rx_st <>.
			      GROUP BY id) as x
					LEFT JOIN (SELECT id, onset_date, min(dod, obs_deadline) format=mmddyy10. as obs_deadline from
									(SELECT id, onset_date, dod, input('12312018', mmddyy10.) format=mmddyy10. as obs_deadline FROM sasfiles.Demo_)) as y
		ON x.id = y.id where x.id ="10080107" & x.date_rx_st >= y.onset_date & x.date_rx_st < y.obs_deadline;
quit;

proc sql;
	alter table sasfiles.Demo_ drop exposure;
run;

proc sql;
		(select * from sasfiles.pthx where prxmatch((select "/"||icd9||"/" from (select *, monotonic() AS num_row from sasfiles.codesmnd) where num_row=1),codes)>0);
	quit;



proc sql;
		(select cats("/",icd9,"/") from (select *, monotonic() AS num_row from sasfiles.codesmnd) where num_row=2);
	quit;


 
proc sql;
		(select * from sasfiles.pthx where prxmatch((select cats("/",icd9,"/") from (select *, monotonic() AS num_row from sasfiles.codesmnd) where num_row=2),codes)>0);
	quit;

 
proc sql;
		(select * from sasfiles.pthx where prxmatch("/^333.1 /",codes)>0);
	quit;


proc sql;
select distinct id from sasfiles.Demo_ where dx_cancer=1 and exposure=0;
quit;


data _null_;
   sdate='05mar1954'd;
   edate='16apr2012'd;
   age=yrdif(sdate, edate, 'AGE');
   put age= 'years';
run;


proc sql;
	select * from 
		(select id, codes, ref_date from sasfiles.dx) as a,
		(select id, onset_date from sasfiles.Demo_) as b 
		where a.id=b.id and a.ref_date= b.onset_date;
quit;


proc sql;
		create table sasfiles.temp_dxmnd as 
		select distinct id,
						1 as &colname from 
		(select * from sasfiles.dxmnd where prxmatch("/^335.2$|^335.29$/",codes)>0);
	quit;


	proc sql;
	select cats("/",icd9,"/") from (select *, monotonic() AS num_row from sasfiles.subtype) where num_row=5;
	quit;

data _null_;
position=prxmatch("/^335.2$|^335.29$/","335.29");
put position=;
run;

proc sql;
select * from
(select id, onset_date, exposure,  dod from sasfiles.Demo_) as a 
			left join ttt as b on a.id=b.id  where b.date_rx_st >= a.onset_date or b.date_rx_st=.;
quit;


where date_rx_st >= onset_date or date_rx_st=.
