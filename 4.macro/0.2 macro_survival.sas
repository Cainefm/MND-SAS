
/* ************************************************************** */
/* Analysis 3: survival analysis between death and rilozle 		  */
/* ************************************************************** */

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
