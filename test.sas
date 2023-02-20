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




