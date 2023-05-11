/****************************************************************************
*  Macro name : sccs                                                        *
*  Version    : 12.05                                                       *
*  Author     : Cécile Landais                                              *
*  Date : 24/09/2014                                                        *
*  ------------------------------------------------------------------------ *
*  Revisions :   Versions     Date       Author                             *
*                                                                           *
*                 12.00      20DEC2004   Bart Spiessens                     *
*                 Creation of the macro                                     *
*                 12.01      02FEB2005   Bart Spiessens                     *
*                 Remove errors: 'pid' replaced by '&pid' at some places    *
*                 Changed the end of the risk period when there is          *
*                   overlapping intervals (endva&i.&j = vac&i1+&risk1-1     *
*                   instead of endva&i.&j = vac&i1-1)                       *
*                 Add overlap parameter to allow overlapping intervals      *
*                 12.02      02MAR2005   Bart Spiessens                     *
*                 Agerange is expressed in days instead of years            *
*                 endobs = min(&endst,&dob_raw + &maxage-1)-&dob_raw;       *
*                 changed to                                                *
*                 endobs = min(&endst,&dob_raw + &maxage)-&dob_raw;         *
*                 12.03      31MAR2005   Bart Spiessens                     *
*                 Allow empty values for age and season                     *
*                 12.04      01APR2005   Bart Spiessens                     *
*                 Include semi-parametric analysis                          *
*                 12.05      24SEP2014   Cécile Landais                     *
*				  Allow risk periods of different length and several        *
*                 treatments                                                *
*  ------------------------------------------------------------------------ *
*  Description : Macro to create dataset for case series analysis           *
*  ------------------------------------------------------------------------ *
*  INPUT - parameters                                                       *
*                                                                           *
*    data             :  Input dataset                                      *
*    pid              :  subject ID number                                  *
*    dob_raw          :  Date of birth                                      *
*    events           :  Dates for events                                   *
*    start_risk       :  Dates for start of risk period                     *
*    (if more than 1 treatment : dates corresponding to the first treatment *
*     then to the second treatment...)                                      *
*    end_risk         :  Dates for end of risk period                       *
*    (if more than 1 treatment : dates corresponding to the first treatment *
*     then to the second treatment...)                                      *
*     -> We suppose that there are not overlapping intervals for a same     *
*     treatement.                                                           *
*    treatments_ind   :  Index of the different treatments                  *
*    Ex : if the first three dates of start_risk correspond to a treatment  *
*         and the next four to another treatment : treatments_ind = 1 3 4 7 *
*    treatments_names :  Names of the treatments                            *
*    startst          :  Study start                                        *
*    endst            :  Study end                                          *
*    covars           :  Covariates to be put in output dataset             *
*                                                                           *
*  INPUT - macros variables (other than standards)                          *
*                                                                           *
*     agerange : e.g., 0 730 (in days)                                      *
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
*       RISK_&treatment = 1 if the interval lies in a risk period of the    *
*                           treatment &treatment                            *
*       RISK_&treatment_k = 1 if the interval lies in the kth risk period   *
*                              of the treatment &treatment                  *
*                           treatment &treatment                            *
*                                                                           *
*  ------------------------------------------------------------------------ *
*  UPDATE - table                                                           *
*                                                                           *
/***************************************************************************/


%MACRO sccs(data=, pid=pid, dob_raw=, events=, start_risk=, end_risk=,treatments_ind=,treatments_names=, startst=, endst=, covars=, outdata=);

%GLOBAL nb_start_risk nb_end_risk nb_evt;

%LET semi = %UPCASE(&semi);

%element(lstvar=&start_risk , pref = start_risk);
%element(lstvar=&end_risk , pref = end_risk);
%element(lstvar=&treatments_ind, pref=ind);
%element(lstvar=&treatments_names, pref=treatments);
%element(lstvar=&events , pref = evt);

%IF &semi=Y %THEN %DO;
DATA _events;
  SET &data;
  ARRAY x(*) &events;
  DO i=1 TO dim(x);
    IF x[i] NE . THEN DO;
      y = x[i];
	  OUTPUT;
	END;
  END;
RUN;

PROC SQL NOPRINT;
  SELECT DISTINCT y INTO :age SEPARATED BY ' '
  FROM _events; 
QUIT;

%PUT &age;
%END;


%IF &age = %THEN %DO;
  %LET age = -1;
%END;
%element(lstvar=&age , pref = age);
%IF &season = %THEN %DO;
  %LET season = 31DEC;
%END;
%element(lstvar=&season , pref = season);

%IF &agerange NE %THEN %DO;
  %LET minage = %SCAN(&agerange,1,' ');
  %LET maxage = %SCAN(&agerange,2,' ');
%END;

PROC SQL NOPRINT;
  SELECT min(put(&startst,year4.)) INTO :year1
  FROM &data;
  SELECT max(put(&endst,year4.)) INTO :year2
  FROM &data;
QUIT;


DATA wk_sccs;
  SET &data;
  %IF &dob_raw= %THEN %DO;
    %LET dob_raw=_dob;
	_dob=0;
  %END;
  *KEEP &pid &dob_raw &events &vacc &startst &endst &covars;
  **setting the season cut-points;

  %DO i=&year1 %TO &year2;
    %DO j=1 %TO &nb_season;
        ss&i.&j = (input("&&season&j"||"&i",date9.)-&dob_raw);
    %END;
  %END;
RUN;

DATA wk_sccs;
  SET wk_sccs;
  %IF &agerange NE %THEN %DO;
  **observation period for each subject will start at the beginning of the study period 
  	or date of the minimum eligible age in the age range, whichever is later;
    startobs = max(&startst,&dob_raw + &minage)-&dob_raw-1;
  **age at start of obs period;
  **observation period was going to end at the end of the study period or the day 
    specified in maxage, whichever is earlier;
    endobs = min(&endst,&dob_raw + &maxage)-&dob_raw;	**age at end of obs period;
  %END;
  %ELSE %DO;
    startobs = &startst-&dob_raw-1;
	endobs = &endst-&dob_raw;
  %END;
  **setting the risk periods;
  %DO i = 1 %TO &nb_start_risk;
   	start_risk&i = &&start_risk&i.-&dob_raw.- 1;
	end_risk&i = &&end_risk&i.-&dob_raw.;
  %END;

  **setting the IS (event) cut-points;
  %DO i = 1 %TO &nb_evt;
	event&i = &&evt&i-&dob_raw;
  %END;

RUN;

/*	The observation period will be chopped up into intervals at each event (i.e. start
	of obs period, starts and end of risk periods, age cut points, end of obs period).  
    These intervals will not include the lower value and will include the upper	value.  
    Therefore, the following code re-sets the lower value of each interval to the original 
    lower value minus 1, so that the original value will be the first value included in the interval.*/


/*	Creating a dataset of 1 record per interval...setting a variable 'stop'
	to be equal to each 'cut point' if that cut point is within the
	observation period.*/

DATA wk_sccs;
  SET wk_sccs;
  %DO i=1 %TO &nb_age;
    age&i = &&age&i;
  %END;
  %DO i=1 %TO &nb_season;
    season&i = input("&&season&i"||"2000",date9.)-input("01JAN2000",date9.)+1;
  %END;


  ARRAY x(*) startobs endobs
  %DO i=1 %TO &nb_age;
    age&i
  %END;
  %DO i = 1 %TO &nb_start_risk;
	start_risk&i end_risk&i 
  %END;
  %DO i=&year1 %TO &year2;
    %DO j=1 %TO &nb_season;
      ss&i.&j
    %END;
  %END;
  ;
  FORMAT stopdate date9.;
  DO i=1 TO DIM(x);
    IF x[i] NE . AND x[i] >= startobs AND x[i] <= endobs THEN DO;
      stop = x[i];
	  stopdate = x[i] + &dob_raw;
	  dayyear = stopdate-input("01JAN"||trim(left(year(stopdate))),date9.)+1;
	  age = 0
	  %DO i=1 %TO &nb_age;
	    + (stop > age&i)
	  %END;
	  ;
	  season = 0
	  %DO i=1 %TO &nb_season;
	    + (dayyear > season&i)
	  %END;
	  ;
	  IF season = &nb_season THEN season = 0;
	  OUTPUT;
	END;
  END;
RUN;

/*sort by pid and date order of cut points*/
PROC SORT DATA=wk_sccs;
  BY &pid stop;
RUN;

/*	In the dataset of 1 record per interval...setting a variable 'start'
	to be equal to the 'cut point' ('stop' value) of the interval before.
	If it's the first interval for the pid, then 'start' will be the start
	of the obs period (the variable startobs).
	The offset variable is created to equal the length of each interval.
*/
DATA wk_sccs;
  RETAIN &pid &dob_raw start stop nevt offset &covars age season

  %DO i=1 %TO &nb_treatments ;
 	%let treatment=&&treatments&i;
	risk_&treatment
	%let ind_min=%eval(2*&i - 1);
	%let ind_max=%eval(2*&i);
	%let k=1;
	%DO j=&&ind&ind_min %TO &&ind&ind_max;
		risk_&treatment._&k
		%let k=%eval(&k+1);
	%END;
	%let nb_&treatment=%eval(&k-1);
  %END;
  ;
  SET wk_sccs;
  BY &pid;
  start = lag(stop);
  IF first.&pid THEN start = startobs;
  IF stop=start THEN DELETE;	**deleting intervals of no length;
  offset = (stop-start);		**offset=length of each interval;
  l_off = log(offset);			**l_off=ln(offset);
  nevt = 0;
  %DO i=1 %TO &nb_evt;
    nevt = nevt + (start < event&i <= stop);	**=1 if IS in interval, =0 if not;
  %END;
  
  %DO i=1 %TO &nb_treatments ;
  	%LET treatment=&&treatments&i;
   	%DO k=1 %TO &&nb_&treatment;
		%let ind_min=%eval(2*&i - 1);
		%let l=%eval(&&ind&ind_min + &k -1) ;
		%put &l;
		risk_&treatment._&k = (start_risk&l <= start AND stop <= end_risk&l);
   	%END;
  %END;

  %DO i=1 %TO &nb_treatments ;
  	%LET treatment=&&treatments&i;
  	risk_&treatment = 0
   	%DO k=1 %TO &&nb_&treatment;
		+ risk_&treatment._&k 
   	%END;
	;
  %END;
   int = 1;
RUN;

%IF &outdata= %THEN %DO;
  %LET outdata=wk_sccs;
%END;


DATA &outdata;
  SET wk_sccs;
  KEEP &pid &dob_raw start stop offset l_off int &covars age nevt season
  %DO i=1 %TO &nb_treatments ;
  	%LET treatment=&&treatments&i;
  	risk_&treatment
   	%DO k=1 %TO &&nb_&treatment;
		risk_&treatment._&k 
   	%END;
  %END;
  ;/*end of the KEEP statement*/
RUN;

%IF &semi=Y %THEN %DO;
DATA &outdata;
  SET &outdata;
  IF stop IN (&age);
  DROP start offset l_off;
RUN;
%END;

%MEND sccs;
