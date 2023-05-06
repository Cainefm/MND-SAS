/****************************************************************************
*  Macro name : motor neurone disease                                       *
*  Version    : 1.00                                                        *
*  Author     : FAN Min		                                                *
*  Date : 26JUL2022                                                         *
*  ------------------------------------------------------------------------ *
*  Revisions :   Versions     Date       Author                             *
*                 1.0        02MAY2023   FAN Min                            *
*				  SCCS model finished; code structure cleaned				*
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
%let rc = %sysfunc(dlgcdir("&wd.")); /* working path for my projects */


filename dx "./1.raw data/dx.xlsx";
filename ip "./1.raw data/ip.xlsx";
filename rx "./1.raw data/rx.xlsx";
filename demo "./1.raw data/demo.xlsx";
filename codesmnd "./1.raw data/codes_mnd.xlsx";

/* Define macro and output directories */
libname sasfiles "./2.sas data";
libname output "./3.output";

%LET macdir = &wd/4.macro;
%LET outdir = &wd/3.output;

/* Read in macros */
%INCLUDE "&macdir\0.0 macro_basic.sas";
%INCLUDE "&macdir\0.0 Table 1s.sas";

/*%INCLUDE "&macdir\macro_sccs.sas";*/

/* Data reading */
%reading(filename=demo,out=demo)
%reading(filename=dx,out=dx)
%reading(filename=ip,out=ip)
%reading(filename=rx,out=rx)

%reading(filename=codesmnd,out=codesmnd)
%reading(filename=codesmnd,sheet=hx,out=hx)
%reading(filename=codesmnd,sheet=subtype,out=subtype)
%reading(filename=codesmnd,sheet=rx,out=codesrx)

/*SCCS model*/
%INCLUDE "&macdir\0.3 sccs.sas";
%INCLUDE "&macdir\0.3 poisreg.sas";
%INCLUDE "&macdir\0.3 element.sas";

%INCLUDE "&macdir\0.3 macro_sccs.sas";

%run_sccs(title=primary_analysis);
%run_sccs(title=sg_ae,ae=T)
%run_sccs(title=sg_pneumonia,icd_defined = "/486/")
%run_sccs(title=sg_arf,icd_defined = "/518.8[12]/")
%run_sccs(title=sens_collapse,collapse=T)

%run_sccs_collapse()

%put &icd_defined;
%let icd_defined = "/^486/";
%let icd_defined = "/^518.8[12]/";




	
