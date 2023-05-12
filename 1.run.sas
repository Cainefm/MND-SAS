/****************************************************************************
*  Macro name : motor neurone disease                                       *
*  Version    : 1.1                                                        *
*  Author     : FAN Min		                                                *
*  Date : 26JUL2022                                                         *
*  ------------------------------------------------------------------------ *
*  Revisions :   Versions     Date       Author                             *
*				  1.1		 11MAY2023   FAN Min							*
*				  Fixed the multiple prescription problem by adding a       *
*				  additional chopping function in the original sccs_macro   *
*				  provided. =_= Ready to go i think.						*
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
*  Description : Macro to data cleaning and analysis for MND study 			*
*  ------------------------------------------------------------------------ *
*	INPUT - global parameters
*	  sysdx            : The diagnosis system in your system
*				         (pls check the codes_mnd.xlsx sheet name:hx)
*				 		 icd9 / icd10 / icd9_icd10 / icd9_icd10_nodecimal
*	  sysrx            : The prescripiton system in your system
*				 	   	 (pls check the codes_mnd.xlsx sheet name:rx)
*				 		 BNF / ATC / NCS / ATC_NCS
*	  studydate_st	   : The study observation start date
*	  studydate_ed 	   : The study observation end date 
*	  sccs_obs_st	   : The observation start date in SCCS model
*	  sccs_obs_ed	   : The observation end date in SCCS model
*	  wd			   : The working directory
*  ------------------------------------------------------------------------ *
*	DIRECTORY - folders
*	  0.documents      : 
*	  1.raw data       : Raw data (demo, dx, ip, rx, and codes_mnd.xlsx)
*     2.sas data       : Temp folder for sas data storage
*     3.output         : Folder for results exporting
*     4.macro          : Macro for all analysis
*	
****************************************************************************/

/* Define macro and output directories */
%let sysdx=icd9; 
%let sysrx=atc;
%let studydate_st=01jan1994;
%let studydate_ed=31dec2018;
%let sccs_obs_st=24aug2001;
%let sccs_obs_ed=31dec2018;

%let wd = C:/Users/LabPCSLi03/Desktop/mnd project;
options dlcreatedir; /* Create folders */
%let rc = %sysfunc(dlgcdir("&wd."));

%LET macdir = &wd/4.macro;
%LET outdir = &wd/3.output;

libname sasfiles "./2.sas data";

filename dx "./1.raw data/dx.xlsx";
filename ip "./1.raw data/ip.xlsx";
filename rx "./1.raw data/rx.xlsx";
filename demo "./1.raw data/demo.xlsx";
filename codesmnd "./1.raw data/codes_mnd.xlsx";




/* ************************************************************** */
/* Analysis 1: Descriptive study for MND						  */
/* ************************************************************** */

/* Read in macros */
%INCLUDE "&macdir\0.0 macro_basic.sas";
%INCLUDE "&macdir\0.0 Table 1s.sas";

/* Data reading */
%reading(filename=demo,out=demo)
%reading(filename=dx,out=dx)
%reading(filename=ip,out=ip)
%reading(filename=rx,out=rx)
%reading(filename=codesmnd,out=codesmnd)
%reading(filename=codesmnd,sheet=hx,out=hx)
%reading(filename=codesmnd,sheet=subtype,out=subtype)
%reading(filename=codesmnd,sheet=rx,out=codesrx)

/* ************************************************************** */
/* Analysis 2: demographic information (table one) 		  	  	  */
/* ************************************************************** */

%INCLUDE "&macdir\0.1 macro_demo.sas" / source2;

/* ************************************************************** */
/* Analysis 3: survival analysis between death and rilozle 		  */
/* ************************************************************** */

%INCLUDE "&macdir\0.2 macro_survival.sas" / source2;

/* ************************************************************** */
/* Analysis 3: Descriptive study for MND						  */
/* ************************************************************** */
%INCLUDE "&macdir\0.3 sccs.sas";
%INCLUDE "&macdir\0.3 poisreg.sas";
%INCLUDE "&macdir\0.3 element.sas";
%INCLUDE "&macdir\0.3 macro_sccs.sas";

%run_sccs(title=pri);
%run_sccs(title=sg_ae,ae=T);
%run_sccs(title=sg_pne,icd_defined = "/486/");
%run_sccs(title=sg_arf,icd_defined = "/518.8[12]/");
%run_sccs(title=sens_collapse,collapse=T);
