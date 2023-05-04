/****************************************************************************
*  Macro name : poisreg                                                     *
*  Version    : 12.02                                                       *
*  Author     : Bart Spiessens                                              *
*  Date : 20DEC2004                                                         *
*  ------------------------------------------------------------------------ *
*  Revisions :   Versions     Date       Author                             *
*                                                                           *
*                 12.00      20DEC2004   Bart Spiessens                     *
*                 Creation of the macro                                     *
*                 12.01      02FEB2005   Bart Spiessens                     *
*                 Improvement: Replace &data by wk_pois                     *
*                 12.02      21AUG2006   Bart Spiessens                     *
*                 Improvement: option to print and save covariance matrix   *
*                              and correlation matrix of parameter estimates*
*  ------------------------------------------------------------------------ *
*  Description : This macro fits a Poisson (case series)                    *
*                regression model with the possibility to eliminate a       *
*                factor from the model, to reduce the computation time      *
*  Reference : R. THOMPSON, Biometrics, 1977, 33, 485-495 'The estimation   *
*              of heritability with unbalanced data. I. Observations        *
*              available on parents and offspring                           *
*  ------------------------------------------------------------------------ *
*  INPUT - parameters                                                       *
*                                                                           *
*     data     :  Input dataset                                             *
*     y        :  Response variable for poisson regression (count data)     *
*     covar    :  Covariates in model that are not in class or elim         *
*     class    :  Class variables in model                                  *
*     elim     :  Variable that should be eliminated from the model         *
*                 i.e., this variable is in the model but not estimated     *
*                 e.g., pid                                                 *
*     offset   :  offset variable                                           *
*     beta0    :  starting values, if empty                                 *
*     outdata  :  dataset with parameter estimates (DEFAULT =out)           *
*     eps      :  convergence criteria (DEFAULT=1e-08), the maximum of the  * 
*                 absolute value of the first derivative is calculated, if  *
*                 smaller than eps, the algorithm stops                     *
*     alpha    :  significance level for confidence interval (DEFAULT=0.05) *
*     prntyn   :  print results (DEFAULT=Y)                                 *
*                                                                           *
*  INPUT - macros variables (other than standards)                          *
*                                                                           *
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
*  ------------------------------------------------------------------------ *
*  UPDATE - table                                                           *
*                                                                           *
****************************************************************************/

%MACRO poisreg(data=,y=,covar=,class=,elim=,offset=,beta0=,outdata=out,eps=1e-08,alpha=0.05,prntyn=Y,covb=N,title=) /* / STORE DES='Poisson regression with elimination factor' */ ;

TITLE &title;

%LET covb=%UPCASE(%SUBSTR(&covb,1,1));

DATA wk_pois;
  SET &data;
RUN;

/* Create matrix UIT with number of measurements per level of &elim  */
/* This matrix is read in IML later                          */
PROC SORT DATA=wk_pois;
  BY &elim;
RUN;
PROC FREQ DATA=wk_pois;
  TABLES &elim / out=uit NOPRINT;
RUN;

/* Start IML procedure*/
PROC IML;
  /* Write PUT statements to the SAS log */
  FILE log;
  /* Module INVDERIV calculates the inverse of minus the second derivative matrix times
  the first derivative to update the parameter in the Newton-Raphson procedure */
  START invderiv(aantal,W,mu,deriv) GLOBAL(invSa);
    /* A is diagonal matrix, instead of using the whole matrix, we work only with the 
       diagnonal */
    *A = t(U)*diag(mu)*U;
    p1 = nrow(aantal)-1;
	p2 = ncol(W);
	p = p1 + p2;
	/* Calculate diagonal of A */
	first = 1;
    DO i=1 TO p1;
      ni = aantal[i];
	  last = first + ni - 1;
	  hulp = sum(mu[first:last,]);
	  dA = dA//hulp;
	  first = last + 1;
    END;
	/* Calculate B */
    muW = mu#W;
	first = 1;
    DO i=1 TO p1;
      ni = aantal[i];
	  last = first + ni - 1;
	  muWi = muW[first:last,];
	  muWi = muWi[+,];
	  B = B//muWi;
	  first = last + 1;
    END;

    C = t(B);
    D = t(W)*(mu#W);
	invdA = 1/dA;
	Sa = D-C*(invdA#B);
    invSa = inv(Sa);
	IA = invdA#deriv[1:p1] + (invdA#B)*(invSa*((C#t(invdA))*deriv[1:p1]));
    IB = -(invdA#B)*(invSa*deriv[(p1+1):p]);
    IC = -invSa*(C#t(invdA))*deriv[1:p1];
    ID = invSa*deriv[(p1+1):p];
    out = (IA+IB)//(IC+ID);
	RETURN(out);
  FINISH;

  /* Read in necessary vectors and matrices */
  USE uit;
  READ all var {count} INTO aantal;
  CLOSE uit;
  USE wk_pois;
  READ all var {&y} INTO y;
  %IF &covar NE %THEN %DO;
  READ all var {&covar} INTO W1;
  %END;
  %IF &class NE %THEN %DO;
  READ all var {&class} INTO W2;
  %END;
  %IF &offset NE %THEN %DO;
  READ all var {&offset} INTO offset;
  %END;
  CLOSE wk_pois;
  /* Calculate the log of the offset */
  %IF &offset NE %THEN %DO;
    l_off = log(offset);
  %END;
  %ELSE %DO;
    l_off = 0;
  %END;

  W = W1;
  %IF &class NE %THEN %DO;
  DO i=1 TO ncol(W2);
    hulp = design(W2[,i]);
	nclass = nclass//ncol(hulp);
	W = W||hulp[,1:ncol(hulp)-1];
  END;
  %END;

  /* Convergence criteria */
  eps = &eps;

  /* n = number of observations */
  n = nrow(W);
  /* p1 = number of categories in &elim - 1 = number of dummy variables for &elim */
  p1 = nrow(aantal)-1;
  /* p2 = number of parameters in the rest of the model */
  p2 = ncol(W);
  /* p = total number of parameters */
  p = p1 + p2;
  /* If starting values are not given than initialize beta0 to 0 */
  %IF &beta0 NE  %THEN %DO;
    hulp = &beta0;
    beta0 = repeat(0,p1,1)//hulp;
  %END;
  %ELSE %DO;
    beta0 = repeat(0,p,1);
  %END;
  /*
  We want to calculate mu = exp(X*beta + l_off) where X = (U|W) and U is a design
  matrix containing the design vectors for &elim. This is a huge matrix (s x s-1) where 
  s = number of subjects and contains a lot of zeros.
  All calculations will be done using the number of measurements for each
  level of &elim instead of U.
  */
  /* We split the calculation of X*beta as U*beta1 + W*beta2 where beta1 is p1x1 and
  beta2 = p2x1. U*beta1 is a nx1 column vector where the beta1[1] is repeated aantal[1]
  times, ... */
  DO i=1 TO p1;
    Ubeta0 = Ubeta0//repeat(beta0[i],aantal[i],1);
  END;
  /* At the end we have to add zeros because of the reference subject */
  Ubeta0 = Ubeta0//repeat(0,aantal[p1+1],1);
  /* Add W part to calculation */
  mu0 = exp(Ubeta0+W*beta0[(p1+1):p] + l_off);

  /* Calculate the first derivative */
  /* deriv1 = t(X)*(y-mu) and this can be written as t(U)*(y-mu)//t(W)*(y-mu) */
  /* t(U)*x is a p1x1 vector with elements x[1] + ... + x[n1], x[n1+1]+...+x[n1+n2],... 
  where ni is the number of measurements for the ith level of the factor &elim */
  first = 1;
  DO i=1 TO p1;
    ni = aantal[i];
	last = first + ni - 1;
	hulp = sum(y[first:last,]-mu0[first:last,]);
	Uderiv1 = Uderiv1//hulp;
	first = last + 1;
  END;
  /* Add W part to calculation */
  deriv1 = Uderiv1//(t(W)*(y-mu0));

  mu = mu0;
  beta = beta0;
  /* tel counts the number of iterations */
  tel = 0;
  /* Convergence criteria is the maximum of the absolute value of the first derivative */
  DO WHILE (max(abs(deriv1)) > eps);
    tel = tel + 1;
	/* put tel; */
	hulp = max(abs(deriv1));
    /* put hulp; */
	/* Update parameter using Newton-Raphson */
    beta = beta + invderiv(aantal,W,mu,deriv1);
	/* Initialize Ubeta matrix */
    Ubeta = loc(0);
	/* See above for explanation of update */
    DO i=1 TO p1;
      Ubeta = Ubeta//repeat(beta[i],aantal[i],1);
    END;
    Ubeta = Ubeta//repeat(0,aantal[p1+1],1);
    mu = exp(Ubeta+W*beta[(p1+1):p] + l_off);

	/* Initialize Uderiv1 matrix */
    Uderiv1 = loc(0);
	/* See above for explanation of update */
    first = 1;
    DO i=1 TO p1;
      ni = aantal[i];
	  last = first + ni - 1;
  	  hulp = sum(y[first:last,]-mu[first:last,]);
  	  Uderiv1 = Uderiv1//hulp;
	  first = last + 1;
    END;
    deriv1 = Uderiv1//(t(W)*(y-mu));
  END;
  /* Print output */
  ll = sum(-mu + y#log(mu));
  stderr = sqrt(vecdiag(invSa));
  %IF &prntyn = Y %THEN %DO;
    print "Log-likelihood";
    print ll;
  %END;
  *print "Parameters";
  params = beta[(p1+1):p];
  *print params;
  *print "Standard errors";
  *print stderr;
  mat = params[1:ncol(W1)]||stderr[1:ncol(W1)];
  %IF &class NE %THEN %DO;
  first = ncol(W1)+1;
  DO i = 1 TO ncol(W2);
    ni = nclass[i]-1;
	last = first+ni-1;
    hulp=(params[first:last]||stderr[first:last])//repeat(0,1,2);
    mat=mat//hulp;
	first = last + 1;
  END;
  %END;
  *mat = params||stderr;
  %IF &covar NE %THEN %DO;
  rownm = t({&covar});
  %END;
  %IF &class NE %THEN %DO;
  rownm2 = {&class};
  DO i = 1 TO ncol(rownm2);
    hulp = t(char(unique(W2[,i])));
    rownm = rownm//concat(repeat(rownm2[,i],nclass[i],1),hulp);
    *print rownm;
  END;
  %END;
  colnm = "Estimate"||"StErr";
  CREATE labels FROM rownm [COLNAME={Params}];
  APPEND FROM rownm;
  CREATE &outdata FROM mat [COLNAME=colnm];
  APPEND FROM mat;
  %IF &prntyn = Y AND &covb = Y %THEN %DO;
    print "Estimated covariance matrix";
    print invSa;
    CREATE _covb FROM invSa;
    APPEND FROM invSa;
  %END;
QUIT;

DATA &outdata;
  MERGE labels &outdata;
  z = estimate/sterr;
  chisq = (z)**2;
  pvalue = 1-probchi(chisq,1);
  ll = estimate - probit(1-&alpha/2)*sterr;
  ul = estimate + probit(1-&alpha/2)*sterr;
  expest = exp(estimate);
  expll = exp(ll);
  expul = exp(ul);
RUN;

%LET conf = %SYSEVALF(100*(1-&alpha));

%IF &prntyn = Y %THEN %DO;

PROC REPORT DATA=&outdata SPLIT="#" NOWINDOWS;
COLUMN params estimate sterr ("Wald &conf% Confidence Limits" ll ul) chisq pvalue expest 
("Exp &conf% Confidence Limits" expll expul);
DEFINE params / ID WIDTH=20 "Parameter";
DEFINE estimate / WIDTH=12 FORMAT=8.4 "Estimate";
DEFINE sterr / WIDTH=12 FORMAT=8.4 "Standard#Error";
DEFINE ll / WIDTH=12 FORMAT=8.4 "Lower";
DEFINE ul / WIDTH=12 FORMAT=8.4 "Upper";
DEFINE chisq / WIDTH=12 FORMAT=8.2 "Chi-square";
DEFINE pvalue / WIDTH=12 FORMAT=pvalue8.4 "Pr>ChiSq";
DEFINE expest / WIDTH=12 FORMAT=8.4 "Exp Estimate";
DEFINE expll / WIDTH=12 FORMAT=8.4 "Lower";
DEFINE expul / WIDTH=12 FORMAT=8.4 "Upper";
RUN;
TITLE ;

%END;

%MEND poisreg;