
%MACRO element(lstvar=,pref=);

%GLOBAL nb_&pref;
%LET i=1;
%DO %WHILE (%SCAN(&lstvar,&i,' ') NE %STR());
  %GLOBAL &pref.&i;
  %LET &pref.&i = %SCAN(&lstvar,&i,' ');
  %LET nb_&pref = &i;
  %LET i = %EVAL(&i+1);
%END;

%MEND element;

