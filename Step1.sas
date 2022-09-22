/*On commence par représenter nos séries temporelles*/
proc timeseries data=memoire.prod plot=series;
   id date interval=month;
   var prod;
run;
proc timeseries data=memoire.M2 plot=series;
   id date interval=month;
   var M2;
run;
proc timeseries data=memoire.credit plot=series;
   id date interval=month;
   var credit;
run;

/*On transforme nos variables en log*/
data memoire.logprod;
   set memoire.prod;
   logprod = log(prod);
run;
data memoire.logM2;
   set memoire.M2;
   logM2 = log(M2);
run;
data memoire.logcredit;
   set memoire.credit;
   logcredit = log(credit);
run;

/*On représnete graphiquement les séries temporelles avec les log*/
proc timeseries data=memoire.logprod plot=series;
   id date interval=month;
   var logprod;
run;
proc timeseries data=memoire.logM2 plot=series;
   id date interval=month;
   var logM2;
run;
proc timeseries data=memoire.logcredit plot=series;
   id date interval=month;
   var logcredit;
run;
