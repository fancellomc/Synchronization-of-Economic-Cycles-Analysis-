
/*Test de la stationnarité des cycles et leurs retards*/ 
proc arima data=memoire.cycle;  
identify var=prod_cycle stationarity=(adf=2);  
run;  
quit; 
proc arima data=memoire.cycle;  
identify var=M2_cycle stationarity=(adf=2);  
run;  
quit; 
proc arima data=memoire.cycle;  
identify var=credit_cycle stationarity=(adf=2);  
run;  
quit; 
/*C'est stationnaire car le PR<ROH est inférieur à 5%, on regarde moyenne 0 car on a enlever la tendance*/
/*Cela nous donne des AR2*/


/*On crée les variables retardées.*/
data memoire.cycle;
set memoire.cycle;
prodcyclelag1 = lag1( prod_cycle );
prodcyclelag2 = lag2( prod_cycle );
run;
data memoire.cycle;
set memoire.cycle;
M2cyclelag1 = lag1( M2_cycle );
M2cyclelag2 = lag2( M2_cycle );
run;
data memoire.cycle;
set memoire.cycle;
creditcyclelag1 = lag1( credit_cycle );
creditcyclelag2 = lag2( credit_cycle );
run;

/*On souhaite obtenir les résidus*/
proc reg data=memoire.cycle;
model prod_cycle = prodcyclelag1 prodcyclelag2;
output out=memoire.cycle r=residprod;
run; 
quit; 
proc reg data=memoire.cycle;
model M2_cycle = M2cyclelag1 M2cyclelag2;
output out=memoire.cycle r=residM2;
run; 
quit; 
proc reg data=memoire.cycle;
model credit_cycle = creditcyclelag1 creditcyclelag2;
output out=memoire.cycle r=residcredit;
run; 
quit; 
/*On a bien vérifié que nos résidus sont significatif*/ 


/*Test: la corrélation croisée
proc timeseries data = memoire.cycle crossplot=all outcrosscorr=outcrosscorr;
   crosscorr lag n ccov ccf ccfstd;
   var residprod ;
   crossvar residM2;
run;
proc timeseries data = memoire.cycle crossplot=all outcrosscorr=outcrosscorr;
   crosscorr lag n ccov ccf ccfstd;
   var residprod ;
   crossvar residcredit;
run; */

data residu; set memoire.cycle; keep residprod2 residM2 residcredit; run; 
 

	/*On fait la cross correlation entre prod et M2*/
proc iml;
reset log;
use residu;
read all var {residprod2} into matresidprod; /* lecture */
print matresidprod;
read all var {residM2} into matresidM2; /* lecture */
print matresidM2;
X1=matresidprod;
print X1; 
X2=matresidM2;
X=X1||X2;

c=corr(X);
print c;
/*cross corr : entire sample : lead/lag=tau*/
tau=1;/*if tau=0 : sample correlations*/
Xa=X1[1:nrow(X1)-tau]||X2[1+tau:nrow(X2)];/*from X1 to X2*/
Xb=X2[1:nrow(X1)-tau]||X1[1+tau:nrow(X2)];/*from X2 to X2*/
/*from X1 to X2*/
ca=corr(Xa);
ca=ca[1,2];
/*from X2 to X1*/
cb=corr(Xb);
cb=cb[1,2];
resu=ca||(c[1,2])||cb;
print resu;

/*corr : sliding windows : I take 20 for the window*/
free c1;
do t=1 to nrow(X1)-20;
X=X1[t:20+t-1]||X2[t:20+t-1];/*form X1 to X2*/
c=corr(X);
c1=c1//((t+20-1)||c[1,2]);
end;
print c1;

/*On sépare les vecteur*/
date_20=c1[,1];
crosscorM2prod20=c1[1:577,2];
print date_20; 
print crosscorM2prod20;

/*On le met dans un tableau*/
create crosscorrM220 var {date_20 crosscorM2prod20};
append;
close crosscorrM220;


/*On fait la cross correlation entre prod et credit*/
proc iml;
reset log;
use residu;
read all var {residprod2} into matresidprod; /* lecture */
print matresidprod;
read all var {residcredit} into matresidcredit; /* lecture */
print matresidcredit;
X1=matresidprod;
print X1; 
X2=matresidcredit;
X=X1||X2;

c=corr(X);
print c;
/*cross corr : entire sample : lead/lag=tau*/
tau=1;/*if tau=0 : sample correlations*/
Xa=X1[1:nrow(X1)-tau]||X2[1+tau:nrow(X2)];/*from X1 to X2*/
Xb=X2[1:nrow(X1)-tau]||X1[1+tau:nrow(X2)];/*from X2 to X2*/
/*from X1 to X2*/
ca=corr(Xa);
ca=ca[1,2];
/*from X2 to X1*/
cb=corr(Xb);
cb=cb[1,2];
resu=ca||(c[1,2])||cb;
print resu;

/*corr : sliding windows : I take 20 for the window*/
free c2;
do t=1 to nrow(X1)-20;
X=X1[t:20+t-1]||X2[t:20+t-1];/*form X1 to X2*/
c=corr(X);
c2=c2//((t+20-1)||c[1,2]);
end;
print c2;

/*On sépare les vecteur*/
date_20=c2[,1];
crosscorcreditprod20=c2[,2];

/*On le met dans un tableau*/
create crosscorrcredit20 var {date_20 crosscorcreditprod20};
append;
close crosscorrcredit20;
