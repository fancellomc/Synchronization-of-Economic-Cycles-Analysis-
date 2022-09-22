/*Filtrage HP*/
proc ucm data=memoire.logprod;
   id date interval=month;
   model logprod;
   irregular plot=smooth;
   level var=0 noest plot=smooth;
   slope var=0.0000694 noest;
   cycle  period=12 rho=1 noest=(period rho) plot=smooth;
   estimate PROFILE;
   forecast outfor=memoire.UCMprod plot=(decomp);
run;

proc ucm data=memoire.logM2;
   id date interval=month;
   model logM2;
   irregular plot=smooth;
   level var=0 noest plot=smooth;
   slope var=0.0000694 noest;
   cycle period=12 rho=1 noest=(period rho) plot=smooth;
   estimate PROFILE;
   forecast outfor=memoire.UCMM2 plot=(decomp);
run;

proc ucm data=memoire.logcredit;
   id date interval=month;
   model logcredit;
   irregular plot=smooth;
   level var=0 noest plot=smooth;
   slope var=0.0000694 noest;
   cycle plot=smooth;
   estimate PROFILE;
   forecast outfor=memoire.UCMcredit plot=(decomp);
run;


/*On créé une base de données composé des valeurs des cycles lissés*/
data prod2; 
set memoire.Ucmprod;
keep date S_CYCLE; 
rename S_cycle=prod_cycle;
label S_cycle=cycle_lisse_de_prod;
run; 
data m22; 
set memoire.Ucmm2;
keep date S_CYCLE;
rename S_cycle=M2_cycle; 
label S_cycle=cycle_lisse_de_M2;
run;  
data credit2; 
set memoire.Ucmcredit;
keep date S_CYCLE; 
rename S_cycle=credit_cycle;
label S_cycle=cycle_lisse_de_credit;
run; 

/*On crée une base de données avec les cycles*/
data memoire.cycle;
merge prod2 m22 credit2;
by date;
run; 


