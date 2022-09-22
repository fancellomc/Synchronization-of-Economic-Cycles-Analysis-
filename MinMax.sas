/*On charge les variables dans IML*/
proc iml;
	/*Pour la production*/
use memoire.logprod;
read all var {'logprod'} into matprod; /* lecture */
print matprod;
read all var {'date'} into matdate; /* lecture */
print matdate;
	/*Pour la masse monétaire*/ 
use memoire.logM2;
read all var {'logM2'} into matM2; /* lecture */
print matM2;
	/*Pour la crédit*/ 
use memoire.logcredit;
read all var {'logcredit'} into matcredit; /* lecture */
print matcredit;


/* Return the location of the local maxima of a vector (including end points). 
If there are duplicate values at a maximum, the index    
of the first and last maximum value is returned. */ 
start locpeaks(x);  
small = min(x)-1; /* smaller than any point in the series */   
y = small // colvec(x) // small;/* original values are now in the interior */   
difSgn = dif(sign(dif(y)));     /* dif of sign of slopes */    
/*subtract 2: -1 for inserting 'small' at the beginning, and -1       
for the fact that negative values occurs AFTER the maximum */
return(loc(element(difSgn, {-1 -2})) - 2);
finish;

	/*On sort les minimums et maximuns locaux pour la production*/
LocMaxprod = t(locpeaks(matprod)); /*Maxima locaux*/
LocMinprod = t(locpeaks(-matprod)); /*Minima locaux*/
print LocMaxprod;
print LocMinprod;
	/*On sort les minimums et maximuns locaux pour la masse monétaire*/
LocMaxM2 = t(locpeaks(matM2)); /*Maxima locaux*/
LocMinM2 = t(locpeaks(-matM2)); /*Minima locaux*/
print LocMaxM2;
print LocMinM2;
	/*On sort les minimums et maximuns locaux pour le crédit*/
LocMaxcredit = t(locpeaks(matcredit)); /*Maxima locaux*/
LocMincredit = t(locpeaks(-matcredit)); /*Minima locaux*/
print LocMaxcredit;
print LocMincredit;

a=nrow(LocMaxprod); 
print a; 
b=nrow(LocMaxM2); 
print b; 
c=nrow(LocMaxcredit); 
print c; 
d=nrow(LocMinprod); 
print d; 
e=nrow(LocMinM2); 
print e; 
f=nrow(LocMincredit); 
print f;

/*On cherche le tau*/

do j=2 to 114;
m[j]=LocMaxprod[j]-LocMaxprod[j-1];
n[j]=LocMaxprod[j+1]-LocMaxprod[j];
end; 


z=min(e,f);
print z; 

/*On crée une variable indicatrice lorsqu'il y a un min et un max*/
	/*Pour production*/
		/*Le max*/
prodmax=matdate[,1]; /*On crée un vecteur colonne initial*/
maxprod=t(loc(prodmax)); /*maxprod prend les indice de 1 à 586*/
print maxprod; 
do i=1 to 115 ;
a=locmaxprod[i]; /*a est égal à la valeur rpise par locmax*/
maxprod[a]=1; /*Dans maxprod à la valeur a on met 1 parce qu'il y a un maximum local*/
end;
print maxprod; 
do i=1 to 586;
if maxprod[i]=i then maxprod[i]=0; /*Si ya pas de 1 il y aura un 0*/
end;
print maxprod;
		/*Le min*/
prodmin=matdate[,1];
minprod=t(loc(prodmin));
print minprod;
do i=1 to 115 ;
a=locminprod[i];
minprod[a]=1;
end;
print minprod;
do i=1 to 586;
if minprod[i]=i then minprod[i]=0;
end;
print minprod;

/*Pour la masse monétaire*/
		/*Le max*/
M2max=matdate[,1]; 
maxM2=t(loc(M2max));
print maxM2; 
do i=1 to 20 ;
a=locmaxM2[i]; 
maxM2[a]=1; 
end;
print maxM2; 
do i=1 to 586;
if maxM2[i]=i then maxM2[i]=0; 
end;
print maxM2;
		/*Le min*/
M2min=matdate[,1];
minM2=t(loc(M2min));
print minM2;
do i=1 to 20 ;
a=locminM2[i];
minM2[a]=1;
end;
print minM2;
do i=1 to 586;
if minM2[i]=i then minM2[i]=0;
end;
print minM2;

/*Pour le credit*/
		/*Le max*/
creditmax=matdate[,1]; 
maxcredit=t(loc(creditmax)); 
print maxcredit; 
do i=1 to 157 ;
a=locmaxcredit[i]; 
maxcredit[a]=1; 
end;
print maxcredit; 
do i=1 to 586;
if maxcredit[i]=i then maxcredit[i]=0; 
end;
print maxcredit;
		/*Le min*/
creditmin=matdate[,1];
mincredit=t(loc(creditmin));
print mincredit;
do i=1 to 159 ;
a=locmincredit[i];
mincredit[a]=1;
end;
print mincredit;
do i=1 to 586;
if mincredit[i]=i then mincredit[i]=0;
end;
print mincredit;



/*On crée une table avec les maximums et minimums*/
num_obs=t(1:586); print(num_obs); 
create nn var {num_obs maxprod minprod maxM2 minM2 maxcredit mincredit};
append;
close nn;
end; 
data date; set memoire.logprod; keep date;run;
data date; set date; 
retain num_obs 0;
num_obs+1; 
run; 
data memoire.minimum_maximum; merge date nn; by num_obs; run; 



/*On trouve la synchronisation entre prod et M2*/
data memoire.quiroga;  
set memoire.minimum_maximum;   
if maxprod=maxM2 & maxprod>0 then synmaxprodM2=0.5; 
else if maxprod=lag(maxM2) & maxprod>0 then synmaxprodM2=1;
else if maxprod=lag2(maxM2) & maxprod>0 then synmaxprodM2=1;
else if maxprod=lag3(maxM2) & maxprod>0 then synmaxprodM2=1;
else if maxprod=lag4(maxM2) & maxprod>0 then synmaxprodM2=1;
else if maxprod=lag5(maxM2) & maxprod>0 then synmaxprodM2=1;
else if maxprod=lag6(maxM2) & maxprod>0 then synmaxprodM2=1;
else synmaxprodM2=0; 
run;

data memoire.quiroga;  
set memoire.quiroga;   
if minprod=minM2 & minprod>0 then synminprodM2=0.5; 
else if minprod=lag(minM2) & minprod>0 then synminprodM2=1;
else if minprod=lag2(minM2) & minprod>0 then synminprodM2=1;
else if minprod=lag3(minM2) & minprod>0 then synminprodM2=1;
else if minprod=lag4(minM2) & minprod>0 then synminprodM2=1;
else if minprod=lag5(minM2) & minprod>0 then synminprodM2=1;
else if minprod=lag6(minM2) & minprod>0 then synminprodM2=1;
else synminprodM2=0; 
run;

/*On trouve la synchronisation entre M2 et prod*/ 
data memoire.quiroga;  
set memoire.quiroga;   
if maxprod=maxM2 & maxprod>0 then synmaxM2prod=0.5; 
else if maxM2=lag(maxprod) & maxM2>0 then synmaxM2prod=1;
else if maxM2=lag2(maxprod) & maxM2>0 then synmaxM2prod=1;
else if maxM2=lag3(maxprod) & maxM2>0 then synmaxM2prod=1;
else if maxM2=lag4(maxprod) & maxM2>0 then synmaxM2prod=1;
else if maxM2=lag5(maxprod) & maxM2>0 then synmaxM2prod=1;
else if maxM2=lag6(maxprod) & maxM2>0 then synmaxM2prod=1;
else synmaxM2prod=0; 
run;

data memoire.quiroga;  
set memoire.quiroga;   
if minprod=minM2 & minprod>0 then synminM2prod=0.5; 
else if minM2=lag(minprod) & minM2>0 then synminM2prod=1;
else if minM2=lag2(minprod) & minM2>0 then synminM2prod=1;
else if minM2=lag3(minprod) & minM2>0 then synminM2prod=1;
else if minM2=lag4(minprod) & minM2>0 then synminM2prod=1;
else if minM2=lag5(minprod) & minM2>0 then synminM2prod=1;
else if minM2=lag6(minprod) & minM2>0 then synminM2prod=1;
else synminM2prod=0; 
run;

/*On trouve la synchronisation entre prod et credit*/
data memoire.quiroga;  
set memoire.quiroga;   
if maxprod=maxcredit & maxprod>0 then synmaxprodcredit=0.5; 
else if maxprod=lag(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else if maxprod=lag2(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else if maxprod=lag3(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else if maxprod=lag4(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else if maxprod=lag5(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else if maxprod=lag6(maxcredit) & maxprod>0 then synmaxprodcredit=1;
else synmaxprodcredit=0; 
run;

data memoire.quiroga;  
set memoire.quiroga;   
if minprod=mincredit & minprod>0 then synminprodcredit=0.5; 
else if minprod=lag(mincredit) & minprod>0 then synminprodcredit=1;
else if minprod=lag2(mincredit) & minprod>0 then synminprodcredit=1;
else if minprod=lag3(mincredit) & minprod>0 then synminprodcredit=1;
else if minprod=lag4(mincredit) & minprod>0 then synminprodcredit=1;
else if minprod=lag5(mincredit) & minprod>0 then synminprodcredit=1;
else if minprod=lag6(mincredit) & minprod>0 then synminprodcredit=1;
else synminprodcredit=0; 
run;

/*On trouve la synchronisation entre credit et prod*/
data memoire.quiroga;  
set memoire.quiroga;   
if maxprod=maxcredit & maxcredit>0 then synmaxcreditprod=0.5; 
else if maxcredit=lag(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else if maxcredit=lag2(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else if maxcredit=lag3(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else if maxcredit=lag4(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else if maxcredit=lag5(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else if maxcredit=lag6(maxprod) & maxcredit>0 then synmaxcreditprod=1;
else synmaxcreditprod=0; 
run;

data memoire.quiroga;  
set memoire.quiroga;   
if minprod=mincredit & mincredit>0 then synmincreditprod=0.5; 
else if mincredit=lag(minprod) & mincredit>0 then synmincreditprod=1;
else if mincredit=lag2(minprod) & mincredit>0 then synmincreditprod=1;
else if mincredit=lag3(minprod) & mincredit>0 then synmincreditprod=1;
else if mincredit=lag4(minprod) & mincredit>0 then synmincreditprod=1;
else if mincredit=lag5(minprod) & mincredit>0 then synmincreditprod=1;
else if mincredit=lag6(minprod) & mincredit>0 then synmincreditprod=1;
else synmincreditprod=0; 
run;

proc means sum data=memoire.quiroga; 
var maxprod; 
var minprod;
var maxM2;
var minM2; 
var maxcredit;
var mincredit; 
var synmaxprodM2; 
var synminprodM2; 
var synmaxM2prod; 
var synminM2prod;
var synmaxprodcredit; 
var synminprodcredit;
var synmaxcreditprod; 
var synmincreditprod;
run; 

data lol; set memoire.minimum_maximum; if num_obs<124 then delete; else if num_obs>197 then delete; run; 

data lol; set memoire.minimum_maximum; if num_obs<368 then delete; else if num_obs>477 then delete; run;
