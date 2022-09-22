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

reset log;
tx=locminprod;
ty=locmincredit;
N=586;
delta=18;
date=(1:N)`;
a=minprod;
b=mincredit; 


start SynchVtau(tx,ty,date,ind,delta,N);
/*Computing the lag*/
tau=min(dif(tx),dif(ty))/2;

start Jij(a,b,tau);
J=0;
if (a-b)>0 & (a-b)<=tau then J=1;
else tt=1;
if (a=b) then J=0.5;
else tt=1; 
return(J);
finish Jij;

start S(c,d);
if (c-d)<=0 then H=0;
else H=1;
return(H);
finish S;

if ind=0 then do;
Cxy=0;
do i=2 to nrow(tx)-1;
	do j=2 to nrow(ty)-1;
	tauij=(min(tx[i+1]-tx[i],tx[i]-tx[i-1],ty[j+1]-ty[j],ty[j]-ty[j-1]))/2;
	Cxy=cxy+Jij(tx[i],ty[j],tauij);
	end;
end;

Cyx=0;
do i=2 to nrow(ty)-1;
	do j=2 to nrow(tx)-1;
	tauji=(min(ty[i+1]-ty[i],ty[i]-ty[i-1],tx[j+1]-tx[j],tx[j]-tx[j-1]))/2;
	Cyx=cyx+Jij(ty[i],tx[j],tauji);
	end;
end;

if (nrow(tx)*nrow(ty))^=0 then do;
Qtau=(Cyx+Cxy)/sqrt(nrow(tx)*nrow(ty));
qt=(Cyx-Cxy)/sqrt(nrow(tx)*nrow(ty));
end;
else do;
Qtau=0;
qt=0;
end;
resu=Qtau//Qt;
end;

/*Time-Resolved Variants*/
else do;

free Cnxy;
do t=1 to N;
Cxya=0;
do i=2 to nrow(tx)-1;
	do j=2 to nrow(ty)-1;
	tauij=(min(tx[i+1]-tx[i],tx[i]-tx[i-1],ty[j+1]-ty[j],ty[j]-ty[j-1]))/2;
	Cxya=cxya+Jij(tx[i],ty[j],tauij)*S(t,tx[i]);
	end;
end;
Cnxy=Cnxy//Cxya;
end;


free Cnyx;
do t=1 to N;
Cyxb=0;
do i=2 to nrow(ty)-1;
	do j=2 to nrow(tx)-1;
	tauji=(min(ty[i+1]-ty[i],ty[i]-ty[i-1],tx[j+1]-tx[j],tx[j]-tx[j-1]))/2;
	Cyxb=cyxb+Jij(ty[i],tx[j],tauji)*S(t,ty[i]);
	end;
end;
Cnyx=Cnyx//Cyxb;
end;

Qtaun=Cnyx+Cnxy;
qtn=Cnyx-Cnxy;

eventx=j(N,1,0);
eventx[tx]=1;
eventy=j(N,1,0);
eventy[ty]=1;

free Qpn delay;
do t=1 to N-delta+1;
evX=eventX[t:delta+t-1];
evy=eventY[t:delta+t-1];
Qa=Qtaun[delta+t-1]-Qtaun[t];
Qb=qtn[delta+t-1]-qtn[t];
Nx=sum(evx);
Ny=sum(evy);
if Nx*Ny^=0 then do;
Qpn=Qpn//((Qa/sqrt(Nx*Ny))||date[delta+t-1]);
delay=delay//((Qb/sqrt(Nx*Ny))||date[delta+t-1]);
end;
else do;
Qpn=Qpn//(0||delta+t-1);
delay=delay//(0||delta+t-1);
end;
end;
resu=Qpn[,2]||Qtaun[delta:N]||Qtn[delta:N]||Qpn[,1]||delay[,1]||eventX[delta:N]||eventY[delta:N];
end;
return(resu);
finish SynchVtau;

quiroga=SynchVtau(tx,ty,date,0.3,delta,N);
print quiroga; 

Qpn=quiroga[,1]; 
Qtaun=quiroga[,2];
Qtn=quiroga[,3];
Qpn2=quiroga[,4];
delay=quiroga[,5];
EventX=quiroga[,6];
EventY=quiroga[,7];


create memoire.quiroga_mincredit_minprod var {Qpn Qtaun Qtn Qpn2 delay EventX EventY};
append; 
close memoire.quiroga_mincredit_minprod; 
