data Master1.logprod;
set master1.Prod;
logprod=log(prod);
run;
proc timeseries data=Master1.logprod plot=series;
id date interval=month;
var logprod;
run;
proc timeseries data=Master1.logprod
                out=series
                outtrend=trendlog
                outseason=seasonlog print=seasons;
   id date interval=month accumulate=avg;
   var logprod;
run;
title1 "Trend Statistics";

data trendprodyearlog;
   set trendlog;
   year = year(date);
run;

title1 "Trend Statistics by Year";
proc sgplot data=trendprodyearlog;
   series x=_season_ y=mean / group=year lineattrs=(pattern=solid);
   xaxis values=(1 to 12 by 1);
run;
title1 "Seasonal Statistics";
proc sgplot data=seasonlog;
   series x=_season_ y=max  / lineattrs=(pattern=solid);
   series x=_season_ y=mean / lineattrs=(pattern=solid);
   series x=_season_ y=min  / lineattrs=(pattern=solid);
   yaxis display=(nolabel);
   xaxis values=(1 to 12 by 1);
run;
