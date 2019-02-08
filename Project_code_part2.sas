
libname doe 'C:\Users\Nupur Baviskar\Desktop\Nupur IAA Material\SPRING 1\DESIGN OF EXPERIMENTS';

proc import datafile = "C:\Users\Nupur Baviskar\Desktop\Nupur IAA Material\SPRING 1\DESIGN OF EXPERIMENTS\orange team 7.csv"
out = doe.results
dbms = csv;
run;
/*There is quasi-complete separation on Location =4*/
/*All respondents who were sent a sample with location = 4 responded with a NO irrespective of other variables*/
PROC LOGISTIC Data=doe.results;
	CLASS location(ref ='4') price (ref = '4') experience (ref= '3') other (ref = '1');
	MODEL will_attend (event="1") = location price experience other;
RUN; 

/*Model 1: Try without location variable*/
PROC LOGISTIC Data=doe.results;
	CLASS price (ref = '4') experience (ref= '3') other (ref = '1');
	MODEL will_attend (event="1") = price experience other;
RUN; 

/*Model 2: Drop location 4 and try with all the variables*/
proc sql;
create table doe.results_filtered as 
select *
from doe.results
where location NE 4;
quit;

PROC LOGISTIC Data=doe.results_filtered;
	CLASS location(ref ='1') price (ref = '4') experience (ref= '3') other (ref = '1');
	MODEL will_attend (event="1") = location price experience other;
RUN;
