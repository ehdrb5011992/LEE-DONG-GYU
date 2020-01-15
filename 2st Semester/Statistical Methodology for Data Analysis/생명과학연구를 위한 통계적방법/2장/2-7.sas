data abc;
INFILE "C:\Users\82104\Desktop\대학원 수업\1학년 2학기\통계분석방법론\1주차\data2.txt" firstobs=2 dsd dlm = " ";
input No$ Value Diet$ Health$ ;
run;

proc anova data=abc;
class diet health ;
model Value = diet health ; 
run;

proc anova data=abc;
class diet health ;
model  value = diet health;
means diet/ scheffe lines ;
run;


proc anova data=abc;
class diet health ;
model  value = diet health;
means health/ scheffe lines ;
run;
