data abc;
INFILE "C:\Users\82104\Desktop\���п� ����\1�г� 2�б�\���м������\1����\data2.txt" firstobs=2 dsd dlm = " ";
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
