data abc;
INFILE "C:\Users\82104\Desktop\data1.txt" firstobs=2 dsd dlm = " ";
input No$ Value Cell$ ;
run;

proc ttest data = abc sides=L ;
class Cell;
var Value;
run;
quit;
