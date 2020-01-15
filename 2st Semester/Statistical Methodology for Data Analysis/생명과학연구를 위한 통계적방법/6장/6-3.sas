DATA sixthree;
INPUT Gender Exercise1_Minute_1 Exercise1_Minute_2 Exercise1_Minute_3 Exercise1_Minute_4
                       Exercise2_Minute_1 Exercise2_Minute_2 Exercise2_Minute_3 Exercise2_Minute_4 @@;
CARDS;
1 21.3 21.1 22.6 24.2 21.1 22.1 31.3 25.2
1 22.1 22.6 25.2 26.8 22.6 26.2 30.5 26.2
1 21.6 25.2 25.7 28.2 34.1 31.9 27.9 25.9
1 24.7 25.7 26.2 27.9 28.2 35.6 26.5 30.2
1 22.6 24.1 23.8 24.5 29.2 29.6 30.5 33.1

2 27.3 20.1 20.1 24.1 28.9 29.5 26.3 23.7
2 22.6 26.5 30.4 26.8 29.2 28.5 25.9 26.2
2 24.2 23.6 24.2 27.2 30.2 30.1 33.2 25.2
2 26.7 23.7 25.2 27.5 27.6 26.5 31.0 22.6
2 22.3 28.2 28.1 24.7 25.6 29.3 29.9 32.9
; run;
PROC GLM;
CLASS Gender;
MODEL Exercise1_Minute_1 -- Exercise2_Minute_4 = Gender/NOUNI SS3;
REPEATED Exercise 2(1 2), Minute 4(1 2 3 4)/PRINTE;
RUN;

proc export data=sixthree
outfile = 'C:\Users\82104\Desktop\6_3.txt'
dbms = dlm replace;
run;
