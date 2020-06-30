
/*step1 */

/*
����		����		��ȥ		����		����		(a,b,c,d,e)
�μ�	 ������ȹ����		�ǰ�����	 �ٹ�����		����_tot	 (f,g,h,i,j)
����_com		����_sen		����_ins		��������		������������	 (k,l,m,n,o)
�ӻ�����	���������	�����ӻ�����		�������������_����		�����ӻ��¿� (p,q,r,s,t)
�����ӻ��¿�_�Ҽ����ݿø�	�����ӻ��³�		����_2 (u,v,w)
 */

data x;
infile 'C:\Users\user\Desktop\2000.csv' dsd firstobs=2;
input a b c d e f g h i j k l m n o p q r s t u v w;
run;

/*KM*/

proc lifetest method = km plots=(s) graphics ;
time u*o(0) ;
strata b;
symbol1 v=none color=black line=1;
symbol2 v=none color=black line=2;
run;

/*Cox*/

data x2;
set x;
if b=1 then trt_b = 1; else trt_b = 0 ;
if c=1 then trt_c = 1; else trt_c = 0 ;

if d=1 then trt_d1 = 1; else trt_d1 = 0 ;
if d=2 then trt_d2 = 1; else trt_d2 = 0 ;
if d=3 then trt_d3 = 1; else trt_d3 = 0 ;
if d=4 then trt_d4 = 1; else trt_d4 = 0 ;

if e=1 then trt_e1 = 1; else trt_e1 = 0 ;
if e=2 then trt_e2 = 1; else trt_e2 = 0 ;

if f=1 then trt_f = 1; else trt_f = 0 ;
if g=1 then trt_g = 1; else trt_g = 0 ;

if h=1 then trt_h1 = 1; else trt_h1 = 0 ;
if h=2 then trt_h2 = 1; else trt_h2 = 0 ;
if h=3 then trt_h3 = 1; else trt_h3 = 0 ;

if i=1 then trt_i1 = 1; else trt_i1 = 0 ;
if i=2 then trt_i2 = 1; else trt_i2 = 0 ;
if i=3 then trt_i3 = 1; else trt_i3 = 0 ;
if i=4 then trt_i4 = 1; else trt_i4 = 0 ;
if i=5 then trt_i5 = 1; else trt_i5 = 0 ;

if w=1 then trt_w1 = 1; else trt_w1 = 0 ;
if w=2 then trt_w2 = 1; else trt_w2 = 0 ;
if w=3 then trt_w3 = 1; else trt_w3 = 0 ;
run;


/*��ü */

proc phreg data=x2;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
/*��ʼ� ����*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*1group : 4�� */
data group1;
set x2;
if (w=1);
run;
proc phreg data=group1;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*2group : 46�� */
data group2;
set x2;
if (w=2);
run;
proc phreg data=group2;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*3group : 175�� */
data group3;
set x2;
if (w=3);
run;
proc phreg data=group3;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;



/*4group 404�� */
data group4;
set x2;
if (w=4);
run;
proc phreg data=group4;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;






































/*step2 */


/*
����		����		��ȥ		����		����		(a,b,c,d,e)
�μ�	 ������ȹ����		�ǰ�����	 �ٹ�����		����_tot	 (f,g,h,i,j)
����_com		����_sen		����_ins		��������		������������	 (k,l,m,n,o)
�ӻ�����	���������	�����ӻ�����		�������������_����		�����ӻ��¿� (p,q,r,s,t)
�����ӻ��¿�_�Ҽ����ݿø�	�����ӻ��³�		����_2 (u,v,w)
 */

/*KM*/

data x;
infile 'C:\Users\user\Desktop\600.csv' dsd firstobs=2;
input a b c d e f g h i j k l m n o p q r s t u v w;
run;


proc lifetest method = km plots=(s) graphics ;
time u*o(0) ;
strata b;
symbol1 v=none color=black line=1;
symbol2 v=none color=black line=2;
run;

/*Cox*/

data x2;
set x;
if b=1 then trt_b = 1; else trt_b = 0 ;
if c=1 then trt_c = 1; else trt_c = 0 ;

if d=1 then trt_d1 = 1; else trt_d1 = 0 ;
if d=2 then trt_d2 = 1; else trt_d2 = 0 ;
if d=3 then trt_d3 = 1; else trt_d3 = 0 ;
if d=4 then trt_d4 = 1; else trt_d4 = 0 ;

if e=1 then trt_e1 = 1; else trt_e1 = 0 ;
if e=2 then trt_e2 = 1; else trt_e2 = 0 ;

if f=1 then trt_f = 1; else trt_f = 0 ;
if g=1 then trt_g = 1; else trt_g = 0 ;

if h=1 then trt_h1 = 1; else trt_h1 = 0 ;
if h=2 then trt_h2 = 1; else trt_h2 = 0 ;
if h=3 then trt_h3 = 1; else trt_h3 = 0 ;

if i=1 then trt_i1 = 1; else trt_i1 = 0 ;
if i=2 then trt_i2 = 1; else trt_i2 = 0 ;
if i=3 then trt_i3 = 1; else trt_i3 = 0 ;
if i=4 then trt_i4 = 1; else trt_i4 = 0 ;
if i=5 then trt_i5 = 1; else trt_i5 = 0 ;

if w=1 then trt_w1 = 1; else trt_w1 = 0 ;
if w=2 then trt_w2 = 1; else trt_w2 = 0 ;
if w=3 then trt_w3 = 1; else trt_w3 = 0 ;
run;


/*��ü */

proc phreg data=x2;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
/*��ʼ� ����*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*1group : 4�� */
data group1;
set x2;
if (w=1);
run;
proc phreg data=group1;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*2group : 46�� */
data group2;
set x2;
if (w=2);
run;
proc phreg data=group2;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*3group : 175�� */
data group3;
set x2;
if (w=3);
run;
proc phreg data=group3;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;



/*4group 404�� */
data group4;
set x2;
if (w=4);
run;
proc phreg data=group4;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;






