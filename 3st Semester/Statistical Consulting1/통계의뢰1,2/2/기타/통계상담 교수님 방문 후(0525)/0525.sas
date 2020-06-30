
/*step1 */

/*
나이		성별		결혼		종교		학위		(a,b,c,d,e)
부서	 이직계획유무		건강상태	 근무지역		공감_tot	 (f,g,h,i,j)
공감_com		공감_sen		공감_ins		이직여부		이직퇴직여부	 (k,l,m,n,o)
임상경력일	병동경력일	최종임상경력일		최종병동경력일_참고만		최종임상경력월 (p,q,r,s,t)
최종임상경력월_소수점반올림	최종임상경력년		년차_2 (u,v,w)
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


/*전체 */

proc phreg data=x2;
model u*o(0) = a trt_b trt_c trt_e1 trt_e2 trt_f trt_g trt_h1 trt_h2 trt_h3 k l m / selection = backward slstay=0.15 ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(u);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*1group : 4명 */
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


/*2group : 46명 */
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


/*3group : 175명 */
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



/*4group 404명 */
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
나이		성별		결혼		종교		학위		(a,b,c,d,e)
부서	 이직계획유무		건강상태	 근무지역		공감_tot	 (f,g,h,i,j)
공감_com		공감_sen		공감_ins		이직여부		이직퇴직여부	 (k,l,m,n,o)

--- 여기는 변화된 내용 ---
임상경력(일) 		최종임상경력(일)		최종임상경력(월)		최종임상경력(년)	 병동경력(일)	 (p,q,r,s,t)
년차_최종		연차별시작문항응답	연차별최종문항응답 (u,v,w)
-----------------------

u -> r
w -> u

------여기는 기존 ------
임상경력일	병동경력일	최종임상경력일		최종병동경력일_참고만		최종임상경력월 (p,q,r,s,t)
최종임상경력월_소수점반올림	최종임상경력년		년차_2 (u,v,w)
----------------------
 */

/*KM*/

data x;
infile 'C:\Users\user\Desktop\600.csv' dsd firstobs=2;
input a b c d e f g h i j k l m n o p q r s t u v w;
run;


proc lifetest method = km plots=(s) graphics ;
time r*o(0) ;
strata b;
symbol1 v=none color=black line=1;
symbol2 v=none color=black line=2;
run;

/*Cox*/

data x2;
set x;
if b=1 then trt_b = 1; else trt_b = 0 ;
if c=1 then trt_c = 1; else trt_c = 0 ;
if d=1 then trt_d = 1; else trt_d = 0 ;
if e=1 then trt_e= 1; else trt_e = 0 ;
if f=1 then trt_f = 1; else trt_f = 0 ;
if g=1 then trt_g = 1; else trt_g = 0 ;
if h=1 then trt_h = 1; else trt_h = 0 ;
if i=1 then trt_i = 1; else trt_i = 0 ;

if u=1 then trt_u1 = 1; else trt_u1 = 0 ;
if u=2 then trt_u2 = 1; else trt_u2 = 0 ;
run;




/*전체 */
proc phreg data=x2;
model r*o(0) = a   ; /* 0.0001 */
run;
proc phreg data=x2;
model r*o(0) = trt_b   ; /* 0.3642 */
run;
proc phreg data=x2;
model r*o(0) = trt_c   ; /* 0.0001 */
run;
proc phreg data=x2;
model r*o(0) = trt_d   ; /* 0.7235 */
run;
proc phreg data=x2;
model r*o(0) = trt_e   ; /* 0.0078 */
run;
proc phreg data=x2;
model r*o(0) = trt_f   ; /* 0.5022 */
run;
proc phreg data=x2;
model r*o(0) = trt_g   ; /* 0.0001 */
run;
proc phreg data=x2;
model r*o(0) = trt_h   ; /* 0.0236 */
run;
proc phreg data=x2;
model r*o(0) = trt_i   ; /* 0.4681 */
run;


/*1. 전체 - 공감변수 3개*/
proc phreg data=x2;
model r*o(0) = a trt_c trt_e trt_g trt_h k l m / rl  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*2. 전체 - 공감변수 tot*/
proc phreg data=x2;
model r*o(0) = a trt_c trt_e trt_g trt_h j  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;




/*************************                       아래는 참고                       *************************/
/*Cox PH or Cox Regression*/
proc phreg data=x2;
model r*o(0) = a trt_c trt_e trt_g trt_h k l m  ;
output out=coxph logsurv=h /method=ch;
run;
data coxph1;
set coxph;
h=-h;
cons=1;
run;
proc phreg data=coxph1;
model r*o(0)=cons;
output out=ph logsurv=ls /method=ch;
run;
data ph1;
set ph;
haz=-ls;
run;
proc sort data=ph1;
by h;
run;
title "Cox PH Model";
axis1 minor=none;
axis2  minor=none label=(a=90);
symbol1 i=join c=black line=2;
symbol2 i=join c=black line=1;
proc gplot data=ph1;
plot haz*h =1 h*h=2 /overlay haxis=axis1 vaxis=axis2 noframe;
label haz="Cumulative Hazard";
label h="Cox-Snell Residuals";
run;


/*Martingale Residual*/
proc phreg data=x2;
model r*o(0) = a trt_c trt_e trt_g trt_h k l m  ;
output out=coxph logsurv=h /method=ch;
run;

PROC PHREG DATA=x2;
model r*o(0) = a trt_c trt_e trt_g trt_h k l m;
OUTPUT OUT=outp XBETA=xb RESMART=mart RESDEV=dev RESSCH =ressch LMAX=lmax
RESSCO=ressco;
RUN; 

symbol1 i=none v=circle h=1  c=blue;
PROC GPLOT DATA=outp ;
TITLE1 "Martingale residuals plot";
PLOT mart*xb /CFRAME=white OVERLAY VAXIS=axis1 HAXIS=axis2 FRAME VREF=0
VMINOR=0 HMINOR=0 CAXIS = BLACK NAME='plot3';
AXIS1 LABEL=(A=90 R=0 F="<ttf> Arial ""Martingale Residual")WIDTH=2;
AXIS2 LABEL=("Linear Predictor")  WIDTH=2; RUN; QUIT; 

/*************************                       위는 참고                       *************************/







/*1group : 50명 */
data group1;
set x2;
if (u=1);
run;
proc phreg data=group1;
model r*o(0) = a   ; /* 0.6224 */
run;
proc phreg data=group1;
model r*o(0) = trt_b   ; /* 0.9923 */
run;
proc phreg data=group1;
model r*o(0) = trt_c   ; /* 0.3426 */
run;
proc phreg data=group1;
model r*o(0) = trt_d   ; /* 0.3476 */
run;
proc phreg data=group1;
model r*o(0) = trt_e   ; /* 0.7940 */
run;
proc phreg data=group1;
model r*o(0) = trt_f   ; /* 0.5475 */
run;
proc phreg data=group1;
model r*o(0) = trt_g   ; /* 0.1555 */
run;
proc phreg data=group1;
model r*o(0) = trt_h   ; /* 0.4130 */
run;
proc phreg data=group1;
model r*o(0) = trt_i   ; /* 0.5243 */
run;


/*1. 전체 - 공감변수 3개*/
proc phreg data=group1;
model r*o(0) = trt_g k l m  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
symbol1 i=none v=none;
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;



/*2. 전체 - 공감변수 tot*/
proc phreg data=group1;
model r*o(0) = trt_g j  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;








/*2group : 175명 */
data group2;
set x2;
if (u=2);
run;
proc phreg data=group2;
model r*o(0) = a   ; /* 0.3407 */
run;
proc phreg data=group2;
model r*o(0) = trt_b   ; /* 0.0078 */
run;
proc phreg data=group2;
model r*o(0) = trt_c   ; /* 0.4037 */
run;
proc phreg data=group2;
model r*o(0) = trt_d   ; /* 0.2212 */
run;
proc phreg data=group2;
model r*o(0) = trt_e   ; /* 0.4180 */
run;
proc phreg data=group2;
model r*o(0) = trt_f   ; /* 0.7675 */
run;
proc phreg data=group2;
model r*o(0) = trt_g   ; /* 0.0162 */
run;
proc phreg data=group2;
model r*o(0) = trt_h   ; /* 0.2345 */
run;
proc phreg data=group2;
model r*o(0) = trt_i   ; /* 0.3245 */
run;


/*1. 전체 - 공감변수 3개*/
proc phreg data=group2;
model r*o(0) = trt_b trt_g k l m  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*2. 전체 - 공감변수 tot*/
proc phreg data=group2;
model r*o(0) = trt_b trt_g j  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;









/*3group : 404명 */
data group3;
set x2;
if (u=3);
run;
proc phreg data=group3;
model r*o(0) = a   ; /* 0.8436 */
run;
proc phreg data=group3;
model r*o(0) = trt_b   ; /* 0.3992 */
run;
proc phreg data=group3;
model r*o(0) = trt_c   ; /* 0.2118 */
run;
proc phreg data=group3;
model r*o(0) = trt_d   ; /* 0.2531 */
run;
proc phreg data=group3;
model r*o(0) = trt_e   ; /* 0.4249 */
run;
proc phreg data=group3;
model r*o(0) = trt_f   ; /* 0.4590 */
run;
proc phreg data=group3;
model r*o(0) = trt_g   ; /* 0.0278 */
run;
proc phreg data=group3;
model r*o(0) = trt_h   ; /* 0.1458 */
run;
proc phreg data=group3;
model r*o(0) = trt_i   ; /* 0.7384 */
run;


/*1. 전체 - 공감변수 3개*/
proc phreg data=group3;
model r*o(0) = trt_g trt_h k l m  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;


/*2. 전체 - 공감변수 tot*/
proc phreg data=group3;
model r*o(0) = trt_g trt_h j  ;
baseline out=c survival =s loglogs = lls logsurv=ls ;
run;
data c;
set c;
log_t = log(r);
run;
/*비례성 검토*/
proc gplot  data=c;
plot lls*log_t ;
symbol1 interpol=join color = black line = 1;
symbol2 interpol=join color=black line=2;
run;
