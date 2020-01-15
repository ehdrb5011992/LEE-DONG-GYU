data ordered_multi ;
infile 'C:\Users\82104\Desktop\ordered_multi.csv' dsd firstobs=2 ;
INPUT No impu_trt_pe$	impu_trt_fs impu_trt_lvdd impu_trt_wmi impu_trt_group$ impu_trt_aaha$ impu_trt_aa1$ ;
run;
proc logistic order=data data=ordered_multi;
class impu_trt_pe impu_trt_group impu_trt_aa1;
model impu_trt_aaha = impu_trt_pe impu_trt_fs impu_trt_lvdd impu_trt_wmi impu_trt_group  impu_trt_aa1 /selection = stepwise slentry = 0.2 slstay =0.1  scale=none aggregate;
output out=prob pred=p;
run; quit;

proc logistic  data=ordered_multi;
model impu_trt_aaha = impu_trt_lvdd /  scale=none aggregate;
output out=pp pred=p;
run; quit;

proc print data=pp (obs=10);
run; quit;

