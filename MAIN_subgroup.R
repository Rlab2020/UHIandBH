library(survival)
library(survminer)

##TIME-INDEPENDENT
##LST AND MD BY AGE
model1<-coxph(Surv(md_time,md)~blst*relevel(",j,", ref='",k,"')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(",y,",",x,")~",z,"*relevel(",j,", ref='",l,"')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
 
