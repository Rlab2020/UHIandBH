##md_time = follow-up period
##md = mental disorder (1 or 0)
##blst = baseline summer normalized LST as continious exposure
##blstc = baseline summer normalized LST as categorial exposure
##buhii = baseline summer normalized UHII as continious exposure
##buhiic = baseline summer normalized UHII as categorial exposure
 

library(survival)
library(survminer)

##LST AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(md_time,md)~blst, dataset)
model2 <- coxph(Surv(md_time,md)~blstc, dataset) 


##LST AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(md_time,md)~blst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) 
model4 <- coxph(Surv(md_time,md)~blstc+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD UNIVARIABLE MODEL
model5 <- coxph(Surv(md_time,md)~buhii, dataset)
model6 <- coxph(Surv(md_time,md)~buhiic, dataset) 


##UHII AND MD MULTIVARIABLE MODEL
model7 <- coxph(Surv(md_time,md)~buhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  
model8 <- coxph(Surv(md_time,md)~buhiic+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


The associations of UHI and substance use disorder, depressive disorder, and anxiety disorder were also obtained by running the above code, respectively.

