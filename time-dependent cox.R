##md_time = follow-up period
##md = mental disorder (1 or 0)
##blst = baseline summer normalized LST as continious exposure
##blstc = baseline summer normalized LST as categorial exposure
##buhii = baseline summer normalized UHII as continious exposure
##buhii = baseline summer normalized UHII as categorial exposure


library(survival)
library(survminer)

##LST AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(start, stop, event)~lst, data_md)
model2 <- coxph(Surv(start, stop, event)~lstc, data_md) 

##LST AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(start, stop, event)~lst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md) 
model4 <- coxph(Surv(start, stop, event)~lstc+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md) 


##UHII AND MD UNIVARIABLE MODEL
model5 <- coxph(Surv(start, stop, event)~uhii, data_md)
model6 <- coxph(Surv(start, stop, event)~uhiic, data_md)

##UHII AND MD MULTIVARIABLE MODEL
model7 <- coxph(Surv(start, stop, event)~uhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud)
model8 <- coxph(Surv(start, stop, event)~uhiic+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud) 

