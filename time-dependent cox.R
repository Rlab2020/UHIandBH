##start = start from each year
##stop = end of each year
##event = mental disorder (1 or 0)
##lst = summer normalized LST as continious exposure
##lstc = summer normalized LST as categorial exposure
##uhii = summer normalized UHII as continious exposure
##uhiic = summer normalized UHII as categorial exposure


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
model7 <- coxph(Surv(start, stop, event)~uhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model8 <- coxph(Surv(start, stop, event)~uhiic+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md) 


The associations of UHI and substance use disorder, depressive disorder, and anxiety disorder were also obtained by running the above code, respectively.
