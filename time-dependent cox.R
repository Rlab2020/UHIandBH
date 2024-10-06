library(survival)
library(survminer)

##LST AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(start, stop, event)~lst, data_md) ##continious exposure
summary(model1)
model2 <- coxph(Surv(start, stop, event)~lstc, data_md) ##categorial exposure
summary(model2)

##LST AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(start, stop, event)~lst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)  ##continious exposure
summary(model3)
model4 <- coxph(Surv(start, stop, event)~lstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md) ##categorial exposure
summary(model4)

##LST AND SUD UNIVARIABLE MODEL
model5 <- coxph(Surv(start, stop, event)~lst, data_sud) ##continious exposure
summary(model5)
model6 <- coxph(Surv(start, stop, event)~lstc, data_sud) ##categorial exposure
summary(model6)

##LST AND SUD MULTIVARIABLE MODEL
model7 <- coxph(Surv(start, stop, event)~lst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud)  ##continious exposure
summary(model7)
model8 <- coxph(Surv(start, stop, event)~lstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud) ##categorial exposure
summary(model8)

##LST AND MDD UNIVARIABLE MODEL
model9 <- coxph(Surv(start, stop, event)~lst, data_mdd) ##continious exposure
summary(model9)
model10 <- coxph(Surv(start, stop, event)~lstc, data_mdd) ##categorial exposure
summary(model10)

##LST AND MDD MULTIVARIABLE MODEL
model11 <- coxph(Surv(start, stop, event)~lst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_mdd)  ##continious exposure
summary(model11)
model12 <- coxph(Surv(start, stop, event)~lstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_mdd) ##categorial exposure
summary(model12)

##LST AND GAD UNIVARIABLE MODEL
model13 <- coxph(Surv(start, stop, event)~lst, data_gad) ##continious exposure
summary(model13)
model14 <- coxph(Surv(start, stop, event)~lstc, data_gad) ##categorial exposure
summary(model14)

##LST AND GAD MULTIVARIABLE MODEL
model15 <- coxph(Surv(start, stop, event)~lst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_gad)  ##continious exposure
summary(model15)
model16 <- coxph(Surv(start, stop, event)~lstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_gad) ##categorial exposure
summary(model16)


##UHII AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(start, stop, event)~uhii, data_md) ##continious exposure
summary(model1)
model2 <- coxph(Surv(start, stop, event)~uhiic, data_md) ##categorial exposure
summary(model2)

##UHII AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(start, stop, event)~uhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud)  ##continious exposure
summary(model3)
model4 <- coxph(Surv(start, stop, event)~uhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud) ##categorial exposure
summary(model4)

##UHII AND SUD UNIVARIABLE MODEL
model5 <- coxph(Surv(start, stop, event)~uhii, data_sud) ##continious exposure
summary(model5)
model6 <- coxph(Surv(start, stop, event)~uhiic, data_sud) ##categorial exposure
summary(model6)

##UHII AND SUD MULTIVARIABLE MODEL
model7 <- coxph(Surv(start, stop, event)~uhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud)  ##continious exposure
summary(model7)
model8 <- coxph(Surv(start, stop, event)~uhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_sud) ##categorial exposure
summary(model8)

##UHII AND MDD UNIVARIABLE MODEL
model9 <- coxph(Surv(start, stop, event)~uhii, data_mdd) ##continious exposure
summary(model9)
model10 <- coxph(Surv(start, stop, event)~uhiic, data_mdd) ##categorial exposure
summary(model10)

##UHII AND MDD MULTIVARIABLE MODEL
model11 <- coxph(Surv(start, stop, event)~uhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_mdd)  ##continious exposure
summary(model11)
model12 <- coxph(Surv(start, stop, event)~uhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_mdd) ##categorial exposure
summary(model12)

##UHII AND GAD UNIVARIABLE MODEL
model13 <- coxph(Surv(start, stop, event)~uhii, data_gad) ##continious exposure
summary(model13)
model14 <- coxph(Surv(start, stop, event)~uhiic, data_gad) ##categorial exposure
summary(model14)

##UHII AND GAD MULTIVARIABLE MODEL
model15 <- coxph(Surv(start, stop, event)~uhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_gad)  ##continious exposure
summary(model15)
model16 <- coxph(Surv(start, stop, event)~uhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_gad) ##categorial exposure
summary(model16)
