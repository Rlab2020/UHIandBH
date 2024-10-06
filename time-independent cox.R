library(survival)
library(survminer)

##LST AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(md_time,md)~blst, dataset) ##continious exposure
summary(model1)
model2 <- coxph(Surv(md_time,md)~blstc, dataset) ##categorial exposure
summary(model2)

##LST AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(md_time,md)~blst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model3)
model4 <- coxph(Surv(md_time,md)~blstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model4)

##LST AND SUD UNIVARIABLE MODEL
model5 <- coxph(Surv(sud_time,sud)~blst, dataset) ##continious exposure
summary(model5)
model6 <- coxph(Surv(sud_time,sud)~blstc, dataset) ##categorial exposure
summary(model6)

##LST AND SUD MULTIVARIABLE MODEL
model7 <- coxph(Surv(sud_time,sud)~blst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model7)
model8 <- coxph(Surv(sud_time,sud)~blstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model8)

##LST AND MDD UNIVARIABLE MODEL
model9 <- coxph(Surv(mdd_time,mdd)~blst, dataset) ##continious exposure
summary(model9)
model10 <- coxph(Surv(mdd_time,mdd)~blstc, dataset) ##categorial exposure
summary(model10)

##LST AND MDD MULTIVARIABLE MODEL
model11 <- coxph(Surv(mdd_time,mdd)~blst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model11)
model12 <- coxph(Surv(mdd_time,mdd)~blstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model12)

##LST AND GAD UNIVARIABLE MODEL
model13 <- coxph(Surv(gad_time,gad)~blst, dataset) ##continious exposure
summary(model13)
model14 <- coxph(Surv(gad_time,gad)~blstc, dataset) ##categorial exposure
summary(model14)

##LST AND GAD MULTIVARIABLE MODEL
model15 <- coxph(Surv(gad_time,gad)~blst+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model15)
model16 <- coxph(Surv(gad_time,gad)~blstc+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model16)


##UHII AND MD UNIVARIABLE MODEL
model1 <- coxph(Surv(md_time,md)~buhii, dataset) ##continious exposure
summary(model1)
model2 <- coxph(Surv(md_time,md)~buhiic, dataset) ##categorial exposure
summary(model2)

##UHII AND MD MULTIVARIABLE MODEL
model3 <- coxph(Surv(md_time,md)~buhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model3)
model4 <- coxph(Surv(md_time,md)~buhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model4)

##UHII AND SUD UNIVARIABLE MODEL
model5 <- coxph(Surv(sud_time,sud)~buhii, dataset) ##continious exposure
summary(model5)
model6 <- coxph(Surv(sud_time,sud)~buhiic, dataset) ##categorial exposure
summary(model6)

##UHII AND SUD MULTIVARIABLE MODEL
model7 <- coxph(Surv(sud_time,sud)~buhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model7)
model8 <- coxph(Surv(sud_time,sud)~buhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model8)

##UHII AND MDD UNIVARIABLE MODEL
model9 <- coxph(Surv(mdd_time,mdd)~buhii, dataset) ##continious exposure
summary(model9)
model10 <- coxph(Surv(mdd_time,mdd)~buhiic, dataset) ##categorial exposure
summary(model10)

##UHII AND MDD MULTIVARIABLE MODEL
model11 <- coxph(Surv(mdd_time,mdd)~buhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model11)
model12 <- coxph(Surv(mdd_time,mdd)~buhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model12)

##UHII AND GAD UNIVARIABLE MODEL
model13 <- coxph(Surv(gad_time,gad)~buhii, dataset) ##continious exposure
summary(model13)
model14 <- coxph(Surv(gad_time,gad)~buhiic, dataset) ##categorial exposure
summary(model14)

##UHII AND GAD MULTIVARIABLE MODEL
model15 <- coxph(Surv(gad_time,gad)~buhii+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)  ##continious exposure
summary(model15)
model16 <- coxph(Surv(gad_time,gad)~buhiic+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset) ##categorial exposure
summary(model16)
