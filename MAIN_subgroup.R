library(survival)
library(survminer)

##TIME-INDEPENDENT
##LST AND MD BY AGE
##continious exposure
model1<-coxph(Surv(md_time,md)~blst*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(md_time,md)~blst*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model1)
summary(model2)

##categorial expsoure
model1<-coxph(Surv(md_time,md)~blstc*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(md_time,md)~blstc*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model1)
summary(model2)


##LST AND MD BY SEX
##continious exposure
model3<-coxph(Surv(md_time,md)~blst*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~blst*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(md_time,md)~blstc*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~blstc*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)


##LST AND MD BY WHR
##continious exposure
model3<-coxph(Surv(md_time,md)~blst*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~blst*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(md_time,md)~blstc*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~blstc*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)


##LST AND MD BY HTNP
##continious exposure
model5<-coxph(Surv(md_time,md)~blst*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~blst*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(md_time,md)~blstc*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~blstc*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)


##LST AND MD BY DMP
##continious exposure
model7<-coxph(Surv(md_time,md)~blst*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
mode8<-coxph(Surv(md_time,md)~blst*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model7)
summary(model8)

##categorial expsoure
model7<-coxph(Surv(md_time,md)~blstc*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
mode8<-coxph(Surv(md_time,md)~blstc*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model7)
summary(model8)


##LST AND MD BY CADP
##continious exposure
model5<-coxph(Surv(md_time,md)~blst*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~blst*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(md_time,md)~blstc*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~blstc*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)



##UHII AND MD BY AGE
##continious exposure
model1<-coxph(Surv(md_time,md)~buhii*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(md_time,md)~buhii*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model1)
summary(model2)

##categorial expsoure
model1<-coxph(Surv(md_time,md)~buhiic*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(md_time,md)~buhiic*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model1)
summary(model2)


##UHII AND MD BY SEX
##continious exposure
model3<-coxph(Surv(md_time,md)~buhii*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~buhii*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(md_time,md)~buhiic*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~buhiic*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)


##UHII AND MD BY WHR
##continious exposure
model3<-coxph(Surv(md_time,md)~buhii*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~buhii*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(md_time,md)~buhiic*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~buhiic*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model3)
summary(model4)


##UHII AND MD BY HTNP
##continious exposure
model5<-coxph(Surv(md_time,md)~buhii*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~buhii*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(md_time,md)~buhiic*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~buhiic*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)


##UHII AND MD BY DMP
##continious exposure
model7<-coxph(Surv(md_time,md)~buhii*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model8<-coxph(Surv(md_time,md)~buhii*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model7)
summary(model8)

##categorial expsoure
model7<-coxph(Surv(md_time,md)~buhiic*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model8<-coxph(Surv(md_time,md)~buhiic*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model7)
summary(model8)


##UHII AND MD BY CADP
##continious exposure
model5<-coxph(Surv(md_time,md)~buhii*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~buhii*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(md_time,md)~buhiic*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~buhiic*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
summary(model5)
summary(model6)




##TIME-DEPENDENT
##LST AND MD BY AGE
##continious exposure
model1<-coxph(Surv(start, stop, event)~lst*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model2<-coxph(Surv(start, stop, event)~lst*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model1)
summary(model2)

##categorial expsoure
model1<-coxph(Surv(start, stop, event)~lstc*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model2<-coxph(Surv(start, stop, event)~lstc*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model1)
summary(model2)


##LST AND MD BY SEX
##continious exposure
model3<-coxph(Surv(start, stop, event)~lst*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model4<-coxph(Surv(start, stop, event)~lst*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(start, stop, event)~lstc*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model4<-coxph(Surv(start, stop, event)~lstc*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model3)
summary(model4)


##LST AND MD BY WHR
##continious exposure
model5<-coxph(Surv(start, stop, event)~lst*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model6<-coxph(Surv(start, stop, event)~lst*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(start, stop, event)~lstc*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model6<-coxph(Surv(start, stop, event)~lstc*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model5)
summary(model6)


##LST AND MD BY HTNP
##continious exposure
model7<-coxph(Surv(start, stop, event)~lst*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model8<-coxph(Surv(start, stop, event)~lst*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model7)
summary(model8)

##categorial expsoure
model7<-coxph(Surv(start, stop, event)~lstc*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model8<-coxph(Surv(start, stop, event)~lstc*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model7)
summary(model8)


##LST AND MD BY DMP
##continious exposure
model9<-coxph(Surv(start, stop, event)~lst*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model10<-coxph(Surv(start, stop, event)~lst*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model9)
summary(model10)

##categorial expsoure
model9<-coxph(Surv(start, stop, event)~lstc*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model10<-coxph(Surv(start, stop, event)~lstc*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model9)
summary(model10)


##LST AND MD BY CADP
##continious exposure
model11<-coxph(Surv(start, stop, event)~lst*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model12<-coxph(Surv(start, stop, event)~lst*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model11)
summary(model12)

##categorial expsoure
model11<-coxph(Surv(start, stop, event)~lstc*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model12<-coxph(Surv(start, stop, event)~lstc*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model11)
summary(model12)



##UHII AND MD BY AGE
##continious exposure
model1<-coxph(Surv(start, stop, event)~uhii*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model2<-coxph(Surv(start, stop, event)~uhii*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model1)
summary(model2)

##categorial expsoure
model1<-coxph(Surv(start, stop, event)~uhiic*relevel(age0, ref='levle1')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model2<-coxph(Surv(start, stop, event)~uhiic*relevel(age0, ref='level2')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model1)
summary(model2)


##UHII AND MD BY SEX
##continious exposure
model3<-coxph(Surv(start, stop, event)~uhii*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model4<-coxph(Surv(start, stop, event)~uhii*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model3)
summary(model4)

##categorial expsoure
model3<-coxph(Surv(start, stop, event)~uhiic*relevel(sex, ref='male')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model4<-coxph(Surv(start, stop, event)~uhiic*relevel(sex, ref='female')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model3)
summary(model4)


##UHII AND MD BY WHR
##continious exposure
model5<-coxph(Surv(start, stop, event)~uhii*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model6<-coxph(Surv(start, stop, event)~uhii*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model5)
summary(model6)

##categorial expsoure
model5<-coxph(Surv(start, stop, event)~uhiic*relevel(whr0, ref='poor')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model6<-coxph(Surv(start, stop, event)~uhiic*relevel(whr0, ref='ideal')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model5)
summary(model6)


##UHII AND MD BY HTNP
##continious exposure
model7<-coxph(Surv(start, stop, event)~uhii*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model8<-coxph(Surv(start, stop, event)~uhii*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model7)
summary(model8)

##categorial expsoure
model7<-coxph(Surv(start, stop, event)~uhiic*relevel(htnp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model8<-coxph(Surv(start, stop, event)~uhiic*relevel(htnp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model7)
summary(model8)


##UHII AND MD BY DMP
##continious exposure
model9<-coxph(Surv(start, stop, event)~uhii*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model10<-coxph(Surv(start, stop, event)~uhii*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model9)
summary(model10)

##categorial expsoure
model9<-coxph(Surv(start, stop, event)~uhiic*relevel(dmp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model10<-coxph(Surv(start, stop, event)~uhiic*relevel(dmp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model9)
summary(model10)


##UHII AND MD BY CADP
##continious exposure
model11<-coxph(Surv(start, stop, event)~uhii*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model12<-coxph(Surv(start, stop, event)~uhii*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model11)
summary(model12)

##categorial expsoure
model11<-coxph(Surv(start, stop, event)~uhiic*relevel(cadp, ref='yes')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model12<-coxph(Surv(start, stop, event)~uhiic*relevel(cadp, ref='no')+age0+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
summary(model11)
summary(model12)
 
