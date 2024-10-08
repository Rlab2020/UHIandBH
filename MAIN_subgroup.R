##md_time = follow-up period
##md = mental disorder (1 or 0)
##blst = baseline summer normalized LST as continious exposure
##blstc = baseline summer normalized LST as categorial exposure
##buhii = baseline summer normalized UHII as continious exposure
##buhiic = baseline summer normalized UHII as categorial exposure
##start = start from each year
##stop = end of each year
##event = mental disorder (1 or 0)
##lst = summer normalized LST in the respective year as continious exposure
##lstc = summer normalized LST in the respective year as categorial exposure
##uhii = summer normalized UHII in the respective year as continious exposure
##uhiic = summer normalized UHII in the respective year as categorial exposure

library(survival)
library(survminer)

##TIME-INDEPENDENT
##LST AND MD BY AGE
##continious exposure
model1<-coxph(Surv(md_time,md)~blst*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model2<-coxph(Surv(md_time,md)~blst*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model3<-coxph(Surv(md_time,md)~blstc*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model4<-coxph(Surv(md_time,md)~blstc*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##LST AND MD BY SEX
##continious exposure
model5<-coxph(Surv(md_time,md)~blst*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model6<-coxph(Surv(md_time,md)~blst*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model7<-coxph(Surv(md_time,md)~blstc*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model8<-coxph(Surv(md_time,md)~blstc*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##LST AND MD BY WHR
##continious exposure
model9<-coxph(Surv(md_time,md)~blst*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model10<-coxph(Surv(md_time,md)~blst*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model11<-coxph(Surv(md_time,md)~blstc*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model12<-coxph(Surv(md_time,md)~blstc*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##LST AND MD BY HTNP
##continious exposure
model13<-coxph(Surv(md_time,md)~blst*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model14<-coxph(Surv(md_time,md)~blst*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model15<-coxph(Surv(md_time,md)~blstc*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model16<-coxph(Surv(md_time,md)~blstc*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##LST AND MD BY DMP
##continious exposure
model17<-coxph(Surv(md_time,md)~blst*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model18<-coxph(Surv(md_time,md)~blst*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model9<-coxph(Surv(md_time,md)~blstc*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
mode20<-coxph(Surv(md_time,md)~blstc*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##LST AND MD BY CADP
##continious exposure
model21<-coxph(Surv(md_time,md)~blst*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model22<-coxph(Surv(md_time,md)~blst*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model23<-coxph(Surv(md_time,md)~blstc*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model24<-coxph(Surv(md_time,md)~blstc*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)




##UHII AND MD BY AGE
##continious exposure
model25<-coxph(Surv(md_time,md)~buhii*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model26<-coxph(Surv(md_time,md)~buhii*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model27<-coxph(Surv(md_time,md)~buhiic*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model28<-coxph(Surv(md_time,md)~buhiic*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD BY SEX
##continious exposure
model29<-coxph(Surv(md_time,md)~buhii*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model30<-coxph(Surv(md_time,md)~buhii*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model31<-coxph(Surv(md_time,md)~buhiic*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model32<-coxph(Surv(md_time,md)~buhiic*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD BY WHR
##continious exposure
model33<-coxph(Surv(md_time,md)~buhii*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model34<-coxph(Surv(md_time,md)~buhii*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model35<-coxph(Surv(md_time,md)~buhiic*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model36<-coxph(Surv(md_time,md)~buhiic*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD BY HTNP
##continious exposure
model37<-coxph(Surv(md_time,md)~buhii*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model38<-coxph(Surv(md_time,md)~buhii*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr0+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model39<-coxph(Surv(md_time,md)~buhiic*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model40<-coxph(Surv(md_time,md)~buhiic*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD BY DMP
##continious exposure
model41<-coxph(Surv(md_time,md)~buhii*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model42<-coxph(Surv(md_time,md)~buhii*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model43<-coxph(Surv(md_time,md)~buhiic*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model44<-coxph(Surv(md_time,md)~buhiic*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)



##UHII AND MD BY CADP
##continious exposure
model45<-coxph(Surv(md_time,md)~buhii*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model46<-coxph(Surv(md_time,md)~buhii*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)


##categorial expsoure
model47<-coxph(Surv(md_time,md)~buhiic*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)
model48<-coxph(Surv(md_time,md)~buhiic*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,dataset)





##TIME-DEPENDENT
##LST AND MD BY AGE
##continious exposure
model49<-coxph(Surv(start, stop, event)~lst*relevel(age, ref='levle1')+age0+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model50<-coxph(Surv(start, stop, event)~lst*relevel(age, ref='level2')+age0+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model51<-coxph(Surv(start, stop, event)~lstc*relevel(age, ref='levle1')+age0+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model52<-coxph(Surv(start, stop, event)~lstc*relevel(age, ref='level2')+age0+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##LST AND MD BY SEX
##continious exposure
model53<-coxph(Surv(start, stop, event)~lst*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model54<-coxph(Surv(start, stop, event)~lst*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model55<-coxph(Surv(start, stop, event)~lstc*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model56<-coxph(Surv(start, stop, event)~lstc*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##LST AND MD BY WHR
##continious exposure
model57<-coxph(Surv(start, stop, event)~lst*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model58<-coxph(Surv(start, stop, event)~lst*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model59<-coxph(Surv(start, stop, event)~lstc*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model60<-coxph(Surv(start, stop, event)~lstc*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##LST AND MD BY HTNP
##continious exposure
model61<-coxph(Surv(start, stop, event)~lst*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model62<-coxph(Surv(start, stop, event)~lst*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model63<-coxph(Surv(start, stop, event)~lstc*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model64<-coxph(Surv(start, stop, event)~lstc*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##LST AND MD BY DMP
##continious exposure
model65<-coxph(Surv(start, stop, event)~lst*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model66<-coxph(Surv(start, stop, event)~lst*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model67<-coxph(Surv(start, stop, event)~lstc*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model68<-coxph(Surv(start, stop, event)~lstc*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##LST AND MD BY CADP
##continious exposure
model69<-coxph(Surv(start, stop, event)~lst*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model70<-coxph(Surv(start, stop, event)~lst*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model71<-coxph(Surv(start, stop, event)~lstc*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model72<-coxph(Surv(start, stop, event)~lstc*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)




##UHII AND MD BY AGE
##continious exposure
model73<-coxph(Surv(start, stop, event)~uhii*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model74<-coxph(Surv(start, stop, event)~uhii*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model75<-coxph(Surv(start, stop, event)~uhiic*relevel(age0, ref='levle1')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model76<-coxph(Surv(start, stop, event)~uhiic*relevel(age0, ref='level2')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##UHII AND MD BY SEX
##continious exposure
model77<-coxph(Surv(start, stop, event)~uhii*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model78<-coxph(Surv(start, stop, event)~uhii*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model79<-coxph(Surv(start, stop, event)~uhiic*relevel(sex, ref='male')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model80<-coxph(Surv(start, stop, event)~uhiic*relevel(sex, ref='female')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##UHII AND MD BY WHR
##continious exposure
model81<-coxph(Surv(start, stop, event)~uhii*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model82<-coxph(Surv(start, stop, event)~uhii*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model83<-coxph(Surv(start, stop, event)~uhiic*relevel(whr0, ref='poor')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model84<-coxph(Surv(start, stop, event)~uhiic*relevel(whr0, ref='ideal')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##UHII AND MD BY HTNP
##continious exposure
model85<-coxph(Surv(start, stop, event)~uhii*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model86<-coxph(Surv(start, stop, event)~uhii*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model87<-coxph(Surv(start, stop, event)~uhiic*relevel(htnp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model88<-coxph(Surv(start, stop, event)~uhiic*relevel(htnp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##UHII AND MD BY DMP
##continious exposure
model89<-coxph(Surv(start, stop, event)~uhii*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model90<-coxph(Surv(start, stop, event)~uhii*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model91<-coxph(Surv(start, stop, event)~uhiic*relevel(dmp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model92<-coxph(Surv(start, stop, event)~uhiic*relevel(dmp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)



##UHII AND MD BY CADP
##continious exposure
model93<-coxph(Surv(start, stop, event)~uhii*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model94<-coxph(Surv(start, stop, event)~uhii*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)


##categorial expsoure
model95<-coxph(Surv(start, stop, event)~uhiic*relevel(cadp, ref='yes')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)
model96<-coxph(Surv(start, stop, event)~uhiic*relevel(cadp, ref='no')+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,data_md)

##The subgroup analyses for substance use disorder, depressive disorder, and anxiety disorder were also run through the above code, respectively. 
