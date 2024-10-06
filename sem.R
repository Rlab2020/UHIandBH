library(lavaan)

####md model 
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              phq9 =~  finad + ftired + fdepr + fconc + fappe + finte
              gad7 =~  fwor + frelax + fann + fworr + ffore + fnerv
              ### regressions moodel
              md ~   buhiis + happy + phq9 + gad7
              happy ~ phq9 + gad7 + buhiis
              gad7 ~ buhiis 
              phq9 ~ buhiis '
fit <- sem(mymodel, data=newdata1, ordered = c("md"), std.lv=T)
summary(fit, fit.measures=TRUE)

####sud model
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              phq9 =~  finad + ftired + fdepr + fconc + fappe + finte
              gad7 =~  fwor + frelax + fann + fworr + ffore + fnerv
              ### regressions moodel
              sud ~   buhiis + happy + phq9 + gad7
              happy ~ phq9 + gad7 + buhiis
              gad7 ~ buhiis 
              phq9 ~ buhiis '
fit <- sem(mymodel, data=newdata1, ordered = c("sud"), std.lv=T)
summary(fit, fit.measures=TRUE)

####mdd model
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              phq9 =~  finad + ftired + fdepr + fconc + fappe + finte
              ### regressions moodel
              mdd ~   buhiis + happy + phq9 
              happy ~ phq9 + buhiis
              phq9 ~ buhiis '
fit <- sem(mymodel, data=newdata1, ordered = c("mdd"), std.lv=T)
summary(fit, fit.measures=TRUE)

####gad model
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              gad7 =~  fwor + frelax + fann + fworr + ffore + fnerv
              ### regressions moodel
              gad ~ buhiis + happy +  gad7
              happy ~ buhiis + gad7
              gad7 ~ buhiis '
fit <- sem(mymodel, data=newdata1, ordered = c("gad"), std.lv=T)
summary(fit, fit.measures=TRUE)
