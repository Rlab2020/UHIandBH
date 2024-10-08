


library(lavaan)

####md model for LST
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              phq9 =~  finad + ftired + fdepr + fconc + fappe + finte
              gad7 =~  fwor + frelax + fann + fworr + ffore + fnerv
              ### regressions moodel
              md ~   blst + happy + phq9 + gad7
              happy ~ phq9 + gad7 + blst
              gad7 ~ blst 
              phq9 ~ blst '
fit <- sem(mymodel, data=newdata1, ordered = c("md"), std.lv=T)
summary(fit, fit.measures=TRUE)



####md model for UHII
mymodel <- '  ### mesurement model
              happy =~ fhapp + fhh + flife
              phq9 =~  finad + ftired + fdepr + fconc + fappe + finte
              gad7 =~  fwor + frelax + fann + fworr + ffore + fnerv
              ### regressions moodel
              md ~   buhii + happy + phq9 + gad7
              happy ~ phq9 + gad7 + buhii
              gad7 ~ buhii 
              phq9 ~ buhii '
fit <- sem(mymodel, data=newdata1, ordered = c("md"), std.lv=T)
summary(fit, fit.measures=TRUE)


