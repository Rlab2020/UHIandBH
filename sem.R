##blst = baseline summer normalized LST as continious exposure
##buhii = baseline summer normalized UHII as continious exposure
##md = mental disorder
##fhapp = General happy
##fhh = Happiness with own health
#flife = Belief that own life is meaningful
##finad = Recent feelings of inadequacy
##ftired = Recent feelings of tiredness or low energy
##fdepr = Recent feelings of depression
##fconc = Recent trouble concentrating on things
##fappe = Recent poor appetite or overeating
##finte = Recent lack of interest or pleasure in doing things
##fwor = Recent inability to stop or control worrying
##frelax = Recent trouble relaxing
##fann = Recent easy annoyance or irritability
##fworr = Recent worrying too much about different things
##ffore = Recent feelings of foreboding
##fnerv = Recent feelings or nervousness or anxiety

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


##the SEM was fitted for substance use disorder, depressive disorder, and anxiety disorder, respectively.
