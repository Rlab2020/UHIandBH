##blst = baseline summer normalized LST as continious exposure
##buhii = baseline summer normalized UHII as continious exposure
##wm_md_tfma = tract forceps major
##


###LST AND WM
model1 <- glm(wm_md_tfma~blst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

###UHI AND WM
model2 <- glm(wm_md_tfma~buhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

##The associations of UHI and other WM microstructure were also obtained by running the above code, respectively.
