

###LST AND WM
model1 <- glm(wm_md_tfma~blsts+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

###UHI AND WM
model2 <- glm(wm_md_tfma~buhiis+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

##The associations of UHI and other WM microstructure were also obtained by running the above code, respectively.
