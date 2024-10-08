

###LST AND SYMPTOMS 
model1 <- glm(fhapp~blst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,family='binomial', dataset)

###UHII AND SYMPTOMS
model2 <- glm(fhapp~buhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,family='binomial', dataset)

