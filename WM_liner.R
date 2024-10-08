##blst = baseline summer normalized LST as continious exposure
##buhii = baseline summer normalized UHII as continious exposure
##wm_md_tfma = tract forceps major for md value
##the secondary outcome included 15 WM microstructure for MD and ISOVF values as follows: tract forceps major; tract forceps minor; tract middle cerebellar peduncle;
##tract acoustic radiation; tract anterior thalamic radiation; tract cingulate gyrus part of cingulum; tract corticospinal tract; 
##tract inferior fronto-occipital fasciculus; tract inferior longitudinal fasciculus; tract medial lemniscus; tract parahippocampal part of cingulum;
##tract posterior thalamic radiation; tract superior longitudinal fasciculus; tract superior thalamic radiation; tract uncinate fasciculus 


###LST AND WM
model1 <- glm(wm_md_tfma~blst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

###UHI AND WM
model2 <- glm(wm_md_tfma~buhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp, family='gaussian', dataset)

##The associations of UHI and other WM microstructure were also obtained by running the above code, respectively.
