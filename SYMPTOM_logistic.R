##blst = baseline summer normalized LST as continious exposure
##buhii = baseline summer normalized UHII as continious exposure
##fhapp = General happy (1/0)
##the secondary outcome included 19 psychiatric symptoms as follows: General happy; Happiness with own health; Belief that own life is meaningful;
##Recent thoughts of suicide or self-harm; Trouble falling asleep, or sleeping too much; Recent changes in speed of moving or speaking; Recent feelings of inadequacy;
##Recent feelings of tiredness or low energy; Recent feelings of depression; Recent trouble concentrating on things; Recent poor appetite or overeating;
##Recent lack of interest or pleasure in doing things; Recent inability to stop or control worrying; Recent restlessness; Recent trouble relaxing; 
##Recent easy annoyance or irritability; Recent worrying too much about different things; Recent feelings of foreboding; Recent feelings or nervousness or anxiety


###LST AND SYMPTOMS 
model1 <- glm(fhapp~blst+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,family='binomial', dataset)

###UHII AND SYMPTOMS
model2 <- glm(fhapp~buhii+age+sex+edu+income+ethnic+whr+nosmoking+drinkingM+sleeph+regular+dieth+htnp+dmp+cadp,family='binomial', dataset)

##The associations of UHI and other psychiatric symptoms were also obtained by running the above code, respectively.
