library(glmmTMB)
library(MuMIn)
library(effects)
library(car)
library(performance)
library(PerformanceAnalytics)
library(sjPlot)
library(beepr)
library(ggpubr)
library(tidyverse)


####################################
#### Modelling daily detection rates
####################################

# # check collinearity
# 
# modelCheck <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", 
#                       
#                       formula = BTP~
#                         
#                         day+
#                         
#                         Site+
#                         
#                         meanTempAtNight+
#                         
#                         meanTempAt18+
#                         
#                         nightLength+
#                         
#                         Rainfall+
#                         
#                         meanMoonIllumination+
#                         
#                         maxMoonIllumination+
#                         
#                         meanCloudCover+
#                         
#                         meanMoonFraction
#                       
# )
# 
# check_collinearity(modelCheck)

#Model selection - dredging wih MuMIn

# global model with all the predictors

  globalModel <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", na.action = "na.fail",
                       
                       formula = BTP~
                         
                         day*Site+
                        
                         meanTempAtNight*meanCloudCover+
                         
                         meanTempAtNight*nightLength+
                         
                         meanTempAtNight*Rainfall+
                         
                   #      nightLength*Rainfall+
                        
                         meanMoonIllumination*meanCloudCover
                         
                         # meanMoonFraction*meanCloudCover+
                         # 
                         # maxMoonIllumination*meanCloudCover
                         
                      )
                       

  
# Model checks
                         
summary(globalModel)                        

check_collinearity(globalModel)

plot(allEffects(globalModel))


#Selecting candidate models 


dredgeSubsetDD <- dredge(globalModel, subset= ( ("cond(meanCloudCover)" && "cond(Rainfall)" && "cond(nightLength)") ) )

#Model averaging 


dd_average <-  model.avg(dredgeSubsetDD, subset = delta<=2, fit = TRUE) 


summary(dd_average)

C(dd_average, transform = NULL, file = "dd_best_model.doc")



# Details of candidate models

dd_cm1 <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", na.action = "na.fail",
                 
                 formula = BTP~
                   
                   day*Site+
                   
                   meanTempAtNight*meanCloudCover+
                   
                   meanTempAtNight*nightLength+
                   
                   meanTempAtNight*Rainfall
                 
          
)

summary(dd_cm1)

r2(dd_cm1)

dd_cm2 <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", na.action = "na.fail",
                 
                 formula = BTP~
                   
                   day*Site+
                   
                   meanTempAtNight*meanCloudCover+
                   
                   meanTempAtNight*nightLength+
                   
                   meanTempAtNight*Rainfall+
                   
                   meanMoonIllumination
                 
                 
)

summary(dd_cm2)

r2(dd_cm2)

dd_cm3 <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", na.action = "na.fail",
                 
                 formula = BTP~
                   
                   day*Site+
                   
                   meanTempAtNight*meanCloudCover+
                   
                   meanTempAtNight*nightLength+
                   
                   meanTempAtNight*Rainfall+
                   
                   meanMoonIllumination*meanCloudCover
                 
                 
)

summary(dd_cm3)

r2(dd_cm3)

dredgedModel <-   globalModel <- glmmTMB(data=dailyDetectionsStand, family ="nbinom1", 
                                         
                                         formula = BTP~
                                           
                                           day*Site+
                                           
                                           meanTempAtNight*meanCloudCover+
                                           
                                           meanTempAtNight*nightLength+
                                           
                                           meanTempAtNight*Rainfall+
                                           
                                           nightLength*Rainfall+
                                           
                                           meanMoonIllumination*meanCloudCover
                                         
)



############################
##### Time of activity model
############################


taModel_global <- glmmTMB(data=BTP_model_st1, family=beta_family(link = "logit"), na.action = "na.fail",
                          
                          formula=nightFraction~
                            
                            meanTempAtNight*nightLength+
                             
                            meanTempAtNight*meanCloudCover+
                             
                          #  meanTempAtNight*Rainfall+
                             
                            lunarNoonNightFraction*meanMoonIllumination+
                            
                            (1|Camera)
                            
                    )


summary(taModel_global)

plot(allEffects(taModel_global))

tab_model(taModel_global, transform = NULL)


# dredge candidate models and notify when done


ta_dredge <- dredge(taModel_global) 
beep(sound=2)


# model average


ta_average <-  model.avg(ta_dredge, subset = delta<=2, fit = TRUE) 

summary(ta_average)

tab_model(ta_average, transform = NULL, file = "ta_best_model.doc")


# Individual candidate models

# Model 1

taModel1 <- glmmTMB(data=BTP_model_st1, family=beta_family(link = "logit"), na.action = "na.fail",
                          
                          formula=nightFraction~
                            
                            meanTempAtNight*nightLength+
                            
                            meanTempAtNight*meanCloudCover+
                            
                            lunarNoonNightFraction*meanMoonIllumination+
                            
                            (1|Camera)
                          
)

summary(taModel1)

r2(taModel1)

# Model 2

taModel2 <- glmmTMB(data=BTP_model_st1, family=beta_family(link = "logit"), na.action = "na.fail",
                    
                    formula=nightFraction~
                      
                      meanTempAtNight+
                      
                      nightLength+
                      
                      meanTempAtNight*meanCloudCover+
                      
                  
                      
                      lunarNoonNightFraction*meanMoonIllumination+
                      
                      (1|Camera)
                    
)

summary(taModel2)

r2(taModel2)

# Model 3

taModel3 <- glmmTMB(data=BTP_model_st1, family=beta_family(link = "logit"), na.action = "na.fail",
                    
                    formula=nightFraction~
                      
                      meanTempAtNight*nightLength+
                      
                    
                      
                      lunarNoonNightFraction*meanMoonIllumination+
                      
                      (1|Camera)
                    
)

summary(taModel3)

r2(taModel3)


# Generating effect plots

# Daily detections

# Generating model with non-standardised values

ddModelFinal1 <- glmmTMB(data=dailyDetections, family ="nbinom1", na.action = "na.fail",
                  
                  formula = BTP~
                    
                    day*Site+
                    
                    meanTempAtNight*meanCloudCover+
                    
                    meanTempAtNight*nightLength+
                    
                    meanTempAtNight*Rainfall
                  
                  
)

# individual plots

p1 <- plot(effect(mod=ddModelFinal1, xlevels=list(Rainfall=c(0, 20, 40, 60)), term="meanTempAtNight:Rainfall"),
     x.var=1,
    # main="Mean temperature at night * Rainfall",
    main="(a)", 
    xlab="Mean temperature at night (\u00B0C)",
     ylab="Number of daily detections",
     key.args = list(x=0.95, y=0.05, corner=c(1,0), columns=1, border=F, title="Rainfall (mm)",cex.title=1, cex=1),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)



p2 <- plot(effect(mod=ddModelFinal1, xlevels=4, term="meanTempAtNight:nightLength"),
     x.var=1,
    # main="Mean temperature at night * night length",
    main="(b)", 
    xlab="Mean temperature at night (\u00B0C)",
    ylab=NULL,
     key.args = list(x=0.95, y=0.95, corner=c(1,1), columns=1, border=F, title="Night length (h)",cex.title=1, cex=1 ),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)


p3 <- plot(effect(mod=ddModelFinal1, xlevels=4, term="meanTempAtNight:meanCloudCover"),
     x.var=1,
  #   main="Mean temperature at night * mean cloud cover",
    main="(c)", 
    xlab="Mean temperature at night (\u00B0C)",
  ylab=NULL,
     key.args = list(x=0.95, y=0.95, corner=c(1,1), columns=1, border=F, title="Mean cloud cover (%)",cex.title=1 ),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)



ggarrange(p1, p2, p3, nrow=1)



#########################
# Time of activity

# recalculating the model with non-standardised values


taModel1 <- glmmTMB(data=BTP_model_st1, family=beta_family(link = "logit"), na.action = "na.fail",
                    
                    formula=nightFraction~
                      
                      meanTempAtNight*nightLength+
                      
                      meanTempAtNight*meanCloudCover+
                      
                      lunarNoonNightFraction*meanMoonIllumination+
                      
                      (1|Camera)
                    
)

summary(taModel1)

tab_model(taModel1)

plot(allEffects(taModel1, xlevels=4),
     multiline=T,
     ci.style="band")






p1 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:nightLength"),
     x.var=1,
 #    main="Mean temperature at night * Night length",
     main="(a)",
     xlab="Mean temperature at night",
     ylab="Mean time of activity",
     key.args = list(x=0.95, y=0.05, corner=c(1,0), columns=1, border=F, title="Night length (h)",cex.title=1, cex=1 ),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)

p2 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:meanCloudCover"),
     x.var=2,
     #main="Mean temperature at night * Mean cloud cover",
     main="(b)",
     xlab="Mean cloud cover",
     ylab=NULL,
     key.args = list(x=0.15, y=0.05, corner=c(0,0), columns=1, border=F, title="Mean temperature at night (\u00B0C)",cex.title=1 ),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)

p3 <- plot(effect(mod=taModel_plot, xlevels=4, term="lunarNoonNightFraction:meanMoonIllumination"),
     x.var=1,
     #main="Time of lunar noon * Mean moonlight intensity",
     main="(c)",
     xlab="Time of lunar noon",
     ylab=NULL,
     key.args = list(x=0.9, y=0.05, corner=c(1,0), columns=1, border=F, title="Mean moonlight intensity",cex.title=1, cex=1),
     lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)

ggarrange(p1, p2, p3, nrow=1)



#####



taModel_plot <- glmmTMB(data=BTP_model1, family=beta_family(link = "logit"), na.action = "na.fail",
                        
                        formula=nightFraction~
                          
                          meanTempAtNight*nightLength+
                          
                          meanTempAtNight*meanCloudCover+
                          
                          lunarNoonNightFraction*meanMoonIllumination+
                          
                          (1|Camera)
                        
)


summary(taModel_plot)

plot(allEffects(taModel_plot, xlevels=4),
     multiline=F,
     ci.style="band")



p1 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:nightLength"),
           x.var=1,
           #    main="Mean temperature at night * Night length",
           main="(a)",
           xlab="Mean temperature at night (\u00B0C)" ,
           ylab="Mean time of activity",
           key.args = list(x=0.95, y=0.05, corner=c(1,0), columns=1, border=F, title="Night length",cex.title=1, cex=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

p2 <- plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:meanCloudCover"),
           x.var=2,
           #main="Mean temperature at night * Mean cloud cover",
           main="(b)",
           xlab="Mean cloud cover (%)",
           ylab=NULL,
           key.args = list(x=0.10, y=0.05, corner=c(0,0), columns=1, border=F, title="Mean temperature at night",cex.title=1 ),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

p3 <- plot(effect(mod=taModel_plot, xlevels=4, term="lunarNoonNightFraction:meanMoonIllumination"),
           x.var=1,
           #main="Time of lunar noon * Mean moonlight intensity",
           main="(c)",
           xlab="Time of lunar noon",
           ylab=NULL,
           key.args = list(x=0.9, y=0.05, corner=c(1,0), columns=1, border=F, title="Mean moonlight intensity",cex.title=1, cex=1),
           lines=list(multiline=T, col="black", lty=1:4),
           ci.style="band",
           transform= NULL
)

ggarrange(p1, p2, p3, nrow=1)



plot(effect(mod=taModel_plot, xlevels=4, term="meanTempAtNight:meanCloudCover"),
     x.var=2,
     #main="Mean temperature at night * Mean cloud cover",
     main="(b)",
     xlab="Mean cloud cover",
     ylab=NULL,
     key.args = list(x=0.10, y=0.05, corner=c(0,0), columns=1, border=F, title="Mean temperature at night",cex.title=1 ),
#    lines=list(multiline=T, col="black", lty=1:4),
     ci.style="band",
     transform= NULL
)

