optim_A <- function(data=NULL,old_predictors=NULL,new_predictor=NULL,category='Roughness'){

  #lmer.0 <- lmer(Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID),data=data,control = lmerControl(optimizer="Nelder_Mead"))
  
  f<-paste('Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID)','+',old_predictors[1],'+',old_predictors[2],'+',old_predictors[3],'+',old_predictors[4])
  print(f)  
  F <- formula(f)
  baseline_model <-lmer(F,data=data,control = lmerControl(optimizer="Nelder_Mead"))
  summary(baseline_model)
  if(category=='Roughness')  {
    f2 <- paste('Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID)','+',new_predictor,'+',old_predictors[2],'+',old_predictors[3],'+',old_predictors[4])
    F2 <- formula(f2)  
  }
  if(category=='Harmonicity')  {
    f2 <- paste('Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID)','+',old_predictors[1],'+',new_predictor,'+',old_predictors[3],'+',old_predictors[4])
    F2 <- formula(f2)  
  }
  if(category=='Familiarity')  {
    f2 <- paste('Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID)','+',old_predictors[1],'+',old_predictors[2],'+',new_predictor,'+',old_predictors[4])
    F2 <- formula(f2)  
  }
  if(category=='Sharpness')  {
    f2 <- paste('Rating ~ (1|chord_size) + (1|dataset) + (1|RandomID)','+',old_predictors[1],'+',old_predictors[2],'+',old_predictors[3],'+',new_predictor)
    F2 <- formula(f2)  
  }
  print(f2)
  alternative_model <-lmer(F2,data=data,control = lmerControl(optimizer="Nelder_Mead"))
 # summary(alternative_model)
  a <- anova(baseline_model,alternative_model,REML=FALSE,refit=FALSE) 
  CHI<-a$Chisq[2]
  return<-CHI
}