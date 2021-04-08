predictor_categories <- function(category=NULL){
  
if(category=='Roughness'){
  old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','(1|RandomID)')
  new_pred <-c('seth_93_roughness','vass_01_roughness','wang_13_roughness','spflux')
}
if(category=='Harmonicity'){
    old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','(1|RandomID)')
    new_pred <-c('milne_13_harmonicity','parn_88_root_ambig','parn_94_complex','bowl_18_min_freq_dist','TDL','stolz_15_periodicity','gill_09_harmonicity')
}
if(category=='Familiarity'){
    old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','(1|RandomID)')
    new_pred <-c('keyclar','CorpPop','CorpJazz','CorpClas')
}
if(category=='Sharpness'){
    old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','(1|RandomID)')
    new_pred <-c('spcentr','sharpness','spirreg','sproll')
}
return<-new_pred
}