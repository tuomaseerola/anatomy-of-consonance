# permutated_analysis.R

getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

#x<-getPerms(letters[1:4])
perms<-getPerms(c('Roughness','Harmonicity','Familiarity','Sharpness'))
perms
####
source('optim_A.R')
source('predictor_categories.R')

# start with harrison's model
old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','1')
predictor_category_names <-c('Roughness','Harmonicity','Familiarity','Sharpness')

# LONG PROCESS
run_permutation<-0
if(run_permutation==1){

# Keep track of every k winners (24)
results <- matrix(0,nrow(perms),4)
results_chi <- matrix(0,nrow(perms),4)

for (k in 1:nrow(perms)) {
  old_preds <- c('hutch_78_roughness','har_18_harmonicity','har_19_corpus','1') # re-initialise the starting point every time
  for (l in 1:length(predictor_category_names)) {
    print(paste0(k,'/',l,':',perms[k,l]))
    print(old_preds)
    cat_pred <- perms[k,l]
    new_pred <- predictor_categories(category = cat_pred)
    chisquare<-NULL
    for (i in 1:length(new_pred)) {
      chisquare[i] <- optim_A(data=df,old_predictors=old_preds,new_predictor = new_pred[i],category = cat_pred)
    }
  # replace the variable for the next iteration
    ind <- -1
    if(max(chisquare>0)){
      ind<-which(max(chisquare)==chisquare)
      old_preds[cat_pred==predictor_category_names]<-new_pred[ind]
      print(new_pred[ind])
      print(old_preds)
    }
  # collect the results
    if(ind== -1){ind<-0}
  results[k,l]<-ind
  results_chi[k,l]<-max(chisquare)
  }
}

write.csv(results,file = 'results_order_of_analysis.csv')
write.csv(results_chi,file = 'results_chi_order_of_analysis.csv')
}

#### summarising the analysis ----------------
results<-read.csv('results_order_of_analysis.csv',header = TRUE)
results<-results[,2:5]
results_chi<-read.csv('results_chi_order_of_analysis.csv',header = TRUE)
results_chi<-results_chi[,2:5]

# Sort the columns based on the original order
results_sorted <- matrix(0,nrow(perms),4)
results_chi_sorted <- matrix(0,nrow(perms),4)
predictor_category_names

for (k in 1:24) {
  for (l in 1:4) {
    IND <- perms[k,]==predictor_category_names[l]
    results_sorted[k,l]<-results[k,IND]
    results_chi_sorted[k,l]<-results_chi[k,IND]
  }
}
results_sorted # suggests that there is no change
results_chi_sorted # suggests that there is no change

results_sorted_text<-matrix(NA,24,4)
for(k in 2:4){
  P<-predictor_categories(category = predictor_category_names[k])
  results_sorted_text[,k]<-P[results_sorted[,k]]
}
colnames(results_sorted_text)<-predictor_category_names
perms2<-data.frame(perms)
perms2$C<-paste(perms2$X1,perms2$X2,perms2$X3,perms2$X4)
results_sorted_text<-data.frame(Order=perms2$C,results_sorted_text)
print(knitr::kable(results_sorted_text))
