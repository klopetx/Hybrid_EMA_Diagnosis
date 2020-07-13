# Metrics -----------------------------------------------------------------


Kappa <- function(CM){
  
  agree <- sum(diag(CM)) / sum(CM)
  
  claswise_p_1 <- apply(CM, 1, sum)/sum(CM)
  claswise_p_2 <- apply(CM, 2, sum)/sum(CM)
  
  chance_agree <- sum(claswise_p_1 * claswise_p_2)
  
  k <- (agree - chance_agree) / (1 - chance_agree)
  
  return(k)
}



metrics <- function(reality, prediction){
  
  CM <- table(reality, prediction)
  
  ACC <- sum(diag(CM))/ sum(CM)
  
  groupwise_ACC <-  mapply(diag(CM),apply(CM, 1,sum),  FUN = '/')
  
  cohen_kappa <- Kappa(CM)
  
  
  return(list(ACC = ACC, groupwise_ACC = groupwise_ACC, cohen_kappa = cohen_kappa, CM = CM) )
  
}

get_metrics <- function(predictions){
  
  ACC <- sapply(predictions, '[[', 'ACC')
  groupwise_ACC <- sapply(predictions, '[[', 'groupwise_ACC')
  cohen_Kappa <- sapply(predictions, '[[', 'cohen_kappa')
  
  return(list(ACC = ACC, groupwise_ACC = groupwise_ACC, cohen_Kappa = cohen_Kappa ))    
}





