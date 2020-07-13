
# Prediction algorithms LDA -----------------------------------------------

predict_lda <- function(experimentation, useless_vars = ''){
  
  
  experimentation$train_x <- experimentation$train_x %>% 
    select(-one_of(useless_vars))
  experimentation$test_x <- experimentation$test_x %>% 
    select(-one_of(useless_vars))
  
  lda_model <-  MASS::lda(x = experimentation$train_x, grouping = experimentation$train_y[,1] )
  
  prediction <- predict(object = lda_model, newdata = experimentation$test_x)$class
  
  reality <- experimentation$test_y[,1]
  
  worst_var <- row.names(lda_model$scaling)[which.max(abs(lda_model$scaling))]
  
  performance <- metrics(reality, prediction)
  
  return(c(performance, list(worst_var = worst_var, prediction = prediction, lda_model = lda_model) ) )
}


predict_majority_lda <- function(experimentation){
  
  lda_model <-  MASS::lda(x = experimentation$train_x, grouping = experimentation$train_y[,1] )
  
  prediction <- predict(object = lda_model, newdata = experimentation$test_x)$class %>% 
    as.data.frame() %>% 
    setNames(., 'Uchronic')
  
  prediction <- prediction %>% 
    mutate(Grouping = experimentation$grouping_var) %>% 
    group_by(Grouping) %>% 
    mutate(M_vote = majority_vote(Uchronic))
  
  reality <- experimentation$test_y[,1]
  
  performance <- metrics(reality, prediction$M_vote)
  
}

majority_vote <- function(predictions){
  
  
  class_predictions <- table(predictions)
  
  majority_vote <- names(class_predictions)[which.max(class_predictions)]  
  
  majority_prediction <- rep(majority_vote, length(predictions))
  majority_prediction <- factor(majority_prediction, levels = levels(predictions))
  
  return(majority_prediction)
}

check_equal <- function(real_labels){
  
  label_value  <- ifelse(length(unique(real_labels) ) == 1, real_labels[1], NA)
  label_value  
}

print_prediction <- function(prediction){
  return(list(
    sprintf('Acc: %f ',prediction$ACC ),
    sprintf('Lubrication ACC: %f  Spalling ACC: %f Nominal ACC: %f', prediction$groupwise_ACC[1], prediction$groupwise_ACC[2], prediction$groupwise_ACC[3] ),
    sprintf('Kappa: %f ',prediction$cohen_kappa )
  ))
}



