# Experiments -------------------------------------------------------------



data_case_filter <- function(raw, train_cases, test_cases, features = NULL, remove_vars, show_validity = FALSE){
  
  if(is.null(features)){
    
    features <- names(raw)[!names(raw) %in% remove_vars]  
    
  }
  
  # Filter train and test separately
  
  train <- raw[train_cases, c(features, remove_vars)]
  
  test <- raw[test_cases,c(features, remove_vars)]
  
  if(show_validity){
    print('Training')
    print(ftable(table(train$Origin, train$Test_ID, train$Load)) )
    print('Testing')
    print(ftable(table(test$Origin, test$Test_ID, test$Load)) )
    print(features)
  }
  
  return(list(train = train, test = test))  
}

create_experiment <- function(seed = 666,  train_test,  remove_vars, target_var = 'Test_ID', scale_data = TRUE, train_prop = 0.75, grouping_vars = NULL){
  
  train <- train_test$train
  test <- train_test$test
  
  
  
  # Changes (or keeps) random seed
  
  set.seed(seed)  
  
  # Randomly sample from train and test    
  
  train_idx <- caret::createDataPartition(train[, target_var], p = train_prop, list = FALSE)
  
  if(identical(train, test)){
    
    test_idx <-  seq(nrow(train))[!seq(nrow(train)) %in% train_idx] 
    
  }else{
    
    test_idx <- caret::createDataPartition(test[, target_var], p = train_prop, list = FALSE)
    
  }
  
  # Subset from train/test cases
  
  train <- train[train_idx,]
  test <- test[test_idx,]
  
  
  # Grouping var (only for chronologycal use)
  if(!is.null(grouping_vars)){
    grouping_var <- interaction(select(test, grouping_vars[!grouping_vars %in% 'Segment']))
  }else{
    grouping_var <- NULL
  }  
  
  
  # Get class value
  
  train_y <- train[target_var] %>% 
    lapply(., factor) %>% 
    as.data.frame()
  
  test_y <- test[target_var] %>% 
    lapply(., factor) %>% 
    as.data.frame()
  
  # Scale and get train/test instances
  
  if(scale_data){
    
    sc_train <- train %>%
      select(-one_of(remove_vars)) %>%
      scale_n_store()
    
    train_x <-   sc_train$sc_data
    
    test_x <- test %>%
      select(-one_of(remove_vars)) %>%
      scale_df(sc_train$scaler , .)
    
  }else{
    
    train_x <- train %>%
      select(-one_of(remove_vars)) 
    
    test_x <- test %>%
      select(-one_of(remove_vars))    
  }
  
  
  # Pack as experiment
  
  experimentation <- list(train_x = train_x, train_y = train_y, test_x = test_x , test_y = test_y, grouping_var = grouping_var)
  
  return(experimentation)
}



# Pre-processing ----------------------------------------------------------


scaling <- setClass("scaling",
                    
                    slots = c(
                      var_names = "character",
                      mu = "numeric",
                      sdev = "numeric"
                    ),
                    prototype = list(
                      var_names = NA_character_,
                      mu = NA_integer_, 
                      sdev = NA_integer_
                    )
                    ,
                    validity = function(object)
                    {
                      msg <- TRUE
                      if((length(object@var_names) != length(object@mu)) | (length(object@sdev)!= length(object@mu))  ){
                        
                        msg <- 'Mismatch in input dimensions'
                      }
                      return(msg)
                    }
                    
)


setGeneric("scale_df", function(object,df){
  
  standardGeneric("scale_df")
})

setMethod( "scale_df" , signature(object = "scaling") , function(object, df){
  
  if(ncol(df) != length(object@mu)){
    stop('Dataframe dimension is different to scaling dimension.')
  }
  
  MU <- matrix(rep(object@mu, nrow(df)), ncol = length(object@mu), byrow = TRUE)
  
  SDEV <- matrix(rep(object@sdev, nrow(df)), ncol = length(object@sdev), byrow = TRUE)
  
  df <- (df - MU) / SDEV
  
  return(df)
}
)


scale_n_store <- function(df){
  
  if(any(sapply(df,  class) != 'numeric')){
    
    stop('All input variables must be of type numeric')
  }
  
  mu <- apply(df, 2, mean)
  sdev <- apply(df, 2, sd)
  
  MU <- matrix(rep(mu, nrow(df)), ncol = ncol(df), byrow = TRUE)
  
  SDEV <- matrix(rep(sdev, nrow(df)), ncol = ncol(df), byrow = TRUE)
  
  scaled_df <- (df - MU) / SDEV
  
  scaler <- new(Class = 'scaling', var_names = names(df), mu = mu , sdev = sdev)
  
  
  return(list(sc_data = scaled_df, scaler = scaler))
}


# Augmentation ------------------------------------------------------------



augment_class <- function(experimentation , target_class, reps){
  
  stopifnot(target_class %in% c('train', 'lub', 'point'))
  
  if(!is.na(reps) & reps !=0){
    
    experimentation$train_x <- rbind(experimentation$train_x, sapply(experimentation$train_x[which(experimentation$train_y == target_class),], rep.int, reps ))
    experimentation$train_y <- rbind(experimentation$train_y, set_names(as.data.frame(rep(experimentation$train_y[which(experimentation$train_y == target_class),], reps) ), 'Test_ID'))
  }
  
  return(experimentation)
}


augment_classwisse <- function(experimentation, train_reps = NA, point_reps = NA, lub_reps = NA, show_validity = FALSE){
  
  if(show_validity){
    print(table(train_y))
  }
  # This can probably be removed with a Reduce like function
  
  experimentation <- augment_class(experimentation, target_class = 'lub', lub_reps )
  
  
  experimentation <- augment_class(experimentation, target_class = 'point', point_reps )
  
  
  experimentation <- augment_class(experimentation, target_class = 'train', train_reps )
  
  
  
  if(show_validity){
    print(table(train_y))
  }
  
  
  return(experimentation)
}


