
# Simulation file parsers -------------------------------------------------


read_n_add_meta_syn <- function(syn_file_path){
  
  file_data <- as.data.frame(R.matlab::readMat(con=here::here('Data','Synthetic_data', syn_file_path) )[[1]])
  
  names(file_data) <- c('STime', 'Current', 'Pos_error', 'Displacement_1', 'Displacement_2', 'Setpoint' )
  
  metadata <- parse_mat_files(syn_file_path)
  
  
  final_file <- cbind(metadata, file_data)
  
  return(final_file)
}


parse_mat_files <- function(files_2_parse){
  
  source_file <- files_2_parse
  
  loads <- stringr::str_extract(files_2_parse, '^Load[-]{0,1}[0-9]{2}')
  
  files_2_parse <- stringr::str_remove(files_2_parse, paste0(loads, '_') )
  
  
  viscFR <- stringr::str_extract(files_2_parse, '^viscFr[0-9]{1,4}')
  
  
  files_2_parse <- stringr::str_remove(files_2_parse, paste0(viscFR, '_') )
  
  spFR <- stringr::str_extract(files_2_parse, '^spFr[0-9]{1,4}')
  
  files_2_parse <- stringr::str_remove(files_2_parse, paste0(spFR, '_') )
  
  spST <- stringr::str_extract(files_2_parse, '^spst[0-9]{1,4}')
  
  files_2_parse <- stringr::str_remove(files_2_parse, paste0(spST, '_') )
  
  spEND <- stringr::str_extract(files_2_parse, '^spend[0-9]{1,4}')
  
  files_2_parse <- stringr::str_remove(files_2_parse, paste0(spEND, '_') )
  
  signCH <- stringr::str_extract(files_2_parse, '^signt[0-9]{1,4}')
  
  
  loads <- stringr::str_remove(loads, 'Load' )
  viscFR <- stringr::str_remove(viscFR, 'viscFr' )
  spFR <- stringr::str_remove(spFR, 'spFr' )
  spST  <- stringr::str_remove(spST , 'spst' )
  spEND  <- stringr::str_remove(spEND , 'spend' )
  signCH  <- stringr::str_remove(signCH , 'signt' )
  
  parsed_files <- as.data.frame(matrix(nrow = length(files_2_parse), ncol = 7))
  
  names(parsed_files) <- c('Sourcefile',
                           'Load',
                           'ViscousFR',
                           'SpallingFR',
                           'StartSpalling',
                           'EndSpalling',
                           'SignChange')
  
  parsed_files$Sourcefile <- source_file
  parsed_files$Load <- as.numeric(loads)
  parsed_files$ViscousFR <- as.numeric(viscFR)
  parsed_files$SpallingFR <- as.numeric(spFR)
  parsed_files$StartSpalling <- as.numeric(spST)
  parsed_files$EndSpalling <- as.numeric(spEND)
  parsed_files$SignChange <- as.numeric(signCH)
  
  return(parsed_files)
}


# Feature extraction  -----------------------------------------------------
extract_features <- function(segment, time_var = 'STime'){
  
  output <- tryCatch( 
    { 
      segment_names <- names(segment)
      
      if(time_var ==  'STime' & (length(grep('STime', segment_names, ignore.case = TRUE) ) == 0) ){
        
        if(length(grep('Time', segment_names, ignore.case = TRUE) ) == 0){
          
          stop('Insert variable name matching current variables in dataset (No STime nor Time variables found).')
        }else{
          
          new_names <- segment_names
          new_names[grep('Time', segment_names, ignore.case = TRUE)[1]] <- 'STime'
          
          names(segment) <- new_names
        }
      }else if(time_var != 'STime'){
        
        if(length(grep(time_var, segment_names, ignore.case = TRUE) ) == 0){
          
          stop('Insert variable name matching current variables in dataset.')
        }
        
        new_names <- segment_names
        new_names[grep(time_var, segment_names, ignore.case = TRUE)[1]] <- 'STime'
        
        names(segment) <- new_names
        
        
      }
      
      
      
      segment %<>% 
        mutate(Region = ifelse(Setpoint > 139 & between(STime, 5, 10), 'Extended',
                               ifelse( (Setpoint>0 & Setpoint < 139) & c(0, diff(Setpoint))>0 & between(STime, 1,7), 'Extending',
                                       ifelse( Setpoint < 21, 'Idling',
                                               ifelse((Setpoint>0 & Setpoint < 139) & c(0, diff(Setpoint))<0 & between(STime, 9,15),'Retracting', 'Other'))
                               ) )
        ) 
      
      length_retraction <- sum(segment$Region == 'Retracting')
      
      
      mean_retraction <- segment %>% filter(Region == 'Retracting') %>% 
        describe(., length_retraction)
      
      
      length_extension <- sum(segment$Region == 'Extending')
      
      mean_extension <- segment %>% filter(Region == 'Extending') %>% 
        describe(., length_extension)
      
      
      
      
      features <- 
        cbind(
          setNames(mean_extension, paste0('Ext_', names(mean_extension))), 
          setNames(mean_retraction, paste0('Ret_', names(mean_retraction)))
        )
      
      
      
      features
    },
    error = function(cond){
      # If anything is added to previous descriptors has to be modified also here
      
      features <- setNames( as.data.frame(matrix(NA, ncol = 16, nrow = 1 )), 
                            c("Ext_mean_current", "Ext_peak_current",            
                              "Ext_overshoot_current",       "Ext_mean_Pos_error",         
                              "Ext_peak_Pos_error",          "Ext_overshoot_Pos_error",    
                              "Ret_mean_current",            "Ret_peak_current",           
                              "Ret_overshoot_current",       "Ret_mean_Pos_error",         
                              "Ret_peak_Pos_error",          "Ret_overshoot_Pos_error"
                            )
      ) 
      return(features) 
    }
  )
  
  return(output)
}


describe <- function(data, segment_region_length = NA, return_location = FALSE, tail_proportion = 2/3, head_proportion = 1/4 ){
  
  if(is.na(segment_region_length)){
    segment_region_length <- dim(data)[1]
  }
  
  n_tail <- round(segment_region_length*tail_proportion) 
  n_head <- round(segment_region_length*head_proportion)
  
  if(!return_location){ 
    
    data <- data %>% 
      summarise(
        
        # Current features
        
        mean_current = mean(tail(Current, n= n_tail ), na.rm = TRUE ),
        max_current = max(tail(Current, n= n_tail )),
        min_current = min(tail(Current, n= n_tail )),
        peak_current = max_current-min_current,
        overshoot_current = max(head(Current, n= n_head ), na.rm = TRUE),
        
        # Position error features
        
        mean_Pos_error = mean(tail(Pos_error, n= n_tail ), na.rm = TRUE ),
        min_Pos_error = min(tail(Pos_error, n= n_tail )),
        max_Pos_error = max(tail(Pos_error, n= n_tail )),
        peak_Pos_error = max_Pos_error - min_Pos_error,
        overshoot_Pos_error = max(head(abs(Pos_error), n= n_head ), na.rm = TRUE)
      )
    
  } else{
    
    data <- data %>% 
      summarise(
        
        # Current features
        
        mean_current = mean(tail(Current, n= n_tail ), na.rm = TRUE ),
        max_current = max(tail(Current, n= n_tail )),
        w_max_current = which.max(tail(Current, n= n_tail )) + segment_region_length - n_tail  ,
        min_current = min(tail(Current, n= n_tail )),
        w_min_current = which.min(tail(Current, n= n_tail )) + segment_region_length - n_tail ,
        peak_current = max_current-min_current,
        overshoot_current = max(head(Current, n= n_first_quarter ), na.rm = TRUE),
        w_overshoot_current = which.max(head(Current, n= n_first_quarter )),
        
        # Position error features
        
        mean_Pos_error = mean(tail(Pos_error, n= n_tail ), na.rm = TRUE ),
        min_Pos_error = min(tail(Pos_error, n= n_tail )),
        w_min_Pos_error = which.min(tail(Pos_error, n= n_tail )) + (segment_region_length - n_tail ),
        max_Pos_error = max(tail(Pos_error, n= n_tail )),
        w_max_Pos_error = which.max(tail(Pos_error, n= n_tail )) + segment_region_length - n_tail ,
        peak_Pos_error = max_Pos_error - min_Pos_error,
        overshoot_Pos_error = max(head(abs(Pos_error), n= n_first_quarter ), na.rm = TRUE),
        w_overshoot_Pos_error = which.max(head(abs(Pos_error), n= n_first_quarter))
      )
    
    
  }
  
  return(data)
}

# features <- extract_features(segment = normal_segment)

read_simfile <- function(path_simfile)
{
  simulation <- R.matlab::readMat(path_simfile)$content.table
  
  simulation <- as.data.frame(simulation)
  
  names(simulation) <- c('Time', 'Current', 'Pos_error', 'Position', 'newSetpoint', 'Setpoint')
  
  return(simulation)
}


generate_noisy_data <- function(syntethic_mean_df, noise_sd_df, observations){
  
  for(syn_row in 1:dim(syntethic_mean_df)[1]){
    
    A = syntethic_mean_df[syn_row,]
    
    short_df <-  mapply(FUN = function(A, B, n){rnorm(n, A, B)},
                        A = A,
                        B = noise_sd_df,
                        MoreArgs = list(n = observations)
    ) %>% as.data.frame()
    
    if(!exists('long_df')){
      long_df <- short_df
    }else{
      
      long_df <- rbind(long_df, short_df)
    } 
    
  }
  
  return(long_df)
}

add_noise_to_load <- function(syn_data, real_data, Load_filter, rseed = 100, new_observations = 50){ 
  
  syn_data <- syn_data %>% filter(Load == !!(Load_filter) )
  
  real_data <- real_data %>% filter(Load == !!(Load_filter), Test_ID == 'train' )
  
  real_feature_sd <- real_data %>% select_if(is.numeric) %>% select(-one_of(c( "Degradation_stage", "Load", "Repetition", "Segment"))) %>% 
    apply(.,
          2,
          sd,
          na.rm = TRUE
    )
  set.seed(rseed)
  
  syn_features <- syn_data %>% select_if(is.numeric) %>% select(-one_of(c( "Degradation_stage", "Load")))
  
  
  stopifnot(names(syn_features) == names(real_feature_sd))
  
  
  
  noisyn_data <- generate_noisy_data( syn_features,  real_feature_sd , new_observations)
  
  meta_noisyn <- rep_meta(syn_data, c('Degradation_stage', 'Load', 'Test_ID'), new_observations = new_observations )
  
  nosyn_data <- cbind(meta_noisyn, noisyn_data)
  
  
  return(nosyn_data)
}

rep_meta <- function(df, meta_vars, new_observations){
  
  output <- list()
  
  for(var_name in meta_vars){
    
    aux_output <- rep(df[, names(df) == var_name], each = new_observations)
    
    output <- c(output, list(aux_output) )
    
    
  }
  
  
  output <- as.data.frame(output, stringsAsFactors = FALSE, col.names = meta_vars) 
  
  return(output)     
}


