strip_load <- function(testname, is_negative){
  
  load_n_rep <- stringr::str_extract(testname, '[0-9]{1,}kg[0-9]{1,}')
  
  load <- stringr::str_extract(load_n_rep, '[0-9]{1,}k') %>% stringr::str_remove(., 'k')
  
  load <- ifelse(is_negative, as.numeric(load) *(-1), as.numeric(load) ) 
  rep <- as.numeric(stringr::str_remove(load_n_rep, '[0-9]{1,}kg'))
  stripped_name <- stringr::str_remove(testname, load_n_rep)
  
  return(list(load = load, rep = rep, stripped_name = stripped_name))  
  
}

parse_test_name <- function(testname){
  
  metadata <- list(Test_ID=list(), Motion=list(), Degradation_stage=list(), Load=list(), Repetition=list())
  
  is_negative <- stringr::str_detect(testname,pattern = 'neg')
  
  testname <- ifelse(is_negative, stringr::str_remove(testname, 'neg'), testname)
  
  deg_stage <- stringr::str_extract(testname, '(sin|trap)[0-9](st|nd|rd|th)')
  
  if(!is.na(deg_stage)){
    deg_stage <- stringr::str_remove(deg_stage, 'sin|trap') %>%  stringr::str_remove(., '(st|nd|rd|th)') %>%  as.numeric()
    testname <- stringr::str_remove(testname, paste0(deg_stage,'(st|nd|rd|th)') )
  } else{
    deg_stage <- NA_integer_ 
  }
  
  load_n_rep <- strip_load(testname, is_negative)
  
  motion <- ifelse(stringr::str_detect(testname, 'sin'), 'sinusoidal', 'trapezoidal')
  
  testname <- stringr::str_remove(load_n_rep$stripped_name, 'sin|trap')
  
 
  
  metadata$Test_ID <- testname
  metadata$Motion <- motion
  metadata$Degradation_stage <- deg_stage
  metadata$Load <- load_n_rep$load
  metadata$Repetition <- load_n_rep$rep
  
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  
  return(metadata)
} 

add_metadata <- function(data, metadata){
  
  data <- as.data.frame(data)
  
  names(data) <- c('Setpoint', 'Pos_error', 'Current')
  
  data <- cbind(data, metadata)
  return(data)
}

read_n_merge_mat <- function(mat_file_path){
  
  file_data <- R.matlab::readMat(con=here::here('Data', 'Raw', mat_file_path) )
  
  tests <- names(file_data)
  
  parsed_names <- lapply(tests, parse_test_name)
  
  dataBIG <- mapply(file_data, parsed_names, FUN=add_metadata, SIMPLIFY = FALSE)
  
  dataBIG <- do.call('rbind', dataBIG)
  return(dataBIG)
}

segment_in_cycles <- function(data){
 
  start_idx <- 32
  end_idx <- 431
  data$Segment <- 0
  RIndex <- seq(1, dim(data)[1])
  
  for(segment in seq_len(ceiling( ( max(RIndex)-start_idx)/400) ) ){
    
    current_st_idx <- start_idx + 400*(segment-1)
    current_end_idx <- ifelse( (end_idx + 400*(segment-1)) <  max(RIndex), end_idx + 400*(segment-1), max(RIndex) )
    
    idx_seq <- seq(current_st_idx, current_end_idx)
    
    data$Segment[idx_seq] <- segment
  }
  
  
  return(data)
}