rm(list = ls())
gc()

library(tibble)
library(dplyr)
library(stringr)
library(purrr)

crates = read.csv('data/input_day5.txt', header = F)

# split crates into positions of crates
positions = crates[1:9,] %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(
    value = paste0(value, ' ')
  )

# create col names for new positions dataframe
col_names = positions[9,] %>% 
  stringr::str_replace_all(
    string = ., 
    pattern = ' ', 
    replacement = '') %>% 
  stringr::str_split(pattern = '') %>%
  unlist()

# split each of the stacks into a column
n = 1
for (i in seq(1, 36, 4)){
  
  col_name = col_names[n]
  
  positions[col_name] = map_chr(.x = positions$value, .f = function(s){
    
    s = stringr::str_sub(s, i, i+3)
    
  })
  
  n = n + 1
  
}

# final wrangle
positions = positions[1:8,] %>%
  dplyr::select(-any_of(c('value'))) %>%
  dplyr::mutate(across(.cols = everything(),.fns = stringr::str_trim))

# set up movements into more machine readable
movements = crates[10:nrow(crates),] %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    n_crates = stringr::str_replace_all(
      string = value, 
      pattern = '^move ([0-9]) .*$', 
      replacement = '\\1'),
    from = stringr::str_replace_all(
      string = value, 
      pattern = '.* from ([0-9]) .*$', 
      replacement = '\\1'),
    to = stringr::str_replace_all(
      string = value, 
      pattern = '.* to ([0-9])$', 
      replacement = '\\1')
    ) %>%
  dplyr::select(-value)

# function to find the highest crate in the stack and multiple levels if needed
find_highest_crate = function(stack, levels = 1){
  
  stack = as.character(stack)
  positions_stack = positions[stack]
  l = list() 
    
  for (i in 1:nrow(positions)){
    
    if (grepl(pattern = ']', x = positions_stack[i,])){
      
      highest_crate = positions_stack[i,]
      
      if (levels == 1){
      
      break
      
      } else {
        
        if (length(l) == levels){
          
          break
          
        }
        
        l = append(l, highest_crate)
        
        }
    }
    
  }
  
  if (levels == 1){
    
    return(highest_crate %>% pull())
    
  } else {
    
    return(l %>% unlist())

  }
  

}

find_highest_crate(3, 2)
