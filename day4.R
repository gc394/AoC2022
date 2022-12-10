rm(list = ls())
gc()

library(stringr)
library(dplyr)
library(purrr)

assignment_pairs = read.csv(
  'data/input_day4.txt', 
  header = F, col.names = c('elf_1', 'elf_2'))

assignment_pairs_vectors = assignment_pairs %>%
  dplyr::mutate(
    across(
      # convert - to : to allow for parse/eval
      .cols = everything(), 
      .fns = function(s){
        s = stringr::str_replace_all(
          string = s, 
          pattern = '-', 
          replacement = ':')
      
        return(s)
        },
      .names = '{.col}_vector'),
    is_subset = purrr::map2_lgl(
      .x = elf_1,
      .y = elf_2,
      .f = function(x, y){
        
        # turns strings to vectors
        elf_1_vec = eval(parse(text = x)) 
        elf_2_vec = eval(parse(text = y)) 
        
        # see if either vector is a subset of the other
        if(all(elf_1_vec %in% elf_2_vec) | all(elf_2_vec %in% elf_1_vec)){
          
          return(TRUE)
          
        } else {
          
          return(FALSE)

          }
        }
      ),
    any_overlap = purrr::map2_lgl(
      .x = elf_1,
      .y = elf_2,
      .f = function(x, y){
        
        # turns strings to vectors
        elf_1_vec = eval(parse(text = x)) 
        elf_2_vec = eval(parse(text = y)) 
        
        # see if either vector is a subset of the other
        if(any(elf_1_vec %in% elf_2_vec) | any(elf_2_vec %in% elf_1_vec)){
          
          return(TRUE)
          
        } else {
          
          return(FALSE)
          
        }
      }
    )
    )

sum(assignment_pairs_vectors$is_subset)
sum(assignment_pairs_vectors$any_overlap)

