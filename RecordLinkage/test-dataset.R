library(tidyverse)
library(RecordLinkage)

# Inputs
input_dir <- "~/Desktop/donation/"

input_files <- input_dir %>% list.files(pattern = "*.zip$", full.names = TRUE)


#----------------
# Load data
#----------------

dt <- input_files %>% map(read_file) %>% bind_rows()

dt %>% dim()  
# [1] 5749132      12

dt %>% glimpse()
  
#----------------
# Functions
#----------------

read_file <- function(file){
  file %>% read_delim(delim = ","
                      , col_types = cols(cmp_fname_c1 = col_character()
                                         , cmp_bd = col_character()
                                         , cmp_bm = col_character()
                                         , cmp_by = col_character()))
}  




