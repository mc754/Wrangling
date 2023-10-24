library(tidyverse)
library(readr)
library(dslabs)

# read raw murders data without read_csv
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)


# # split at commas with str_split function, remove row of column names
# x <- str_split(lines, ",")
# col_names <- x[[1]]
# x <- x[-1]
# 
# # extract columns 1-5 as characters, then convert to proper format
# library(purrr)
# dat <- data.frame(parse_guess(map_chr(x, 1)),
#                   parse_guess(map_chr(x, 2)),
#                   parse_guess(map_chr(x, 3)),
#                   parse_guess(map_chr(x, 4)),
#                   parse_guess(map_chr(x, 5))) %>%
#   setNames(col_names)
# 
# # more efficient code for the same thing
# dat <- x %>%
#   transpose() %>%
#   map( ~ parse_guess(unlist(.))) %>%
#   setNames(col_names) %>% 
#   as.data.frame()