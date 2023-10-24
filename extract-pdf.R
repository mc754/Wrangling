#install.packages("pdftools")

# download PDF and import into R
library(tidyverse)
library(stringr)
library(pdftools)
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# txt is a character vector with an entry for each page
raw_data_research_funding_rates <- txt[2]

# create a list with the lines of the text as elements
tab <- str_split(raw_data_research_funding_rates, "\n")

# Because we start off with just one element in the string, we end up with a list with just one entry:
tab <- tab[[1]]

# information for the column names is the third and fourth entires
the_names_1 <- tab[3]
the_names_2 <- tab[5]

# remove the leading space and everything following the comma
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)

# trim the leading space and then split by space
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)

# join these to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")   #str_c combines char vectors
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")

# get the actual data
new_rates <- tab[7:16] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)