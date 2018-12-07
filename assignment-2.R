library(tidyverse)

# Question 1 ------------------------------------------------------------------------------------------------------

#' Tidying data frame, preserving variables based on column_prefix
#'
#' @param data 
#' @param column_prefix 
#'
#' @return output is a tidy data frame with each word having its own row
tidy_df <- function(data, column_prefix = "var"){
  
  identify_columns <- str_extract_all(colnames(data), 
                              str_c("^[^(", column_prefix, ")](.)*"),
                              simplify = TRUE)
  gather(data, 
         key = variable, 
         value = value, 
         -c(identify_columns[identify_columns != "",]))
} #other variables cannot have the same prefix


# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}
get_jane_austen_data()

#' Extract possible names out of texts
#'
#' @param data in the format of the gutenberg project
#' 
#' @details needs the tidy_df function in the global environment
#'
#' @return data frame with three columns, the possible names in one column uncapitalized, the words unique id, and an id indicating in what line the word can be found in the original data
extract_possible_names <- function(data){
  prep <- data$text %>% 
    str_extract_all("[A-Z][A-Za-z]*",
                    simplify = TRUE) %>%
    as.tibble() %>% 
    mutate(text_id = data$id) %>% 
    tidy_df("V") %>% 
    filter(value != "") %>% 
    mutate(value = str_to_lower(value))
  prep %>% 
    distinct(value) %>% 
    mutate(id = row_number()) %>% 
    right_join(prep, by = "value") %>% 
    select(name = value, id, text_id)
}
possible_names <- extract_possible_names(austen_text)


# Question 3 ------------------------------------------------------------------------------------------------------

#' Filtering names out of texts
#'
#' @param data in the format of the gutenberg project
#' 
#' @details needs the extract_possible_names and tidy_df functions in the global environment, and needs the word frequencies data file in the working directory
#' 
#' @return data frame with one column where the filtered names are listed, one word per row, all words lower case
filter_names <- function(data){
  austen_word_freqs <- readRDS("austen_word_freqs.Rds")
  data %>%
    extract_possible_names() %>% 
    count(name) %>%
    left_join(austen_word_freqs, by = c("name" = "word")) %>% 
    filter(n/count>=.75) %>% 
    select(name)
}
filtered_names <- filter_names(austen_text)


# Question 4 ------------------------------------------------------------------------------------------------------

#' Counting unique names 
#'
#' @param data in the format of the gutenberg project
#' @param namelist list of names that are searched for in the data, uncapitalized, like the output of the filter_names function
#'
#' @details needs the extract_possible_names and tidy_df functions in the global environment
#' 
#' @return data frame with three columns, the title of the book, the number of occurences of names from the namelist in the text, the number of unique names found in the text
count_names_per_book <- function(data, namelist) {
  prep <- data %>%
    extract_possible_names() %>% 
    left_join(select(data, title, id), by = c("text_id" = "id")) %>%
    inner_join(namelist, by = "name")
  name_occurrences_per_book <- prep %>% 
    count(title) %>% 
    rename(name_occurences = n)
  unique_names_per_book <- prep %>% 
    group_by(title) %>%
    distinct(name) %>% 
    count(title) %>% 
    rename(unique_names = n)
  inner_join(name_occurrences_per_book, unique_names_per_book, by = "title")
}
names_per_book <- count_names_per_book(austen_text, filtered_names)