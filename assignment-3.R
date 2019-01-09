library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'
#' @return
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  map(x = raw_html, xpath_expressions, xml_find_all) %>% map(~as_list(.)) %>% map_df(~unlist(.)) %>% 
    #make the necessary adjustments to the data frame as given by the assignment
    mutate(country_link = str_sub(country_link, 4)) %>% 
    rename(population = value,
           rank.population = rank)
}


#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  url <- str_c(base_url, country_link)
  map(url, getURL, .encoding = "UTF-8", .opts = list(followlocation = FALSE)) %>% map(read_html) %>% 
    map(xml_find_all, xpath) %>% map(~as_list(.)) %>% map_chr(~unlist(.))
}


#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  population_ranking <- get_population_ranking() %>% mutate(population = as.numeric(gsub(",", "", population)))
  land_area <- get_land_area(population_ranking$country_link) %>% str_sub(end = -7) %>% str_replace(" million", "000000") %>%
    gsub(",", "", .) %>% as.numeric() %>% as_tibble() %>% rename(land_area = value)
  bind_cols(population_ranking, land_area) %>% 
    mutate(population_density = population/land_area)
}


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  map(x = raw_html, xpath, xml_find_all) %>% map(~as_list(.)) %>% map_df(~unlist(.)) %>% 
    mutate(characteristic_link = str_sub(characteristic_link, 4),
           characteristic = str_to_lower(str_sub(characteristic, end = -2)))
}


#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url <- str_c(base_url, url)
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  map(x = raw_html, xpath_expressions, xml_find_all) %>% map(~as_list(.)) %>% map_df(~unlist(.)) %>% 
    mutate(country_link = str_sub(country_link, 4)) %>% 
    rename(!!characteristic:= value,
           !!str_c("rank.", characteristic):= rank)
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
  xpath <- str_c("//div[@id='", xpath_field_id ,"']/div[", item ,"]/span[2]")
  url <- str_c(base_url, country_link)
  map(url, getURL, .encoding = "UTF-8", .opts = list(followlocation = FALSE)) %>% map(read_html) %>% 
    map(xml_find_all, xpath) %>% map(~as_list(.)) %>% map_chr(~unlist(.))
}


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  map2(rankings$characteristic_link, rankings$characteristic, get_ranking) %>% 
    reduce(full_join)
}



