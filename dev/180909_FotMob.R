

# Packages and Set Up -----------------------------------------------------

library(rvest)
library(dplyr)
library(tidyr)
library(XML)

# Data Import and Set Up---------------------------------------------------
webAddress <- https://www.fotmob.com/leagues/47/

epl_data <- function(webAddress){
  
  eplTable <- read_html(paste0(webAddress,'table')) %>% 
              html_nodes('.fm-table') %>% 
              html_table() %>% 
              as.data.frame()
  
  matchResults <- read_html(paste0(webAddress,'matches/')) %>%
                html_nodes('table')
  


# Main Function -----------------------------------------------------------

fotmob <-  epl_data('https://www.fotmob.com/leagues/47/')
