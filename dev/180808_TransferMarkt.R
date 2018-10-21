# Author: Matt Leary
# Date: August 8, 2018
# Tottenham Hotspur Outputs
# 


# Packages and Set Up -----------------------------------------------------

library(rvest)
library(dplyr)
library(tidyr)



# Data Import -------------------------------------------------------------

team_data <- function(webAddress){
  
  mktPage <- read_html(webAddress)
  mktXpath <- '//*[@id="yw1"]/table'
  mktClubLinksXpath <- '//*[@id="yw1"]/table/tbody/tr/td/a'
  
  eplMarket <-mktPage %>% 
              html_nodes(xpath = mktXpath) %>% 
              html_table(fill = T, dec = ",") %>%
              as.data.frame() %>% 
              select(Club.1:Total.MV) %>% 
              drop_na()
  
  mktClubLinks <- mktPage %>% 
              html_nodes(xpath = mktClubLinksXpath) %>% 
              html_attr('href') %>%
              unique()
    
  eplMarket$webLink <- paste0("https://www.transfermarkt.com",
                              mktClubLinks[grepl("startseite", mktClubLinks)],
                              "/plus/1")
  
  epl_df <- eplMarket
  
  return(epl_df)
}

# Sub Functions -----------------------------------------------------------

player_data <- function(eplData){

  for (i in eplData){
  
    player_df <- read_html(eplData$webLink[[1]])  %>% 
           html_nodes("#yw1 .bilderrahmen-fixed") %>% 
           html_attr("title") %>% 
           as.data.frame()
            
    player_df$link <- paste0("https://www.transfermarkt.com",read_html(eplData$webLink[[1]])  %>%
           html_nodes("#yw1 .spielprofil_tooltip") %>% 
           html_attr('href') %>% 
           unique())
    
    player_test <- read_html(player_df$link[[1]]) %>% 
           html_nodes('.auflistung td')
        
 
  }
 
}


# Main Function -----------------------------------------------------------

main <- function(){
  eplData <- team_data('https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1/saison_id/2018/plus/1')
  
}

# Scratch work ------------------------------------------------------------

test <- read_html(eplData$webLink[[1]]) %>% 
  html_nodes(xpath = xpath) %>% 
  #html_table(fill = T, dec = ",") %>%
  as.data.frame() #>% 
select(Club.1:Total.MV) %>% 
  drop_na()
