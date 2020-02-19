#script for webscraper#
#Load Necessary Libraries First#
library(rvest)
library(tidyverse)
library(ggplot2)


#This first section is for the Review of Educational Research#

#We will define a function that cleans the Doi component of the Review of Educational Research#
strip_doi_chars <- function(string){
  
  # delete the front material from the DOI links
  out_string = gsub("/doi/([[:alnum:]]*)/", "", string)
  
  return(out_string)
}


# Iterative Function for Review of Educational Research
get_RERA <- function(volume){
  
  #This assigns our base url # 
  
  base_url = "https://journals.sagepub.com/toc/rera/"
 
   #This combines the base url with the volume and dynamic issue element#
 
   rera_url <- paste0(base_url, volume, "/%d")
  
   #This ifelse statement assigns the approriate number of issues to each corresponding volume, since there are 6 issues in 87,88, but 4 everywhere else#
  n_issues <- ifelse(volume %in% 87:88, 6, 4)
  
  #We make mapping our data frame contigent on 1 to our n_issues#
  out <- map_df(1:n_issues, function(i){
    #Now we make the url readable in R#
    page <- read_html(sprintf(rera_url, i))
    
    # We must retrieve data for our doi's before we can create a data frame#
    doi_url <- unique( page %>% html_nodes('hlFld-Tittle ,a.ref.nowrap') %>% html_attr('href'))
    
    #This creates the data frame#
    data.frame( #First we tell R to get the page, then retrieve the node we want, and then translate the info into text#
      
      Title = page %>% html_nodes('.hlFld-Title') %>% html_text(),
               
      Date = page %>% html_nodes('.maintextleft')%>% html_text(),
      
       
      #We have to run doi's differently, due to the way they are formatted in the html code#
     
       DOI= doi_url %>%strip_doi_chars() %>% unique()) %>% 
      #This combines the DF with new columns, and we set out Volume=Volume and our issues=i# 
      mutate(volume = volume, 
             issue = i)
    }) 
  #We return the Output #
  return(out)
  
}

#We Set our range for volumes#
reravols = 84:89
rera_articles <- list()
for(i in 1:length(reravols)){
  reravol <- reravols[i]
  rera_articles[[i]] <- get_RERA(reravol)
}

rera_articles

# articles <- lapply(1:length(vols),
#                    FUN=function(i) get_RERA(vols[i])
#                    )

article_df <- bind_rows(rera_articles)


#Now we repeat this flow for Psychological Bulletin#
get_pb <- function(volume){

pb_base <- 'https://psycnet.apa.org/PsycARTICLES/journal/bul/'

pb <- paste0(pb_base, volume, '/%d')

k_issues <- ifelse(volume %in% 140:141, 6, 12)
                

pbdf <- map_df(1:k_issues, function(x){
  
  webpage <- read_html(sprintf(pb, x))  
  
  data.frame(
    Title= webpage %>% html_nodes('.article-title') %>%html_text(),
            
     Authors= webpage %>% html_nodes('br+ span span:nth-child(2)')%>% html_text(),
             
    DOI = webpage %>% html_nodes('.doi') %>% html_text()) %>%
    
    mutate(volume = volume, 
           issue = i)
})

return(pbdf)
}

pbvols = 140:145
pb_articles <- list()
for(i in 1:length(pbvols)){
  pbvol <- pbvols[i]
  pb_articles[[i]] <- get_pb(pbvol)
}

pb_articles

get

#JRST#

JRST <-'https://onlinelibrary.wiley.com/toc/10982736/2014/51/%d'
         

JRST_51 <- map_df(1:10, function(i){
  webpage <- read_html(sprintf(JRST , i))
  data.frame(Title= webpage %>% html_nodes('.issue-items-container+ .issue-items-container h2') %>%html_text(),
             
             Authors= webpage %>% html_nodes('.author-style')%>% html_text(),
             
             Date = webpage %>% html_nodes('.loa-authors-trunc+ .issue-item__details .ePubDate span+ span') %>% html_text())
})

View(JRST_51)
