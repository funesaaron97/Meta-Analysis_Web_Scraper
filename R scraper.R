#script for webscraper#
#This first section is for the Review of Educational Reseach#
#Volume 89#
Rev_of_ED_89 <- 'https://journals.sagepub.com/toc/rera/89/%d'
rev_of_ed89 <-map_df(1:4, function(i){
  
  page <- read_html(sprintf(Rev_of_ED_89, i))
  
  data.frame( Article = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             
              Date= page %>% html_nodes('.maintextleft')%>% html_text(),
             
              Doi= unique(page %>% html_nodes('hlFld-Tittle ,a.ref.nowrap') %>% html_attr('href')),
              
              Authors= page %>% html_nodes('.all') %>% html_text()
             )
  })

#Volume 88#
Rev_of_ED_88 <- 'https://journals.sagepub.com/toc/rera/88/%d'
rev_of_ed88 <-map_df(1:6, function(i){
  page <- read_html(sprintf(Rev_of_ED_88, i))
  data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             Date= page %>% html_nodes('.maintextleft')%>% html_text()
  )
})
#Volume 87#
Rev_of_ED_87 <- 'https://journals.sagepub.com/toc/rera/87/%d'
rev_of_ed87 <-map_df(1:6, function(i){
  page <- read_html(sprintf(Rev_of_ED_87, i))
  data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             Date= page %>% html_nodes('.maintextleft')%>% html_text()
  )
})

#Volume 86#
Rev_of_ED_86 <- 'https://journals.sagepub.com/toc/rera/86/%d'
rev_of_ed86 <-map_df(1:4, function(i){
  page <- read_html(sprintf(Rev_of_ED_86, i))
  data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             Date= page %>% html_nodes('.maintextleft')%>% html_text()
  )
})

#Volume 85#
Rev_of_ED_85 <- 'https://journals.sagepub.com/toc/rera/85/%d'
rev_of_ed85 <-map_df(1:4, function(i){
  page <- read_html(sprintf(Rev_of_ED_85, i))
  data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             Date= page %>% html_nodes('.maintextleft')%>% html_text()
  )
})

#Volume 84#
Rev_of_ED_84 <- 'https://journals.sagepub.com/toc/rera/84/%d'
rev_of_ed84 <-map_df(1:4, function(i){
  page <- read_html(sprintf(Rev_of_ED_84, i))
  data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
             Date= page %>% html_nodes('.maintextleft')%>% html_text()
  )
})
# Function for....
get_RERA <- function(volume){
  
  base_url = "https://journals.sagepub.com/toc/rera/"
  rera_url <- paste0(base_url, volume, "/%d")
  n_issues <- ifelse(volume %in% 87:88, 6, 4)
  
  out <- map_df(1:n_issues, function(i){
    page <- read_html(sprintf(rera_url, i))
    data.frame(Title_for_Rev = page %>% html_nodes('.hlFld-Title') %>%html_text(),
               Date= page %>% html_nodes('.maintextleft')%>% html_text()
      ) %>%
      mutate(volume = volume, 
             issue = i)
    }) 
  
  return(out)
  
}


vols = 84:89
articles <- list()
for(i in 1:length(vols)){
  vol <- vols[i]
  articles[[i]] <- get_RERA(vol)
}

# articles <- lapply(1:length(vols),
#                    FUN=function(i) get_RERA(vols[i])
#                    )

article_df <- bind_rows(articles)

rev_of_ed <- rbind.fill(rev_of_ed84, rev_of_ed85, rev_of_ed86, rev_of_ed87,rev_of_ed88, rev_of_ed89)
View(rev_of_ed)

#This is for Psych Bulletin#

Psych_Bull <- 'https://psycnet.apa.org/PsycARTICLES/journal/bul/140/%d
                

psychbuilddf <- for(x in Psych_Bull){map_df(1:12, function(i){
  webpage <- read_html(sprintf(Psych_Bull , i))
  data.frame(Title= webpage %>% html_nodes('.article-title') %>%html_text(),
             Authors= webpage %>% html_nodes('br+ span span:nth-child(2)')%>% html_text(),
             DOI = webpage %>% html_nodes('.doi') %>% html_text())
})}

View(psychbuilddf)

#JRST#

JRST <-'https://onlinelibrary.wiley.com/toc/10982736/2014/51/%d'
         

JRST_51 <- map_df(1:10, function(i){
  webpage <- read_html(sprintf(JRST , i))
  data.frame(Title= webpage %>% html_nodes('.issue-items-container+ .issue-items-container h2') %>%html_text(),
             
             Authors= webpage %>% html_nodes('.author-style')%>% html_text(),
             
             Date = webpage %>% html_nodes('.loa-authors-trunc+ .issue-item__details .ePubDate span+ span') %>% html_text())
})

View(JRST_51)
