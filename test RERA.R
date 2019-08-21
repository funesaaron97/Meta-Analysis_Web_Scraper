Rev_of_ED_base <- 'https://journals.sagepub.com/toc/rera/89/%d'
rev_of_ed <-map_df(1:4, function(i){
  articles <- read_html(Rev_of_ED_base)%>% html_nodes('.hlFld-Title') %>% html_text()
  s <- html_session(Rev_of_ED_base)
  for(x in articles){
    article <- s %>% follow_link(x) %>% read_html()
    stored_information <- data.frame(Title= article %>% html_nodes('h1')%>% html_text(),
                             Author=article %>% html_node('.header .entryAuthor , .contribDegrees > .entryAuthor')%>% html_text(),
                             DOI= article %>% html_node('.doiWidgetLink')%>% html_text(),
                             Date= article %>% html_node('.dates')%>% html_text())
  }}

  view(stored_information)