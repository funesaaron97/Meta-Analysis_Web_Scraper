#single webpage scaped#
page <- 'https://journals.sagepub.com/doi/full/10.3102/0034654319853545'
article<- read_html(page)
 stored_info<- data.frame(Title= article %>% html_nodes('h1')%>% html_text(),
           Author=article %>% html_node('.header .entryAuthor , .contribDegrees > .entryAuthor')%>% html_text(),
           DOI= article %>% html_node('.doiWidgetLink')%>% html_text(),
           Date= article %>% html_node('.dates')%>% html_text())
view(stored_info)