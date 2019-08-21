strip_doi_chars <- function(string){
  
  # delete the front material from the DOI links
  out_string = gsub("/doi/([[:alnum:]]*)/", "", string)
  
  return(out_string)
}

Rev <- 'https://journals.sagepub.com/toc/rera/89/3'
s <- read_html(Rev)
hyperlink <- unique(s %>% html_nodes('hlFld-Tittle ,a.ref.nowrap') %>% html_attr('href'))
a <- html_session(Rev)
hyperlink %>%
  strip_doi_chars() %>%
  unique()



