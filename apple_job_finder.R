job_finder <- function(x,y){
  
  job_scrapper <- function(my_url){
    
    url <- read_html(my_url)
    
    links <- url %>% 
      html_nodes('.table--advanced-search__title') %>%
      html_attr('href')
    
    
    titles <- url %>%
      html_nodes('.table--advanced-search__title') %>%
      html_text()
    
    
    department <- url %>%
      html_nodes('.table--advanced-search__role') %>%
      html_text()
    
    date_posted <- url %>%
      html_nodes('.table--advanced-search__date') %>%
      html_text()
    
    job_location <- url %>%
      html_nodes('.table-col-2 span') %>%
      html_text()
    
    df <- data.frame('title' = titles, 'department' = department, 'location' = job_location,'date posted' = date_posted, 'link' = links)
    return(df)
    }
  
  
  url_list <- paste0('https://jobs.apple.com/en-us/search?location=united-states-USA&sort=relevance&search=',x,'&page=',1:y)
  
  df_list <- lapply(url_list,job_scrapper)
  
  database <- rbindlist(df_list)
  write.csv(database,"apple_jobs.csv")
  saveRDS(database, file = "apple_jobs.rds")
  
}

job_finder('cloud',2) ## Please close the excel file before trying to run again with new search parameters
