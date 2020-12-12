
race_results <- function(x){
  
url <- read_html(x)

#### Pull race names
gp_name <- url %>%
  html_nodes('.limiter+ .bold') %>%
  html_text()
if (length(gp_name) ==0) {
  gp_name <- ''}
  else {
    # fix race names
    gp_name <- trimws(gsub("\n","",gp_name))
  }
  
# fix race names
#gp_name <- trimws(gsub("\n","",gp_name))


#### Pull race dates
race_date <- url %>%
  html_nodes('.dark.hide-for-mobile') %>%
  html_text()
if (length(race_date) == 0) {
  race_date <- ''
}
  
#### Pull race winners
driver_winner <- url %>%
  html_nodes('.dark+.bold') %>%
  html_text()
if (length(driver_winner) ==0) {
  driver_winner <- ''}
else{
  # Fix race winner names
  driver_winner <- trimws(gsub("\n","",driver_winner)) # remove \n from string
  driver_winner <- gsub("\\s+"," ",driver_winner) # remove all empty spaces between words
  driver_winner <- gsub('.{4}$', '', driver_winner) # remove last 4 characters
}

# Fix race winner names
#driver_winner <- trimws(gsub("\n","",driver_winner)) # remove \n from string
#driver_winner <- gsub("\\s+"," ",driver_winner) # remove all empty spaces between words
#driver_winner <- gsub('.{4}$', '', driver_winner) # remove last 4 characters

#### Pull team names
team <- url %>%
  html_nodes('.uppercase') %>%
  html_text()
if (length(team) ==0) {
  team  <- ''}
else{
  # Fix team name
  team<- team[seq(2, length(team), 2)] # extract every 2nd element from vector
}
  
# Fix team name
#team<- team[seq(2, length(team), 2)] # extract every 2nd element from vector

#### Pull no of laps
laps <- url %>%
  html_nodes('.bold.hide-for-mobile') %>%
  html_text()
if (length(laps) ==0) {
  laps  <- ''
}

# Join data in dataframe
df <- data.frame('race_name' = gp_name, 'race_date' = race_date, 'winner' = driver_winner,'team' = team, 'laps' = laps)
return(df)
}


#### Apply lapply function
get_all_seasons_results <- function(my_url){
  #season_urls <- paste0('https://www.formula1.com/en/results.html/',my_url,'/races.html')
  #database <- rbindlist(lapply(season_urls,race_results))
  database <- rbindlist(lapply(my_url,race_results))
  database <<- database
  return(database)
}

#### Set for loop to get URLs
i <- 1
my_urls <- list()
for(page_result in seq(from = 2015, to = 2019, by = 1)) {
  my_urls[i] = paste0('https://www.formula1.com/en/results.html/',page_result,'/races.html')
  i <- i+1
}


#### Call function
get_all_seasons_results(my_urls)