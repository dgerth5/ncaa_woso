library(rvest)
library(tidyverse)

# goals for/against # 
year <- 2021
goal_table <- data.frame()

system.time({
for(i in year){
  
  url <- paste0("http://wosoindependent.com/college/", i, "/?game&recs")
  webpage <- read_html(url)
  
  datalist <- as.data.frame(html_text(html_nodes(webpage, 'td')))
  
  teams.df <- as.data.frame(datalist[-1,])
  
  teams <- teams.df[seq(1, nrow(teams.df), 17), ]
  conf <- teams.df[seq(2,nrow(teams.df),17), ]
  gf <- as.numeric(teams.df[seq(9,nrow(teams.df),17), ])
  ga <- as.numeric(teams.df[seq(10,nrow(teams.df),17), ])
  year <- i
  
  dgdf <- data.frame(teams, conf, gf, ga, year)
  
  goal_table <- rbind(goal_table, dgdf)
  
}
}) # 56 seconds

# schedules
id <- c(1:194, 196:344) # some reason 195 doesn't exist
mv <- data.frame()

system.time({
for (j in id){
  
  url_s <- paste0("http://wosoindependent.com/college/2021/schedule/",j)
  webpage_s <- read_html(url_s)
  
  # away
  
  datalist_s <- as.data.frame(html_text(html_nodes(webpage_s, '.schedulegame td')))
  oteam <- as.data.frame(html_text(html_nodes(webpage_s, '.teaminfoname')))[1,]
  
  date <- datalist_s[seq(1, nrow(datalist_s), 13), ]
  opp <- datalist_s[seq(2, nrow(datalist_s), 13), ]
  res <- datalist_s[seq(3, nrow(datalist_s), 13), ]
  it_team <- oteam
  
  # makes sure row lengths are the same
  
  max <- max(length(date), length(opp), length(res))
  
  length(date) <- max
  length(opp) <- max
  length(res) <- max
  
  df <- data.frame(date,opp,res,it_team)
  
  df_check <- df %>%
    mutate(year = substr(date, nchar(date)-3, nchar(date)),
           month = substr(date, 1,2)) %>%
    filter(year == "2021") %>%
    filter(month == "8/"|month == "9/"|month == "10"|month == "11") # filtering out games in spring 2021
  
  
  mv <- rbind(mv, df_check)
  # 
  print(paste0("date: ", length(date), " opp ", length(opp), " res ", length(res), " max ", max, " team ", it_team, " ",j))
}
}) # 120 seconds

master <- mv %>%
  mutate(home_team = if_else(substr(opp,5,5) == "*", substr(opp,6,nchar(opp)), substr(opp,5,nchar(opp))),
         home_score = if_else(substr(res,1,1) == "W", substr(res,5,5), substr(res,3,3)),
         away_score = if_else(substr(res,1,1) == "W", substr(res,3,3), substr(res,5,5)), 
         exh = if_else(substr(opp,nchar(opp)-3,nchar(opp)) == "EXH)", 1,0)) %>%
  filter(exh == 0)

# need to figure out double digits

# modeling #

devtools::install_github("hturner/BradleyTerry2")
library(BradleyTerry2)

mod <- BTm()
