# Web scraping data from glassdoor.com with job title 'data scientist'
# Using package 'rvest' for scparing and collecting raw data for
# company name, location (city, state) and job description
# Output a csv file containing above information.

# Currently we scape only several hundreds of jobs for testing. 
# The following code can be easily generalized into thousands of jobs.

require(rvest)
library(dplyr)
library(stringr)

#Initializing
company <- rep(NA, 10000)
location <- rep(NA, 10000)
description <- rep(NA, 10000)
currentpage <- html_session("https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=true&clickSource=searchBtn&typedKeyword=data+sc&sc.keyword=data+scientist&locT=&locId=&jobType=")
count <- 0 # count the number of jobs

# scaping. 'i' is the index of page. 
# Currently use page 1 to 5 with each containing roughly 30 jobs.
for(i in 1:5){
  link <- currentpage %>% html_nodes("a.jobLink") %>% html_attr("href")
  link <- paste('www.glassdoor.com', link, sep = '')
  link <- unique(link)
  for(j in link){
    subpage <- html_session(j) # jump to the second level webpage
    if(grepl("glassdoor", subpage$url)){
      add_company <- subpage %>% html_node(".padRtSm") %>% html_text()
      if(is.na(add_company) | add_company %in% company){
        next
      }else{
        count <- count + 1
        company[count] <- add_company
        location[count] <- subpage %>% html_node(".subtle") %>% html_text()
        description[count] <- subpage %>% html_node(".desc") %>% html_text()
      }
    }
  }
  # navigate to next page
  nextpage <- currentpage %>% html_nodes("#FooterPageNav a") %>% html_attr("href")
  nextpage <- paste('www.glassdoor.com', nextpage, sep = '')
  currentpage <- html_session(nextpage[length(nextpage)])
  
  Sys.sleep(5)
}

# preprocessing location data for output
location <- location[1:count] %>%
  sapply(function(s) substr(s, 5, nchar(s))) %>%
  str_split(", ") 
for(i in 1: count){
  n <- length(location[[i]])
  if(n == 1){
    location[[i]] <- c(NA, NA)
  }else if(n > 2){
    location[[i]] <- location[[i]][-(1:(n-2))]
  }
}
location <- t(as.data.frame(location))

#output data frame
demo_raw_data <- data.frame(company = trimws(company[1:count]), 
                            city = location[,1], 
                            state = location[,2], 
                            description = description[1:count])
rownames(demo_raw_data) <- NULL
write.csv(demo_raw_data, "demo_raw_data.csv")
