# collecting data for glassdoor
require(rvest)
ptm <- proc.time()
currentpage <- html_session("https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=true&clickSource=searchBtn&typedKeyword=data+sc&sc.keyword=data+scientist&locT=&locId=&jobType=")
company <- rep(NA, 10000)
# salary <- NULL
location <- rep(NA, 10000)
description <- rep(NA, 10000)
count <- 0
for(i in 1:3){
  # company <- c(company, currentpage %>% html_nodes(".empLoc div") %>% html_text())
  # salary <- c(salary, currentpage %>% html_nodes(".small") %>% html_text())
  # location <- c(location, currentpage %>% html_nodes(".loc") %>% html_text())
  link <- currentpage %>% html_nodes("a.jobLink") %>% html_attr("href")
  link <- paste('www.glassdoor.com', link, sep = '')
  link <- unique(link)
  for(j in link){
    subpage <- html_session(j)
    if(length(grep("glassdoor", subpage$url))){
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
  nextpage <- currentpage %>% html_nodes("#FooterPageNav a") %>% html_attr("href")
  nextpage <- paste('www.glassdoor.com', nextpage, sep = '')
  currentpage <- html_session(nextpage[length(nextpage)])
  Sys.sleep(5)
}
proc.time() - ptm
count
demo_raw_data <- data.frame(company = company[1:count], location = location[1:count], 
                            description = description[1:count])
write.csv(demo_raw_data, "demo_raw_data.csv")
