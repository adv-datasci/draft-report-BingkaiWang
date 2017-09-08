# collecting data for glassdoor
require(rvest)
currentpage <- html_session("https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=true&clickSource=searchBtn&typedKeyword=data+sc&sc.keyword=data+scientist&locT=&locId=&jobType=")
company <- NULL
# salary <- NULL
location <- NULL
description <- NULL
for(i in 1:5){
  # company <- c(company, currentpage %>% html_nodes(".empLoc div") %>% html_text())
  # salary <- c(salary, currentpage %>% html_nodes(".small") %>% html_text())
  # location <- c(location, currentpage %>% html_nodes(".loc") %>% html_text())
  link <- currentpage %>% html_nodes(".jobLink") %>% html_attr("href")
  link <- paste('www.glassdoor.com', link, sep = '')
  link <- unique(link)
  for(j in link){
    subpage <- html_session(j)
    if(length(grep("glassdoor", subpage$url))){
      company <- c(company, subpage %>% html_node(".padRtSm") %>% html_text())
      location <- c(location, subpage %>% html_node(".subtle") %>% html_text())
      description <- c(description, subpage %>% html_node(".desc") %>% html_text())
    }
  }
  nextpage <- glassdoor %>% html_nodes("#FooterPageNav a") %>% html_attr("href")
  nextpage <- paste('www.glassdoor.com', nextpage, sep = '')
  currentpage <- html_session(nextpage[length(nextpage)])
}
demo_raw_data <- data.frame(company = company, location = location, 
                            description = description)
write.csv(demo_raw_data, "demo_raw_data.csv")
