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
webpages <- 33 # 30 jobs per page
company <- rep(NA, webpages * 30)
location <- rep(NA, webpages * 30)
description <- rep(NA, webpages * 30)
companysize <- rep(NA, webpages * 30)
industry <- rep(NA, webpages * 30)
rating <- rep(NA, webpages * 30)
salary <- rep(NA, webpages * 30)
currentpage <- html_session("https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=true&clickSource=searchBtn&typedKeyword=quanti&sc.keyword=quantitative+analyst&locT=&locId=&jobType=")
ptm <- proc.time()

# scaping
for(i in 1:webpages){
  info <- currentpage %>% html_nodes(".jl") %>% html_text
  rating[(i-1) * 30 + (1:30)] <- str_extract(info, pattern = "^ [:digit:].[:digit:]") %>% trimws
  salary[(i-1) * 30 + (1:30)] <- str_extract(info, pattern = "\\$.*\\(Glassdoor") %>% str_sub(1, -11)
  link <- currentpage %>% html_nodes("#MainCol .flexbox .jobLink") %>% html_attr("href")
  if(length(link) != 30) stop("wrong length of link")
  link <- paste('https://www.glassdoor.com', link, sep = '')
  for(j in 1:30){
    currentlink <- link[j]
    subpage <- html_session(currentlink) # jump to the second level webpage
    if(grepl("glassdoor", subpage$url)){
      company[(i-1) * 30 + j] <- subpage %>% html_node(".padRtSm") %>% html_text()
      location[(i-1) * 30 + j] <- subpage %>% html_node(".subtle") %>% html_text()
      description[(i-1) * 30 + j] <- subpage %>% html_node(".desc") %>% html_text()
      sourcefile <- suppressWarnings(readLines(currentlink))
      pinpoint <- grep("employer", sourcefile)
      sourcefile <- sourcefile[pinpoint[1]+ 0:20]
      current_industry <- sourcefile[str_detect(sourcefile, "\'industry\'")] %>%
        str_extract("\"(.*)\"") %>% 
        str_sub(2, -2)
      industry[(i-1) * 30 + j] <- if(length(current_industry)>0){current_industry}else{NA}
      current_size <- sourcefile[str_detect(sourcefile, "\'size\'")] %>%
        str_extract("\"(.*)\"") %>% 
        str_sub(2, -2)
      companysize[(i-1) * 30 + j] <- if(length(current_size)>0){current_size}else{NA}
    }
    Sys.sleep(1)
  }
  # navigate to next page
  nextpage <- paste0('www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP', 
                     as.character(i+1), '.htm')
  # nextpage <- currentpage %>% html_nodes("#FooterPageNav a") %>% html_attr("href")
  # nextpage <- paste('www.glassdoor.com', nextpage, sep = '')
  currentpage <- html_session(nextpage)
  
  Sys.sleep(5)
}
proc.time() - ptm

# preprocessing location data for output
location <- trimws(location)
type1_indi <- grepl("[[:alpha:]]", str_sub(location, 1, 1))
location[!type1_indi] <- sapply(location[!type1_indi], 
                                function(s) substr(s, 4, nchar(s)))
location <- location %>% str_split(", ") 
for(i in 1: (webpages*30)){
  n <- length(location[[i]])
  if(n == 1){
    location[[i]] <- c(NA, NA)
  }else if(n > 2){
    location[[i]] <- location[[i]][-(1:(n-2))]
  }
}
location <- t(as.data.frame(location))

#output data frame
demo_raw_data <- data.frame(company = trimws(company), 
                            rating = as.numeric(rating),
                            salary = salary,
                            city = location[,1], 
                            state = location[,2], 
                            description = description,
                            companysize = companysize,
                            industy = str_replace_all(industry, "&amp;", "&"))
rownames(demo_raw_data) <- NULL

write.csv(demo_raw_data, "quant.csv")

# cleaning data for raw_data.csv
library(dplyr)
library(stringr)
library(tidytext)

raw_data <- read.csv("raw_data.csv", stringsAsFactors = F)

raw_data$description <- tolower(raw_data$description) #description to lower case
raw_data$companysize <- str_replace_all(raw_data$companysize, "10000--1", "10000+")
raw_data$salary <- str_extract_all(raw_data$salary, "\\$[[:digit:]]*") %>%
  sapply(function(x) {str_sub(x, 2, -1) %>% as.numeric %>% mean})

# generate dictionary of skills
seg <- str_split(raw_data$description, ",|:|[.] | and | or | ;") %>% unlist %>% trimws
seg_word_count <- seg %>% sapply(str_count,"\\S+") %>% as.vector()
seg <- seg[seg_word_count < 3 & seg_word_count != 0] %>% tolower()
dictionary <- data.frame(skill = seg, stringsAsFactors = F) %>%
  group_by(skill) %>%
  tally() %>%
  filter(n > 50) %>%
  arrange(desc(n))
print(dictionary)
stop_words <- c("color", "religion", "national origin", "sexual orientation", "etc.",
                "age", "sex", "disability", "gender identity", "responsibilities",
                "design", "analytics", "marital status", "analysis", "gender",
                "develop", "tools", "veteran status", "dental", "implement", NA,
                "complex", "pregnancy", "processes", "race", "ancestry", "build",
                "ca", "data", "maintain", "technology", "product managers", "e.g", "etc",
                "experience", "analyze", "genetic information", "services", "state")
dictionary <- dictionary[!(dictionary$skill %in% stop_words), ]
print(dictionary, n = 50)
write.csv(dictionary, "quant_skill_tag.csv")

skill_company_mat <- matrix(NA, nrow = nrow(raw_data), ncol = nrow(dictionary))
colnames(skill_company_mat) <- dictionary$skill
for(i in 1:ncol(skill_company_mat)){
  skill_company_mat[,i] <- 
    str_detect(raw_data$description, 
               pattern = paste0(" ", dictionary$skill[i], "(?: |.|;|,)")) %>% as.numeric
}
clean_data <- cbind(raw_data[,-c(1,7)], skill_company_mat)
# str_detect(raw_data$description, "ph[.]d|phd|master|bachelor") %>% sum(na.rm = T)
saveRDS(clean_data, file = "cleandata.rds")
# write.csv(clean_data, "clean_data.csv")  


