# cleaning data for demo_raw_data.csv
library(dplyr)
library(stringr)
library(tidytext)

demo_raw_data <- read.csv("demo_raw_data.csv", stringsAsFactors = F)

demo_raw_data$description <- tolower(demo_raw_data$description) #description to lower case
demo_raw_data$companysize <- str_replace_all(demo_raw_data$companysize, "10000--1", "10000+")

# generate dictionary of skills
seg <- str_split(demo_raw_data$description, ",|:|[.] | and") %>% unlist() %>% trimws()
seg_word_count <- seg %>% sapply(str_count,"\\S+") %>% as.vector()
seg <- seg[seg_word_count < 3 & seg_word_count != 0] %>% tolower()
dictionary <- data.frame(skill = seg, stringsAsFactors = F) %>%
  group_by(skill) %>%
  tally() %>%
  filter(n > 9) %>%
  arrange(desc(n))
print(dictionary)
stop_words <- c("color", "religion", "national origin", "sexual orientation",
                "age", "sex", "disability", "gender identity", "responsibilities",
                "design", "analytics", "marital status", "analysis", "gender",
                "develop", "tools", "veteran status", "dental", "implement",
                "complex", "pregnancy", "processes", "race", "ancestry", "build",
                "ca", "data", "maintain", "technology", "product managers", "e.g", "etc",
                "experience", "analyze", "genetic information", "services", "mathematics")
dictionary <- dictionary[!(dictionary$skill %in% stop_words), ]
print(dictionary, n = 50)
write.csv(dictionary, "demo_skill_tag.csv")

skill_company_mat <- matrix(NA, nrow = nrow(demo_raw_data), ncol = nrow(dictionary))
colnames(skill_company_mat) <- dictionary$skill
for(i in 1:ncol(skill_company_mat)){
  skill_company_mat[,i] <- 
    str_detect(demo_raw_data$description, 
               pattern = paste0(" ", dictionary$skill[i])) %>% as.numeric
}
demo_clean_data <- cbind(demo_raw_data[,-c(1,7)], skill_company_mat)
write.csv(demo_clean_data, "demo_clean_data.csv")  

