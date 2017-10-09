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
write.csv(dictionary, "skill_tag.csv")

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

