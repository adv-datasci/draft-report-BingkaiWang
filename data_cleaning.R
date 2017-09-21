# cleaning data for demo_raw_data.csv
library(dplyr)
library(stringr)
library(tidytext)
demo_raw_data <- read.csv("demo_raw_data.csv", stringsAsFactors = F)

# generate dictionary of skills
seg <- str_split(demo_raw_data$description, ",|:|[.] | and") %>% unlist() %>% trimws()
seg_word_count <- seg %>% sapply(str_count,"\\S+") %>% as.vector()
seg <- seg[seg_word_count < 3 & seg_word_count != 0] %>% tolower()
dictionary <- data.frame(skill = seg, stringsAsFactors = F) %>%
  group_by(skill) %>%
  tally() %>%
  filter(n > 5) %>%
  arrange(desc(n))
print(dictionary, n = 100)
stop_words <- c("color", "religion", "national origin", "sexual orientation",
                "age", "sex", "disability", "gender identity", "responsibilities",
                "design", "analytics", "marital status", "analysis", "gender",
                "develop", "tools", "veteran status", "dental", "implement",
                "complex", "pregnancy", "processes", "race", "ancestry", "build",
                "ca", "data", "maintain", "technology")
dictionary <- dictionary[!(dictionary$skill %in% stop_words), ]
print(dictionary, n = 100)


processing_data <- demo_raw_data %>%
  unnest_tokens(word, description) %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words)
  

