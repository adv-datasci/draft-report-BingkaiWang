# predicting salary
clean_data <- readRDS("cleandata.rds")
complete_data <- clean_data[complete.cases(clean_data), ]
salary_analysis <- complete_data[,c(3, 5, 6, 8:39)]
lm(salary~., data = salary_analysis) %>% summary
rating_analysis <- complete_data[,c(2, 3, 6, 8:39)]
lm(rating~., data = rating_analysis) %>% summary

# skills anaylsis
