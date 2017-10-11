library(wordcloud)
library(ggplot2)
library(ggmap)
library(cowplot)

clean_data <- readRDS("cleandata.rds")

# word cloud plot for skills
set.seed(3456)
wordcloud(words = colnames(clean_data)[-(1:7)], 
          freq = colSums(clean_data[,-(1:7)], na.rm = T),
          min.freq = 20,max.words=200,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# barplot for skills
skill_distr <- data.frame(skills = colnames(clean_data)[-(1:7)],
                          freq = colSums(clean_data[,-(1:7)], na.rm = T))
skill_distr <-filter(skill_distr, freq >= sort(skill_distr$freq, decreasing = T)[10])
plot_skill_distr <- ggplot(skill_distr) +
  geom_col(aes(x = reorder(skills, freq), y = freq, fill = freq)) +
  coord_flip() +
  ggtitle("Top 10 skills desired for DS") + 
  theme(legend.position="none",
        plot.title = element_text(size = 16),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 10)) +
  labs(x = NULL, y = "count")

# barplot for unique skills compared with quantitative analyst
quant_raw <- raw_data <- read.csv("quant.csv", stringsAsFactors = F)
skill_dic <- colnames(clean_data)[-(1:7)]
ds_skill_for_quant <- matrix(NA, nrow(quant_raw), length(skill_dic))
for(i in 1:ncol(ds_skill_for_quant)){
  ds_skill_for_quant[,i] <- 
    str_detect(quant_raw$description, 
               pattern = paste0(" ", skill_dic[i], "(?: |.|;|,)")) %>% as.numeric
}
unique_distr <- data.frame(skills = skill_dic, 
                           difference = colSums(clean_data[,-(1:7)], na.rm = T) - 
                             colSums(ds_skill_for_quant, na.rm = T))
unique_distr <- filter(unique_distr,difference >= sort(unique_distr$difference, decreasing = T)[10])
plot_unique_distr <- ggplot(unique_distr) +
  geom_col(aes(x = reorder(skills, difference), y = difference, fill = difference)) +
  coord_flip() +
  ggtitle("Top 10 unique skills desired for DS") +
  theme(legend.position="none",
        plot.title = element_text(size = 16, hjust = 1),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 10)) +
  labs(x = NULL, y = "difference in frequency")


# bar plot for types of companies
industry_distr <- data.frame(industry = 
                               clean_data$industy[!is.na(clean_data$industy)], 
                             stringsAsFactors = F)
filtration  <- industry_distr %>%
  group_by(industry) %>%
  tally %>%
  filter(n > 25) %>%
  arrange(desc(n))
industry_distr <- right_join(industry_distr, filtration) %>% arrange(desc(n))
plot_industry_distr <- ggplot(industry_distr, aes(x = reorder(industry, n), fill = n)) +
  ggtitle("Top 9 industries hiring DS") +
  labs(x = NULL) +
  geom_bar() +
  coord_flip() +
  theme(legend.position="none",
        plot.title = element_text(size = 16, hjust = 2),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 10))

# map plot for job location
location <- str_c(clean_data$city, clean_data$state, sep = ", ")
location <- data.frame(loc = location[!is.na(location)], stringsAsFactors =  F) %>%
  group_by(loc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  mutate(longi = NA, latti = NA)


for(i in 1:nrow(location)){
  location[i, 3:4] <- geocode(location[i,1]) %>% as.numeric
}
location <- location[location$n > 2, ]
usa_center <- as.numeric(geocode("United States"))
USAMap <- ggmap(get_googlemap(center=usa_center, zoom=4, maptype = "roadmap"), 
                extent="normal")
plot_job_geodistr <- USAMap + geom_point(aes(x = longi, y = latti), data = location, 
                    alpha = 0.6, col = "orange",
                    size = location$n/3) + 
  geom_point(aes(x = longi, y = latti), data = location, 
             alpha = 0.3, col = "blue", size = 1) +
  scale_size_continuous(range = range(location$n)) +
  theme( axis.ticks = element_blank(), 
         axis.line = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_blank(),
         plot.title = element_text(size = 16)) +
  ggtitle("Geo-distributing of DS positions")

plot_grid(plot_skill_distr, plot_unique_distr,plot_industry_distr, plot_job_geodistr, 
          labels = c("A", "B", "C", "D"), ncol = 2)
ggsave("explortory_plot.png")
