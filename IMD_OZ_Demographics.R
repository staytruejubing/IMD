library(tidyverse)

setwd("/Users/kyrasturgill/Desktop/Policy Lab/Data")

#read demographics data
list.files(full.names = TRUE)

filenames <- list.files(full.names = TRUE)

demographics <- data.frame()
for (i in grep("data_with_overlays", filenames, value = TRUE)) {
    input <- read.csv(i)
    input$year <- substr(i[length(i)], 10, 13)
  demographics <- plyr::rbind.fill(demographics, input)
  }
demographics <- demographics[-1,]
rm(input)

#variables for race and age, figure out age later (post 2017 variable names change!)
c("DP05_0001E", "DP05_0066E", "DP05_0071E", "DP05_0072E", "DP05_0077E", "DP05_0073E", "DP05_0078E", "DP05_0004E", "DP05_0005E",
"DP05_0006E", "DP05_0007E", "DP05_0008E", "DP05_0009E", "DP05_0010E", "DP05_0011E", "DP05_0012E", "DP05_0013E", "DP05_0014E")

#subset variables of interest
demographics <- subset(demographics, select = c("GEO_ID", "year", "DP05_0001E", "DP05_0066E", "DP05_0071E", "DP05_0072E",
                                                "DP05_0077E", "DP05_0073E", "DP05_0078E"))
demographics <- demographics[!grepl("id", demographics$GEO_ID),]
demographics$GEO_ID <- substr(demographics$GEO_ID, 15, 19)

col_names_1 <- c("tract", "year", "total_pop", "total_hisp_pre17", "total_hisp_post17",
               "total_white_pre17", "total_white_post17", "total_black_pre17", "total_black_post17")
col_names_2 <- c("tract", "year", "total_pop", "total_hisp", "total_white", "total_black")

demographics <- demographics %>%
  set_names(col_names_1)

#filter and re-merge data as variable names change pre and post 2017
post_17 <- demographics %>%
  filter(year == 2017 | year == 2018) %>%
  select(-c("total_hisp_pre17", "total_white_pre17", "total_black_pre17")) %>%
  set_names(col_names_2)

pre_17 <- demographics %>%
  filter(year != 2017 & year != 2018) %>%
  select(-c("total_hisp_post17", "total_white_post17", "total_black_post17")) %>%
  set_names(col_names_2)

race <- rbind(pre_17, post_17)

#calculate percentage
race[col_names_2] <- sapply(race[col_names_2], as.character)
race[col_names_2] <- sapply(race[col_names_2], as.numeric)


race <- race %>%
  mutate(black_perc = (total_black/total_pop*100)) %>%
  mutate(hisp_perc = (total_hisp/total_pop*100)) %>%
  mutate(white_perc = (total_white/total_pop*100))

oz_plot <- race %>%
  filter(tract == 84290)

ggplot(oz_plot, aes(year, total_white)) + geom_line()





