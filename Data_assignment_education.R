setwd("/Users/kyrasturgill/Desktop/Policy Lab/Data/Education")
library(tidyverse)

hs_degree <- read.csv("ACSST5Y2018.S1501_data_with_overlays_2020-02-03T161758.csv")
tract_ca <- readxl::read_excel("2010 Tract to Community Area Equivalency File.xlsx")
ca_names <- read.csv("CommAreas.csv")
emp <- read.csv("ACSST5Y2018.S2301_data_with_overlays_2020-02-03T170549.csv")

unemp <- emp %>%
  subset(select = c("GEO_ID", "S2301_C04_001E"))
unemp <- unemp[-1,]

unemp$GEO_ID <- substr(unemp$GEO_ID, 15, 18)


ca_names <- ca_names %>%
  subset(select = c("COMMUNITY", "AREA_NUM_1"))

tract_ca <- tract_ca %>%
  subset(select = c("GEOID2", "CHGOCA")) %>%
  set_names(c("GEO_ID", "AREA_NUM_1"))

tract_ca <- tract_ca %>%
  left_join(ca_names)

hs_degree <- hs_degree[-1,]


hs_degree <- subset(hs_degree, select = c("GEO_ID", "S1501_C01_009E", "S1501_C01_006E", "S1501_C02_009E"))
hs_degree$GEO_ID <- substr(hs_degree$GEO_ID, 15, 18)

tract_ca$GEO_ID <- substr(tract_ca$GEO_ID, 3, 6)

tract_ca <- tract_ca %>%
  filter(AREA_NUM_1 == 29 |
           AREA_NUM_1 == 27 |
           AREA_NUM_1 == 28)

hs_degree_ca <- hs_degree %>%
  left_join(tract_ca)
hs_degree_ca <- hs_degree_ca %>%
  filter(!is.na(COMMUNITY))

unemp_ca <- unemp %>%
  left_join(tract_ca) %>%
  filter(!is.na(COMMUNITY)) %>%
  set_names(c("census", "unemp_rate", "CA", "CA_name"))


colnames <- c("census", "HS_grad_total_over25", "total_pop_over25", "perc_hs", "CA", "CA_name")
hs_degree_ca <- hs_degree_ca %>%
  set_names(colnames)

hs_degree_ca$HS_grad_total_over25 <- as.numeric(as.character(hs_degree_ca$HS_grad_total_over25))
hs_degree_ca$total_pop_over25 <- as.numeric(as.character(hs_degree_ca$total_pop_over25))
hs_degree_ca$perc_hs <- as.numeric(as.character(hs_degree_ca$perc_hs))


#hs_degree[colnames] <- sapply(hs_degree[colnames], as.character)
#hs_degree[colnames] <- sapply(hs_degree[colnames], as.numeric)

hs_degree <- hs_degree %>%
  mutate(area = case_when(census == "8330" | 
                            census == "2801" |
                            census == "8331"|
                            census =="2819" ~ "West Loop",
                          census == "8378" |
                            census == "2804" |
                            census == "2808" |
                            census == "2809" |
                            census == "8380" |
                            census == "8381" |
                            census == "2827" |
                            census == "2828" |
                            census == "8382" |
                            census == "2831" |
                            census == "2832" |
                            census == "8329" |
                            census == "8333" |
                            census == "8419" |
                            census == "8429" | 
                            census == "2838" ~ "Near West Side",
                          census == "8374" |
                            census == "8433" ~ "Tracts West of IMD"))

hs_degree_rate <- hs_degree_ca %>%
  group_by(CA_name) %>%
  summarise(hs_grad = sum(HS_grad_total_over25), 
            total_pop = sum(total_pop_over25),
            min_perc = min(perc_hs),
            max_perc = max(perc_hs),
            mean_perc = mean(perc_hs),
            med_perc = median(perc_hs),
            sd = sd(perc_hs))


unemp_ca$unemp_rate <- as.numeric(as.character(unemp_ca$unemp_rate))

unemp_rate <- unemp_ca %>%
  group_by(CA_name) %>%
  summarise(min_perc = min(unemp_rate),
            max_perc = max(unemp_rate),
            mean_perc = mean(unemp_rate),
            med_perc = median(unemp_rate),
            sd = sd(unemp_rate),
            count = n())




hs_degree_rate$perc_hs_grad <- hs_degree_rate$hs_grad/hs_degree_rate$total_pop*100


ggplot(data = hs_degree_rate, aes(area, perc_hs_grad)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Population with high school degree over age 25", x = "Area", y = "Percent") +
  theme_minimal() 


write.csv(tract_ca, "tract_ca.csv")
