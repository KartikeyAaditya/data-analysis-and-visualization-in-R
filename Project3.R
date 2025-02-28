library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

nig <- read_csv("https://github.com/KartikeyAaditya/data-analysis-and-visualization-in-R/blob/main/Metadata_Indicator_API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_41.csv.csv")
ger <- read_csv("E:/Sigma_Gyat/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_41.csv", skip = 3)



colnames(nig)
colSums(is.na(nig))
colnames(ger)
colSums(is.na(ger))

nig$Region[is.na(nig$Region)] <- "Unknown"
nig$IncomeGroup[is.na(nig$IncomeGroup)] <- "Unknown"
nig$SpecialNotes[is.na(nig$SpecialNotes)] <- "Unknown"



str(nig)

nig <- nig%>%
  select(-...6)


head(nig)

ger <- ger%>%
  select(-c(`1960`:`1989`))%>%
  mutate(across(`1990`:`2023`, as.numeric))%>%
  mutate(across(`1990`:`2023`, function(k)ifelse(is.na(k), mean(k, na.rm = TRUE), k)))

dataM <- left_join(nig, ger, by = "Country Code")

head(dataM)

summary_stats <- dataM%>%
  select(`1990`:`2023`)%>%
  summary()
         

print(summary_stats)


correlation_stats <- dataM%>%
  select(`1990`:`2023`)%>%
  cor(use = "complete.obs")


print(correlation_stats)


trend_by_region <- dataM%>%
  group_by(Region)%>%
  summarise(across(`1990`:`2023`,~mean(.x, na.rm =TRUE)))

trend_by_region

str(trend_by_region)
head(trend_by_region)


trend_long <- melt(trend_by_region, id.vars = "Region", variable.name = "YEAR", value.name = "GDP_PER_CAPITA")

trend_long$YEAR <- as.numeric(as.character(trend_long$YEAR))

trend_long


avg_regional_gdp <- trend_long%>%
  group_by(Region)%>%
  summarise(avg_gdp = mean(GDP_PER_CAPITA, na.rm = TRUE))


avg_regional_gdp


ggplot(avg_regional_gdp, aes(x = reorder(Region, -avg_gdp), y = avg_gdp, fill = Region))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  labs(title = "Average GDP per Region from (1990-2023)",
      x = "Region",
      y = "Average GDP per Capita")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")
