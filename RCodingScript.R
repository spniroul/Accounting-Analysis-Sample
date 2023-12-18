####Accounting Sample##########

##Setting up Directory
getwd()
#setwd("SET HERE IF NECESSARY")

#Importing the dataset
data <- read.csv("sample_dataset.csv")

library(tidyverse)


##Preparation
sapply(data, function(x) sum(is.na(x))) #No Missing data
str(data)
#Checking duplicated rows
data %>% group_by_all %>% filter(n()>1)  #No duplicates

###################################Task 1#######################################
################################################################################

#Restricting the data to publicly listed companies
new_data <- filter(data, public == 1)
#Checking unique values for auditor name
unique_auditor <- new_data %>% distinct(auditorname) %>% arrange(auditorname)
View(unique_auditor)

#Since the names of big 4 auditing firms have multiple variation across the column
#Modifying multiple names of each big 4 firm into a single name

temp <- new_data %>% 
  distinct(auditorname) %>% 
  arrange(auditorname) %>% 
  mutate(auditorname_new = auditorname)

temp <- temp %>% 
  mutate(auditorname_new = replace(auditorname_new, c(162:199),"Deloitte")) %>% 
  mutate(auditorname_new = replace(auditorname_new, c(231,251:292),"Ernst and Young")) %>%
  mutate(auditorname_new = replace(auditorname_new, c(350:368),"KPMG")) %>% 
  mutate(auditorname_new = replace(auditorname_new, c(513:543),"PricewaterhouseCoopers"))  

#Merging the dataset to replace the auditor column

new_data <- merge(new_data, temp, by = "auditorname")
new_data <- new_data %>% 
  select(-auditorname) %>% 
  rename(auditorname = auditorname_new)


#Checking the variation of bankname in the dataset
View(new_data %>% 
       distinct(bankname) %>% 
       arrange(bankname))

#Grouping the dataset by year to compute the total number of banks in each year 
totalBank_byYear <- new_data %>% 
  group_by(year) %>% 
  count() %>% 
  rename(totalBank = n)

#Filtering the dataset to only include banks using big 4 auditing firms
totalBanks_usingBig4_byYear <- new_data %>% 
  filter(auditorname == "Deloitte" | 
           auditorname == "Ernst and Young" | 
           auditorname == "KPMG" |
           auditorname == "PricewaterhouseCoopers") %>% 
  group_by(year) %>% 
  count() %>% 
  rename(totalBanks_usingBig4 = n)

#Merging totalBank_byYear and totalBanks_usingBig4_byYear for comparison

merge_totalBanks <- merge(totalBank_byYear, totalBanks_usingBig4_byYear, 
                          by = "year")

merge_totalBanks <- merge_totalBanks %>% 
  mutate(percent = totalBanks_usingBig4/totalBank*100)

#Creating a line graph to compare percentage of banks using big 4 by year
merge_totalBanks$year <- as.character(merge_totalBanks$year)
ggplot(data = merge_totalBanks, aes(x = year, y = percent, group = 1)) +
  geom_point(color = "darkblue") + geom_line(color="darkblue") +
  xlab("Year") + ylab("Percentage of Banks") +
  ggtitle("Percentage of Banks Using Big 4 Auditing Firms") +
  theme(plot.title = element_text(hjust=0.45, vjust = 1, size = 21),
        axis.text=element_text(size= 15),
        axis.title = element_text(size = 18))
ggsave("PercentageofBanksUsingBig4.png", width = 11, height = 7)

##Market Share of Asset of Banks served by Big 4 over time
#Total Assets by Year
TotalAsset_byYear <- new_data %>% 
  group_by(year) %>% 
  summarise(totalAssets = sum(totalassets))

#Total Assets of banks served by Big 4 over time
TotalAsset_Big4_byYear <- new_data %>% 
  filter(auditorname == "Deloitte" | 
           auditorname == "Ernst and Young" | 
           auditorname == "KPMG" |
           auditorname == "PricewaterhouseCoopers") %>% 
  group_by(year) %>% 
  summarise(totalAssets_Big4 = sum(totalassets))

merge_totalAsset <- merge(TotalAsset_byYear, TotalAsset_Big4_byYear, by = "year")
merge_totalAsset <- merge_totalAsset %>% 
  mutate(percent_totalAssets = totalAssets_Big4/totalAssets*100)

#Creating a line graph for total asset share of banks served by big 4
merge_totalAsset$year <- as.character(merge_totalAsset$year)
ggplot(data = merge_totalAsset, aes(x = year, y = percent_totalAssets, group = 1)) +
  geom_point(color = "darkblue") + geom_line(color="darkblue") +
  xlab("Year") + ylab("Percentage of Total Assets") +
  ggtitle("Percentage of Total Assets of Banks Served by Big 4 Auditing Firms") +
  theme(plot.title = element_text(hjust=0.45, vjust = 1, size = 21),
        axis.text=element_text(size= 15),
        axis.title = element_text(size = 18))
ggsave("TotalAssetShareBig4.png", width = 11, height = 7)

##Visualizing Market Share of Big 4 Auditor by Bank Assets Across States
library(usmap)
#Extracting the market share of big 4 by state
big4_bystate <- new_data %>% 
  group_by(bankstate) %>% 
  summarise(totalAsset_byState = sum(totalassets))

marketshare_big4_byState <- new_data %>% 
  filter(auditorname == "Deloitte" | 
           auditorname == "Ernst and Young" | 
           auditorname == "KPMG" |
           auditorname == "PricewaterhouseCoopers") %>% 
  group_by(bankstate) %>% 
  summarise(totalAsset_big4_bystate = sum(totalassets))

merge_marketshare_big4 <- merge(big4_bystate, 
                                marketshare_big4_byState,
                                by = "bankstate")
merge_marketshare_big4 <- merge_marketshare_big4 %>% 
  mutate(marketshare = totalAsset_big4_bystate/totalAsset_byState*100)


#Creating a spatial map containing market share across each state
map <- merge_marketshare_big4 %>% 
  rename(state = bankstate)
plot_usmap(data = map, values = "marketshare") + 
  scale_fill_continuous(name = "Market Share", label = scales::comma) + 
  theme(legend.position = "right") + 
  ggtitle("Market Share of Big 4 Auditing Firms by Bank Assets") +
  labs(caption = "Note: Bank assets are averaged from 2000 to 2015 by state") +
  theme(plot.title = element_text(hjust=0.45, vjust = 1, size = 10, face = "bold"),
        plot.caption = element_text(size = 8, face = "bold")) 
ggsave("USMap.png", width = 11, height = 7)


###################################Task 3#######################################
################################################################################

#Filtering by year 2015
bank_state <- new_data %>% 
  filter(year == 2015)

bank_state %>% 
  group_by(bankstate) %>% 
  distinct(bankname)  #All observations are distinct

bank_state <- bank_state %>% 
  #Calulating Total Assets Across All States
  mutate(totalassets_allbank = sum(totalassets)) %>%
  group_by(bankstate) %>% 
  #Calculating total banks in each state
  mutate(nbanks = n()) %>% 
  #Calculating total public banks in each state
  mutate(npublic = ifelse(public == 1, n())) %>% 
  #Calculating total bank assets in each state
  mutate(totass = sum(totalassets)) %>% 
  #Calculating mean bank asset in each state
  mutate(avass = mean(totalassets)) %>% 
  #Calculating maximum bank asset in each state
  mutate(maxass = max(totalassets)) %>% 
  #Calculating ratio of total publicly listed bank asset to total bank assets in each state
  mutate(ratiopublic = ifelse(public == 1, sum(totalassets)/totass)) %>% 
  #Calculating ratio of total bank assets in each state to total bank assets across all states
  mutate(ratiototal = totass/totalassets_allbank) %>% 
  #Selecting required columns
  select(bankstate, nbanks, npublic, totass, avass,
         maxass, ratiopublic, ratiototal) %>% 
  distinct_all()

################################################################################
################################################################################
