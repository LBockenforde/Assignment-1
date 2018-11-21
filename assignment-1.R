library(tidyverse)
library(maps)
library(readr)

# 1,2 -----------------------------------------------------------------------
forbes <- read_csv("forbes.csv") #1
forbes2 <- forbes %>%
  mutate(rank = as.numeric(str_sub(forbes$rank, start = 2)),
         net_worth = as.numeric(str_sub(forbes$net_worth, start= 2, end = -3)) * ifelse(str_sub(forbes$net_worth, start = -1)=="B", 1000000000, 1000000),
         age = as.numeric(forbes$age)) %>%
  filter(net_worth >= 1000000000) #2

# 3 -----------------------------------------------------------------------
qplot(age, net_worth, data=forbes2) #3
qplot(age, log(net_worth), data=forbes2)
## in the log plot i cannot see the magnitude of the outliers as good but the density of net worth as well as a slight positive relation
## in the normal plot outliers are very ponounced, but i do not have good insight into how well net worth varies in an age and cannot really see a trend

# 4 -----------------------------------------------------------------------
forbes3 <- forbes2 %>% #4
  group_by(country) %>%
  summarise(identify = length(name)>5,
            difference = max(net_worth)-min(net_worth)) %>% 
  ungroup() %>%
  full_join(forbes2, by = "country") %>%
  filter(identify==1) %>%
  select(-identify) %>%
  arrange(difference)

# 5 -----------------------------------------------------------------------
forbes3 %>% #5
  distinct(difference, country) %>% 
  ggplot(aes(country, difference)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# 6 -----------------------------------------------------------------------
forbes3 %>% #6
  distinct(difference, country) %>% 
  ggplot(aes(x = reorder(country, difference), y = difference/1000000000)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Country", y = "Wealth difference in billion US$")

# 7 -----------------------------------------------------------------------
forbes3 %>% #7
  group_by(rank) %>% 
  summarise(count = length(rank)) %>% 
  filter(count>1)

# 8 -----------------------------------------------------------------------
forbes4 <- forbes3 %>% #8
  group_by(rank) %>% 
  summarise(count = length(rank)) %>% 
  right_join(forbes3, by = "rank") %>% 
  group_by(rank) %>% 
  mutate(average_rank = (rank*length(rank)+length(rank)*(length(rank)-1)/2)/length(rank)) %>% 
  ungroup() %>% 
  select(rank, average_rank) %>% 
  arrange(rank)

# 9 -----------------------------------------------------------------------
logsumworth <- forbes3 %>% 
  select(country, net_worth) %>% 
  group_by(country) %>% 
  summarise(sum = log(sum(net_worth)))

map_data('world') %>% #9
  rename(country = region) %>% 
  left_join(logsumworth, by = "country") %>% 
  ggplot(aes(long, lat, group=group)) +
  geom_polygon(aes(fill = sum))


  