# load libraries

library(gridExtra)
library(maps)
library(geosphere)
library(tidyverse)
library(dtplyr)
library(readxl)
library(tidyselect)
library(janitor)
library(skimr)
library(tools)


# import, rename, clean table

df_raw <- read_csv("C:/Users/Clint/OneDrive/Documents/Data/cdc_heart_disease/nvss_hd_2010_2020_raw.csv")
clean_df <- clean_names(df_raw) #lower case all col, no spaces
print(clean_df)

str(clean_df) #review col structure

#remove NA from table
clean_df1 <- clean_df[!is.na(clean_df$data_value), ]

#----------------------------------Age Stz Table ---------------------------------------------

df_age1 <- clean_df1 %>% #filter 2020, omit 'overall" from break_out, exclude negative #'s, type id "age" = break out category is gender/race only
  filter(data_value_type_id == "AgeStdz") %>%
  filter(year_start != "2020") %>%
  filter(break_out_category != "Overall") %>%
  filter(data_value_alt > 0)
print(df_age1)


#rename columns
new_names <- c("year", "state", "type", "rate", "group", "sub_group")
colnames(df_age1)[c(2, 3, 4, 7, 9, 10)] <- new_names
print(df_age1)


#update type values, verify
df_age1$type[grepl("^Major |^Diseases of", df_age1$type)] <- "Type Unspecified"
df_age1$type[grepl("^Acute", df_age1$type)] <- "Heart Attack"
unique(df_age1$type)


#rename sub groups, verify
df_age1$sub_group[grepl("^Non-Hispanic Wh", df_age1$sub_group)] <- "White"
df_age1$sub_group[grepl("^Non-Hispanic Bl", df_age1$sub_group)] <- "Black"
df_age1$sub_group[grepl("^Oth", df_age1$sub_group)] <- "Unknown/Other"
unique(df_age1$sub_group)  


#remove columns outside objective
as1 <- df_age1[, c(1:4, 7, 9:10)]
print(as1)


#rename
age_stdz_clean <- as1
view(age_stdz_clean)

#Export table as csv file
write.csv(age_stdz_clean, file = "cdc_agestdz_2010_2019.csv")




#------------------------------------------------------------------- create summary tables

#summarize gender sub_group
plot1.1 <- as1 %>% #------------------------------------- By Gender
  filter(group == 'Gender') %>%
  group_by(year, sub_group) %>%
  summarize(rate_sum = sum(rate),
            avg_rate = mean(rate),
            min_rate = min(rate),
            max_rate = max(rate))
plot1 <- plot1.1 %>%
  mutate(avg_rate = round(avg_rate, 1))
view(plot1)



#summarize race sub_group
plot2.1 <- as1 %>% #------------------------------------- By Race
  filter(group == 'Race') %>%
  group_by(year, sub_group) %>%
  summarize(rate_sum = sum(rate),
            avg_rate = mean(rate),
            min_rate = min(rate),
            max_rate = max(rate))

  plot2 <- plot2.1 %>%
    mutate(avg_rate = round(avg_rate, 1))
  view(plot2)


plot3.1 <- as1 %>% #------------------------------------- By Type, All Genders
  filter(group == 'Gender', type != 'Type Unspecified') %>%  #excludes missing type
  group_by(year, type, sub_group) %>%
  summarize(avg_rate = mean(rate))

  plot3 <- plot3.1 %>%
    mutate(avg_rate = round(avg_rate, 1))
  view(plot3)
  

plot4 <- as1 %>% #------------------------------------- By Type, Overall
  filter(type != 'Type Unspecified') %>%  #excludes missing type
  group_by(year, type) %>%
  summarize(avg_rate = mean(rate))
view(plot4)


plot5.1 <- as1 %>% #------------------------------------- By Top/Bottom 5 States
  filter(state != "DC") %>%
  group_by(state) %>%
  summarize(avg_rate = mean(rate))
view(plot5.1)

plot5.2 <- plot5.1[order(-plot5.1$avg_rate), ] #sort table

top_bottom <- plot5.2 %>% #create new table, top/bottom 5
  arrange(avg_rate) %>%
  filter(row_number() <= 5 | row_number() > n() - 5) %>%
print(top_bottom)
     
plot5 <- top_bottom




# -------------------------------------------------------------CREATE PLOTS >>> Gender/Race/Overall
ggplot(plot1, aes(x = year, y = avg_rate, color = sub_group)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Gender",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Gender") +
  theme_bw()


ggplot(plot2, aes(x = year, y = avg_rate, color = sub_group)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Race",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Race") +
  theme_bw()


ggplot(plot3, aes(x=type, y=avg_rate, fill=type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sub_group) +
  labs(title = "Heart Disease Mortality Rate - Gender+Type",
       subtitle = "(2010-2019)",
       x = "Type",
       y = "Avg Rate per 100K",
       color = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(plot4, aes(x = year, y = avg_rate, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(title = "Heart Disease Mortality Rate - Type",
       subtitle = "(2010-2019)",
       x = "Year",
       y = "Avg Rate per 100K",
       color = "Type") +
  theme_bw()


ggplot(plot5, aes(x = reorder(state, -avg_rate), y = avg_rate, fill=avg_rate)) + 
  geom_bar(stat = "identity") +
  #facet_wrap(~ sub_group) +
  labs(title = "Heart Disease Mortality Rates - Top/Bottom 5 States",
       subtitle = "(2010-2019)",
       x = "State",
       y = "Avg Rate per 100K",
       color = "Rate")
  #coord_flip()
  theme_minimal()





