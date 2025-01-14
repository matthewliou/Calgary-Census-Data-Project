# Calgary Census Data Analysis Using R

```
##install.packages##

library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(plotly)
library(tidyverse)
library(ggplot2)
library(MASS)
library(writexl)

##Data Collection, Wrangling, and Cleaning##

#Income Data
census.income.url <- 'https://data.calgary.ca/resource/wj3a-wgmh.json?$limit=50000&$offset=0'
census.income.data <- httr::GET(url = census.income.url)
census.income.data <- content(census.income.data, as = "text")
census.income.data.table <- do.call("rbind.data.frame", lapply(census.income.data, fromJSON))
census.income <- census.income.data.table[,-12]
census.income$ward <- seq(from = 14, to = 1)
income.table <- census.income %>% mutate_at(c(2:11), as.numeric)
#Define Higher Class, Middle Class, Lower Class
#To make it easier will split in three, "under_20,000" "20,000 to 39,000" "40,000 to 59,999" will be lower class
#"60,000 to 79,999", "80,000 to $99,999", "100,000 to 124,999" will be middle class
#"125,000 to 149,999" "150,000 to 199,999" "200,000 and over" will be upper class
ward.income.table <- data.frame(ward = income.table$ward,
                                lower.class = as.numeric((income.table[,3] + income.table[,4]+ income.table[,5])/income.table$total_household_total_income),
                                middle.class = as.numeric((income.table[,6] + income.table[,7]+ income.table[,8])/income.table$total_household_total_income),
                                upper.class = as.numeric((income.table[,9] + income.table[,10]+ income.table[,11])/income.table$total_household_total_income))

#Community to Ward Lookup
community.ward.url <- 'https://data.calgary.ca/resource/jd78-wxjp.json?$limit=50000&$offset=0'
community.ward.data <- httr::GET(url = community.ward.url)
community.ward.data <- content(community.ward.data, as = "text")
community.ward <- do.call("rbind.data.frame", lapply(community.ward.data, fromJSON))
community.ward.data.frame <- data.frame(ward.number = community.ward$ward_num, community = community.ward$name)

#Language Data
language.url <- 'https://data.calgary.ca/resource/t2zu-59kf.json?$limit=50000&$offset=0'
language.url.data <- httr::GET(url = language.url)
language.url.data <- content(language.url.data, as = "text")
language.data <- do.call("rbind.data.frame", lapply(language.url.data, fromJSON))
language.data.census <- language.data[,-31]
language.data.census$ward <- language.data.census$ward_num
language.data.census <- language.data.census %>% mutate_at(c(3:15,17,18,20,21,23,24,26,27,29,30,31), as.numeric)
language.data.census

#Household Composition Data
general.census.url <- 'https://data.calgary.ca/resource/hfwb-eab8.json?$limit=50000&$offset=0'
general.census.url.data <- httr::GET(url = general.census.url)
general.census.url.data <- content(general.census.url.data, as = "text")
general.census.url.data.table <- do.call("rbind.data.frame", lapply(general.census.url.data, fromJSON))
ncol(general.census.url.data.table)
general.census <- general.census.url.data.table[,-130]
general.census$ward <- community.ward.data.frame$ward.number[match(unlist(general.census$name), community.ward.data.frame$community)]
census.income$ward <- seq(from = 14, to = 1)
residential_census<- general.census%>%
  mutate(res_cnt = as.numeric(res_cnt),
         cat_cnt = as.numeric(cat_cnt),
         dog_cnt = as.numeric(dog_cnt),
         ward = as.numeric(ward),
         emplyd_cnt= as.numeric(emplyd_cnt))%>%
  filter(str_sub(comm_code,1,1)!='0')%>%
  filter(class!='Industrial')%>%
  filter(comm_structure!='UNDEVELOPED')%>%
  filter(comm_structure!='OTHER')

### Q1 ###
###How do languages correlate with income and ward? ###

#Merging Income and Language tables
language.regression <- merge(ward.income.table, language.data.census)

## Convert to excel to use for visualizations using Tableau 
write_xlsx(language.regression,"C://Users/Desktop/UofC/BTMA 431/Final Project//file name.xlsx")

## Relationship with English not spoken often at home to Income Classes
#Correlation language not spoken often at home
cor(language.regression$lower.class, language.regression$eng_not_spk_oft_home_per)
cor(language.regression$middle.class, language.regression$eng_not_spk_oft_home_per)
cor(language.regression$upper.class, language.regression$eng_not_spk_oft_home_per)

#lower class
regression <- lm(formula = lower.class ~eng_not_spk_oft_home, data = language.regression)
print(regression) # regression equation: y = 3.746e-05x + 2.650e+01  

language.regression %>% 
  plot_ly(x = ~eng_not_spk_oft_home) %>%
  layout(title = 'Lower Class Proportion vs English Not Spoken Often at Home', xaxis = list(title = 'No. Households where English is Not Spoken Often'), yaxis = list(title = 'Lower Class Proportion'))%>% 
  add_markers(y = ~lower.class) %>%
  add_lines(x= ~eng_not_spk_oft_home, y = fitted(regression), name = 'Trend Line')%>%
  layout(showlegend = F)

#middle class
regression <- lm(formula = middle.class ~eng_not_spk_oft_home, data = language.regression)
print(regression) # regression equation: y = 5.418e-04x + 2.761e+01

language.regression %>%
  plot_ly(x = ~eng_not_spk_oft_home) %>%
  layout(title = 'Middle Class Proportion vs English Not Spoken Often at Home', xaxis = list(title = 'No. Households where English is Not Spoken Often'), yaxis = list(title = 'Middle Class Proportion'))%>% 
  add_markers(y = ~middle.class) %>%
  add_lines(x= ~eng_not_spk_oft_home, y = fitted(regression), name = 'Trend Line')%>%
  layout(showlegend = F)

#upper class
regression <- lm(formula = upper.class ~eng_not_spk_oft_home, data = language.regression)
print(regression) # regression equation: y =  -0.0005794x + 45.8898270

language.regression %>%
  plot_ly(x = ~eng_not_spk_oft_home) %>%
  layout(title = 'Upper Class Proportion vs English Not Spoken Often at Home', xaxis = list(title = 'No. Households where English is Not Spoken Often'), yaxis = list(title = 'Upper Class Proportion'))%>% 
  add_markers(y = ~upper.class) %>%
  add_lines(x= ~eng_not_spk_oft_home, y = fitted(regression), name = 'Trend Line')%>%
  layout(showlegend = F)

# top languages by ward
chart <- plot_ly(language.regression, x = ~ward, y = ~top_language_num, type = 'bar', text = ~top_language, textposition = 'auto', name = "Top Language") %>%
  layout(title = 'Top 3 Languages Spoken per Ward' , xaxis = list(title = 'Ward', tickvals = ~ward), yaxis = list(title = 'Languages', tickvals = seq(from=0, to=18000, by=2000)))
chart <- chart %>% add_trace(x = ~ward, y = ~top_2_language_num, type = 'bar', text = ~top_2_language, textposition = 'auto', name = "2nd Top Language")
chart <- chart %>% add_trace(x = ~ward, y = ~top_3_language_num, type = 'bar', text = ~top_3_language, textposition = 'auto', name = "3rd Top Language")
print(chart)


### Q2 ###
## How does Pet Distribution Contribute to factors within wards/communities ##

#Relationship with Cats to employed individuals
catemplm<- lm(cat_cnt ~ emplyd_cnt, data = residential_census)
summary(catemplm)
plotcats<- ggplot(data = residential_census, aes(x = emplyd_cnt,y = cat_cnt)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle('Number of Employed Calgarians vs Number of Cats in Each Community')

plotcats

#there is a linear relationship between the number of employed individuals in a community and the number of cats owned 

#Relationship with Dogs to employed individuals
dogemplm<- lm(dog_cnt ~ emplyd_cnt, data = residential_census)
summary(dogemplm)
plotdogs<- ggplot(data = residential_census, aes(x = emplyd_cnt,y = dog_cnt)) +
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle('Number of Employed Calgarians vs Number of Dogs in Each Community')
plotdogs
#there is a linear relationship between the employed individuals in a community and the number dogs owned


inapt<-residential_census%>%
  filter(apt_person!='0.0')%>%
  filter(apartment!='0.0')

#graph pets to people per sector
pet_ratios_by_sector <- residential_census %>%
  mutate(cat_cnt = as.numeric(cat_cnt),
         dog_cnt = as.numeric(dog_cnt),
         res_cnt = as.numeric(res_cnt)) %>%
  group_by(sector) %>%
  summarise(dog_ratio = sum(dog_cnt)/sum(res_cnt),
            cat_ratio = sum(cat_cnt)/sum(res_cnt)) %>%
  ungroup() %>%
  gather(measure, ratio, dog_ratio, cat_ratio)

ggplot(data = pet_ratios_by_sector, aes(fill = measure, y=ratio, x = sector)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of Pets to Total Residents In Each Sector')


# graph pets to people per ward
pet_ratios_by_ward <- residential_census %>%
  mutate(cat_cnt = as.numeric(cat_cnt),
         dog_cnt = as.numeric(dog_cnt),
         res_cnt = as.numeric(res_cnt)) %>%
  group_by(ward) %>%
  summarise(dog_ratio = sum(dog_cnt)/sum(res_cnt),
            cat_ratio = sum(cat_cnt)/sum(res_cnt)) %>%
  ungroup() %>%
  gather(measure, ratio, dog_ratio, cat_ratio)

ggplot(data = pet_ratios_by_ward, aes(fill = measure, y=ratio, x = ward)) +
  scale_x_continuous("ward", labels = as.character(pet_ratios_by_ward$ward), breaks = pet_ratios_by_ward$ward)+
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of Pets to Total Residents In Each Ward') 

# graph people living in apartments per sector
ppl_to_apts <- inapt %>%
  mutate(apt_person = as.numeric(apt_person),
         res_cnt = as.numeric(res_cnt)) %>%
  group_by(sector)%>%
  summarise(apt_ratio = sum(apt_person)/sum(res_cnt))%>%
  ungroup() %>%
  gather(measure, ratio, apt_ratio)

ggplot(data=ppl_to_apts, aes(y = ratio, x = sector))+
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of People Living in Apartments to Total Residents in Each Sector')

# we see a lot of people live in apartments in centre. 
#typically we associate individuals living in apartments as having a lower income or less certain income
# these proportions will be our representations of relative income

#graph people living in apartments as a ratio to residents in each ward

ppl_to_apts <- inapt %>%
  mutate(apt_person = as.numeric(apt_person),
         res_cnt = as.numeric(res_cnt)) %>%
  group_by(ward)%>%
  summarise(apt_ratio = sum(apt_person)/sum(res_cnt))%>%
  ungroup() %>%
  gather(measure, ratio, apt_ratio)

ggplot(data=ppl_to_apts, aes(y = ratio, x = ward))+
  scale_x_continuous("ward", labels = as.character(ppl_to_apts$ward), breaks = ppl_to_apts$ward) +
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of People Living in Apartments to Total Residents in Each Ward')


#graph the relationships between cats, dogs and apartments by sector
ppl_to_apts <- inapt %>%
  mutate(apt_person = as.numeric(apt_person),
         res_cnt = as.numeric(res_cnt)) %>%
  group_by(sector)%>%
  summarise(apt_ratio = sum(apt_person)/sum(res_cnt))%>%
  ungroup() %>%
  gather(measure, ratio, apt_ratio)

ggplot(data=ppl_to_apts, aes(y = ratio, x = sector))+
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of People Living in Apartments to Total Residents in Each Sector')

# prop of pets to apt living by sector
apts_to_pets_by_sector<-inapt %>%
  mutate(apt_person = as.numeric(apt_person),
         res_cnt = as.numeric(res_cnt),
         cat_cnt = as.numeric(cat_cnt),
         dog_cnt = as.numeric(dog_cnt)) %>%
  group_by(sector)%>%
  summarise(apt_ratio = sum(apt_person)/sum(res_cnt),
            dog_ratio = sum(dog_cnt)/sum(res_cnt),
            cat_ratio = sum(cat_cnt)/sum(res_cnt))%>%
  ungroup() %>%
  gather(measure, ratio, apt_ratio, cat_ratio, dog_ratio)

ggplot(data = apts_to_pets_by_sector, aes(fill = measure, y=ratio, x = sector)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratios of People Living in Apartments, Cats, and Dogs to Total Residents')

# SE has a large prop of dogs to a lower prop of people living in apts. More open space, more people living in houses, richer neighborhoods


# graph the relationships between cats, dogs, and apartments by ward
apts_to_pets_by_ward<-inapt %>%
  mutate(apt_person = as.numeric(apt_person),
         res_cnt = as.numeric(res_cnt),
         cat_cnt = as.numeric(cat_cnt),
         dog_cnt = as.numeric(dog_cnt)) %>%
  group_by(ward)%>%
  summarise(apt_ratio = sum(apt_person)/sum(res_cnt),
            dog_ratio = sum(dog_cnt)/sum(res_cnt),
            cat_ratio = sum(cat_cnt)/sum(res_cnt))%>%
  ungroup() %>%
  gather(measure, ratio, apt_ratio, cat_ratio, dog_ratio)

ggplot(data = apts_to_pets_by_ward, aes(fill = measure, y=ratio, x = ward)) +
  scale_x_continuous("ward", labels = as.character(apts_to_pets_by_ward$ward), breaks = apts_to_pets_by_ward$ward) +
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratios of People Living in Apartments, Cats, and Dogs to Total Residents')


# compare income to ward to apt living to pets
as.data.frame(ward.income.table)
catward<-apts_to_pets_by_ward%>%
  filter(measure=='cat_ratio')
dogward<-apts_to_pets_by_ward%>%
  filter(measure=='dog_ratio')
aptward<-apts_to_pets_by_ward%>%
  filter(measure=='apt_ratio')
ward.income.table.sorted<-ward.income.table[order(ward.income.table$ward),]

Q2final<-bind_cols(catward$ratio, dogward$ratio,aptward$ratio,
                   ward.income.table.sorted$lower.class,
                   ward.income.table.sorted$middle.class,
                   ward.income.table.sorted$upper.class)
colnames(Q2final)
Q2final<-Q2final%>%
        rename(
          catratio = 1,
          dogratio=2,
          aptratio=3,
          lower=4,
          middle=5,
          upper = 6
        )
upperaptlm<-lm(upper~aptratio,data=Q2final)
summary(upperaptlm)

middleaptlm<-lm(middle~aptratio,data=Q2final)
summary(middleaptlm)

loweraptlm<-lm(lower~aptratio,data=Q2final)
summary(loweraptlm)
#this is really close to being statistically significant
ggplot(data=Q2final, aes(x=aptratio, y=lower))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle('Proportions of Lower Income Residents to Proportions of Apartment Living in Wards')
#cat check
uppercatlm<-lm(upper~catratio,data=Q2final)
summary(uppercatlm)

middlecatlm<-lm(middle~catratio,data=Q2final)
summary(middlecatlm)

lowercatlm<-lm(lower~catratio,data=Q2final)
summary(lowercatlm)

ggplot(data=Q2final, aes(x=catratio, y=lower))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle('Proportions of Lower Income Residents to Proportions of cats in Wards')

#this is statistically significant!

upperdoglm<-lm(upper~dogratio,data=Q2final)
summary(upperdoglm)

middledoglm<-lm(middle~dogratio,data=Q2final)
summary(middledoglm)

lowerdoglm<-lm(lower~dogratio,data=Q2final)
summary(lowerdoglm)

#nothing is statistically significant with dogs


### Q3 ###
### How does gender affect income? ####
#check class of the data frame
sapply(general.census, class) #they are all characters 

#change from characters to numeric
general.census$male_cnt <- as.numeric(general.census$male_cnt)
general.census$female_cnt <- as.numeric(general.census$female_cnt)
gender <- subset(general.census, select = c(name,male_cnt,female_cnt)) #create gender dataframe
sapply(gender, class) #those columns are now numeric

#shorten the data by filtering 0s in both male and female meaning there is no residents 
#in that community
gender <- gender %>% 
  rowwise() %>% 
  filter(sum(c(male_cnt,female_cnt)) != 0) 

#change from characters to numeric
community.ward.data.frame$ward.number <- as.numeric(community.ward.data.frame$ward.number)
#assign ward number to gender table according to the community.ward.data.frame
gender$ward.number <- community.ward.data.frame$ward.number[match(gender$name, community.ward.data.frame$community)]

# new table for sum of the residents in each gender for each ward
ward.gender <- gender %>%
  group_by(ward.number) %>%
  summarise(across(c(male_cnt, female_cnt), sum))

# rearrange ward.income.table in ascending order 
ward.income.table <- ward.income.table %>% arrange(ward) 
# combined genders and ward income tables 
ward.gender.income <- cbind(ward.income.table, ward.gender)

# Bar chart of Gender Ratio in Each Ward
gender_ratios_by_sector <- ward.gender.income %>%
  group_by(ward) %>%
  summarise(male_ratio = sum(male_cnt)/sum(male_cnt+female_cnt),
            female_ratio = sum(female_cnt)/sum(male_cnt+female_cnt)) %>%
  ungroup() %>%
  gather(measure, ratio, male_ratio, female_ratio)

ggplot(data = gender_ratios_by_sector, aes(fill = measure, y=ratio, x = ward)) +
  scale_x_continuous("ward", labels = as.character(gender_ratios_by_sector$ward), breaks = gender_ratios_by_sector$ward)+
  geom_bar(position = 'dodge', stat = 'identity')+
  ggtitle('Ratio of Genders to Total Residents In Each Ward') 

# checking the highest point in each income class
which.max(ward.gender.income$upper.class) #row 6
ward.gender.income[6,1] #ward 6
ward.gender.income[6,6] #number of male in ward 6
ward.gender.income[6,7] #number of female in ward 6 **more 

which.max(ward.gender.income$middle.class) #row 5
ward.gender.income[5,1] #ward 5
ward.gender.income[5,6] #number of male in ward 5 **more
ward.gender.income[5,7] #number of female in ward 5  

which.max(ward.gender.income$lower.class) # row 9
ward.gender.income[9,1] # ward 9
ward.gender.income[9,6] # number of male in ward 9 **more
ward.gender.income[9,7] # number of female in ward 9

#Scatter plots of genders in each income class
ggplot(data = ward.gender.income, aes(x = male_cnt, y = upper.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Male vs Proportion of Upper Class Income in each Ward')

ggplot(data = ward.gender.income, aes(x = male_cnt, y = middle.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Male vs Proportion of Middle Class Income in each Ward')


ggplot(data = ward.gender.income, aes(x = male_cnt, y = lower.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Male vs Proportion of Lower Class Income in each Ward')


ggplot(data = ward.gender.income, aes(x = female_cnt, y = lower.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Female vs Proportion of Lower Class Income in each Ward')

ggplot(data = ward.gender.income, aes(x = female_cnt, y = middle.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Female vs Proportion of Middle Class Income in each Ward')

ggplot(data = ward.gender.income, aes(x = female_cnt, y = upper.class))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(label=rownames(ward.gender.income), hjust=0, vjust=0, size=10)+
  ggtitle('Number of Female vs Proportion of Upper Class Income in each Ward')


#Regression of the 6 graphs above 
m.low <- lm(lower.class ~ male_cnt, data = ward.gender.income)
summary(m.low) # p = < 0.05 no effect on proportion in lower class in ward
cor(ward.gender.income$male_cnt, ward.gender.income$lower.class)  
f.low <- lm(lower.class ~ female_cnt, data = ward.gender.income)
summary(f.low) # p = < 0.05 no effect on proportion in lower class in ward
cor(ward.gender.income$female_cnt, ward.gender.income$lower.class)  

m.middle <- lm(middle.class ~ male_cnt, data = ward.gender.income)
summary(m.middle) # p = < 0.05 no effect on proportion in middle class in ward
cor(ward.gender.income$male_cnt, ward.gender.income$middle.class)  
f.middle <- lm(middle.class ~ female_cnt, data = ward.gender.income)
summary(f.middle) # p = < 0.05 no effect on proportion in middle class in ward
cor(ward.gender.income$female_cnt, ward.gender.income$middle.class)  

m.upper <- lm(upper.class ~ male_cnt, data = ward.gender.income)
summary(m.upper) # p = 0.6607  < 0.05 no effect on proportion in upper class in ward
cor(ward.gender.income$male_cnt, ward.gender.income$upper.class) #-0.1288289 
f.upper <- lm(upper.class ~ female_cnt, data = ward.gender.income)
summary(f.upper) # p = < 0.05 no effect on proportion in upper class in ward
cor(ward.gender.income$female_cnt, ward.gender.income$upper.class) 
#all p-vales are larger than 0.05 meaning we don't have sufficient evidence 
#all correlation coefficients are below 0.2 indicating a weak correlation 
#further proof there is no significant correlation/relationship between 
#gender and income class
``` 
