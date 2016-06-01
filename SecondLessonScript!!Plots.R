surveys <- read.csv(file="data/portal_data_joined.csv")
library(dplyr)
#select some columns
select(surveys,plot_id,species_id,weight)
#filter some rows
filter(surveys,year == 1995)
#or
filter(surveys,year>1995)
## pipes     (cntrl+shift+m = %>% )
surveys %>% #you select the data set
  filter(year == 1995) %>% #you choose your filter
  select(species_id,sex,weight, year) %>%  #and select the variables you want to filter from the data set
head()
#head()#first six rows of data set
#tail()#last six rows of data set

##challenge
surveys %>% 
  filter(year <1993, weight>15) %>% 
  select(year,sex,weight) %>% 
  arrange(weight) %>%  #arrange is to sort the data
head()

##information about data frame
str(surveys) 
dim(surveys) #rows and columns
nrow(surveys) #rows
ncol(surveys) #columns
names(surveys) #names

#if you want to use these for the pipped data frame just pipe the command at the end
surveys %>% 
  filter(year <1993, weight>15) %>% 
  select(year,sex,weight) %>% 
  arrange(desc(weight)) %>%  #arrange is to sort the data in descending order
  head() 

##mutate
surveys %>% 
mutate(weight_kg=weight/1000) %>% 
  tail

#to filter the missing data
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000)

##challenge
surveys %>% 
  select(species_id, hindfoot_length) %>% 
  filter(!is.na(hindfoot_length), hindfoot_length <30) %>% 
  mutate(hindfoot_half=hindfoot_length/2) %>% 
 head()
  
##asignar un nombre nuevo a la data filtrada y escribir un csv
surveys_hf_below30 <- surveys %>% 
  select(species_id, hindfoot_length) %>% 
  filter(!is.na(hindfoot_length), hindfoot_length <30) %>% 
  mutate(hindfoot_half=hindfoot_length/2)

write.csv(surveys_hf_below30,file="surveys_hfbelow30.csv",row.names=FALSE)
#row.names=FALSE es para quitar los numeros al lado de cada row en excel

##split-apply-combine
surveys %>%   
  group_by(sex) %>% 
  tally #calculate number of observations


surveys %>% 
group_by(sex,species_id) %>% 
  summarize(mean_weight=mean(weight,na.rm=TRUE)) %>% 
  filter(!is.nan(mean_weight),sex!="") #para filtrat los sex en blanco

##1how many times each 'plot type' has been surveyed? 5 ?
##2Use group_by and summarize to find the mean, min, maximum for the hindfoot_length of each species
##3[optional] what was the heaviest animal measured in ech year? (return the columns year, genus, species, and weight)

##1
surveys %>% 
  group_by(plot_type) %>% 
  tally

#2
surveys %>% 
  group_by(species_id) %>% 
  summarize(mean_hindfoot_length= mean(hindfoot_length,na.rm=TRUE), min_hindfoot_length=min(hindfoot_length,na.rm=TRUE), max_hindfoot_length=max(hindfoot_length,na.rm=TRUE)) %>%  
  filter(!is.nan(mean_hindfoot_length)) 

#3
surveys %>% 
  group_by(year) %>% 
  filter(!is.na(weight)) %>% 
  summarize(max_weight=max(weight,na.rm=TRUE)) %>% 
  select(year,genus,spcies,weight)


##Plotting
library(ggplot2)
surveys_complete <- surveys %>% 
  filter(species_id!="",!is.na(weight),!is.na(hindfoot_length),sex%in%c("M","F"))
#to remove rare animals

species_count <- surveys_complete %>% 
  group_by(species_id) %>% 
  tally
species_count
# now you have species count now u look for the ones less than 10 and lose them

frequent_species <- species_count %>% 
filter(n>=10) %>% 
  select(species_id)
frequent_species

## now only select the frequent ones

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% frequent_species$species_id)
nrow(surveys_complete)

ggplot(data=surveys_complete, aes(x=weight,y=hindfoot_length))+ geom_point(alpha=0.1,color="gold")

ggplot(data=surveys_complete, aes(x=species_id,y=hindfoot_length))+ geom_point(alpha=0.3,color="tomato", position="jitter") + geom_boxplot() 

#num of animals caught thru the years

yearly_counts <- surveys_complete %>% 
  group_by(year,species_id) %>% 
  tally

ggplot(yearly_counts, aes(x=year, y =n, group=species_id,color=species_id)) + geom_line()

#faceting
ggplot(yearly_counts, aes(x=year, y =n, group=species_id,color=species_id)) + geom_line() + facet_wrap(~species_id)

#now using sex too
yearly_sex_counts <- surveys_complete %>% 
  group_by(year, species, sex) %>% 
  tally

ggplot(yearly_sex_counts, aes(x=year, y=n, group=sex,color=sex)) + geom_line() + facet_wrap(~species_id)

##Challenge
#Plot the average weight of each species changes through the course of the experiment
#draw one line for males, one line for females

yearly_weight_mean <- surveys_complete %>% 
  group_by(year, species_id, sex) %>% 
  summarize(mean_weight=mean(weight, na.rm=TRUE))

ggplot(yearly_weight_mean, aes(x=year, y=mean_weight, group=sex, color=sex)) + geom_line() + facet_wrap(~species_id)

##how to export
##ggsave("filename.format", width and height in inches)
ggsave("yealy_weight_changes.png",width=15,height=10) 
