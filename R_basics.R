View(penguins)
vec_1 <- c(13,15,17)
vec_1

install.packages("tidyverse")
library(ggplot2)
data("diamonds")
View(diamonds)

str(diamonds)
colnames(diamonds)
library(tidyverse)
mutate(diamonds,carat_2=carat*200)

today()

data("ToothGrowth")
view(ToothGrowth)

filtered_tg <- filter(ToothGrowth,dose==0.5)
view(filtered_tg)

arrange(filtered_tg,len)




install.packages("here")
library("here")
install.packages("skimr")
library("skimr")
install.packages("janitor")
library("janitor")

install.packages("dplyr")
library("dplyr")

install.packages("palmerpenguins")
library("palmerpenguins")
skim_without_charts(penguins)
glimpse(penguins)



penguins %>%
  rename(island_new=island)

rename_with(penguins,toupper)

rename_with(penguins,tolower)
clean_names(penguins)


penguins %>%
  select(-species)

library(tidyverse)
penguins %>% arrange(bill_length_mm)
arrange(penguins,bill_length_mm)
penguins %>% group_by(island) %>% drop_na() %>% summarize(mean_bill_length_mm = mean(bill_length_mm))
penguins %>% group_by(island) %>% drop_na() %>% summarize(max_bill_length_mm = max(bill_length_mm))
penguins %>% group_by(island,species) %>% drop_na() %>% summarize(max_bl = max(bill_length_mm),mean_bl=mean(bill_length_mm))
penguins %>% filter(species=="Adelie")

penguins %>% 
  
  drop_na() %>% 
  
  group_by(species) %>%
  summarize(max=max(flipper_length_mm))

ggplot(data = diamonds,aes(x=carat, y=price))+geom_point()
hotel_bookings <- read_csv("hotel_bookings.csv")

bookings_df <- read_csv("hotel_bookings.csv")
head(bookings_df)

str(bookings_df)


glimpse(bookings_df)
colnames(bookings_df)
skim_without_charts(bookings_df)

trimmed_df <- bookings_df %>% 
  select(hotel, is_canceled, lead_time)

trimmed_df %>% 
  select(hotel, is_canceled, lead_time) %>% 
  rename(hotel_type = hotel)

trimmed_df %>% 
  rename(hotel_type = hotel)

example_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")

example_df <- bookings_df %>%
  mutate(guests = adults + children + babies)

example_df <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled),
            average_lead_time = mean(lead_time))

head(example_df)

id <- c(1:10)

name <- c("John Mendes", "Rob Stewart", "Rachel Abrahamson", "Christy Hickman", "Johnson Harper", "Candace Miller", "Carlson Landy", "Pansy Jordan", "Darius Berry", "Claudia Garcia")

job_title <- c("Professional", "Programmer", "Management", "Clerical", "Developer", "Programmer", "Management", "Clerical", "Developer", "Programmer")

employee <- data.frame(id, name, job_title)

View(employee)
separate(employee, name, into=c('first_name','last_name'), sep=' ')

unite(employee, 'name', first_name, last_name, sep=' ')
rlang

install.packages('Tmisc')
library(Tmisc)
data(quartet)
View(quartet)

quartet %>%
  group_by(set) %>%
  summarize(mean(x),sd(x),mean(y),sd(y),cor(x,y))

ggplot(quartet,aes(x,y))+geom_point()+geom_smooth(method = lm,se=FALSE)+ facet_wrap(~set)

install.packages('datasauRus')
library('datasauRus')

#good visulaisation

ggplot(datasaurus_dozen,aes(x=x,y=y,colour=dataset))+geom_point()+theme_void()+theme(legend.position ="none" )+facet_wrap(~dataset,)



install.packages("SimDesign")
library(SimDesign)
actual_temp<- c(68.3, 70,72.4, 71, 67, 70)
predicted_temp<- c(67.9, 69, 71.5, 70, 67, 69)
bias(actual_temp,predicted_temp)

glimse(ToothGrowth)


#######visulaisation

install.packages("ggplot2")
install.packages("palmerpenguins")

library(ggplot2) 
library(palmerpenguins)
colnames(penguins)
data("penguins")
View(penguins)

ggplot(data=penguins)+geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g))
#or
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_point()

ggplot(data=penguins)+geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g,alpha=species),color="purple")

ggplot(data=penguins)+geom_smooth(mapping = aes(x=flipper_length_mm,y=body_mass_g,linetype=species))+ geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g))

ggplot(data=diamonds)+geom_bar(mapping = aes(x=color,fill=cut))+facet_wrap(~color)

ggplot(data=penguins)+geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g,color=species))+facet_grid(sex~species)

install.packages('tidyverse')
library(tidyverse)
onlineta_city_hotels_v2 <- hotel_bookings %>%
  filter(hotel=="City Hotel") %>%
  filter(market_segment=="Online TA")

View(onlineta_city_hotels_v2)

ggplot(data=penguins)+geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g))+labs(title = "Palmer Penguins: Body Mass Vs Flipper Length",subtitle = "Sample of three penguin species",
                                                            caption = "dTAA COLLECTED BY dR. kRISTEN gORMAN") + annotate("text", x=220,y=3500,label="The Gentoos are the largest",color="purple",fontface="bold",size=4.5,angle=25)
p<-ggplot(data=penguins)+geom_point(mapping = aes(x=flipper_length_mm,y=body_mass_g))+labs(title = "Palmer Penguins: Body Mass Vs Flipper Length",subtitle = "Sample of three penguin species",
                                                                                           caption = "dTAA COLLECTED BY dR. kRISTEN gORMAN")

p+annotate("text", x=220,y=3500,label="The Gentoos are the largest",color="purple",fontface="bold",size=4.5,angle=25)

ggsave("three penguins.png")

ggplot(data=penguins)+geom_jitter(mapping = aes(x=flipper_length_mm,y=body_mass_g))

install.packages("digest")
install.packages("rmarkdown")
library(rmarkdown)
library(digest)


