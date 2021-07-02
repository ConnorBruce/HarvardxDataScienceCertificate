install.packages("tidyverse", "dslabs", "NHANES","ggthemes", "ggrepel","gridExtra", "dplyr","RColorBrewer","readxl")
library(tidyverse)
library(dslabs)
library(NHANES)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(gtools)
library(readxl)

getwd()
newpath = "C:/Users/conno/OneDrive/Documents/Data Science Certificate/HarvardxDataScienceCertificate/Course 6"
setwd(newpath)

path = system.file("extdata", package = "dslabs")
list.files(path)
filename = "murders.csv"
fullpath = file.path(path, filename)
fullpath

file.copy(fullpath, getwd())

file.exists(filename)

#inspect the first 3 lines
read_lines("./data/murders.csv", n_max = 3)

#read file in csv format
dat = read_csv("./data/murders.csv")

#Examples
path = system.file("extdata", package = "dslabs")
files = list.files(path)
files

filename = "murders.csv"
filename1 = "life-expectancy-and-fertility-two-countries-example.csv"
filename2 = "fertility-two-countries-example.csv"
dat = read.csv(file.path(path, filename))
dat1 = read.csv(file.path(path, filename1))
dat2 = read.csv(file.path(path, filename2))
head(dat)
#head(dat1)
#head(dat2)


url = "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat = read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename = tempfile()
download.file(url, tmp_filename)
dat = read.csv(tmp_filename)
file.remove(tmp_filename)

url2 = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url2, n_max = 5)
ncol(read_csv(url2, col_names=F))


data(gapminder)

#create and inspect a tidy data frame
tidy_data = gapminder %>%
  filter(country %in% c("South Korea","Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#plotting tidy data is simple
tidy_data %>%
  ggplot(aes(year, fertility, color=country)) +
  geom_point()

#import and inspect example of original Gapminder data in wide format
path = system.file("extdata", package = "dslabs")
filename = file.path(path, "fertility-two-countries-example.csv")
wide_data = read_csv(filename)
select(wide_data,country,'1960':'1967')

#original wide data
path = system.file("extdata", package="dslabs")
filename = file.path(path, "fertility-two-countries-example.csv")
wide_data = read_csv(filename)

#tidy data from dslabs
data("gapminder")
tidy_data = gapminder %>%
  filter(country %in% c("South Korea","Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#gather wide data to make new tidy data
new_tidy_data = wide_data %>%
  gather(year, fertility, '1960':'2015')
head(new_tidy_data)

#gather all columns except country
new_tidy_data = wide_data %>%
  gather(year, fertility, -country)
head(new_tidy_data)

#gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

#convert gathered column names to numeric
new_tidy_data = wide_data %>%
  gather(year, fertility, -country, convert = T)
class(new_tidy_data$year)

#spread tidy data to generate wide data
new_wide_data = new_tidy_data  %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1967')


#import data
path = system.file("extdata", package ="dslabs")
filename = file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat = read_csv(filename)
select(raw_dat,1:5)

#gather all columns except country
dat = raw_dat %>%gather(key, value, -country)
head(dat)
dat$key[1:5]

#separate on underscores
dat %>% separate(key, c("year","variable_name"),"_")
dat %>% separate(key, c("year","variable_name"))

#split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year","first_variable_name","second_variable_name"),fill = "right")

#split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year","variable_name"),sep = "_",extra = "merge")

#separate then spread
dat %>% separate(key, c("year","variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value)

#separate then unite
dat %>% separate(key, c("year", "first_variable_name","second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep = "_")

#full code for tidying data
dat %>%
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep = "_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

#2.1 Assessment
data(co2)
co2_wide = data.frame(matrix(co2,ncol=12,byrow=T)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy = gather(co2_wide,month,co2,-year)
head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data(admissions)
dat = admissions %>% select(-applicants)
head(dat)
dat_tidy = spread(dat, gender, admitted)
dat_tidy

tmp = gather(admissions, key, value, admitted:applicants)
tmp

tmp2 = unite(tmp, column_name, c(key, gender))
tmp2

#import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(polls_us_election_2016, results_us_election_2016)

#join the murders table and us election results table
tab = left_join(murders, results_us_election_2016, by="state")
head(tab)

#plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes,label=abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = F)
  
#make two smaller tables to demonstrate joins
tab1 = slice(murders, 1:6) %>% select(state, population)
tab1
tab2 = results_us_election_2016 %>% filter(state %in% c("Alabama","Alaska","Arizona","California","Connecticut","Delaware"))%>% arrange(by=state) %>% select(state, electoral_votes)
tab2

#experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1,tab2)
semi_join(tab1,tab2)
anti_join(tab1,tab2)
full_join(tab1,tab2)


bind_cols(a = 1:3, b = 4:6)

tab1 = tab[,1:3]
tab2 = tab[,4:6]
tab3 = tab[,7:9]
new_tab = bind_cols(tab1,tab2,tab3)
head(new_tab)

tab1 = tab[1:2,]
tab2 = tab[3:4,]
bind_rows(tab1,tab2)

#intersect of data frames
intersect(1:10,6:15)
intersect(c("a","b","c"),c("b","c","d"))
tab1 = tab[1:5,]
tab2 = tab[3:7,]
intersect(tab1,tab2)

#perform a union of vectors or data frames
union(1:10,6:15)
union(c("a","b","c"),c("b","c","d"))
union(tab1,tab2)

#set difference of vectors or data frames
setdiff(1:10,6:15)
setdiff(c("a","b","c"),c("b","c","d"))
setdiff(tab1,tab2)

#setequal determines whether sets have the same elements regardless of order
setequal(1:5,1:6)
setequal(1:5,5:1)
setequal(tab1,tab2)

#2.2 Assessment
tab1 = murders %>% filter(state %in% c("Alabama","Alaska","Arizona","Delaware","District of Columbia")) %>% select(state, population)
tab1
tab2 = results_us_election_2016 %>% filter(state %in% c("Alabama","Alaska","Arizona","California","Connecticut","Colorado")) %>% select(state, electoral_votes)
tab2

left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1,tab2)
semi_join(tab1,tab2)
anti_join(tab1,tab2)
full_join(tab1,tab2)

install.packages("Lahman")
library(Lahman)
top = Batting %>%
  filter(yearID==2016) %>%
  arrange(desc(HR)) %>% #arrange by descending home run count
  slice(1:10)#take top 10
top %>%as_tibble()

Master %>% as_tibble()

awd = AwardsPlayers %>% filter(yearID == 2016)
left_join(top,awd)
nrow(anti_join(awd,top) %>% select(playerID) %>% unique())
