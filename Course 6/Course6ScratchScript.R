install.packages("tidyverse", "dslabs", "NHANES","ggthemes", "ggrepel","gridExtra", "dplyr","RColorBrewer","readxl","rvest","htmlwidgets")
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
library(rvest)
library(htmlwidgets)

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

url = "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h = read_html(url)
class(h)
h

tab = h %>% html_nodes("table")
tab = tab[[2]]

tab = tab %>% html_table()
class(tab)

tab = tab %>% setNames(c("state","population","total","murders","gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))
head(tab)

#For the guac recipe, the selectors have already been determined
h = read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe = h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time = h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients = h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

#You can see how complex the selectors are
guacamole = list(recipe,prep_time,ingredients)
guacamole

#Since recipes from this website follow this general layout, we can write a function that extracts the information
get_recipe = function(url){
  h = read_html(url)
  recipe = h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time = h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients = h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time,ingredients = ingredients))
  
}

#Testing function
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


#2.3 Assessment

url = "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h = read_html(url)

nodes = html_nodes(h, "table")

html_text(nodes[[8]])

html_table(nodes[[8]])

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

html_table(nodes[[21]])
html_table(nodes[[20]])
html_table(nodes[[19]])


tab_1 = html_table(nodes[[10]])
tab_1 = tab_1 %>% select(X2,X3,X4) %>% setNames(c("Team","Payroll","Average"))
tab_1 = tab_1[-1,]
tab_1
tab_2 = html_table(nodes[[19]])
tab_2 = tab_2 %>% setNames(c("Team","Payroll","Average"))
tab_2 = tab_2[-1,]
tab_2

full_join(tab_1,tab_2,"Team")

url = "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h = read_html(url)
tab = html_nodes(h, "table")

html_table(tab[[5]], fill=T)

url = "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw = read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] %>%
  setNames(c("state","population","total","murder_rate"))

head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

s = "Hello!" #double quotes defining a string
s = 'Hello!' #single quotes defining a string

s = '"Hello!' #double quote in single quot
cat(s)

s = "5'"
cat(s)

#To include both double and single quotes in a string, escape with \
s = '5\'10"'
cat(s)
s = "5'10\""
cat(s)

#detect whether there are any commas
commas = function(x) any(str_detect(x,","))
murders_raw %>% summarize_all(funs(commas))

#replace commas with the empty string and convert to numeric
test_1 = str_replace_all(murders_raw$population,",","")
test1 = as.numeric(test_1)
head(test_1)
class(test1)

#parse_number also removes commas and converts to numeric
test_2 = parse_number(murders_raw$population)
head(test_2)
class(test_2)

murders_new = murders_raw %>% mutate_at(2:3,parse_number)
murders_new %>% head()


#load raw heights data and inspect
data(reported_heights)
class(reported_heights$height)

#convert to numeric, inspect, count NAs
x = as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

#keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10)

#calculate cutoffs that calculate 99.99% of the human population
alpha = 1/10^6
qnorm(1-alpha/2,69.1,2.9)
qnorm(alpha/2,63.7,2.7)

#keep only entries that either results in NAs or are outside plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

#number of problematic entries
problems = reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

#10 examples of x'y or x'y" or x'y\"
pattern = "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems,pattern) %>% head(n=10) %>%cat

#10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#detect whether a comma is present
pattern = ","
str_detect(murders_raw$total,pattern)

#show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

#use the "or" symbol inside a regex (|)
yes = c("180 cm", "70 inches")
no = c("180","70''")
s = c(yes,no)
str_detect(s,"cm") | str_detect(s,"inches")
str_detect(s,"cm|inches")

#using \\d for digits
yes = c("5","6","5'10","5 feet", "4'11")
no = c("",".","Five","six")
s = c(yes,no)
pattern = "\\d"
str_detect(s,pattern)

#highlight occurrence of pattern
str_view(s,pattern)

#highlight all instances of pattern
str_view_all(s, pattern)


#using \\d for digits
yes = c("5","6","5'10","5 feet", "4'11")
no = c("",".","Five","six")
s = c(yes,no)
pattern = "\\d"

#[56] means 5 or 6
str_view(s,"[56]")

#[4-7] means 4, 5, 6, or 7
yes = as.character(4:7)
no = as.character(1:3)
s = c(yes,no)
str_detect(s,"[4-7]")

#^ means start of a string, $ means end of string
pattern = "^\\d$"
yes = c("1","5","9")
no = c("12","123","1","a4","b")
s = c(yes,no)
str_view(s,pattern)

#curly braces define quantifiers: 1 or 2 digits
pattern = "^\\d{1,2}$"
yes = c("1","5","9","12")
no = c("123","a4","b")
str_view(c(yes,no),pattern)

#combining character class, anchors and quantifier
pattern = "^[4-5]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)
