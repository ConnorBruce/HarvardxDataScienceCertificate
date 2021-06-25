install.packages("RColorBrewer")
library(tidyverse)
library(dslabs)
library(NHANES)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
data(heights)
head(heights)

prop.table(table(heights$sex))


a <- seq(min(heights$height), max(heights$height), length = 100)    
heightcdf_function <- function(x) {    
  mean(heights$height <= x)
}
heightcdf_values <- sapply(a, heightcdf_function)
lines(a, heightcdf_values)
heightcdf_function(72)

library(tidyverse)
library(dslabs)
data(heights)
index = heights$sex=="Male"
x = heights$height[index]

average = sum(x)/length(x)
sd = sqrt(sum((x - average)^2)/length(x))

average2 = mean(x)
sd2 = sd(x)

c(average = average2, sd = sd2)

z = scale(x)
head(z)
mean(abs(z) <2)


1-pnorm(70.5,mean(x),sd(x))

plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x=a)")

mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

summary(heights$height)
p = seq(.01,.99,.01)
percentiles = quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
quantile(heights$height, .12)

index = heights$sex == "Male"
x = heights$height[index]
z = scale(x)
p = seq(.05,.95,.05)
obs_quant = quantile(x,p)
theo_quant = qnorm(p, mean(x),sd(x))
plot(theo_quant, obs_quant)
abline(0,1)

library(tidyverse)
library(dslabs)
data(murders)

p = ggplot(data=murders) # Method 1
p = murders %>% ggplot() # Method 2
class(p)
p

murders %>% ggplot() + geom_point(aes(x=population/10^6, total))

p = ggplot(data=murders)
p + geom_point(aes(population/10^6, total)) + geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))


# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)

# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "Region")

install.packages("ggthemes")
install.packages("ggrepel")
library(ggrepel)
library(ggthemes)
ds_theme_set()
p + theme_economist()
p + theme_fivethirtyeight()

murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

library(dslabs)
data(heights)
p = heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x=height))

p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")


#Basic qq plot
p = heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

#qq plot with norams dist
params = heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) + geom_abline()

#qq plots of scaled data against the standard normal
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


install.packages("gridExtra")
library(gridExtra)
p = heights %>% filter(sex == "Male") %>% ggplot(aes(x=height))
p1 = p +geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 = p +geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 = p +geom_histogram(binwidth = 3, fill = "blue", col = "black")

grid.arrange(p1,p2,p3, ncol = 3)

data(murders)
print(ggplot(murders))
library(tidyverse)

p = heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x=height))

data(heights)
p + geom_density(fill = "blue")

heights %>% ggplot(aes(height, group = sex, color = sex, fill = sex))

heights %>%
  filter(sex == "Male") %>%
  summarize(range = quantile(height, c(0, 0.5, 1)))

#Group heights by male and female
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

#Group median murders by region
murders = murders %>%
  mutate(murder_rate = total/population*100000)
murders %>%
  group_by(region) %>%
  summarize(mediate_rate = median(murder_rate))

murders = murders %>%
  mutate(murder_rate = total/population *100000)

#Arrange by population column
murders %>% arrange(population) %>% head()

#Arrange by murder rate
murders %>% arrange(murder_rate) %>% head()

#Arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

#Arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

#Show the top 10 states with the highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

#Show the top 10 states with the highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)


install.packages("NHANES")
library(NHANES)
data(NHANES)
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
mean(na_example, na.rm = T)
sd(na_example, na.rm = T)

library(tidyverse)
library(dslabs)
data(gapminder)
head(gapminder)

gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

filter(gapminder, year == 2015) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

years = c(1962, 1970, 1980, 1990, 2000, 2012)
continents = c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

install.packages("NHANES", "tidyverse")


gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries = c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, group = country, col = country)) +
  geom_line()

labels = data.frame(country = countries, x = c(1975,1965), y = c(60,72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data=labels, aes(x,y, label = country), size = 5) +
  theme(legend.position = "none")

gapminder = gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#histogram of dollars per day
past_year = 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

#repeat with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

#repeat with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

gapminder = gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#number of regions
length(levels(gapminder$region))

past_year = 1970
p = gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

#rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

fac = factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

value = c(10, 11, 12, 6, 4)
fac = reorder(fac, value, FUN = mean)
levels(fac)

p = gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

#log2 scale y-axis and add points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = F)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

present_year = 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Get countries that have data in both 1970 and 2010
country_list_1 = gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 = gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list = intersect(country_list_1, country_list_2)

#Remake histogram with these countries
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

p = gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p +geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~.)

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()


p = gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill=group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = .2) + facet_grid(year ~.)

#Add group as a factor and group the regions
gapminder = gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

#reorder region factor levels
gapminder = gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#Stacked density plot
p = gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = .2, bw = .75, position = "stack") +
  facet_grid(year ~ .)

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = .2, bw = .75, position = "stack") +
  facet_grid(year ~.)

  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia", 
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income = gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(.875, .9981), breaks = c(.85,.9,.95,.99,.995,.998)) +
  geom_label(size = 3, show.legend = F)

heights %>% ggplot(aes(sex,height)) + geom_point()

#jittered plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = .1, alpha = .2)

color_blind_friendly_cols = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 = data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x,y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat = gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + .22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color=country), show.legend = F) +
  geom_text(aes(x=location, label = country, hjust = hjust), show.legend = F) +
  xlab("") +
  ylab("Life Expectancy")

library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

data(us_contagious_diseases)
str(us_contagious_diseases)

the_disease = "Measles"
dat = us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count/population *10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state,rate))

dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

avg = us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = T)/sum(population, na.rm = TRUE)*10000)

dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = F, alpha = .2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x,y,label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, color = "blue")

install.packages("titanic")
library(titanic)
?titanic_train
titanic = titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% ggplot(aes(Age, y =..count.., fill = Sex)) +
  geom_density(alpha = .4)

params = titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_bar(position = position_dodge())

titanic %>% filter(Fare != 0) %>%
  ggplot(aes(Fare, Survived)) +
  geom_boxplot()

titanic %>% ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>% ggplot(aes(Age, fill = Survived, y = ..count..)) +
  geom_density(alpha = .5) +
  facet_grid(Sex ~ Pclass)

data(stars)
options(digits = 3)
?stars
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)

stars %>%
  ggplot(aes(magnitude)) +
  geom_density()
stars %>%
  ggplot(aes(temp)) +
  geom_density()

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()

stars %>%
  filter(temp > 5000) %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_text(aes(label = star))

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_text(aes(label = star))
 
stars %>%
  ggplot(aes(temp, magnitude, color = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_label(aes(label = type))


data(temp_carbon)
data(greenhouse_gas)
data(historic_co2)
temp_carbon

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max() #works
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max() #works
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(year) #doesn't work
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max() #works
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year) #doesn't work

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% min()
temp_carbon$carbon_emissions[temp_carbon$year == 2014] / temp_carbon$carbon_emissions[temp_carbon$year == 1751]


temp_carbon %>% filter(!is.na(temp_anomaly)) %>% select(year) %>% min()
temp_carbon %>% filter(!is.na(temp_anomaly)) %>% select(year) %>% max()
temp_carbon$temp_anomaly[temp_carbon$year == 2018] - temp_carbon$temp_anomaly[temp_carbon$year == 1880]

p = temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) + geom_point()

p = p + geom_hline(aes(yintercept = 0), col = "blue")

p = p + ylab("Temperature Anomaly (degrees C)") + ggtitle("Temper Anomaly Relative to 20th Century Mean, 1880-2018") +geom_text(aes(x=2000, y = .05, label = "20th Century Mean"), col = "blue")

p + geom_line(aes(year, ocean_anomaly), color = "green") + geom_line(aes(year, land_anomaly), color = "red")

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept = 1850))

temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

co2_time = historic_co2 %>%
  ggplot(aes(year, co2)) +
  geom_line(aes(color=source))

co2_time + xlim(-800000, -775000)
co2_time + xlim(-375000,-330000)
co2_time + xlim(-140000,-120000)
co2_time + xlim(-3000,2018)
