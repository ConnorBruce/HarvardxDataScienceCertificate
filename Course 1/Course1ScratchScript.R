library(tidyverse)
library(dslabs)
library(dplyr)
data(murders)

murders %>%
  ggplot(aes(population, total, label = abb, color =region)) +
  geom_label()

a = 2
b = -1
c = -4

(-b + sqrt(b^2-4*a*c))/(2*a)
(-b - sqrt(b^2-4*a*c))/(2*a)

class(murders)
str(murders)
head(murders)
pop = murders$population
length(pop)

class(murders$state)

levels(murders$region)

log(1024,4)


codes = c(380,124,818)
country = c("italy", "canada", "egypt")

#Can assign name with each number
codes = c(italy = 380, canada = 124, egypt = 818)
codes
names(codes) = country

x = c(1,"canada",3)
x
x = 1:5
x
y = as.character(x)
y
as.numeric(y)
x = c("1", "b", "2")
as.numeric(x)

sort(murders$total)
order(murders$total)
index = order(murders$total)
murders$abb[index]

max(murders$total)
which.max(murders$total)
i_max = which.max(murders$total)
murders$state[i_max]

x = c(31,4,15,92,65)
sortx = sort(x)
orderx = order(x)
rankx = rank(x)
sorderank = matrix(c(x,sortx,orderx,rankx), ncol = 4)
colnames(sorderank) = c("original", "sort", "order", "rank")
rownames(sorderank) = c(" "," "," "," "," ")
sorderank

murder_rate = murders$total/murders$population*100000
murders$state[order(murder_rate, decreasing = T)]

index = murder_rate <= .71
index
murders$state[index]

west = murders$region == "West"
safe = murder_rate <= 1
index = safe & west
murders$state[index]

x = c(F,T,F,T,T,F)
which(x)
index = which(murders$state == "Massachusetts")
index
index = match(c("New York","Florida","Texas"), murders$state)
index
x = c("a","b","c","d","e")
y = c("a","d","f")
y %in% x

murders = mutate(murders, rate = total/population*100000)
head(murders)
filter(murders, rate <= .71)
new_table = select(murders, state, region, rate)
filter(new_table, rate <= .71)

murders %>% select(state, region, rate) %>% filter(rate <= .71)


grades = data.frame(names = c("John", "Juan", "Jean","Yao"),exam_1 = c(95,80,90,85),exam_2 = c(90,85,85,90), stringsAsFactors = F)
grades

x = murders$population / 10^6
y = murders$total
plot(x,y)

hist(murders$rate)

boxplot(rate~region, data=murders)

ind = which.min(murder_rate)
if(murder_rate[ind] < .5) {
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

a=0
ifelse(a > 0,1/a, NA)
a=5
ifelse(a > 0,1/a, NA)
a = c(0,1,2,-4,5)
ifelse(a > 0,1/a, NA)

data(na_example)
sum(is.na(na_example))
no_nas = ifelse(is.na(na_example),0,na_example)

z = c(T,T,F)
z2 = c(F,F,F)
z3 = c(T,T,T)
any(z)
all(z)
any(z2)
all(z2)
any(z3)
all(z3)

avg = function(x){
  s = sum(x)
  n = length(x)
  s/n
}
avg(c(1,2,3,4,5,6,7))
x = 1:100
identical(mean(x),avg(x))

avg2 = function(x, arithmetic = TRUE){
  n = length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg2(1:10)
avg2(1:10, F)


compute_s_n = function(n){
  x = 1:n
  sum(x)
}
compute_s_n(3)
compute_s_n(100)

for(i in 1:5){
  print(i)
}
i

m = 25
#create an empty vector
s_n = vector(length=m)
for(n in 1:m){
  s_n[n]=compute_s_n(n)
}
n = 1:m
plot(n,s_n)
s_n
lines(n,(n*(n+1))/2)

altman_plot = function(x,y){
  plot(y-x,x+y)
}
altman_plot(c(1,2,3,4,5),c(3,2,4,5,1))
