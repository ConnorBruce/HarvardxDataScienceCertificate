# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N = 1500

N*p
sqrt(N*p*(1-p))

sqrt(p*(1-p)/N)
2*sqrt(p*(1-p)/N)
d

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$spread)
sd(brexit_polls$x_hat)

brexit_polls[1,]
brexit_polls = brexit_polls %>%
  mutate(se_hat = sqrt(x_hat*(1-x_hat)/samplesize))
brexit_polls[1,]$x_hat - qnorm(.975)*brexit_polls[1,]$se_hat
brexit_polls[1,]$x_hat + qnorm(.975)*brexit_polls[1,]$se_hat


# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2) %>%
  mutate(se_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate(lower = spread - qnorm(.975)*2*se_hat) %>%
  mutate(upper = spread + qnorm(.975)*2*se_hat)
  

# final proportion voting "Remain"
p <- 0.481

june_polls = brexit_polls %>% filter(enddate > "2016-06-01")
nrow(june_polls)
mean(june_polls$lower <= -.038 & june_polls$upper >=-.038)


june_polls %>% group_by(pollster) %>%
summarize(hit_rate = between(-.038,lower,upper)/n()) %>%
  arrange(hit_rate)

june_polls = june_polls %>% group_by(pollster) %>%
  summarize(hit_rate = between(-.038,lower,upper)/n())

june_polls %>% group_by(poll_type) %>% ggplot(aes(poll_type,spread)) + geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type %>% mutate(se_hat = sqrt(p_hat*(1-p_hat)/N), 
                            lower = spread - qnorm(.975)*2*se_hat,
                            upper = spread + qnorm(.975)*2*se_hat)
combined_by_type

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit) 


t1 = brexit_hit %>% group_by(poll_type) %>% summarize(n())
t1[1,2]
t2 = brexit_hit %>% group_by(poll_type) %>% summarize(mean(hit))
t2
tbt = tibble(hit = c("yes","no"),
             online = c(48,37),
             telephone = c(10,32))
tbt
chisq = tbt %>%
  select(-hit) %>%
  chisq.test()
chisq$p.value

((48/85)/(37/85))/((10/42)/(32/42))

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote)) %>%
  group_by(vote) %>%
  ggplot(aes(enddate, proportion, color = "poll_type")) +
  geom_smooth(method = "loess", span = .3) +
  geom_point()
brexit_long
