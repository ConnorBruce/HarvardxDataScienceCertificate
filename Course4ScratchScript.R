install.packages("tidyverse", "dslabs", "NHANES","ggthemes", "ggrepel","gridExtra", "dplyr","RColorBrewer")
library(tidyverse)
library(dslabs)
library(NHANES)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(gtools)

take_poll(25)

X_hat = .48
se = sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

p = .45 #unknown p to estimate
N = 1000

#simulate one poll of size N and determine x_hat
x = sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
x_hat = mean(x)

#Simulate B polls of size N and determine average x_hat
B = 10000 #number of replicates
N = 1000  #sample size per replicate
x_hat = replicate(B,{
                  x = sample(c(0,1), size = N, replace = T, prob = c(1-p,p))
                  mean(x)
                  })
p1 = data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = .005, color = "black")
p2 = data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample=x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline()+
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1,p2,nrow = 1)

N = 100000
p = seq(.35, .65, length = 100)
SE = sapply(p,function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p,SE=SE) %>%
  ggplot(aes(p,SE)) +
  geom_line()

N = seq(100,5000, len = 100)
p = .5
se = sqrt(p*(1-p)/N)
data.frame(N=N, se=se) %>%
  ggplot(aes(N,se)) + geom_line()

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

p = .45
N = 1000
X = sample(c(0,1), size = N, replace = T, prob = c(1-p,p))
X_hat = mean(X)
SE_hat = sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

z = qnorm(.995)
pnorm(qnorm(.995))
pnorm(qnorm(1-.995))
pnorm(z)-pnorm(-z)

B = 10000
inside = replicate(B, {
  X = sample(c(0,1), size = N, replace = T, prob = c(1-p,p))
  X_hat = mean(X)
  SE_hat = sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - qnorm(.975)*SE_hat, X_hat + qnorm(.975)*SE_hat)
})
mean(inside)

N = 25
X_hat = .48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N))

N = 100
z = sqrt(N) *.02/.5
1 - (pnorm(z)-pnorm(-z))

data(polls_us_election_2016)
polls = polls_us_election_2016 %>% filter(state == "U.S." & enddate >= 2016-10-31)
polls

d = .039
Ns = c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p = (d+1)/2

#calculate confidence intervals of the spread
confidence_intervals = sapply(Ns, function(N){
  X = sample(c(0,1), size = N, replace = T, prob = c(1-p,p))
  X_hat = mean(X)
  SE_hat = sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

#generate a data frame storing results
polls = data.frame(poll = 1:ncol(confidence_intervals),
                   t(confidence_intervals), sample_size = Ns)
names(polls) = c("poll", "estimate","low","high","sample_size")
polls

d_hat = polls %>%
  summarize(avg = sum(estimate*sample_size)/sum(sample_size)) %>%
  .$avg
p_hat = (1+d_hat)/2
moe = 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
round(d_hat*100,1)
round(moe*100,1)

library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

#keep only national polls from week before election with a grade considered reliable
polls = polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & (grade %in% c("A+","A","A-","B+")|is.na(grade)))

#add spread estimate
polls = polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#compute estimated spread for combined polls
d_hat = polls %>%
  summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>%
  .$d_hat

#compute margin of error
p_hat = (d_hat+1)/2
moe = 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

#histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

#number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

#plot results by pollster with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>% summarize(se = 2*sqrt(p_hat*(1-p_hat) /median(samplesize)))

#collect last results before the election for each pollster
one_poll_per_pollster = polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%  #keep latest poll
  ungroup()

#histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = .01)

#construct 95% confidence interval
results = one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg +1.96*se)
round(results*100,1)

prev = .00025 #disease prevalence
N = 100000 #number of tests
outcome = sample(c("Disease","Healthy"),N,replace = T, prob = c(prev, 1-prev))

N_D = sum(outcome == "Disease") #number with disease
N_H = sum(outcome == "Healthy") #number healthy

#for each person, randomly determine if the test is + or -
accuracy = .99
test = vector("character", N)
test[outcome=="Disease"] = sample(c("+","-"), N_D, replace = T, prob = c(accuracy,1-accuracy))
test[outcome=="Healthy"] = sample(c("-","+"), N_H, replace = T, prob = c(accuracy,1-accuracy))

table(outcome,test)

polls = polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
         (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100-rawpoll_trump/100)

one_poll_per_pollster = polls %>% group_by(pollster) %>% filter(enddate == max(enddate)) %>% ungroup()

results = one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
results

mu = 0
tau = .035
Y = results$avg
sigma = results$se
B = sigma^2/(sigma^2+tau^2)
posterior_mean = B*mu + (1-B)*Y
posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se


#95% credible interval
posterior_mean + c(-1.96,1.96)*posterior_se

#probability of d>0
1 - pnorm(0,posterior_mean,posterior_se)

J = 6
N = 2000
d = .021
p = (d+1)/2
X = d +rnorm(J,0,2*sqrt(p*(1-p)/N))
X

I = 5
J = 6
N = 2000
d = .021
p = (d+1)/2
X = sapply(1:I, function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p)/N))
})
X

mu = 0
tau = .035
sigma = sqrt(results$se^2 + .025^2) #sigma includes an estimate of the variability
Y = results$avg
B = sigma^2/(sigma^2 + tau^2)

posterior_mean = B*mu + (1-B)*Y
posterior_se = sqrt(1/(1/sigma^2+1/tau^2))

1-pnorm(0,posterior_mean,posterior_se)


I = 5
J = 6
N = 2000
d = .021
p = (d+1)/2
h = rnorm(I,0,.025) #assume standard error of pollster-to-pollster variability is .025
X = sapply(1:I, function(i){
  d + h[i] + rnorm(J,0,2*sqrt(p*(1-p)/N))
})
X

results = polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

#10 closest races
results %>% arrange(abs(avg))

#joining electoral college votes and results
results = left_join(results,results_us_election_2016,by="state")

#states with no polls
results_us_election_2016 %>% filter(!state %in% results$state)

#assigns sd to states with just one poll as median of other sd values
results = results %>%
  mutate(sd=ifelse(is.na(sd),median(results$sd,na.rm=T),sd))

mu = 0
tau = .02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/(sigma^2+tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)))%>%
  arrange(abs(posterior_mean))

mu = 0 
tau = .02
clinton_EV = replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/(sigma^2+tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes,0)) %>% #award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>% #total votes for Clinton
    .$clinton +7 #7 votes for RI and DC
})
mean(clinton_EV >269) #269 votes win election

#histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

#histogram
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#select all national polls by one pollster
one_pollster = polls_us_election_2016 %>%
  filter(pollster == "Ipsos"& state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#the observed standard error is higher than theory predicts
se = one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

#the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = .01, color = "black")

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

polls_us_election_2016 %>%
  filter(state=="U.S.", enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color=candidate)) +
  geom_point(show.legend = FALSE, alpha = .4) +
  geom_smooth(method = "loess", span = .15) +
  scale_y_continuous(limits = c(30,50))

z = qt(.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z * sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

#quantile from t-dist vs normal dist
qt(.975,14) #14 dof
qnorm(.975)

data(research_funding_rates)
research_funding_rates

#compute totals that were successful or not successful
totals = research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

#compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

tab = matrix(c(3,1,1,3),2,2)
rownames(tab) = c("Poured Before", "Poured After")
colnames(tab) = c("Guessed Before", "Guessed After")
tab

#p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#compute overall funding rate
funding_rate = totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

#construct two-by-two table for observed data
two_by_two = tibble(awarded = c("no","yes"),
                    men = c(totals$no_men, totals$yes_men),
                    women = c(totals$no_women, totals$yes_women))
two_by_two

#compute null hypothesis two-by-two table
tibble(awarded = c("no","yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

#chi-sq test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

odds_men = (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))
odds_women = (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

odds_men/odds_women

two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()
