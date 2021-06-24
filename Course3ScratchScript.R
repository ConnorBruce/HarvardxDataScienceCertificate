library(tidyverse)
library(dslabs)
library(NHANES)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(RColorBrewer)
library(gtools)


beads = rep(c("red","blue"), times = c(2,3)) #create an container with 2 red and 3 blue beads
sample(beads,1) #sample one bead at random

B = 10000 #number of times to draw a bead
events = replicate(B, sample(beads,1))
tab = table(events)
tab
prop.table(tab)

number = "Three"
suit = "Hearts"
paste(number, suit)

paste(letters[1:5], as.character(1:5))

expand.grid(pants = c("blue","black"), shirt = c("white","grey", "plaid"))

suits = c("Diamonds", "Clubs", "Hearts", "Spades")
numbers = c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck = expand.grid(number = numbers, suit = suits)
deck = paste(deck$number, deck$suit)
deck

#probability of drawing a king
kings = paste("King", suits)
mean(deck %in% kings)

permutations(5,2) #ways to choose 2 numbers in order from 1:5
all_phone_numbers = permutations(10,7, v = 0:9)
n = nrow(all_phone_numbers)
index = sample(n,5)
all_phone_numbers[index,]

permutations(3,2) #order matters
combinations(3,2) #order doesn't matter

hands = permutations(52, 2, v = deck)
first_card = hands[,1]
second_card = hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings)/sum(first_card %in% kings)

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands = combinations(52,2,v=deck)

#probability of natural 21 given that ace is first in combination
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#probability of natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))


hand = sample(deck, 2)
hand

B = 10000
results = replicate(B, {
  hand = sample(deck,2)
  (hand[1] %in%aces & hand[2] %in% facecard)  |(hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

n = 50
bdays = sample(1:365, n, replace = T)
any(duplicated(bdays))

B = 10000
results = replicate(B , {
  bdays = sample(1:365, n, replace = T)
  any(duplicated(bdays))
})
mean(results)


compute_prob = function(n, B=10000) {
  same_day = replicate(B, {
    bdays = sample(1:365, n, replace = T)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n = seq(1,60)
prob = sapply(n, compute_prob)
plot(n, prob)

exact_prob = function(n){
  prob_unique = seq(365, 365-n+1)/365
  1 - prod(prob_unique)
}
eprob = sapply(n, exact_prob)

plot(n, prob)
lines(n, eprob, col = "red")

B = 10^seq(1,5,len = 100)
compute_prob = function(B, n = 22){
  same_day = replicate(B, {
    bdays = sample(1:365, n , replace = T)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob = sapply(B, compute_prob)
plot(log10(B), prob, type = "l")

B = 10000
stick = replicate(B, {
  doors = as.character(1:3)
  prize = sample(c("car","goat","goat"))
  prize_door = doors[prize == "car"]
  my_pick = sample(doors,1)
  show = sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick = my_pick
  stick == prize_door
})
mean(stick)

B = 10000
switch = replicate(B, {
  doors = as.character(1:3)
  prize = sample(c("car","goat","goat"))
  prize_door = doors[prize == "car"]
  my_pick = sample(doors,1)
  show = sample(doors[!doors %in% c(my_pick, prize_door)], 1) 
  switch = doors[!doors%in%c(my_pick, show)] 
  switch == prize_door
})
mean(switch)

x = heights %>% filter(sex=="Male") %>% pull(height)
F = function(a) mean(x<=a)
1-F(70)

x = heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x))

#plot dist of exact heights
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

#probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

#probabilities in normal approx match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

#probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

x = seq(-4,4,length = 100)
data.frame(x, f=dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

x = heights %>% filter(sex == "Male") %>% pull(height)

#generate simulated height data using normal dist - both datasets with n observations
n = length(x)
avg = mean(x)
s = sd(x)
simulated_heights = rnorm(n,avg,s)

#plot dist of simulated heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

B = 10000
tallest = replicate(B, {
  simulated_data = rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 7*12)


#sampling model 1: define urn then sample
color = rep(c("Black","Red","Green"), c(18,18,2)) #define the urn for the sampling model
n = 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
sum(X)

#sampling model 2: define urn inside sample function by noting probabilities
x = sample(c(-1,1), n, replace = T, prob = c(9/19, 10/19))
S = sum(x)
S


n = 1000 #number of roulette players
B = 10000 #number of Monte Carlo experiments
S = replicate(B, {
  X = sample(c(-1,1),n,replace = T, prob = c(9/19,10/19)) #simulate 1000 spins
  sum(X) #determine total profit
})

mean(S<0) #probability of casino losing money

s = seq(min(S), max(S), length = 100)
normal_density = data.frame(s=s, f=dnorm(s,mean(S), sd(S)))
data.frame(S=S) %>%
  ggplot(aes(S, ..density..)) +
  geom_histogram(color="black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s,f), color="blue")

n = 1000
loss_per_foreclosure = -200000
p = .02
defaults = sample(c(0,1),n,prob=c(1-p,p),replace = T)
sum(defaults*loss_per_foreclosure)

B = 10000
losses = replicate(B, {
  defaults = sample(c(0,1), n, prob = c(1-p,p), replace=T)
  sum(defaults*loss_per_foreclosure)
})

data.frame(losses_in_millions=losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = .6, col = "black")

n*(p*loss_per_foreclosure + (1-p)*0) #Expected Value
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p)) #Standard Error

x = -loss_per_foreclosure*p/(1-p)
x
rate = x/180000
rate

l = loss_per_foreclosure
z = qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
rate = x/180000
rate
loss_per_foreclosure*p + x*(1-p) #Expected value of the profit per loan
n*(loss_per_foreclosure*p+x*(1-p)) #Expected profit over n loans

B=100000
profit = replicate(B, {
  draws = sample(c(x,loss_per_foreclosure), n, prob = c(1-p,p), replace = T)
  sum(draws)
})
mean(profit) #expected value of profit over n loans
mean(profit<0) # probability of losing money

p = .04
loss_per_foreclosure = -200000
r = .05
x = r*180000
loss_per_foreclosure*p + x*(1-p)

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
