library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

n = 1000
p = death_prob$prob[171]
l = -150000
g = 1150
mu = p*l+(1-p)*g
mu

sig = abs(-150000 - 1150)*sqrt(p*(1-p))
sig

mu2 = mu*n
sig2 = sig*sqrt(n)

1-pnorm(mu2/sig2)


p2 = death_prob$prob[51]
g2 = ((700000/1000)-(-150000)*p2)/(1-p2)
mu3 = -150000*p2 + g2*(1-p2)
sig3 = abs(l-g2)*sqrt(p2*(1-p2))
sig4 = sig3*sqrt(n)
sig4
mu4 = mu3*n
mu4/sig4
1- pnorm(mu4/sig4)

n = 1000
p = .015
a = -150000
b = 1150

mu = a*p+b*(1-p)
mus = mu * n

sig = abs(a-b)*sqrt(p*(1-p))
sigs = sqrt(n) * sig

1-pnorm((mus + 1000000)/sigs)

plist = seq(.01,.03,.001)
mulist = 1000*(a*plist + b*(1-plist))
siglist = sqrt(n)*abs(a-b)*sqrt(plist*(1-plist))
problist = 1-pnorm(mulist/siglist)
data.frame(plist,problist)

plist2 = seq(.01,.03,.0025)
mulist = 1000*(a*plist2 + b*(1-plist2))
siglist = sqrt(n)*abs(a-b)*sqrt(plist2*(1-plist2))
problist2 = 1-pnorm((mulist+1000000)/siglist)
data.frame(plist2,problist2)

p = .015
a = -150000
b = 1150
set.seed(25)
sum(sample(c(a,b),n,prob = c(p,1-p),replace = T))/10^6

set.seed(27)
S = replicate(10000, {
  profits = sample(c(a,b),n,prob = c(p,1-p),replace = T)
  sum(profits)
})
mean(S < -1000000)

z = qnorm(.05)
p = .015
n = 1000
l = -150000
x = x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
l*p + x*(1-p)

S = replicate(10000, {
  profits = sample(c(l,x),n,prob=c(p,1-p),replace = T)
  sum(profits)
})
mean(S<0)

set.seed(29, sample.kind = "Rounding")
S = replicate(10000, {
  p2 = p+sample(seq(-.01,.01,length = 100),1)
  profits = sample(c(l,x),n,prob=c(p2,1-p2),replace=T)
  sum(profits)
})
mean(S < -1000000)
