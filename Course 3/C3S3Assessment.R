avg = .2*1-.8*.25
sd = 1.25*sqrt(.2*.8)*sqrt(44)
1-pnorm(8,avg, sd)

set.seed(21)
S = replicate(10000, {
  guess = sample(c(1,-.25),44,prob = c(.2,.8),replace = T)
  sum(guess)
})
mean(S>8)

.25*44
sqrt(44)*1*sqrt(.25*.75)

p=seq(.25,.95,.05)
avg2 = p*44
sd2 = sqrt(44)*sqrt(p*(1-p))
data.frame(p,100*(1-pnorm(35,avg2,sd2)))

p_win = 5/38
p_lose = 1-p_win
6*p_win - 1*p_lose
7*sqrt(p_win*p_lose)
7*sqrt(p_win*p_lose)/sqrt(500)
avg3 = (6*p_win - 1*p_lose)*500
sd3 = 7*sqrt(p_win*p_lose)*sqrt(500)
pnorm(0,avg3,sd3)
