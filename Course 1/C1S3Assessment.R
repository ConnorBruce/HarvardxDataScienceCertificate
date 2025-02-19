library(dslabs)
data(heights)
options(digits = 3)
head(heights)
ind = which(heights$height > mean(heights$height))
length(ind)
ind2 = which(heights$height > mean(heights$height) & heights$sex == "Female")
length(ind2)
ind3 = which(heights$sex == "Female")
length(ind3)/length(heights$sex)
min(heights$height)
match(50,heights$height)
heights$sex[1032]
max(heights$height)
x = min(heights$height):max(heights$height)
x
sum(!x %in% heights$height)
heights = mutate(heights, ht_cm = heights$height * 2.54)
heights$ht_cm[18]
mean(heights$ht_cm)
females = filter(heights, sex == "Female")
length(females$sex)
mean(females$ht_cm)

library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data = olive)
