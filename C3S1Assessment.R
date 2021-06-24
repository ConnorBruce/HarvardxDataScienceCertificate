library(gtools)
library(tidyverse)

nrow(permutations(8,3))
nrow(permutations(3,3))
(3/8)^3
8^3

set.seed(1)
B=10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
races = replicate(B, {
  race = sample(runners, 3)
  race
  !("USA" %in% race | "Ecuador" %in% race | "Netherlands" %in% race | "France" %in% race | "South Africa" %in% race)
})
mean(races)

6*nrow(combinations(6,2))*2
6*nrow(combinations(6,2))*3
6*nrow(combinations(6,3))*3

choicesentree = function(n){
  n * nrow(combinations(6,2))*3
} 
sapply(1:12, choicesentree)

choicessides = function(n){
  6*nrow(combinations(n,2))*3
}
sapply(2:12, choicessides)

head(esoph)
nrow(esoph)
all_cases = sum(esoph$ncases)
all_cases
all_controls = sum(esoph$ncontrols)
all_controls
levels(esoph$alcgp)
sum(esoph$ncases[esoph$alcgp == "120+"])/(sum(esoph$ncontrol[esoph$alcgp == "120+"])+sum(esoph$ncases[esoph$alcgp == "120+"]))
sum(esoph$ncases[esoph$alcgp == "0-39g/day"])/(sum(esoph$ncontrol[esoph$alcgp == "0-39g/day"])+sum(esoph$ncases[esoph$alcgp == "0-39g/day"]))

sum(esoph$ncases[esoph$tobgp != "0-9g/day"])/all_cases
sum(esoph$ncontrols[esoph$tobgp != "0-9g/day"])/all_controls

sum(esoph$ncases[esoph$alcgp == "120+"])/all_cases
sum(esoph$ncases[esoph$tobgp == "30+"])/all_cases
sum(esoph$ncases[esoph$tobgp == "30+" & esoph$alcgp == "120+"])/all_cases
sum(esoph$ncases[esoph$tobgp == "30+" | esoph$alcgp == "120+"])/all_cases

sum(esoph$ncontrols[esoph$alcgp == "120+"])/all_controls
(sum(esoph$ncases[esoph$alcgp == "120+"])/all_cases)/(sum(esoph$ncontrols[esoph$alcgp == "120+"])/all_controls)
sum(esoph$ncontrols[esoph$tobgp == "30+"])/all_controls
sum(esoph$ncontrols[esoph$tobgp == "30+" & esoph$alcgp == "120+"])/all_controls
sum(esoph$ncontrols[esoph$tobgp == "30+" | esoph$alcgp == "120+"])/all_controls
(sum(esoph$ncases[esoph$tobgp == "30+" | esoph$alcgp == "120+"])/all_cases)/(sum(esoph$ncontrols[esoph$tobgp == "30+" | esoph$alcgp == "120+"])/all_controls)
