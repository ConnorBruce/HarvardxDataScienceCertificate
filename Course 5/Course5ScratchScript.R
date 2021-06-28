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

getwd()
url = "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dest_file = "Course 5/murders/data/murders.csv"
download.file(url,destfile=dest_file)

murders = read_csv("Course 5/murders/data/murders.csv")
murders = murders %>% mutate(region = factor(region), rate = total/population *10^5)
save(murders, file = "Course 5/murders/rda/murders.rda")

murders %>% mutate(abb = reorder(abb, rate)) %>%
  ggplot(aes(abb,rate)) +
  geom_bar(width = .5, stat = "identity", color = "black") +
  coord_flip()

ggsave("Course 5/murders/figs/barplot.png")
