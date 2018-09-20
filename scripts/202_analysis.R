library(tidyverse)

df <- read_csv("results/tidy-experimental-data.csv")
constants <- read_csv("data/constants.csv")

#what is
V <- 1
g <- 1

#calc friction factor and reynolds number
df <- df %>%
  mutate(fac = (pi^2*constants$value[1]^5*g*(h1-h2))/(8*constants$value[2]*V^2),
         num = (4*constants$value[3]*V)/(pi*constants$value[5]*constants$value[1]))

#group mean friction factor and reynolds number by flow rate (1.5, 2.5, 3.5)
dfGrouped <- df %>% group_by(flow) %>% summarise(fac = mean(fac),
                                                 num = mean(num))

#make it pretty
dfGrouped <- cbind(c("Low", "Medium", "High"), dfGrouped)
colnames(dfGrouped) <- c("Flow rate level", "Flow rate (gpm)", "Mean friction factor", "Reynolds number")

write_csv(dfGrouped, "results/mean-fricfac-reynum.csv")
