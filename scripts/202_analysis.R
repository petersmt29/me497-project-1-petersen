library(tidyverse)

df <- read_csv("results/tidy-experimental-data.csv")
constants <- read_csv("data/constants.csv")
D <- constants$value[1]/1000
L <- constants$value[2]/100
rho <- constants$value[3]
epsilon <- constants$value[4]/1000
nu <- constants$value[5]/1000

V <- 94.64e-6
g <- 9.81

df <- df %>%
  mutate(flow_si = flow*6.3e-5)

#calc friction factor(fac) and reynolds number(num)
df <- df %>%
  mutate(fac = (pi^2*D^5*g*(h1-h2))/(8*L*flow_si^2),
         num = (4*rho*flow_si)/(pi*nu*D))

#group mean friction factor and reynolds number by flow rate (1.5, 2.5, 3.5)
dfGrouped <- df %>% group_by(flow) %>% summarise(fac = mean(fac),
                                                 num = mean(num))

#make it pretty
dfGrouped <- cbind(c("Low", "Medium", "High"), dfGrouped)
colnames(dfGrouped) <- c("Flow rate level", "Flow rate (gpm)", "Mean friction factor", "Reynolds number")

write_csv(dfGrouped, "results/mean-fricfac-reynum.csv")
