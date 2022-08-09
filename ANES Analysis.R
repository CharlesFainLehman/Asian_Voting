require(tidyverse)
require(stringr)
require(stargazer)

anes <- read.csv("anes_timeseries_2020_csv_20220210.csv")

#V200010b = weight (post)
#V201018 = Party (pre)
#V201549x = Race/Hisp (pre)
#V201510 = Education (pre)
#V201617x = income Pre
#V202355 = urbanization (post)
#V201435 = religion (pre)
#V201507x = age (pre)
#V201554 = national origin

anes %>%
  select(V200010b, V201018, V201200, V201549x, V201510, V201617x, V202355, V201435, V201507x, V201554) %>%
  filter(V201018 >= -1,
         V201200 > 0,
         V201200 < 99,
         V201549x > 0,
         V201510 > 0,
         V201510 < 95,
         V201617x > 0,
         V202355 > 0,
         V201435 > 0,
         V201507x > -9,
         V201554 > -9) %>%
  mutate(Weight = V200010b,
         Democrat = V201018 == 1,
         Liberal = V201200 %in% 1:3,
         Race.Eth = factor(V201549x, labels = c("White", "Black", "Hispanic", "Asian", "AIAN", "Multiple")),
         Education = case_when(V201510  == 1 ~ "< H.S.",
                               V201510 == 2 ~ "H.S.",
                               V201510 %in% 3:5 ~ "Some Coll./A.A.",
                               V201510 == 6 ~ "Coll.",
                               V201510 %in% 7:8 ~ "Advanced"), #note linear
         HH.Income = factor(V201617x), #note linear
         Urbanization = factor(V202355, labels = c("Rural", "Small Town", "Suburb", "City")),
         Religion = factor(V201435),
         Age = V201507x,
         Foreign.Born = V201554 == 4
         ) %>%
  select(!c(V200010b, V201018, V201200, V201549x, V201510, V201617x, V202355, V201435, V201507x, V201554)) -> anes.summary

#### Analysis of full sample ####
democrat_model1 <- glm(Democrat ~ Race.Eth, "binomial", anes.summary, weights = Weight)
democrat_model2 <- glm(Democrat ~ Race.Eth + Education + HH.Income, "binomial", anes.summary, weights = Weight) 
democrat_model3 <- glm(Democrat ~ Race.Eth + Education + HH.Income + Urbanization + Age, "binomial", anes.summary, weights = Weight)
democrat_model4 <- glm(Democrat ~ Race.Eth + Education + HH.Income + Religion + Urbanization + Age, "binomial", anes.summary, weights = Weight)

liberal_model1 <- glm(Liberal ~ Race.Eth, "binomial", anes.summary, weights = Weight)
liberal_model2 <- glm(Liberal ~ Race.Eth + Education + HH.Income, "binomial", anes.summary, weights = Weight)
liberal_model3 <- glm(Liberal ~ Race.Eth + Education + HH.Income + Urbanization + Age, "binomial", anes.summary, weights = Weight)
liberal_model4 <- glm(Liberal ~ Race.Eth + Education + HH.Income + Religion + Urbanization + Age, "binomial", anes.summary, weights = Weight)

#Writes the final regression table out as HTML
stargazer(democrat_model1, democrat_model2, democrat_model3, democrat_model4,
          liberal_model1, liberal_model2, liberal_model3, liberal_model4,
          type = 'html', out = "table1.html", 
          title = "Table 1. Effect of Race on Registration/Ideology",
          dep.var.labels = c("Registered Democrat", "Ideological Liberal"),
          covariate.labels = c("Black", "Hispanic", "Asian", "AIAN", "Multiple Race", "Constant"),
          notes.label = "",
          notes = c("Source: 2020 American National Election Study"),
          notes.append = T,
          omit = c("Income|Education", "Urbanization|Age", "Religion"),
          omit.stat = c('aic', 'll'),
          add.lines =  list(c('<td colspan="9" style="border-bottom: 1px solid black">Controls</td>'),
                            c("Income/Education", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes"),
                            c("Urbanization/Age", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"),
                            c("Religion", "No", "No", "No", "Yes", "No", "No", "No", "Yes")))

### Analysis of foreign and native-born Asians ###

#constructing new ethnicity category
anes.summary %>%
  mutate(Race.Eth.2 = factor(case_when(Foreign.Born == T & Race.Eth == "Asian" ~ "Foreign-Born Asian",
                                Foreign.Born == F & Race.Eth == "Asian" ~ "Native-Born Asian",
                                T ~ as.character(Race.Eth)),
                             levels = c("White", "Black", "Hispanic", "Native-Born Asian", "Foreign-Born Asian", "AIAN", "Multiple"))) -> foreign.born.anes

fdemocrat_model1 <- glm(Democrat ~ Race.Eth.2, "binomial", foreign.born.anes, weights = Weight)
fdemocrat_model2 <- glm(Democrat ~ Race.Eth.2 + Education + HH.Income, "binomial", foreign.born.anes, weights = Weight) 
fdemocrat_model3 <- glm(Democrat ~ Race.Eth.2 + Education + HH.Income + Urbanization + Age, "binomial", foreign.born.anes, weights = Weight)
fdemocrat_model4 <- glm(Democrat ~ Race.Eth.2 + Education + HH.Income + Religion + Urbanization + Age, "binomial", foreign.born.anes, weights = Weight)

fliberal_model1 <- glm(Liberal ~ Race.Eth.2, "binomial", foreign.born.anes, weights = Weight)
fliberal_model2 <- glm(Liberal ~ Race.Eth.2 + Education + HH.Income, "binomial", foreign.born.anes, weights = Weight)
fliberal_model3 <- glm(Liberal ~ Race.Eth.2 + Education + HH.Income + Urbanization + Age, "binomial", foreign.born.anes, weights = Weight)
fliberal_model4 <- glm(Liberal ~ Race.Eth.2 + Education + HH.Income + Religion + Urbanization + Age, "binomial", foreign.born.anes, weights = Weight)

stargazer(fdemocrat_model1, fdemocrat_model2, fdemocrat_model3, fdemocrat_model4,
          fliberal_model1, fliberal_model2, fliberal_model3, fliberal_model4,
          type = 'html', out = "table2.html", 
          title = "Table 2. Foreign vs. Native-Born Asians (Compared to Whites)",
          dep.var.labels = c("Registered Democrat", "Ideological Liberal"),
          covariate.labels = c("Native-Born (N: 76)", "Foreign-Born (N: 121)"),
          keep = c("Asian"),
          notes.label = "",
          notes = c("Source: 2020 American National Election Study"),
          notes.append = T,
          omit = c("Income|Education", "Urbanization|Age", "Religion"),
          omit.stat = c('aic', 'll', 'n'),
          add.lines =  list(c('<td colspan="9" style="border-bottom: 1px solid black">Controls</td>'),
                            c("Income/Education", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes"),
                            c("Urbanization/Age", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes"),
                            c("Religion", "No", "No", "No", "Yes", "No", "No", "No", "Yes")))