library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

#Question 4a
cases_120 <- sum(esoph$ncases[which(esoph$alcgp == "120+")])
controls_120 <- sum(esoph$ncontrols[which(esoph$alcgp == "120+")])

p_120 <- ((cases_120)/(cases_120+controls_120))

#Question 4b
cases_0to39 <- sum(esoph$ncases[which(esoph$alcgp == "0-39g/day")])
controls_0to39 <- sum(esoph$ncontrols[which(esoph$alcgp == "0-39g/day")])

p_0to39 <- ((cases_0to39)/(cases_0to39+controls_0to39))

#Question 4c
cases_10to19 <- sum(esoph$ncases[which(esoph$tobgp == "10-19")])
cases_20to29 <- sum(esoph$ncases[which(esoph$tobgp == "20-29")])
cases_30 <- sum(esoph$ncases[which(esoph$tobgp == "30+")])

cases_10 <- (cases_10to19+cases_20to29+cases_30)/all_cases

#Question 4d
control_10to19 <- sum(esoph$ncontrols[which(esoph$tobgp == "10-19")])
control_20to29 <- sum(esoph$ncontrols[which(esoph$tobgp == "20-29")])
control_30 <- sum(esoph$ncontrols[which(esoph$tobgp == "30+")])

control_10 <- (control_10to19+control_20to29+control_30)/all_controls


#Question 5a
cases_120/all_cases

#Question 5b
cases_30 <- sum(esoph$ncases[which(esoph$tobgp == "30+")])
cases_30/all_cases

#Question 5c
highest_tob_alc <- sum(esoph$ncases[which(esoph$tobgp == "30+" & esoph$alcgp == "120+")])
highest_tob_alc/all_cases

#Question 5d
highest_tob_or_alc <- sum(esoph$ncases[which(esoph$tobgp == "30+" | esoph$alcgp == "120+")])
highest_tob_or_alc/all_cases
#alternative (ven-diagram):
(cases_120 + cases_30 - highest_tob_alc)/all_cases


#Question 6a
controls_120/all_controls

#Question 6b
(cases_120/all_cases)/(controls_120/all_controls)

#Question 6c
controls_30 <- sum(esoph$ncontrols[which(esoph$tobgp == "30+")])
controls_30/all_controls

#Question 6d
controls_alc_tob <- sum(esoph$ncontrols[which(esoph$tobgp == "30+" & esoph$alcgp == "120+")])
controls_alc_tob/all_controls

#Question 6e
(controls_120+controls_30-controls_alc_tob)/all_controls

#Question 6f
((cases_120 + cases_30 - highest_tob_alc)/all_cases)/((controls_120+controls_30-controls_alc_tob)/all_controls)