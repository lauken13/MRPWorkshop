# More with survey weights

library(survey)
library(srvyr)
library(tidyverse)

clean_data  <- readRDS("../data/cleaned_data_mrp.rds")
popn_ps <- readRDS("../data/acs_ps.rds")


################################################################################
#### First create some simple poststratification weights                  ######
################################################################################

# set inital design

init_design <- svydesign(id = ~1, data = clean_data, 
                       weights = ~rep(1,nrow(clean_data)))

# create population margins from the poststrat matix
female_df <- popn_ps %>%
  group_by(female) %>%
  summarise(Freq = sum(N))%>%
  ungroup()
agegroup_df <- popn_ps %>%
  group_by(age_group) %>%
  summarise(Freq = sum(N))%>%
  ungroup()

rkd_wts_obj <- rake(
  design = init_design,
  sample.margins = list(~female,~age_group), # sample margins (see next slide)
  population.margins = list(female_df, agegroup_df)
)

hist(weights(rkd_wts_obj))

tmd_wts <- trimWeights(rkd_wts_obj, upper = quantile(weights(rkd_wts_obj),.95))     

hist(weights(tmd_wts))

################################################################################
####                 Use weights to make population total estimates        #####
################################################################################

# Using the survey package:

svydsn_obj <- svydesign(id = ~1, data = clean_data, 
                         weights = ~ weights(tmd_wts))

svytotal(~hand, svydsn_obj)

svymean(~N, svydsn_obj)

# With the design effect


svytotal(~hand, svydsn_obj, deff = TRUE)

svymean(~N, svydsn_obj, deff = TRUE)

# Using svyr, the tidverse wrapper of survey

# Good resource: https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html

# add weights to dataframe

clean_data$wts = weights(tmd_wts)

design_srvyr <- clean_data %>% 
  as_survey_design(ids = 1, wts = wts)

#Calculate proportions

design_srvyr %>% 
  group_by(hand)%>%
  summarise(prop_female = survey_mean(female) )%>%
  ungroup()
  mutate

# Just the totals 
  
design_srvyr %>%
  group_by(hand) %>%
  summarise(n_hand = survey_total())%>%
  ungroup()

