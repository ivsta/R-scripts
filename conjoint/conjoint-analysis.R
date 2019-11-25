# http://keii.ue.wroc.pl/pracownicy/tb/Bak_A_and_Bartlomowicz_T_Conjoint_analysis_method_and_its_implementation_in_conjoint_R_package.pdf
# https://www.data-mania.com/blog/conjoint-analysis-in-r/

library(conjoint)
library(tidyverse)
library(magrittr)

data(tea)

# Tea data
tprof %>% as_tibble() %>% glimpse()

# Survey data
tprefm %>% as_tibble() %>% glimpse()

# Levels
tlevn

# Utility value of 1st respondent
caModel(y = tprefm[1,], x = tprof)

caUtilities(y = tprefm[1,], x = tprof, z = tlevn)


# Utility value of 1st 10 respondents
caPartUtilities(y = tprefm[1:10,], x = tprof, z = tlevn)


# Run on all respondents
Conjoint(y = tpref, x = tprof, z = tlevn)


# Segmentation
caSegmentation(y = tpref, x = tprof, c = 3)
