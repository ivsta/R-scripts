library(tidyverse)
library(RecordLinkage)

data(RLdata500)
data(RLdata10000)
RLdata500[1:5, ]

rpairs <- compare.dedup(RLdata500, identity = identity.RLdata500)
rpairs$pairs[1:5, ]


system.time(rpairs_all <- compare.linkage(dataset1 = RLdata500, dataset2 = RLdata10000
                                          , identity1 = identity.RLdata500, identity2 = identity.RLdata10000))
# user  system elapsed 
# 20.379   0.933  21.352

system.time(rpairs_block <- compare.linkage(dataset1 = RLdata500, dataset2 = RLdata10000
                                            , identity1 = identity.RLdata500, identity2 = identity.RLdata10000
                                            , blockfld = list(1, 5:7)))
# user  system elapsed 
# 0.323   0.013   0.336


rpairs_all$pairs %>% dim()
# [1] 5000000      10

rpairs_block$pairs %>% dim()
# [1] 48041    10

rpairs_all$frequencies
# fname_c1    fname_c2    lname_c1    lname_c2          by          bm          bd 
# 0.001805054 0.006666667 0.002732240 0.019607843 0.009259259 0.050000000 0.022222222

rpairs_block$frequencies
# fname_c1    fname_c2    lname_c1    lname_c2          by          bm          bd 
# 0.001805054 0.006666667 0.002732240 0.019607843 0.009259259 0.050000000 0.022222222


weights_all <- rpairs_all %>% epiWeights()
weights_all %>% summary()

weights_all %>% str()


pairs_all <- weights_all %>% getPairs()
pairs_all %>% summary()

classify_all <- weights_all %>% epiClassify(0.1)
classify_all %>% summary()
