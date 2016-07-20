# devtools::install_github("hrbrmstr/waffle")
library(waffle)
savings <- c(`Mortgage ($84,911)`=84911, `Auto and\ntuition loans ($14,414)`=14414, 
             `Home equity loans ($10,062)`=10062, `Credit Cards ($8,565)`=8565)
waffle(savings/392, rows=7, size=0.5, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
       title="Average Household Savings Each Year", 
       xlab="1 square == $392")



parts <- c(`Un-breached\nUS Population`=(318-11-79), `Premera`=11, `Anthem`=79)

waffle(parts, rows=8, size=1, colors=c("#969696", "#1879bf", "#009bda"), 
       title="Health records breaches as fraction of US Population", 
       xlab="One square == 1m ppl")


waffle(parts/10, rows=3, colors=c("#969696", "#1879bf", "#009bda"), 
       title="Health records breaches as fraction of US Population", 
       xlab="One square == 10m ppl")