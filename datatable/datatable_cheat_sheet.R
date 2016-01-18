library(data.table) 

#---------------------------------
# Create a data table
#---------------------------------

set.seed(45L)
DT <- data.table(V1=c(1L,2L),V2=LETTERS[1:3], V3=round(rnorm(4),4), V4=1:12)

#---------------------------------
# Subsetting rows using i
#---------------------------------

DT[3:5,]
DT[ V2 == "A"]
DT[ V2 %in% c("A","C")] 

#---------------------------------
# Manipulating on columns in J
#---------------------------------

DT[,V2]
DT[,.(V2,V3)]
DT[,sum(V1)]
DT[,.(sum(V1),sd(V3))]
DT[,.(Aggregate = sum(V1),Sd.V3 = sd(V3))]
DT[,.(V1, Sd.V3 = sd(V3))]


#---------------------------------
# Doing J by group
#---------------------------------

DT[,.(V4.Sum = sum(V4)),by=V1]
DT[,.(V4.Sum = sum(V4)),by=.(V1,V2)]
DT[,.(V4.Sum = sum(V4)),by=sign(V1-1)]
DT[,.(V4.Sum = sum(V4)),by=.(V1.01 = sign(V1-1))]
DT[1:5,.(V4.Sum = sum(V4)),by=V1]
DT[,.N,by=V1]


#------------------------------------------------------
# Adding/updating columns by reference in J using :=
#------------------------------------------------------

DT[, V1 := round(exp(V1),2)]
DT[, c("V1","V2") := list (round(exp(V1),2), LETTERS [4:6])]
DT[, ':=' (V1 = round(exp(V1),2),V2 = LETTERS[4:6])][]
DT[, V1 := NULL]
DT[, c("V1","V2") := NULL]

Cols.chosen = c("A","B")
DT[, (Cols.chosen) := NULL]


#------------------------------------------------------
# Indexing and keys
#------------------------------------------------------

setkey(DT,V2)
DT["A"]
DT[c("A","C")]

DT["A", mult ="first"]
DT["A", mult = "last"] 

DT[c("A","D")]
DT[c("A","D"), nomatch=0]

DT[c("A","C"),sum(V4)]
DT[c("A","C"),sum(V4), by=.EACHI]

setkey(DT,V1,V2)
DT[.(2,"C")]
DT[.(2,c("A","C"))]


#------------------------------------------------------
# Advanced data table operations
#------------------------------------------------------

DT[.N-1]
DT[,.N]

DT[,.(V2,V3)]
DT[, mean(V3), by=.(V1,V2)]

DT[, print(.SD), by=V2]
DT[,.SD[c(1,.N)], by=V2]
DT[, lapply(.SD, sum), by=V2]

DT[, lapply(.SD,sum), by=V2,.SDcols = c("V3","V4")]
DT[, lapply(.SD,sum), by=V2,.SDcols = paste0("V",3:4)]


#------------------------------------------------------
# Chaining expressions together
#------------------------------------------------------

# No chaining
DT <- DT[, .(V4.Sum = sum(V4)), by = V1]
DT[V4.Sum > 40] 

# With chaining
DT[, .(V4.Sum = sum(V4)), by = V1][V4.Sum > 40 ]


DT[, .(V4.Sum = sum(V4)),by=V1][order(-V1)]


#------------------------------------------------------
# Using the set()-family
#------------------------------------------------------

rows = list(3:4,5:6)
cols = 1:2
for (i in seq_along(rows))
{ set(DT,i=rows[[i]], j = cols[i], value = NA) }

setnames(DT,"V2","Rating")
setnames(DT,c("V2","V3"), Changes two column names. c("V2.rating","V3.DataCamp"))
