library(network)
library(sna)
library(ggplot2)
library(GGally)


#------------------------------
# Example (1): Random graph
#------------------------------

# random graph
set.seed(1)
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]

ggnet2(net)

ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")
ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))

ggnet2(net, mode = "circle")
ggnet2(net, mode = "kamadakawai")
ggnet2(net, mode = "fruchtermanreingold", layout.par = list(cell.jitter = 0.75))
ggnet2(net, mode = "target", layout.par = list(niter = 100))

net %v% "phono" = ifelse(letters[1:10] %in% c("a", "e", "i"), "vowel", "consonant")
ggnet2(net, color = "phono")

net %v% "color" = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato")
ggnet2(net, color = "color")

ggnet2(net, color = "phono", palette = c("vowel" = "steelblue", "consonant" = "tomato"))
ggnet2(net, color = ifelse(net %v% "phono" == "vowel", "steelblue", "tomato"))

ggnet2(net, color = "phono", palette = "Set2")

ggnet2(net, size = "phono")
ggnet2(net, size = "phono", size.palette = c("vowel" = 10, "consonant" = 1))
ggnet2(net, size = sample(0:2, 10, replace = TRUE), max_size = 9)
ggnet2(net, size = "degree")
ggnet2(net, size = "degree", size.cut = 3)

# remove any isolated nodes
x = ggnet2(net, size = "degree", size.min = 1)

# remove all nodes
x = ggnet2(net, size = "degree", size.max = 1)

ggnet2(net, size = sample(0:2, 10, replace = TRUE), size.zero = TRUE)
