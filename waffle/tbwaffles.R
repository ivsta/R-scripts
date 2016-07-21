
library(scales)
library(ggplot2)

# Here's some data I had lying around
tb <- structure(list(region = c("Africa", "Asia", "Latin America", 
                                "Other", "US-born"), ncases = c(36L, 34L, 56L, 2L, 44L)), .Names = c("region", 
                                                                                                     "ncases"), row.names = c(NA, -5L), class = "data.frame")


# A bar chart of counts
ggplot(tb, aes(x = region, weight = ncases, fill = region)) +
  geom_bar()

# Bar chart of percentages
ggplot(tb, aes(x = region, weight = ncases/sum(ncases), fill = region)) +
  geom_bar() +
  scale_y_continuous(label = percent)

# Pie chart equivalents.  Forgive me, Hadley, for I must sin.
ggplot(tb, aes(x = factor(1), weight = ncases, fill = region)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = "", y = "")

ggplot(tb, aes(x = factor(1), weight = ncases/sum(ncases), fill = region)) +
  geom_bar() +
  scale_y_continuous(label = percent) +
  coord_polar(theta = "y") +
  labs(x = "", y = "")


# Waffles
# How many rows do you want the y axis?
ndeep <- 5

# I need to convert my data into a data.frame with a unique specified x
# and y axis for each case
# Note - it's actually important to specify y first for a
# horizontally-accumulating waffle
tb4waffles <- expand.grid(y = 1:ndeep,
                          x = seq_len(ceiling(sum(tb$ncases) / ndeep)))

# Expand the counts into a full vector of region labels - i.e., de-aggregate
regionvec <- rep(tb$region, tb$ncases)

# Depending on the value of ndeep, there might be more spots on the x-y grid
# than there are cases - so fill those with NA
tb4waffles$region <- c(regionvec, rep(NA, nrow(tb4waffles) - length(regionvec)))

# Plot it
ggplot(tb4waffles, aes(x = x, y = y, fill = region)) + 
  geom_tile(color = "white") + # The color of the lines between tiles
  scale_fill_manual("Region of Birth",
                    values = RColorBrewer::brewer.pal(5, "Dark2")) +
  ggtitle("TB Cases by Region of Birth")