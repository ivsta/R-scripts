library(dplyr)

pantheria <- "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
download.file(pantheria, destfile = "~/Desktop/mammals.txt")



mammals <- read.table("~/Desktop/mammals.txt", sep = "\t", header = TRUE, 
                      stringsAsFactors = FALSE)
names(mammals) <- sub("X[0-9._]+", "", names(mammals))
names(mammals) <- sub("MSW05_", "", names(mammals))
mammals <- dplyr::select(mammals, Order, Binomial, AdultBodyMass_g, 
                         AdultHeadBodyLen_mm, HomeRange_km2, LitterSize)
names(mammals) <- gsub("([A-Z])", "_\\L\\1", names(mammals), perl = TRUE)
names(mammals) <- gsub("^_", "", names(mammals), perl = TRUE)
mammals[mammals == -999] <- NA
names(mammals)[names(mammals) == "binomial"] <- "species"
mammals <- dplyr::tbl_df(mammals) # for prettier printing


mammals
mammals %>% glimpse


#-------------------------
# Selecting columns
#-------------------------

mammals %>% select(adult_head_body_len_mm)

mammals %>% select(adult_head_body_len_mm, litter_size)

mammals %>% select(adult_head_body_len_mm:litter_size)

mammals %>% select(-adult_head_body_len_mm)

mammals %>% select(contains("body"))

mammals %>% select(starts_with("adult"))

mammals %>% select(ends_with("g"))

mammals %>% select(1:3)


#-------------------------
# Filtering rows
#-------------------------

mammals %>% filter(adult_body_mass_g > 1e7) %>% select(1:3)

mammals %>% filter(species == "Balaena mysticetus")

mammals %>% filter(order == "Carnivora" & adult_body_mass_g < 200)


#-------------------------
# Arranging rows
#-------------------------

mammals %>% arrange(adult_body_mass_g) %>% select(1:3)

mammals %>% arrange(desc(adult_body_mass_g)) %>% select(1:3)

mammals %>% arrange(order, adult_body_mass_g) %>% select(1:3)


#-------------------------
# Mutating columns
#-------------------------

mammals %>% mutate(adult_body_mass_kg = adult_body_mass_g / 1000) %>% glimpse

mammals %>% mutate(g_per_mm = adult_body_mass_g / adult_head_body_len_mm) %>% glimpse

mammals %>% mutate(g_per_mm = adult_body_mass_g / adult_head_body_len_mm
                   , kg_per_mm = g_per_mm / 1000) %>% glimpse


#-------------------------
# Summarising columns
#-------------------------

mammals %>% summarise(mean_mass = mean(adult_body_mass_g, na.rm = TRUE))

mammals %>% group_by(order) %>% summarise(mean_mass = mean(adult_body_mass_g, na.rm = TRUE)) %>% head


#-------------------------
# Piping data
#-------------------------

?magrittr::`%>%`

mammals %>% arrange(adult_body_mass_g)

mammals %>%
  mutate(mass_to_length = adult_body_mass_g / adult_head_body_len_mm) %>%
  arrange(desc(mass_to_length)) %>%
  select(species, mass_to_length)

mammals %>% group_by(order) %>%
  summarise(median_litter = median(litter_size, na.rm = TRUE)) %>%
  filter(median_litter > 3) %>%
  arrange(desc(median_litter)) %>%
  select(order, median_litter)