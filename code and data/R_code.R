# Andi map ----------------------------------------------------------------
setwd("/home/agricolamz/work/materials/2018.05.11-13_Zilo_classes_Erevan/prezi/code and data")
library(tidyverse)
library(lingtypology)

coord <- read_tsv("coordinates.csv")

# empty <- coord$label[1]
# coord %>%
#   filter(label != empty) %>%
#   summarise(min(Lat), max(Lat), min(Lon), max(Lon))
## A tibble: 1 x 4
#   `min(Lat)` `max(Lat)` `min(Lon)` `max(Lon)`
#        <dbl>      <dbl>      <dbl>      <dbl>
# 1       42.6       42.8       46.2       46.4

coord %>% 
  filter(Lat > 42.6,
         Lat < 42.9,
         Lon > 46.05,
         Lon < 46.5) %>% 
  mutate(label_en = if_else(is.na(label_en), "", label_en)) ->
  coord

map.feature(coord$Language,
            features = coord$Language,
            latitude = coord$Lat,
            longitude = coord$Lon,
            label = coord$label_en,
            label.hide = FALSE,
            zoom.level = 11,
            legend.position = "topleft",
            label.emphasize = list(5, "red"),
            minimap = TRUE)

# 677 x 458

# nouns destribution ------------------------------------------------------
setwd("/home/agricolamz/_DATA/OneDrive1/_Work/_Handouts/2017 II 17.11.03 MSU Classes in Zilo/data")
library(tidyverse)
library(XML)
library(stringdist)
library(extrafont)

# work with dict data -----------------------------------------------------
nouns <- xmlToList("dict/dict.lift")
class_dict <- data.frame(zilo_l = NA, zilo_c = NA, eng = NA, rus = NA, class = NA)

lapply(2:(length(nouns)-1), function(i){
  class_dict[i-1, 1] <<- ifelse(is.list(nouns[i]$entry$citation[[1]]),
                                nouns[i]$entry$`lexical-unit`[[1]]$text,
                                NA)
  class_dict[i-1, 2] <<- ifelse(is.list(nouns[i]$entry$citation[[2]]),
                                nouns[i]$entry$`lexical-unit`[[2]]$text,
                                NA)
  class_dict[i-1, 3] <<- ifelse(is.list(nouns[i]$entry$sense[[2]]),
                                nouns[i]$entry$sense[[2]]$text,
                                NA)
  class_dict[i-1, 4] <<- ifelse(is.list(nouns[i]$entry$sense[[3]]),
                                nouns[i]$entry$sense[[3]]$text,
                                NA)
  class_dict[i-1, 5] <<- ifelse(!is.null(nouns[i]$entry$sense$`grammatical-info`$.attrs), 
                                nouns[i]$entry$field$form$text,
                                NA)
})

class_dict %>% 
  filter(class != "suffix",
         class != "¬an",
         !grepl("pl", eng),
         !grepl("obl", eng)) %>% 
  mutate(class = factor(class, levels = c("m",
                                          "f",
                                          "m/f",
                                          "an",
                                          "¬an1",
                                          "¬an2"))) ->
  class_dict

library(extrafont)
font_import(pattern="[B/b]rill")

class_dict %>% 
  count(class) %>% 
  ggplot(aes(class, n, label = n, ymax = 220))+
  geom_bar(fill = "lightblue", stat = "identity")+
  geom_text(vjust=-0.5)+
  theme_bw()+
  theme(text=element_text(family="Brill"))
  labs(title = paste("Distribution of", 
                     nrow(class_dict),
                     "Zilo nouns"),
       x = "", y = "",
       caption = "fieldwork data")

# 700 x 500