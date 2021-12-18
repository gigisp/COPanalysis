input <- read.csv("/Users/Gisela/Desktop/words.csv")
allCOPs <- read.csv("/Users/Gisela/Desktop/allCOPs.csv")

library(tidyverse)

chunks <- input[input$word_type == "noun chunk",]

nodups <-  chunks %>%
  arrange(year) %>%
  mutate( 
    word = str_replace(word, "^the ", ""), 
    word = str_replace(word, "^an ", ""),
    word = str_replace(word, "^a ", ""), 
    dup = duplicated(word)
    ) %>%
   filter(
     dup == FALSE & 
       year > 2009
   )


write_csv(nodups, "/Users/Gisela/Desktop/nodup_nc.csv")

selection <- nodups %>%
  mutate(
    wordcount = str_count(word, " ") + 1, 
    charcount = str_count(word), 
    year = ifelse(year == 2021, 2020, year)
  ) %>%
  filter(wordcount>1) %>%
  arrange(year, -charcount ) %>%
  group_by(year) %>%
  top_n(10, charcount) %>%
  mutate(row = row_number()) %>%
  filter(row<=10)


selection <- left_join(selection, allCOPs, by = c("year"= "Year"))

selection$facetlabs <- str_c( selection$year, ": ",  selection$City, ", ",
                              selection$Country, " (" , selection$Session, ")")

#write_csv(selection, "/Users/Gisela/Desktop/selection.csv")
#selection <- read_csv("/Users/Gisela/Desktop/selection.csv")

COPgraph  <- selection %>%
  ggplot() +
  theme_light() +
  geom_text(aes(x=-1, y= row, label = word, color=  as.factor(year)), size = 2.5, hjust =0) +
  scale_x_continuous(limits = c(-1, 1)) +
  facet_wrap(vars(facetlabs), ncol=3) +
  ggtitle(
    "A decade of Climate Talks", 
    subtitle = "Longest unique word combinations in opening speech of UN COP (2010-2020)"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title =  element_blank()
 
    
  )

ggsave(
  filename = "/Users/Gisela/Desktop/COPgraph.png",
  COPgraph
)




