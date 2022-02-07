# Below is a function to avoid repetitive wordcloud codes

wordcloud_fn <- function(ph_split, ph_name, wordcloud_color){
  ph_data <- as.data.frame(ph_split)
  colnames(ph_data) <- colnames(ph)
  ph_data_cor <- Corpus(VectorSource(ph_data$tokenized_txt))
  ph_data_tidy <- ph_data_cor %>% 
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, c("one", "will", "must", "can", "since", "just", "yet", "might", "however", "two", "may", stopwords("english"))) %>%
    tm_map(stripWhitespace) %>%
    TermDocumentMatrix() %>% 
    tidy()
  ph_data_overall <- ph_data_tidy %>% 
    group_by(term) %>% 
    summarise(frequency = sum(count)) %>% 
    arrange(desc(frequency))
  wordcloud(ph_data_overall$term, ph_data_overall$frequency,
            scale = c(3,0.1),
            max.words = 70,
            min.freq = 1,
            random.order = FALSE,
            rot.per = 0.35,
            random.color = FALSE,
            colors = brewer.pal(9, wordcloud_color))
  title(main = ph_name,line = 0.1, cex.main = 2)
}

