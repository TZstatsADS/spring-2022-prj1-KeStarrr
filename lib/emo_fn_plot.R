emo_fn <- function(df_a, author_v, author){
  
  # "anger"   "anticipation"   "disgust"     "fear"       
  # "joy"     "sadness"        "surprise"    "trust"
  
  col.use=c("light grey", "firebrick1", "aquamarine", "darkorange1", "darkorchid3",
            "deepskyblue2", "gold1", "burlywood1","gold4")
  
  df_a$topemo_pos = apply(select(df_a, anger:trust), 1, which.max)
  df_a$topemo = apply(select(df_a, anger:trust), 1, max)
  df_a$topemo_pos[df_a$topemo<0.01] = 0
  df_a$topemo_pos = df_a$topemo_pos + 1
  
  temp = df_a$topemo
  df_a$topemo[temp<0.05] = 1
  
  df = df_a %>% 
    filter(author == author_v) %>% 
    select(sent_id, word_count, 
           topemo_pos, topemo)
  
  ptcol.use = alpha(col.use[df$topemo_pos], sqrt(sqrt(df$topemo)))
  
  plot(df$sent_id, df$word_count, 
       col=ptcol.use,
       type="h", main=author)
}