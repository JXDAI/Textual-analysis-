library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
setwd("/Users/bigboss/documents/R")

## read data and process into DTM
df <- readLines("bag of words per unit.txt")
docs <- VCorpus(VectorSource(df))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "#")
## here we could erase first level words 
docs_root <- tm_map(docs, removeWords, c("Knowledge-sharing","Knowledge-loss","Decision-making"))
dtm <- TermDocumentMatrix(docs, control = list(tolower = FALSE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

##bar plot
tiff('test.tiff', units="in", width=5, height=5, res=300)

 ggplot(data=d, aes(x=reorder(word, +freq), y=freq)) +
    geom_bar(stat="identity", fill="steelblue", width=0.5) + theme_minimal() + labs( x = "Concepts", y = "Frequency") + coord_flip()

dev.off()


##word cloud 

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## dendrogram 
KM_dist <- dist(dtm)
hc <- hclust(KM_dist)