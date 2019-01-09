library(vosonSML)
apikey <- "#API-KEY" #put your api key in the quotes
key <- AuthenticateWithYoutubeAPI(apikey)

video <- c('video-link')  #put the url-id of the youtube video in the quotes
ytdata <- CollectDataYoutube(video, key, writeToFile = FALSE)
str(ytdata)
write.csv(ytdata, file='##location of the csv file containing the list of commenters with the reply count', row.names = F)
data <-read.csv(file.choose(), header = T)
str(data)

#data <-data[data$ReplyToAnotherUser != FALSE,]
y <- data.frame(data$User, data$ReplyToAnotherUser )

library(igraph)
net <- graph.data.frame(y, directed=T)      
net <- simplify(net)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

hist(V(net)$degree, 
     col = 'green', 
     main = 'Histogram of the node degree', 
     ylab = 'Frequency', xlab='Degree of vertices')

plot(net, vertex.size = 0.2*V(net)$degree, edge.arrow.size = 0.01, vertex.label.cex = 0.01*V(net)$degree)
# plot(net, vertex.size = 5, edge.arrow.size = 0.01, vertex.label.cex = 0.05)

library(syuzhet)
data <-read.csv(file.choose(), header = T)
str(data)
comments <- iconv(data$Comment, to = 'utf-8')

s <- get_nrc_sentiment(comments)
s$neutral <- ifelse(s$negative+s$positive== 0 , 1, 0)
head(s)
comments[3]
barplot(100*colSums(s)/sum(s), las = 2, col = rainbow(10), ylab = 'Percentage', main = 'Sentiment scores for Youtube comments')


#small world model
# sw <- sample_smallworld(dim=1.5, size=126, nei=3, p=0.1)
# plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

#random graph model
# er <- sample_gnm(n=126, m=200) 
# er1 <- sample_gnp(n=126, p=0.1) 
# plot(er, vertex.size=6, vertex.label=NA)
# plot(er1, vertex.size=6, vertex.label=NA)
# dbinom(135,7875,0.1)

#preferential attachment model
# ba <-  sample_pa(n=126, power=1, m=1,  directed=F)
# plot(ba, vertex.size=2, vertex.label=NA)
