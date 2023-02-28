# LDA Topic Modeling using the package: library(tm)
#install.packages("tm",dependencies=TRUE)
#install.packages("topicmodels",dependencies=TRUE)
#load text mining library
library(tm)

#set working directory (modify path as needed)

setwd('.\secondlevel_200pts_dist95_popularityln')

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(),pattern='*.txt')
#read files into a character vector
files <- lapply(filenames,readLines)
#create corpus from vector
docs <- Corpus(VectorSource(files))
writeLines(as.character(docs[[1]]))

#start preprocessing
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

#remove punctuation
#docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
#docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
#writeLines(as.character(docs[[1]]))
#Stem document
#docs <- tm_map(docs,stemDocument)

#define and eliminate all custom stopwords
#myStopwords <- c("the","joint","san", "new")
#docs <- tm_map(docs, removeWords, myStopwords)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],'word_freq.csv')

##test a case for tf-idf
## wordfreq=5666, docfreq=1748 for mexican restaurant
# tag_count=0
# for (i in 1:length(files))
# {
#   if (grepl("Mexican-Restaurant",files[i])){
#     tag_count <- tag_count+1
#   }
# }


#create document-term matrix with tf-idf
dtm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf, normalize=TRUE))
rownames(dtm_tfidf) <- filenames
freq_tfidf  <- colSums(as.matrix(dtm_tfidf))
length(freq_tfidf)
ord_tfidf <- order(freq_tfidf,decreasing=TRUE)
write.csv(freq_tfidf[ord_tfidf],'word_freq_tfidf.csv')
scaled_tfidf <- (freq_tfidf-min(freq_tfidf))/(max(freq_tfidf)-min(freq_tfidf))
write.csv(scaled_tfidf[ord_tfidf],'word_freq_tfidf_scaled.csv')


#load topic models library
library(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 1000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k<-100

#Number of topics loop
for(k in seq(from=100, to=100, by=5)){
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  #write out results docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  write.csv(ldaOut.topics,file=paste("LDAGibbsDocTopics",k,".csv"))
  #top 15 terms in each topic
  ldaOut.terms <- as.matrix(terms(ldaOut,15))
  write.csv(ldaOut.terms,file=paste("LDAGibbsTopTerms",k,".csv"))
  #probabilities associated with each topic assignment for documents corpus
  tp_colnames<-paste('topic',1:k,sep="")
  matrix_names<-list(NULL,c('filenames',tp_colnames))
  topicProbabilities <- as.matrix(cbind(filenames,ldaOut@gamma))
  dimnames(topicProbabilities) <- matrix_names
  write.csv(topicProbabilities,file=paste("LDAGibbsTopicProb",k,".csv"))
  #probabilities associated with each term assignment for topics
  termsProbabilities <- t(posterior(ldaOut)$terms)
  write.csv(termsProbabilities,file=paste("LDAGibbsTermProb",k,".csv"))
  
  #top 15 terms with prob
  topterms_prob <- matrix(data=NA,nrow = 15, ncol=k)
  for (i in 1:15){
    for(j in 1:k){
      search_term <- ldaOut.terms[i,j]
      search_term_prob <- termsProbabilities[search_term,j]
      topterms_prob[i,j] <- search_term_prob
    }    
  }
  write.csv(rbind(ldaOut.terms,topterms_prob),file=paste("LDAGibbsTopTermProb",k,".csv"))
  
  ##top 15 terms with tf-idf
  termsTopicProbabilities_tfidf <- termsProbabilities*scaled_tfidf
  topterms_prob_tfidf <- matrix(data=NA,nrow = 15, ncol=0)
  for (song in 1:k){
    sort_target <- sort(termsTopicProbabilities_tfidf[1: dim(termsTopicProbabilities_tfidf)[1],song],decreasing=TRUE)
    topterms_prob_tfidf <- cbind(topterms_prob_tfidf,names(sort_target[1:15]),sort_target[1:15])
  }
  rownames(topterms_prob_tfidf) <- NULL
  write.csv(topterms_prob_tfidf,file=paste("LDAGibbsTopTermProb_tfidf",k,".csv"))
  
  ##find the most prominent topic for all documents and associated term probabilities for this topic
  firsttopic <- names(sort(summary(as.factor(ldaOut.topics[1:length(docs)])), decreasing=T)[1])
  firsttopic_data <- as.data.frame(firsttopic)
  firsttopic_data.terms <- t(ldaOut.terms)[as.numeric(firsttopic),]
  firsttopic_data.prob <- termsProbabilities[firsttopic_data.terms,as.numeric(firsttopic)]
  write.csv(firsttopic_data.prob,file=paste("LDAGibbsTermProb",k,"top1ranktopic.csv"))
  doctopicfreq <- summary(as.factor(ldaOut.topics[1:length(docs)]))
  write.csv(doctopicfreq,file=paste("LDAGibbsTermProb",k,"doctopicfreq.csv"))
}

 ##clustering of topics
  termsProbabilitiesVector <- posterior(ldaOut)$terms
  #termsProbabilitiesVector_data <- read.csv("LDAGibbsTermProb130.csv", header=T, as.is=T)
  #termsProbabilitiesVector <- t(termsProbabilitiesVector_data[,-1])
  rownames(termsProbabilitiesVector) <- NULL  
  clusters <- hclust(dist(termsProbabilitiesVector,method = "euclidean"),method="ward.D2")
  png(paste("topicsWardClustering",k,".png"), width = 20, height = 12, units = 'in', res = 300)
  plot(clusters)
  dev.off()
  #consider TF-IDF
  termsProbabilitiesVector_tfidf <- t(termsTopicProbabilities_tfidf)
  clusters_tfidf <- hclust(dist(termsProbabilitiesVector_tfidf,method = "euclidean"),method="ward.D2")
  png(paste("topicsWardClustering_tfidf",k,".png"), width = 20, height = 12, units = 'in', res = 300)
  plot(clusters_tfidf)
  dev.off()
  
## topics predictions for new datasets
setwd('./denver_200pts_popularityln')
test_filenames <- list.files(getwd(),pattern='*.txt')
#read files into a character vector
test_files <- lapply(test_filenames,readLines)
#create corpus from vector
test_docs <- Corpus(VectorSource(test_files))
test_docs <- tm_map(test_docs,content_transformer(tolower))
test_docs <- tm_map(test_docs, removeNumbers)
#Create the test document-term matrix
test_dtm <- DocumentTermMatrix(test_docs)
#predict the test doc topics
test.topics <- posterior(ldaOut,test_dtm)
test_topicProbabilities <- test.topics$topics
test.topics <- apply(test.topics$topics, 1, which.max)
write.csv(cbind(test_filenames,test.topics),file=paste("test_LDAGibbsDocTopics",k,".csv"))
write.csv(cbind(test_filenames,test_topicProbabilities),file=paste("test_LDAGibbsTopicProb",k,".csv"))
