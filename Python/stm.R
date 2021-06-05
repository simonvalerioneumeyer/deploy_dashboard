library(data.table)
library(DBI)
#install.packages('reshape2')
library(RMySQL)
library(tm)
library(SnowballC)
library(xtable)
library(ggplot2)
library(Rmisc)
library(stargazer)
library(stm)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(igraph)
library(tidytext)
library(reshape2)

#How to load data:
#just run this for any RData with the path:
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/stm_25_web_party.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/docs.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/meta.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/out.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/results_ksearch.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/stm_25_content_web_party.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/stm_25_state_party.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/storage.RData')
load('Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/vocab.RData')


#Connect to the database

mydb <- dbConnect(RMySQL::MySQL(), user='bes', password='seb_1_bes', dbname='tweets', 
                  host='3.140.248.246', port=3306)

#Config to read data.tables faster, 0 threads uses all CPUs available:

setDTthreads(threads = 0, restore_after_fork = NULL,throttle = NULL)


#Read data as data.table
query <- "SELECT text, west_east_berlin, east_dummy, party, state, followers_count, favorite_count, retweet_count FROM seb_table"
data <- as.data.table(dbGetQuery(mydb, query))
data <- dbGetQuery(mydb, query)

#stop words
load(file='~/bgse/thesis/Master_thesis/Python/r_stop_words/stop_list.RData')

#Preprocess data using DF:
processed <- textProcessor(documents = data$text, 
                           metadata=data,
                           language="de",
                           lowercase = TRUE,
                           removestopwords = TRUE,
                           removenumbers = TRUE,
                           removepunctuation = TRUE,
                           ucp = FALSE,
                           stem = TRUE,
                           wordLengths = c(3, Inf),
                           customstopwords = stopword_list)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 50)
docs <- out$documents
vocab <- out$vocab
meta  <-out$meta

save(docs, file = "/home/elio/bgse/thesis/docs.RData", compress = FALSE)
save(vocab, file = "/home/elio/bgse/thesis/vocab.RData", compress = FALSE)
save(meta, file = "/home/elio/bgse/thesis/meta.RData", compress = FALSE)



#Run STM and estimate the number of different topics

#Held out likelihood ratio 
#Lower bound
#Residuals

out$meta$state <- as.factor(out$meta$state)

storage <- searchK(out$documents, out$vocab,
                   K = c(5,10,15,20,25,30,35,40,45,50),
                   prevalence =~ party + state, data = meta,  max.em.its = 55)

storage$results

results_ksearch <-storage$results
save(storage, file = "/home/elio/bgse/thesis/searchK_storage.RData", compress = FALSE)
save(results_ksearch, file = "/home/elio/bgse/thesis/results_ksearch.RData", compress = FALSE)




#Plotting the results

p1 <- ggplot(data=results_ksearch, aes(x=K, y=heldout, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Held Out Ratio")


p2 <- ggplot(data=results_ksearch, aes(x=K, y=residual, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Residual")

p3 <- ggplot(data=results_ksearch, aes(x=K, y=lbound, group=1)) +
  geom_line()+
  geom_point()+
  ggtitle("Lower Bound")


#Saving the files
mp <- multiplot(p1, p2, p3, cols=3)


par(mar=c(0.5,0.5,0.5,0.5))
plot(storage)


#Trying different combinations of prevalence
K <- 25
stm_25_all <- stm(documents = docs, 
              vocab = vocab,
              K = K, 
              #for prevalence, careful to use a lot of data or else 
              #it will not work; not enough differences if small amount
              #of data
              prevalence =~ west_east_berlin + party + state,
              #content = ~party, #Here we should use east dummy to be sure
              max.em.its = 55, #can optimize
              data = meta,
              init.type = "Spectral")
save(stm_25_all, file = "bgse/thesis/results/stm_25_all.RData", compress = FALSE)


K <- 25
stm_25_web_party <- stm(documents = docs, 
                  vocab = vocab,
                  K = K, 
                  #for prevalence, careful to use a lot of data or else 
                  #it will not work; not enough differences if small amount
                  #of data
                  prevalence =~ west_east_berlin + party,
                  #content = ~party, #Here we should use east dummy to be sure
                  max.em.its = 55, #can optimize
                  data = meta,
                  init.type = "Spectral")
save(stm_25_web_party, file = "bgse/thesis/results/stm_25_web_party.RData", compress = FALSE)



K <- 35
stm_35_cont_web_party <- stm(documents = docs, 
                        vocab = vocab,
                        K = K, 
                        #for prevalence, careful to use a lot of data or else 
                        #it will not work; not enough differences if small amount
                        #of data
                        prevalence =~ west_east_berlin + party,
                        content = ~west_east_berlin, #Here we should use east dummy to be sure
                        max.em.its = 55, #can optimize
                        data = meta,
                        init.type = "Spectral")
save(stm_35_cont_web_party, file = "/results/stm_35_cont_web_party.RData", compress = FALSE)


K <- 25
stm_25_state_party <- stm(documents = docs, 
                  vocab = vocab,
                  K = K, 
                  #for prevalence, careful to use a lot of data or else 
                  #it will not work; not enough differences if small amount
                  #of data
                  prevalence =~ party + state,
                  #content = ~party, #Here we should use east dummy to be sure
                  max.em.its = 55, #can optimize
                  data = meta,
                  init.type = "Spectral")
save(stm_25_state_party, file = "bgse/thesis/results/stm_25_state_party.RData", compress = FALSE)


K <- 25
stm_25_state <- stm(documents = docs, 
                  vocab = vocab,
                  K = K, 
                  #for prevalence, careful to use a lot of data or else 
                  #it will not work; not enough differences if small amount
                  #of data
                  prevalence =~ state,
                  #content = ~party, #Here we should use east dummy to be sure
                  max.em.its = 55, #can optimize
                  data = meta,
                  init.type = "Spectral")
save(stm_25_state, file = "bgse/thesis/results/stm_25_state.RData", compress = FALSE)


K <- 25
stm_25_web <- stm(documents = docs, 
                    vocab = vocab,
                    K = K, 
                    #for prevalence, careful to use a lot of data or else 
                    #it will not work; not enough differences if small amount
                    #of data
                    prevalence =~ west_east_berlin,
                    #content = ~party, #Here we should use east dummy to be sure
                    max.em.its = 55, #can optimize
                    data = meta,
                    init.type = "Spectral")
save(stm_25_web, file = "bgse/thesis/results/stm_25_web.RData", compress = FALSE)


K <- 25
stm_25_party <- stm(documents = docs, 
                   vocab = vocab,
                   K = K, 
                   #for prevalence, careful to use a lot of data or else 
                   #it will not work; not enough differences if small amount
                   #of data
                   prevalence =~ party,
                   max.em.its = 55,
                   data = meta,
                   init.type = "Spectral")
save(stm_25_party, file = "bgse/thesis/results/stm_25_party.RData", compress = FALSE)


#NOTE: This is for topic 1 !!!!
#Change topic by typing the number in the last []... ex: [12,] for topic 12
#Need to check the correlations happening between party+east_dummy vs party+state




# start here
#all vs party+east_dummy:
cor1b <- cor(stm_25_all$beta[[1]][[1]][1,],stm_25_east_party$beta[[1]][[1]][1,])
cor1t <- cor(stm_25_all$theta[,1],stm_25_east_party$theta[,1])

#party+east_dummy vs party+state:
cor2b <- cor(stm_25_state_party$beta[[1]][[1]][1,],stm_25_east_party$beta[[1]][[1]][1,])
cor2t <- cor(stm_25_state_party$theta[,1],stm_25_east_party$theta[,1])

#east_dummy vs party+east_dummy:
cor3b <- cor(stm_25_east$beta[[1]][[1]][1,],stm_25_east_party$beta[[1]][[1]][1,])
cor3t <- cor(stm_25_east$theta[,1],stm_25_east_party$theta[,1])

#east_dummy vs party:
cor4b <- cor(stm_25_east$beta[[1]][[1]][1,],stm_25_party$beta[[1]][[1]][1,])
cor4t <- cor(stm_25_east$theta[,1],stm_25_party$theta[,1])

#east_dummy vs state:
cor5b <- cor(stm_25_east$beta[[1]][[1]][1,],stm_25_state$beta[[1]][[1]][1,])
cor5t <- cor(stm_25_east$theta[,1],stm_25_state$theta[,1])

#state and east_dummy have the highest betas and thetas, meaning they explain
#practically the same thing, so we can just use one of them

#Can discuss this further, even though tiny bit lower, it is because we have
#a big amount of data. Still this difference is significant enough for us 
#to not drop party, instead use a combination of party+east_dummy


# investigate topics 

#This will gives per topic the respective topics back!
labelTopics(stm_35_cont_web_party, 
            1:K, 
            n = 10)

# investigate topic prevalence
plot(stm_35_cont_web_party, type = "summary", xlim = c(0, .3))
plot(stm_35_cont_web_party, type = "hist", xlim = c(0, .3))

#Map to see most occurring words per topic, but we should have it per topic
plot(stm_25_web_party, type = "perspectives", width = 80, topics=c(1,3), covarlevels = c(east_dummy ==0))



#Word cloud for a particular topic
cloud(stm_25_web_party, topic = 7, scale = c(2, 0.25))



# Estimating the metadata/topic relationship
K<-25
out$meta$party <- as.factor(out$meta$party)
prep <- estimateEffect(1:K ~ west_east_berlin + party, stm_25_web_party,
                       meta = out$meta, uncertainty = "Global")
save(prep, file = "bgse/thesis/results/prep.RData", compress = FALSE)

#Plot the results then

table_1 <- summary(prep, topics = 1)
table_1_output <- table_1[['tables']]

#Put it into a dataframe to be able to work with it
table_1_latex <- as.data.frame(table_1_output)
table_1_latex


#after labelling topics we can see how the effect of east vs west is
plot(prep, covariate = "west_east_berlin", topics = c(6, 7, 20),
     model = stm_25_content_web_party, method = "difference",
     cov.value1 = 1, cov.value2 = 0,
     xlab = "East ... West",
     main = "Effect of East vs. West",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Foreign Policy','Sarah Palin','Bush Presidency'))


#Find thoughts 


thoughts9 <- findThoughts(stm_25_web_party,
                        
                          #VERY IMPORTANT: here, use out$meta$text to get the 
                          #original texts instead of using documents since stm
                          #preprocessing removes those. This is the reason why
                          #we provide metadata=data in the preProcessor!!
                          texts = out$meta$text, 
                          n = 20,
                          topics = c(3))
print(thoughts9)

thoughts5 <- findThoughts(stm_25_east_party, 
                          texts = out$meta$text, 
                          n = 3,
                          topics = c(5))

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts9, width = 30, main = "Topic 9")
plotQuote(thoughts5, width = 30, main = "Topic 5")

#plotQuote takes a set of sentences
plotQuote(thoughts3$docs[[1:2]])


#Topic correlation:
mod.out.corr <- plot.topicCorr(stm_25_east_party)
plot(mod.out.corr)


#per document topic proportion
td_theta <- tidytext::tidy(stm_25_web_party, matrix = "theta")

selectiontdthteta<-td_theta[td_theta$document%in%c(1:15),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

thetaplot1


#STILL NEED:
print(xtable(table_1_latex, type = "latex", tabular.environment="longtable"), file = "test_table_latex.tex")
#maybe the error is in the name of input 4: partyB<fc>ndnis90/Die Gr<fc>nen

print(xtable(as.data.frame.table(table_1[["tables"]]), type = "latex"), file = "test.tex")

methods(xtable(table_1_latex))

stargazer(table_1, title="Regression Results")

