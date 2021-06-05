rm(list=ls())
library(dplyr)
library(tidyr)
library(stm)
library(ggplot2)
library(gridExtra)
library(cowplot)

# config

# read data 
data <- read.csv(
  '/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/data_new/tweets_v7.csv', 
  stringsAsFactors = FALSE
)
data$party <- as.factor(data$party)
data <- data %>% 
  arrange(party)

# preprocessing
docs_ <- data$text_cleaned_prep_text
processed <- textProcessor(docs_, 
                           metadata = data, 
                           removestopwords = FALSE, 
                           removenumbers = FALSE, 
                           removepunctuation = FALSE, 
                           lowercase = FALSE, 
                           stem = FALSE)
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta, 
                     lower.thresh = 3)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

save(docs, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/docs.RData", compress = FALSE)
save(vocab, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/vocab.RData", compress = FALSE)
save(meta, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/meta.RData", compress = FALSE)

# params 
K <- 10

# fit stm with party as topic prevalence
stm_model_10 <- stm(documents = docs, 
                   vocab = vocab,
                   K = K, 
                   prevalence =~ party,
                   max.em.its = 500, 
                   data = meta,
                   init.type = "Spectral", 
                   seed = 40)
save(stm_model_10, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/10_topic_base.RData", compress = FALSE)

stm_model_50 <- stm(documents = docs, 
                   vocab = vocab,
                   K = 50, 
                   prevalence =~ party,
                   max.em.its = 500, 
                   data = meta,
                   init.type = "Spectral", 
                   seed = 40)

save(stm_model_50, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/50_topic_base.RData", compress = FALSE)

stm_model_45 <- stm(documents = docs, 
                    vocab = vocab,
                    K = 45, 
                    prevalence =~ party,
                    max.em.its = 500, 
                    data = meta,
                    init.type = "Spectral", 
                    seed = 40)

save(stm_model_45, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/45_topic_base.RData", 
     compress = FALSE)


load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/50_topic_base.RData")
load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/10_topic_base.RData")
#ave.image()

# investigate topics 
labelTopics(stm_model_1, 
            1:K, 
            n = 10)

labelTopics(stm_model_50, 
            1:50, 
            n = 10)

labelTopics(stm_model_45, 
            1:45, 
            n = 10)




labelTopics(stm_model_50, 
            c(23, 36, 35, 46, 29, 3, 14, 12), 
            n = 10)

# investigate topic prevalence
plot(stm_model_10, type = "summary", xlim = c(0, .3))

#plot.STM(stm_model_1,type = "labels")

# investigate topic shares across parties
topics <- c(12, 25, 34, 43)
eff <- estimateEffect(formula = topics ~ party, 
                      stmobj = stm_model_45,
                      metadata = out$meta, 
                      uncertainty = "Global")

par(mfrow=c(2, 2))
for (i in topics){
  plot(eff, 
       covariate = "party", 
       topics = c(i),
       method = "pointestimate", 
       labeltype = 'custom',
       custom.labels = c('AfD', 'CDU', 'CSU', 'FDP', 'Gruene', 'Linke', 'SPD'), 
       main = paste('Topic', i))
}



# search for K 
n_topics_se <- searchK(documents = docs,
                       vocab = vocab,
                       K = c(5, 10, 20, 50, 100, 200),
                       prevalence =~ party,
                       max.em.its = 500,
                       data = meta,
                       init.type = "Spectral")

save(n_topics_se, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/n_topics_se.RData", compress = FALSE)





# search for K the second 
n_topics_se_3 <- searchK(documents = docs,
                       vocab = vocab,
                       K = c(75, 150),
                       prevalence =~ party,
                       max.em.its = 500,
                       data = meta,
                       init.type = "Spectral", 
                       cores = 2)

save(n_topics_se_3, 
     file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/n_topics_se_3.RData", 
     compress = FALSE)


load(file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/n_topics_se_2.RData")
load(file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/n_topics_se.RData")
load(file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/n_topics_se_3.RData")



x <- plot(n_topics_se)
plot(n_topics_se_2)


rbind(
  n_topics_se_2$results,
  n_topics_se$results %>% 
    filter(K > 50)
)

x <- rbind(
  n_topics_se_2$results,
  n_topics_se$results %>% 
    filter(K > 50), 
  n_topics_se_3$results
) %>% 
  select(-c(semcoh, bound)) %>% 
  gather(var, val, -c(K, em.its))

axis_formatter <- function(x) {
  #x[x == 0.01] <- 0
  formatC(x, format = "f", big.mark = ",", digits = 1)
  #trans_format("log10", math_format(10^.x))
}


ggplot(x, aes(x = K, y = val)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(vars(var), scales = 'free', ncol = 4) + 
  scale_y_continuous(labels = axis_formatter) +
  theme(text = element_text(size = 9),
        axis.text = element_text(size = 9),
        strip.text = element_text(size = 9, face = 'bold'), 
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(), 
        legend.title = element_text(size = 9, face = "bold"),
        axis.title.y = element_blank()) + 
  theme_gray()


ggsave("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/final_plots/stm_K.pdf", 
       height = 3, width = 10)

# interesting questions once you have a topic model
# 1) topic composition: most significant words by 5 metrics 
# 2) topic usage across parties: plots from 
# 3) hellinger MDS on tweets
# 4) hellinger MDS on user
# 5) Bi Plot on MDS 
# 6) model convergence

#############################################################################################
# label topics
#############################################################################################

i <- 45
labelTopics(stm_model_45, 
            i, 
            n = 10)
findThoughts(stm_model_45, texts = meta$text, n = 20, topics = i)

topic_labels <- c(
  'Prevent the diesel ban', #1
  'Bavaria - secure and successful', #2
  'Campaign events: Guttenberg, Schulz & co', #3
  'Crime, violence and migrants', #4
  'Security & Herrmann', #5
  '"Solidarity pension" & improv. country life', #6
  'Strengthen family rights', #7
  'Free daycare centres and education', #8
  '(unclear)', #9 
  '(unclear)', #10 
  '(unclear)', #11
  '(unclear)', #12
  'Merkel attending events', #13
  'Various campaigning events', #14
  'Various local campaigning events', #15
  'Coalition scenarios & green topics', #16
  'Campaigning, buttons & demolitions', #17
  'Gratitude & saying thanks', #18
  'Campaigning & campaign buttons', #19
  'Environmental protection & security', #20
  '(unclear)', #21
  'Campaigning', #22
  'Strenthening employee rights', #23
  '(unclear)', #24
  '(unclear)', #25
  'FDP & Lindner', #26
  'Shaping the future', #27
  'Failure, change & Gauland', #28
  'Go and vote', #29
  'Invest in education, taxes, limit rents', # 30, 
  'Social justice', # 31
  'Keeping pensions stable', # 32 
  'Solidarity tax', # 33
  'Integration & language', # 34
  'TV duel & Merkel', # 35
  'Right of return (to fulltime)', # 36
  '(unclear)', #37
  '(unclear)', #38
  'Discussions at the Dreschschuppenfest', #39
  'Panel discussions & nazis', #40
  'Tax reliefs, listening & "Gefährder"', #41
  '(unclear)', #42
  'Illegal immigration, AfD & Alice Weidel', #43
  'Family migration, constitution, inviolable', #44
  'General election vocabulary' #45
)

topic_labels_enum <- c()

i <- 0 
for (x in topic_labels) {
  i <- i + 1
  topic_labels_enum <- c(topic_labels_enum, paste0(i, ":", " ", x))
}

# notes: there is a lot on campaigning 


#############################################################################################
# topic plots 
#############################################################################################

# stm model 
load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/45_topic_base.RData")
load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/docs.RData")
load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/vocab.RData")
load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/meta.RData")

# investigate topic shares across parties
topics <- 1:45
eff <- estimateEffect(formula = topics ~ party, 
                      stmobj = stm_model_45,
                      metadata = meta, 
                      uncertainty = "Global")

mean_ <- data.frame(
  AfD = numeric(), 
  CDU = numeric(), 
  CSU = numeric(), 
  FDP = numeric(), 
  Gruene = numeric(), 
  Linke = numeric(), 
  SPD = numeric()
)

uci_ <- data.frame(
  AfD = numeric(), 
  CDU = numeric(), 
  CSU = numeric(), 
  FDP = numeric(), 
  Gruene = numeric(), 
  Linke = numeric(), 
  SPD = numeric()
)

lci_ <- data.frame(
  AfD = numeric(), 
  CDU = numeric(), 
  CSU = numeric(), 
  FDP = numeric(), 
  Gruene = numeric(), 
  Linke = numeric(), 
  SPD = numeric()
)


topic_ <- c()
party_ <- c()
type_ <- c()
score_ <- c()

for (i in topics) {
  x <-   plot(eff, 
              covariate = "party", 
              topics = c(i),
              method = "pointestimate", 
              labeltype = 'custom',
              custom.labels = c('AfD', 'CDU', 'CSU', 'FDP', 'Gruene', 'Linke', 'SPD'), 
              main = paste('Topic', i))
  # means
  topic_ <- c(topic_, rep(i, x$uvals %>% length()))
  party_ <- c(party_, levels(x$uvals))
  type_ <- c(type_, rep('mean', x$uvals %>% length()))
  score_ <- c(score_, x$means[[1]])
  
  # uci
  topic_ <- c(topic_, rep(i, x$uvals %>% length()))
  party_ <- c(party_, levels(x$uvals))
  type_ <- c(type_, rep('uci', x$uvals %>% length()))
  score_ <- c(score_, x$cis[[1]][2, ])
  
  # lci
  topic_ <- c(topic_, rep(i, x$uvals %>% length()))
  party_ <- c(party_, levels(x$uvals))
  type_ <- c(type_, rep('lci', x$uvals %>% length()))
  score_ <- c(score_, x$cis[[1]][1, ])

}

topic_plot_df <- data.frame(
  topic = topic_, 
  party = party_, 
  type = type_, 
  score = score_
)

## join topic labels 
#topic_plot_df %>% 
#  left_join(
#    data_frame(
#      labels = topic_labels, 
#      topic = 1:45
#    ), 
#    by = 'topic'
#  )


# reshaping and stuff
topic_plot_df <- topic_plot_df %>% 
  spread(key = type, value = score)

topic_plot_df$party <- as.factor(topic_plot_df$party)

# new indices according to mean desc
topic_reindexed <- topic_plot_df %>% 
  group_by(topic) %>% 
  summarise(max_mean = max(mean)) %>% 
  mutate(label = topic_labels) %>% 
  arrange(desc(max_mean)) %>% 
  mutate(
    topic_reindexed = 1:45, 
    label_enum = paste(topic_reindexed, label)
  )

save(topic_reindexed, file = "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/topic_reindexed.RData", compress = FALSE)

topic_plot_df <- topic_plot_df %>% 
  left_join(
    topic_reindexed %>% 
      select(topic, topic_reindexed), 
    by = 'topic'
  )

# topic adj
topic_plot_df$topic_adj <- topic_plot_df$topic_reindexed - 0.4 + topic_plot_df$party %>% as.numeric() * 0.1

# color stuff 
colors <- c('#039FE1', '#000000', '#505050', '#FFED00', '#1FA22D', '#A20F97', '#E2031A')
col_scale <- scale_colour_manual(name = "party", values = colors)


# plot
plot <- ggplot(data = topic_plot_df, aes(colour = party)) + 
  geom_point(aes(x = topic_adj, y = mean), size = 1) + 
  geom_segment(aes(x = topic_adj, y = lci, xend = topic_adj, yend = uci)) + 
  col_scale + 
  ylim(0, 0.1) + 
  ylab('topic share') + 
  scale_x_continuous(name='topic', 
                     limits = c(0.5, 45.5), breaks = 1:45, 
                     labels = topic_reindexed$label_enum[1:45]) + 
  theme(text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9), 
        axis.ticks.x = element_blank(), 
        legend.position = c(1, 1),
        legend.title = element_text(face = "bold"),
        legend.justification = c("right", "top")) + 
  geom_vline(xintercept = seq(1.5, 44.5, 1), size = 0.2, linetype = 'dotted')

ggsave("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/final_plots/topic_prop.pdf", 
       height = 6, width = 10)



######## export average topic position per user 

# add topics to df
#topic_prop <- as.data.frame(stm_model_45$theta)
#
## reorder
#colnames(topic_prop) <- topic_reindexed %>% arrange(topic) %>% select(topic_reindexed) %>% unname() %>% unlist()
#topic_prop <- topic_prop[ , order(as.numeric(names(topic_prop)))]
#colnames(topic_prop) <- sprintf(
#  'topic_%s', 
#  colnames(topic_prop) 
#)
#
##
#meta_ <- cbind(meta, topic_prop)
#write.csv(meta_, "/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm/meta_.csv")

load("/Users/richardknudsen/Dropbox/Dokumente_Richard/RA_upf/code/stm_models/topic_reindexed.RData")

# add topics to df
topic_prop <- as.data.frame(stm_model_45$theta)

# reorder
colnames(topic_prop) <- topic_reindexed %>% arrange(topic) %>% select(topic_reindexed) %>% unname() %>% unlist()
topic_prop <- topic_prop[ , order(as.numeric(names(topic_prop)))]
colnames(topic_prop) <- sprintf(
  'topic_%s', 
  colnames(topic_prop) 
)
meta_ <- cbind(meta, topic_prop)



