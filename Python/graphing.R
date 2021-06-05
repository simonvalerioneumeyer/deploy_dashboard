library(data.table)
library(DBI)
library(RMySQL)
library(tm)
library(SnowballC)
library(xtable)
library(ggplot2)
library(Rmisc)
library(stm)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tidytext)
library(reshape2)
library(MASS)

#Loading the models:
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/docs_NEW.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/meta_NEW.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/out.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/vocab_NEW.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/results_ksearch.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/storage.RData')
#load(file='~/thesis/Master_thesis/Python/results_june_4/stm_25_content_web_party.RData')
#load(file='~/thesis/Master_thesis/Python/results_june_4/stm_25_state_party.RData')
#load(file='~/thesis/Master_thesis/Python/results_june_4/stm_25_web_party.RData')
#load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/stm_35_cont_web_party.RData')
load(file='/Users/simonneumeyer/Desktop/BGSE_DS/BGSE_term_3/thesis/Master_thesis/Python/results_june_4/stm_50_cont_web_party_FINAL.RData')
#Python/results_june_4/stm_50_cont_web_party_FINAL.RData

#We stick to k=50 as this gives the best accuracy overall.

#This will gives per topic the respective topics back!
K <- 50
labelTopics(stm_50_cont_web_party_FINAL, 
            1:K, 
            n = 10)

# investigate topic prevalence
plot(stm_50_cont_web_party_FINAL, type = "summary", xlim = c(0, .3))
plot(stm_50_cont_web_party_FINAL, type = "hist", xlim = c(0, .3))



#Topics labeled:
#topics <- list(Topic_1= 'Thanks'
#               , Topic_2= 'SPD national party congress 21'
#               , Topic_3= 'Thanks and welcoming of new members'
#               , Topic_4= 'Party campaign FDP'
#               , Topic_5= 'Reactions SPD national party congress 21'
#               , Topic_6= 'Corona vaccination program'##
#               , Topic_7= 'News'
#               , Topic_8= 'Fight against racial discrimination' ##
#               , Topic_9= 'News in Thüringen'
#               , Topic_10= 'Financial subventions/governmental aid'##
#               , Topic_11= 'Bavaria'
#               , Topic_12= 'Covid-19'##
#               , Topic_13= 'Political discussion'
#               , Topic_14= 'The pirate party'
#               , Topic_15= 'Annalena Baerbock'
#               , Topic_16= 'undefined'
#               , Topic_17= 'The left party'
#               , Topic_18= 'CSU/Green Party'
#               , Topic_19= 'SPD'
#               , Topic_20= 'FDP'
#               , Topic_21= 'Tuerkish Tweets/Cem Özdemir'
#               , Topic_22= 'Europa'
#               , Topic_23= 'non substantial'
#               , Topic_24= 'Social fairness/tax policy/welfare'##
#               , Topic_25= 'governmental news'
#               , Topic_26= 'Discussions with media'
#               , Topic_27= 'Labour/Social labour sector'##
#               , Topic_28= 'News'
#               , Topic_29= 'Europa policy'##
#               , Topic_30= 'Freedom'
#               , Topic_31= 'Comments'
#               , Topic_32= 'Israel'
#               , Topic_33= 'Justice/East Germany/Solidarity'
#               , Topic_34= 'Health/Safety/Death'
#               , Topic_35= 'Measures against COVID-19'
#               , Topic_36= 'Campaigns'
#               , Topic_37= 'Sachsen-Anhalt '
#               , Topic_38= 'SPD social welfare program'
#               , Topic_39= 'Armin Laschet'
#               , Topic_40= 'AfD'
#               , Topic_41= 'AfD call for action/donations'
#               , Topic_42= 'SPD/CDU great coalition'
#               , Topic_43= 'Fiscal policy'##
#               , Topic_44= 'Comments on recent events '
#               , Topic_45= 'Climate policy' ##
#               , Topic_46= 'Thüringen'
#               , Topic_47= 'Law/Prosecution/Court'
#               , Topic_48= 'Bodo Ramelow'
#               , Topic_49= 'not identfiable'
#               , Topic_50= 'Economic policy') ##
#
top_topics = c('Topic_6',
                  'Topic_7', 
                  'Topic_9', 
                  'Topic_11', 
                  'Topic_18', 
                  'Topic_26',
                  'Topic_27', 
                  'Topic_37',
                  'Topic_43', 
                  'Topic_44')

top_topics_for_theta = c(6,
                         7,
                         9, 
                         11, 
                         18, 
                         26, 
                         27,
                         37, 
                         43,
                         44)

#Expected Topic Proportion 
df_ <- as_tibble(colMeans(make.dt(stm_50_cont_web_party_FINAL, meta = NULL))[-c(1)])
df_$topic = names(topics)

N <- 50
i <- 1
content_ <- vector("list", N)
for (t in topics){
  content_[i] <- t
  i <- i + 1
}
df_$content = content_
df_$content <- as.character(df_$content)


df_topics_to_use = df_[df_$topic %in% top_topics,]

#NB: you can play around with sized of saving for different pixel quality
p<-ggplot(data=df_topics_to_use) + aes(reorder(content, -value), y=value) +
  geom_bar(stat="identity", fill= 'steelblue') +
  theme(axis.text.x = element_text(angle = 70, vjust =1, hjust=1)) + 
  ggtitle("Expected Topic Proportion") +
  xlab("Topic") + ylab("Expected Value") 
p


#Top Tweets Topic Distribution:

#Create an index column as reference
copy_meta <- cbind(meta)
copy_meta$ID <- seq.int(nrow(copy_meta)) #use this instead of rownames because
                                        #td_theta names all documents not in the
                                        #same format as row.names of meta df


top_retweet <- copy_meta %>% 
  group_by(party) %>% 
  top_n(1, retweet_count) %>% 
  sample_n(size = 1) #sample size=1 for the parties that have many tweets with
                     #the same number of retweets; no max retweet count


top_fav <- copy_meta %>% 
  group_by(party) %>% 
  top_n(1, favorite_count) %>% 
  sample_n(size = 1)


td_theta <- tidytext::tidy(stm_50_cont_web_party_FINAL, matrix = "theta")

#filter the necessary topics only:
td_theta <- td_theta[td_theta$topic%in%top_topics_for_theta,]


#TOP favorites:

#Get the topic distribution for the necessary tweets only
theta_fav_selection<-td_theta[td_theta$document%in%top_fav$ID,]

new_theta_fav <- theta_fav_selection %>% 
  pivot_wider(names_from = topic, values_from = gamma)

temp_theta_fav <- merge(x = new_theta_fav, y = top_fav[ , c('ID','party')], by.x = c('document'), by.y = c('ID') , all.x=TRUE)

new_theta_fav <- temp_theta_fav %>% 
  pivot_longer(cols = c('6', '7', '9', '11', '18', '26', '27', '37', '43', '44') ,names_to = 'topic')


thetaplot1<-ggplot(new_theta_fav, aes(y=value, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = TRUE) +
  facet_wrap(~ party, ncol = 3, nrow = 3) +
  labs(title = "Topic Distribution of most favored tweets per party",
       y = expression(theta), x = "Topics") +
  scale_fill_discrete(name = "labels",labels = c("COVID-19", "Economic/Taxes/Governmental Aid", "COVID-19 vaccination", "Fight against racial discrimination", "Europe", "Labor Market/Care Sector", "Social Policy", "Climate Policy", "Social Inequality", "Digitisation and future of Germany"))

thetaplot1

#TOP favorites:
#c('6', '7', '9', '11', '18', '26', '27', '37', '43', '44')
#Get the topic distribution for the necessary tweets only
theta_ret_selection<-td_theta[td_theta$document%in%top_retweet$ID,]

new_theta_ret <- theta_ret_selection %>% 
  pivot_wider(names_from = topic, values_from = gamma)

temp_theta_ret <- merge(x = new_theta_ret, y = top_retweet[ , c('ID','party')], by.x = c('document'), by.y = c('ID') , all.x=TRUE)

new_theta_ret <- temp_theta_ret %>% 
  pivot_longer(cols = c('6', '7', '9', '11', '18', '26', '27', '37', '43', '44') ,names_to = 'topic')


thetaplot2<-ggplot(new_theta_ret, aes(y=value, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = TRUE) +
  facet_wrap(~ party, ncol = 3, nrow = 3) +
  labs(title = "Topic Distribution of most retweeted tweets per party",
       y = expression(theta), x = "Topics") +
  scale_fill_discrete(name = "labels", labels = c("COVID-19", "Economic/Taxes/Governmental Aid", "COVID-19 vaccination", "Fight against racial discrimination", "Europe", "Labor Market/Care Sector", "Social Policy", "Climate Policy", "Social Inequality", "Digitisation and future of Germany"))

thetaplot2


