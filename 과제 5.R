
library(tidyverse)
library(lubridate)
library(aRxiv)
library(topicmodels) #lda
library(tidytext)
library(tidyverse)
library(scales) #ggplot scale
library(e1071) #svm
library(randomForest) #randomforest
library(Epi)   # ROC, AUC
library(caret) #confusion matrix
library(MLmetrics) # F1 score

rm(list=ls())
gc()

# bio <- arxiv_search(query = '"biology"', limit = 1000)
# write.csv(bio,file='bio.csv') # write csv.
# adm <- arxiv_search(query = '"administration"', limit = 1000)
# write.csv(adm,file='adm.csv') # write csv.

bio <- read.csv('C:\\Users\\82104\\Desktop\\bio.csv') # data import
bio <- bio[,-1] %>% 
  as_tibble()
adm <- read.csv('C:\\Users\\82104\\Desktop\\adm.csv') # data import
adm <- adm[,-1] %>% 
  as_tibble()



## 1. Combine two data sets with additional categroical variable that represents the source, bio or adm.

bio <- bio %>% 
  select(title,abstract) %>% 
  as_tibble() %>% 
  mutate(title=as.character.Date(title),
         abstract=as.character.Date(abstract),
         category='bio')
adm <- adm %>% 
  select(title,abstract) %>% 
  as_tibble() %>% 
  mutate(title=as.character.Date(title),
         abstract=as.character.Date(abstract),
         category='adm')

data <- bio %>% 
  bind_rows(adm)

data

## 2. (Document Modeling) Apply the LDA with 2 topics and evaluate the model performance.

data_dtm <- data %>% 
  mutate(title = str_c(category,title,sep="::")) %>% 
  unnest_tokens(word, abstract) %>% 
  anti_join(stop_words,by='word') %>% 
  filter(!str_detect(word,'[\\d]') ) %>% 
  filter(!str_detect(word,'^_[a-z]+')) %>%  #eliminating digit
  count(title,word) %>% 
  cast_dtm(title,word,n)

data_lda <- LDA(data_dtm, k = 2,
              control = list(seed = 1234))

#gamma
data_gamma <- tidy(data_lda, matrix = "gamma") %>% 
  mutate(document = reorder(document, gamma * topic))


docu_gamma <- data_gamma %>%
  separate(document, c("subject", "document"), sep = "::", convert = T)

#이 box가 위아래로 잘 갈려야함.
docu_gamma %>%
  mutate(subject = reorder(subject, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ subject)

########

docu_classifications <- docu_gamma %>%
  group_by(subject, document) %>%
  top_n(1, gamma) %>%
  ungroup()

subject_topics <- docu_classifications %>%
  count(subject, topic) %>%
  group_by(subject) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = subject, topic)
subject_topics #lda를 통해 돌려 분류함.

# 어느 문서가 분류가 잘못되었나?
wrong_docu <- docu_classifications %>%
  inner_join(subject_topics, by = "topic") %>%
  filter(subject != consensus) %>% 
  count(subject, consensus,document) %>%
  ungroup() %>%
  arrange(desc(n))

accuracy_docu_assign <- 1- sum(wrong_docu$n)/nrow(docu_classifications)
str_c("Documents & Topic Accuracy:",percent(accuracy_docu_assign),sep=' ')

################################################

#Plus
#오분류표 시각화  + 단어기준 분류
assignments <- augment(data_lda, data = data_dtm)
assignments


assignments <- assignments %>%
  separate(document, c("subject", "document"), sep = "::", convert =T) %>% 
  inner_join(subject_topics, by = c(".topic" = "topic"))
print(assignments, n = 7)


assignments %>%
  count(subject,document, consensus, wt = count) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, subject, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Paper words were assigned to",
       y = "Paper words came from",
       fill = "% of assignments")


wrong_words <- assignments %>%
  filter(subject != consensus) %>%
  count(subject, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

accuracy_word_assign <- 1- sum(wrong_words$n)/nrow(assignments)
str_c("Words & Topic Accuracy:",percent(accuracy_word_assign),sep=' ')

## 3. (Document Classification) Train LDA to the pooled data with 10 topics, and apply logistic regression, 
## random forest, and SVM. Then compare their performance.


data_lda2 <- LDA(data_dtm, k = 10,
                control = list(seed = 1234))
data_gamma2 <- tidy(data_lda2, matrix = "gamma") %>% 
  mutate(document = reorder(document, gamma * topic)) %>% 
  spread(topic,gamma)
docu_gamma2 <- data_gamma2 %>%
  separate(document, c("subject", "document"), sep = "::", convert = T) %>% 
  mutate(subject=as.factor(subject))



f <- as.formula(paste('subject','~',str_c(paste0("`",1:10,"`"),collapse = '+')))

#### 3-1) Logistic
#logistic
logistic <- glm(f, data=docu_gamma2, family = 'binomial')
pred_logistic <- as.factor(ifelse(logistic$fitted.values >=0.5 , 'bio','adm'))
confusion_logistic <- confusionMatrix(pred_logistic,docu_gamma2$subject)
confusion_logistic
ROC(test=pred_logistic, stat=docu_gamma2$subject, plot="ROC", AUC=T, main="Logistic")

#### 3-2) SVM
#svm
support_vector_machine <- svm(f,data=docu_gamma2)
pred_svm <- as.factor(support_vector_machine$fitted)
confusion_svm <- confusionMatrix(pred_svm,docu_gamma2$subject)
confusion_svm
ROC(test=pred_svm, stat=docu_gamma2$subject, plot="ROC", AUC=T, main="SVM")


#### 3-3) Random Forest
#random forest
dat_rf <- docu_gamma2 %>% 
  rename(factor1=`1`,factor2=`2`,factor3=`3`,factor4=`4`,factor5=`5`,
         factor6=`6`,factor7=`7`,factor8=`8`,factor9=`9`,factor10=`10`)
set.seed(1234)

rf = randomForest(subject~factor1+factor2+factor3+factor4+factor5+factor6+
                    factor7+factor8+factor9+factor10, data=dat_rf,mtry = floor(sqrt(10)))
pred_rf <- rf$predicted
confusion_rf <- confusionMatrix(pred_rf,docu_gamma2$subject)
confusion_rf
ROC(test=pred_rf, stat=docu_gamma2$subject, plot="ROC", AUC=T, main="SVM")



#F1 score

f1_logistic <- round(F1_Score(docu_gamma2$subject,pred_logistic),3)
f1_svm <- round(F1_Score(docu_gamma2$subject,pred_svm),3)
f1_rf <- round(F1_Score(docu_gamma2$subject,pred_rf),3)

X <- cbind(f1_logistic,f1_svm,f1_rf)
rownames(X) <- "F1 Score"
colnames(X) <- c('Logistic','SVM','Random_Forest')
X
