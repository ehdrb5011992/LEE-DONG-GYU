rm(list=ls())
gc()

library(janeaustenr)
library(tidyverse)
library(tidytext)

austen <- austen_books()
austen
data("stop_words")

# 1. Conduct preprocessing including tokenization (using unnest_tokens) and 
#    removing stopwords (using data(stop_words)).

tokenization <- austen %>% unnest_tokens(word,text)
tokenization
nrow(tokenization)

remove_stopwords <- tokenization %>% anti_join(stop_words)
remove_stopwords
nrow(remove_stopwords)

# 2. Calculate the term-document matrix whose column is novel (Documnet) , 
#    row is word, and value is word frequency. For example,

dat2 <- remove_stopwords  %>% mutate(word=str_extract(word,"[a-z]+")) %>%  count(book,word)  
term_document_freq <- dat2 %>% group_by(book) %>% mutate(proportion = n/sum(n)) %>%
  select(book,word,n) %>% spread(book,n) 
term_document_freq


# 3. Given the term-document matrix, each novel is represented as a vector
#    (which is sparse). Find two-most similar and different novels. Justify your
#    answers.

#3-1 Correlation through the tf-idf index.
temp <- remove_stopwords %>% mutate(word=str_extract(word,"[a-z]+")) %>% count(book,word,sort=T)
dat3 <- temp %>% bind_tf_idf(word,book,n)

term_document_tf_idf <- dat3 %>% select(book,word,tf_idf) %>% spread(book,tf_idf) 
term_document_tf_idf



NAME=names(term_document_tf_idf)[-1]
NAME
len <- length(NAME)
ans <- list()
cor_mat <- matrix(1,len,len)

for (i in 1:(len-1)) {
  
  mat <- c()
  
  for (j in (i+1):len) {
    
    
    dat3_1 <- term_document_tf_idf %>% gather(book,tf_idf,NAME[-i]) 
    f <-  as.formula(paste('~','tf_idf','+',str_c('`',NAME[i],'`')))
    
    result <- cor.test( f ,data=dat3_1[dat3_1$book == NAME[j],])
    est <- result$estimate ; p <- result$p.value ; conf.int <- result$conf.int;
    row <- round(c(est,p_value=p,conf_int=conf.int),3)
    mat <- rbind(mat,row)
    
    cor_mat[i,j] <- cor_mat[j,i] <- round(est,3)
  }
  
  rownames(mat) <-  NAME[(i+1):len]
  ans[[i]] <- mat
  
  
}

names(ans) <- NAME[1:5]
ans
colnames(cor_mat) <- rownames(cor_mat) <- NAME
cor_mat

#가장 높은 상관계수 : Northanger Abbey & Pride & Prejudice = 0.790
#가장 낮은 상관계수 : Emma & Persuasion = 0.018 
#가장 낮은 상관계수는 p-값이 높게 나오지만, 어차피 0 근처라 크게 상관없이 관계없다고 봐도 무방하다.



#3-2 Correlation through the proportion.

term_document_proportion <-  dat2 %>% group_by(book) %>% mutate(proportion = n/sum(n)) %>% 
  select(book,word,proportion) %>% spread(book,proportion)
term_document_proportion



NAME=names(term_document_freq)[-1]
NAME
len <- length(NAME)
ans <- list()
cor_mat <- matrix(1,len,len)

temp <- dat2 %>% group_by(book) %>% mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% spread(book,proportion)
for (i in 1:(len-1)) {
  
  mat <- c()
  
  for (j in (i+1):len) {
    
    dat3_1 <- temp %>% gather(book,proportion,NAME[-i]) 
    f <-  as.formula(paste('~','proportion','+',str_c('`',NAME[i],'`')))
    
    result <- cor.test( f ,data=dat3_1[dat3_1$book == NAME[j],])
    est <- result$estimate ; p <- result$p.value ; conf.int <- result$conf.int;
    row <- round(c(est,p_value=p,conf_int=conf.int),3)
    mat <- rbind(mat,row)
    
    cor_mat[i,j] <- cor_mat[j,i] <- round(est,3)
  }
  
  rownames(mat) <-  NAME[(i+1):len]
  ans[[i]] <- mat
  
}

names(ans) <- NAME[1:5]
ans
colnames(cor_mat) <- rownames(cor_mat) <- NAME
cor_mat


#가장 높은 상관계수 : Northanger Abbey & Mansfield Park = 0.795
#가장 낮은 상관계수 : Emma & Persuasion = 0.439


