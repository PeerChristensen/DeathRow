# DEATH ROW LAST STATEMENTS
# Word frequency and topic modelling of last statements of inmates on Death Row
# Source: https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html

library(tidytext)
library(tidyverse)
library(tm)
library(stm)
library(furrr)
library(beepr)
library(gganimate)
library(ggrepel)
library(ggthemes)
library(widyr)
library(ggraph)
library(igraph)
library(wordcloud)

df <- read_csv("last_statements.csv")

########################################
### Word frequency #####################
########################################

# UNIGRAMS

df1 <- df %>%
  select(Statement)    %>%
  mutate(Statement = tolower(Statement)) %>%
  unnest_tokens(word, Statement)    %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  mutate(word = removeWords(word,c(stopwords(),"warden","y'all","ya'll"))) %>%
  filter(word!="") %>%
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  mutate(row = rev(row_number()))  %>%
  top_n(20,n)

df1 %>%
  ggplot(aes(row, n, fill = log(n))) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = df1$row,
    labels = df1$word,
    expand = c(0,0)) + 
  theme_minimal() +
  scale_fill_continuous_tableau()

dfCloud <- df %>%
  select(Statement)    %>%
  mutate(Statement = tolower(Statement)) %>%
  unnest_tokens(word, Statement)    %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  mutate(word = removeWords(word,c(stopwords(),"warden","y'all","ya'll"))) %>%
  filter(word!="") %>%
  group_by(word) %>% 
  summarise(n = n()) %>%
  filter(n>20)

set.seed(86225)
wordcloud(dfCloud$word,dfCloud$n,colors = c("#4E79A7","#F28E2B","#E15759"))

# BIGRAMS

df2 <- df %>%
  select(Statement) %>%
  unnest_tokens(bigram, Statement, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",],by = c("word1" = "word")) %>% 
  filter(!word1 %in% c("","warden","y'all","ya'll"),
         !word2 %in% c("","warden","y'all","ya'll")) %>%
  transmute(bigram = paste(word1,word2)) %>%
  group_by(bigram) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  mutate(row = rev(row_number()))  %>%
  top_n(20,n)

df2 %>%
  ggplot(aes(row, n, fill = log(n))) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = df2$row,
    labels = df2$bigram,
    expand = c(0,0)) + 
  theme_minimal() +
  scale_fill_continuous_tableau()

# TRIGRAMS

df3 <- df %>%
  select(Statement) %>%
  unnest_tokens(trigram, Statement, token = "ngrams", n = 3) %>%
  filter(!trigram %in% c("","warden","y’all","ya’ll")) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",],by = c("word1" = "word")) %>%
  transmute(trigram = paste(word1,word2,word3)) %>%
  group_by(trigram) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))  %>%
  mutate(row = rev(row_number()))  %>%
  top_n(20,n)

df3 %>%
  ggplot(aes(row, n, fill = log(n))) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = df3$row,
    labels = df3$trigram,
    expand = c(0,0)) + 
  theme_minimal() +
  scale_fill_continuous_tableau()

########################################
### Topic modelling ####################
########################################

# preprocess
dfTM <- df %>%
  select(Statement, Name2)    %>%
  mutate(Statement = tolower(Statement)) %>%
  unnest_tokens(word, Statement)    %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  mutate(word = removeWords(word,c(stopwords(),"warden","y'all","ya'll"))) %>%
  filter(word!="")

# sparse matrix
dfSparse <- dfTM     %>%
  count(Name2, word) %>%
  cast_sparse(Name2, word, n)

# find n topics
plan("default")
start_time_stm <- Sys.time()

nTopics <- seq(2,15)

many_models_stm <- data_frame(K = nTopics) %>%
  mutate(topic_model = future_map(K, ~stm(dfSparse, K = ., verbose = TRUE)))

end_time_stm <- Sys.time() 
end_time_stm - start_time_stm # 7.6 mins

heldout <- make.heldout(dfSparse)

k_result <- many_models_stm %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfSparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, dfSparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
  mutate(mean_semantic_coherence = map(semantic_coherence,mean) %>% unlist(),
         mean_exclusivity = map(exclusivity,mean) %>% unlist())

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")

excl_sem_plot <- k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  filter(K %in% seq(2,15)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

excl_sem_plot

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')

animate(anim_plot, nframes = 14, fps = 0.5)

k_result %>% 
  ggplot(aes(x=mean_semantic_coherence, y = mean_exclusivity,
                        label=K)) +
  geom_point(size=3) +
  geom_text_repel(size=5) 

# select stm model

topic_model_stm <- k_result %>% 
  filter(K ==7)             %>% 
  pull(topic_model)         %>% 
  .[[1]]

topic_model_stm

# plot stm model

td_beta <- tidy(topic_model_stm)

top_terms <- td_beta %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number()))

top_terms %>%
  ggplot(aes(order, beta,fill = factor(topic))) +
  ggtitle("Topics") +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(
    breaks = top_terms$order,
    labels = top_terms$term,
    expand = c(0,0)) +
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,max(top_terms$beta))) +
  labs(x="",y=expression(beta)) +
  theme(axis.title=element_blank()) + 
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank(),
        strip.text.x = element_text(size=16)) +
  scale_fill_tableau()

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(dfSparse))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  top_n(15, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = rev(topic))) +
  geom_col(show.legend = FALSE) +
  #geom_text(hjust = 1.05, vjust=0, size = 3, family = "Helvetica") +
  geom_text(hjust = 0, nudge_y = 0.0100, size = 6,
            family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.6),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma)) +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank()) +
  scale_fill_tableau()

stm_plot

# pairs

word_pairs <- df %>% 
  select(Statement,Name2)    %>%
  mutate(Statement = tolower(Statement)) %>%
  unnest_tokens(word, Statement)    %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  mutate(word = removeWords(word,c(stopwords(),"warden","y'all","ya'll"))) %>%
  filter(word!="") %>%
  pairwise_count(word, Name2, sort = TRUE)

set.seed(611)

word_pairs %>%
  filter(n >= 20)               %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#4E79A7") +
  ggtitle("Word pairs") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme_void()

