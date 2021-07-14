library(dplyr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(patchwork)
library(lme4)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Figure 1 ----------------------------------------------------------------

ids <- read.csv('ids.csv')

count_ids <- ids %>%
  distinct(Name_y, Family) %>%
  group_by(Family) %>%
  mutate(n_of_lang=n()) %>%
  distinct(Family, n_of_lang) 

count_glottlog <- read.csv('data/Languages.csv') %>%
  select(name, child_language_count) %>%
  rename(Family = name)

fig1 <- inner_join(count_ids, count_glottlog, by=c("Family")) %>%
  group_by() %>%
  mutate(sum_data = sum(n_of_lang), sum_glot = sum(child_language_count)) %>%
  mutate(perc_data = (n_of_lang/sum_data), perc_real = (child_language_count/sum_glot)) %>%
  pivot_longer(., cols = c(perc_data,perc_real), names_to = "Var", values_to = "Val") %>%
  ggplot(aes(y=reorder(Family, Val), x=Val, fill=Var))+
  geom_bar(stat='identity', position = "dodge")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_grey(name = "Data", labels = c("IDS", "Glottolog"))+
  labs(x='Percentage', y='Family')+
  theme(axis.title.y=element_blank(), legend.position = 'bottom')

fig2 <- inner_join(count_ids, count_glottlog, by=c("Family")) %>%
  group_by() %>%
  mutate(sum_data = sum(n_of_lang), sum_glot = sum(child_language_count)) %>%
  mutate(perc_data = (n_of_lang/sum_data), perc_real = (child_language_count/sum_glot)) %>%
  mutate(diff = perc_data - perc_real) %>%
  ggplot(aes(x=diff, y=reorder(Family, diff)))+
  geom_bar(stat='identity')+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.32, 0.32))+
  labs(x='Perc. in IDS - Perc. in Glottolog', y='Family')+
  theme(axis.title.y=element_blank())+
  geom_text(aes(label=n_of_lang), position=position_dodge(width=0.9), hjust=-0.25)

fig1 + fig2 +
  plot_annotation(tag_levels = 'A')+
  ggsave('figures/fig1.pdf',width = 10, height = 5)


# Figure 2 ----------------------------------------------------------------


distance <- read.csv('distance_ids.csv',
                     row.names="X")
coords <- read.csv('coords.csv')

distance

distance[distance <= 1500] = 1
distance[distance > 1500] = 0
adj_matrix <- data.matrix(distance)

g_t <- graph.adjacency(adj_matrix, 
                       mode = "undirected", 
                       diag = FALSE)

g_t <- set.vertex.attribute(g_t, "x", value=coords$Longitude) 
g_t <- set.vertex.attribute(g_t, "y", value=coords$Latitude) 
g_t <- set.vertex.attribute(g_t, "family", value=as.character(coords$Family))
g_t <- set.vertex.attribute(g_t, "name", value=as.character(coords$Name_y))

Isolated <-  which(degree(g_t)==0)
g_t <-  delete.vertices(g_t, Isolated)

ggraph(g_t, x=x, y=y)+
  borders("world", colour="gray50", fill=alpha("gray50", 1))+
  theme_void()+
  geom_edge_link(alpha=1, width=0.5)+
  geom_node_point(size=2, alpha=1, aes(color=family))+
  theme(legend.position = "none")+
  ggsave('figures/fig2.pdf',width = 10, height = 4)

# Figure 3 ----------------------------------------------------------------

# see analysis.ipynb

# Figure 4 ----------------------------------------------------------------

read.csv('NPM_datasets/NPM_distribution_1000.csv') %>%
  mutate(Present=replace(Present, Present==1, 'Yes')) %>%
  mutate(Present=replace(Present, Present==0, 'No')) %>%
  ggplot(aes(x=NPM, fill=Present))+
  geom_density(alpha=0.5, color=alpha('white', 0))+
  facet_wrap(~Family, scales = 'free_y')+
  scale_color_grey()+
  theme_classic()+
  ggsave('figures/fig4a.pdf', dpi=300, width = 10, height = 5)

read.csv('NPM_datasets/NPM_distribution_2000.csv') %>%
  mutate(Present=replace(Present, Present==1, 'Yes')) %>%
  mutate(Present=replace(Present, Present==0, 'No')) %>%
  ggplot(aes(x=NPM, fill=Present))+
  geom_density(alpha=0.5, color=alpha('white', 0))+
  facet_wrap(~Family, scales = 'free_y')+
  scale_color_grey()+
  theme_classic()+
  ggsave('figures/fig4b.pdf', dpi=300, width = 10, height = 5)
