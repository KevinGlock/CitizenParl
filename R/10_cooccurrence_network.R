## calculate a cooccurrence network for right and left actors


## load libraries

library("polmineR")
library("magrittr")
library("data.table")
library("networkD3")
library("igraph")
library("DT")
library("RColorBrewer")
library("tm")

use("GermaParl")


## calculate cooccurrences

m2008 <- partition("GERMAPARL", year = 2008, speaker = "Angela Merkel", interjection = FALSE)

terms_to_drop <- terms(m2008, p_attribute = "word") %>% noise() %>% unlist()

coocs <- Cooccurrences(m2008, p_attribute = "word", left = 5L, right = 5L, stoplist = terms_to_drop) %>% 
  decode() %>% # 
  ll() %>%
  subset(ll >= 11.83) %>%
  subset(ab_count >= 5)


## graph plotting

as_igraph(coocs) %>% plot()


G <- as_igraph(coocs)

links <- as.data.frame(cbind(as_edgelist(G, names = FALSE), rep(1, length(E(G)))))

links[,1] <- links[,1] - 1L # "zero-based" Index für Kanten 

links[,2] <- links[,2] - 1L # dito

colnames(links) <- c("source", "target", "value")

nodes <- data.frame(name = V(G)$name, group = rep(1, length(V(G)$name)), size = 3)


## network plotting

forceNetwork(
  Links = links, Nodes = nodes, Source = "source",
  Target = "target", Value = "value", NodeID = "name",
  Group = "group",
  opacity = 0.75, fontSize = 20, zoom = TRUE
)
