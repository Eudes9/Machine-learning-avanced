install.packages('tidyverse')
install.packages('igraph')
install.packages('sbm')
install.packages('latentnet')
install.packages("readr")
library(readr)
library(tidyverse)
library(igraph) # a common network analysis package
library(sbm) # a package for blockmodeling
library(latentnet) # a package for latent space models

# read in the data
edges <- read_csv("C:/Users/jeane/Documents/networks/sampson_edges.csv")
nodes <- read_csv("C:/Users/jeane/Documents/networks/sampson_nodes.csv")

# some explanation of the data:
# this is the Sampson monastery data,
# which is a network of monks in a monastery
# There was a conflict in the monastery, which Sampson observed.
# He classified the monks into 3 factions
# (loyalists, young turks and outcasts)
# based on their behavior during the conflict.
# He also recorded social relationships between the monks.
# Each monk named 3 other monks they liked
# This is ranked 1 (lowest) to 3 (highest)

# let's look at the data
nodes
edges

############
## BASICS ##
############

# let's turn this into an igraph object
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
# let's look at the object
g
# the graph is directed (D), named (N), and weighted (W)
# we have 18 vertices (v), 56 edges (e)

# we access the vertices and edges with V() and E()
# we access the attributes with $attribute
V(g)
V(g)$faction
E(g)
E(g)$weight
# we can create new ones and manipulate them
# first, let's create a copy
g2 <- g
E(g2)$new_weight <- E(g2)$weight + 1
# we can filter the graph
g2 <- g2 - E(g2)[E(g2)$weight == 1]
# we can also filter the vertices
g2 <- g2 - V(g2)[V(g2)$faction == 1]

##############
## PLOTTING ##
##############

# let's try and plot it
plot(g)
# let's snoop around the options and make this look better
?plot.igraph
# we can use the layout argument to specify how the graph is laid out
# notice how different layouts give different results
# also notice how this is a random process -- you might want to set a seed
# or to compute one layout and use it for all plots
# we can use the vertex.color argument to specify the color of the nodes
# there's plenty other options to play with
plot(
  g,
  layout = layout_with_graphopt,
  vertex.color = case_when(
    V(g)$faction == "Turks" ~ "tomato",
    V(g)$faction == "Loyal" ~ "steelblue",
    V(g)$faction == "Outcasts" ~ "forestgreen"
  ),
  edge.width = E(g)$weight,
  vertex.size = 2
)

lay <- layout_with_graphopt(g)
plot(g, layout = lay)

##################################
## OBTAINING NETWORK STATISTICS ##
##################################

# let's try and turn our directed graph into an undirected graph
# we will sum the weights of the edges between two nodes
g <- as.undirected(g, mode = "collapse", edge.attr.comb = "sum")


# usually, we'll want to do some analysis on the graph,
# typically, to run regression later

# let's try and obtain a bunch of things we might want to use
nodes <- nodes %>%
  inner_join(enframe(degree(g), name = "name", value = "degree")) %>%
  inner_join(enframe(
    betweenness(g, normalized = TRUE),
    name = "name", value = "betweenness"
  )) %>%
  inner_join(enframe(
    closeness(g, normalized = TRUE),
    name = "name", value = "closeness"
  )) %>%
  inner_join(enframe(
    transitivity(g, type = "local"),
    name = "name", value = "clustering"
  )) %>%
  inner_join(enframe(
    eigen_centrality(g)$vector,
    name = "name", value = "eigenvector"
  ))

# let's plot the degree distribution
nodes %>%
  group_by(degree) %>%
  count() %>%
  ggplot(aes(degree, n)) +
  geom_col() +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 1)
  )


#########################
## COMMUNITY DETECTION ##
#########################

# all community detection algorithms start with cluster_
cluster_optimal(g)
cluster_louvain(g)
cluster_
# you can then use a variety of functions to deal with the results
?communities # see the doc
# extract the resulting community structure using membership()
cluster_optimal(g) %>% membership()

# exercise: plot the graph with the community structure and compare
# several community structures
# hints:
# - save your layout for easier comparison
# - use par(mfrow = c(nrow, ncol)) before plotting
# to plot graphs on a grid of nrows x ncols

################
## BLOCKMODEL ##
################

# we'll use the sbm package for this
# let's look at the docs of the relevant function
?estimateSimpleSBM
# sbm wants an adjacency matrix
# we can get it with as_adjacency_matrix
as_adj(g)
gmat <- as.matrix(as_adj(g))
# this returns a sparse matrix, we need to convert it to a regular matrix
bm <- estimateSimpleSBM(gmat)

plot(bm)
plot(bm, "expected")
bm$probMemberships
bm$blockProp
bm$connectParam

##################
## LATENT SPACE ##
##################

# the command we'll use is ergmm
# ergmm wants a "network" object
# we can get it with network()

?as.network
gnet <- as.network(gmat, directed = FALSE)
latent <- ergmm(gnet ~ euclidean(d = 2))
summary(latent)
plot(latent)
latent$mcmc.mle$Z # get the estimated positions


#how to estimate the sothocahstic nloc model
 
#visualise differents community detections 
opt_com <- cluster_optimal(g)
louv_com <- cluster_louvain(g)

plot(g, layout = lay, vertex.color = membership(opt_com))
plot(g, layout = lay, vertex.color = membership(louv_com))

#visualise the differents centralicity score
theme_set(theme_minimal())
nodes %>% 
    arrange(degree) %>% 
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = name, y = degree)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Degree Centrality", x = "Node", y = "Degree")
#plot the eigenvector 
ggplot(nodes, aes(x = name, y = eigenvector)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Eigenvector Centrality", x = "Node", y = "Eigenvector") +
  theme_minimal()

