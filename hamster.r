# CSCI 424 Hamster Group 1
library(igraph)


# Global Data
ham_attr <- read.csv("petster-friendships-hamster-uniq.txt", sep = "|")
ham_edge <- read.csv("out.petster-friendships-hamster-uniq", sep = " ")

# Plot Function
question1 <- function() {
  g <- graph_from_data_frame(ham_edge)
  l <- layout.reingold.tilford(g, circular=T)
  plot(g, layout= l, 
       vertex.size=4,
       vertex.label=NA, 
       edge.arrow.size=0,
       edge.width=0.1,
       edge.color=adjustcolor("Black", alpha.f=.5),
       vertex.color = adjustcolor("SkyBlue2", alpha.f = .5))
  return(g)
}

# Adding Node Attributes
question2 <- function() {
  g <- graph_from_data_frame(ham_edge, directed = FALSE)

  V(g)$ID <- match(V(g)$name, ham_attr$ent)
  V(g)$hname <- as.vector(ham_attr$name[V(g)$ID])
  V(g)$join <- as.vector(ham_attr$joined[V(g)$ID])
  V(g)$species <- as.vector(ham_attr$species[V(g)$ID])
  V(g)$hcolor <- as.vector(ham_attr$coloring[V(g)$ID])
  V(g)$gender <- as.vector(ham_attr$gender[V(g)$ID])
  V(g)$birth <- as.vector(ham_attr$birthday[V(g)$ID])
  V(g)$age <- as.vector(ham_attr$age[V(g)$ID])
  V(g)$home <- as.vector(ham_attr$hometown[V(g)$ID])
  V(g)$f_toy <- as.vector(ham_attr$favorite_toy[V(g)$ID])
  V(g)$f_act <- as.vector(ham_attr$favorite_activity[V(g)$ID])
  V(g)$f_food <- as.vector(ham_attr$favorite_food[V(g)$ID])
  V(g)$home_country <- as.vector(substring(substring(ham_attr$hometown[V(g)$ID], regexpr("\\,[^\\,]*$", ham_attr$hometown[V(g)$ID])), regexpr("\\s[^\\s]*$", ham_attr$hometown[V(g)$ID])))
  
  return (g)
}

# TO DO: Centrality Metrics (Degree, Betweenness, etc.)
question3 <- function(){
  #degree
  hams <- question2()
  deg <- betweenness(question2())
  indices <- sort(deg, decreasing = TRUE, index.return = TRUE)$ix[1:5]
  return(V(hams)[indices])
}

# TO DO: ERGM Model

question4 <- function(){
  library(ergm)
  
  netE <- network(ham_edge, directed = FALSE)

  set.vertex.attribute(netE, "ID", ham_attr$ent)
  set.vertex.attribute(netE, "hname", as.character(ham_attr$name))
  set.vertex.attribute(netE, "join", as.character(ham_attr$joined))
  set.vertex.attribute(netE, "species", as.character(ham_attr$species))
  set.vertex.attribute(netE, "gender", as.character(ham_attr$gender))
  set.vertex.attribute(netE, "birth", as.character(ham_attr$birthday))
  set.vertex.attribute(netE, "age", as.character(ham_attr$age))
  set.vertex.attribute(netE, "home", as.character(ham_attr$hometown))
  set.vertex.attribute(netE, "ftoy", as.character(ham_attr$favorite_toy))
  set.vertex.attribute(netE, "facti", as.character(ham_attr$favorite_activity))
  set.vertex.attribute(netE, "ffood", as.character(ham_attr$favorite_food))
  
  
  summary(netE)
  plot(netE, displayisolates = FALSE, vertex.col='home')
  
  # Begin ERGM Models
  
  # What is the probability that a relationship between hamsters occurs based on the same hometown? ANS: 3%
  ham_model1 <- ergm(netE ~ edges + nodematch("home"))
  summary(ham_model1)
  e <- ham_model1$coef[["edges"]]
  g <- ham_model1$coef[["nodematch.home"]]
  eg <- e + g
  prob <- exp(eg) / (1 + exp(eg))
  
  # What is the probability that a relationship forms between same hometown & join date? ANS: 11%
  ham_model2 <- ergm(netE ~ edges + nodematch("home") + nodematch("join"))
  e <- ham_model2$coef[["edges"]]
  g <- ham_model2$coef[["nodematch.home"]]
  j <- ham_model2$coef[["nodematch.join"]]
  egj <- e + g + j
  prob <- exp(egj) / (1 + exp(egj))
  
  # What is the probability that a relationship forms from the United States? ANS: TO BE CONTINUED
  
  summary(ham_model1)
  summary(ham_model2)
}

# TO DO: Degree Distribution
question5 <- function(){
  library(ggplot2)
  q2g <- question2()
  dd <- degree_distribution(q2g)
  
  Degree <- dd
  dd_df <- as.data.frame(dd)
  plot(dd, xlab= "Degree", ylab="Frequency")
  Index <- seq(1, length(dd), by=1)
  
  ggplot(dd_df, aes(Degree,Index)) + geom_point()
}

# Color by location: Only works with graphs with less than 12 locations
library(RColorBrewer)
coloring <- function(){
  g <- ego()
  
  locations <- vertex_attr(g, "home", index = V(g))
  u_locations <- unique(locations)
  
  pal <- brewer.pal(length(u_locations), "Paired")
  plot(g, vertex.color = pal[as.numeric(as.factor(vertex_attr(g, "home")))])
}

#Coreness layout by Jordi Casas-Roma
CorenessLayout <- function(g) {
  coreness <- graph.coreness(g)
  xy <- array(NA, dim=c(length(coreness), 2))
  
  shells <- sort(unique(coreness))
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells))
    nodes_in_shell <- sum(coreness==shell)
    angles <- seq(0,360,(360/nodes_in_shell))
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v
    xy[coreness==shell, 2] <- cos(angles) * v
  }
  return(xy);
}

#plot kcore of graph
k_core <- function() {
  g <- question2()
  # compute coreness
  coreness <- graph.coreness(g)
  # assign colors
  colbar <- rainbow(max(coreness))
  # create layout
  ll <- CorenessLayout(g)
  # plot
  plot(g, layout=ll, vertex.size=15, vertex.color=colbar[coreness], vertex.frame.color=colbar[coreness], main='Coreness', vertex.label=NA)
}

#ego subgraph
ego <- function(){
  g <- question2()
  ego_graph <- make_ego_graph(g, 1, nodes = V(g), mode = "out")
  g3 <- ego_graph[[100]]
  plot(g3)
  return(g3)
}

#TO DO: subgraphs
question7 <- function(){
  g <- question2()
  cities <- vertex_attr(g, "home")
  sing_cities <- grep("Singapore", cities, ignore.case = TRUE)
  sub_g_sing <- induced_subgraph(g, sing_cities)
  
  la_info <- grep("los angeles", cities, ignore.case = TRUE)
  sub_g_la <- induced_subgraph(g, la_info)
  
  US_info <- grep("United States", cities, ignore.case = FALSE)
  sub_g_US <- induced_subgraph(g, US_info)
  
  texas_info <- grep("tx", cities, ignore.case = TRUE)
  sub_g_texas <- induced_subgraph(g, texas_info)
  
  phil_info <- grep("philippines", cities, ignore.case = TRUE)
  sub_g_phil <- induced_subgraph(g, phil_info)
  
  uk_info <- grep("united kingdom", cities, ignore.case = TRUE)
  sub_g_uk <- induced_subgraph(g, uk_info)
  
  canada_info <- grep("canada", cities, ignore.case = TRUE)
  sub_g_canada <- induced_subgraph(g, canada_info)
  
  japan_info <- grep("japan", cities, ignore.case = TRUE)
  sub_g_japan <- induced_subgraph(g, japan_info)
  
  mala_info <- grep("malaysia", cities, ignore.case = TRUE)
  sub_g_mala <- induced_subgraph(g, mala_info)
  
  brazil_info <- grep("brazil", cities, ignore.case = TRUE)
  sub_g_brazil <- induced_subgraph(g, brazil_info)
  
  colouring <- as.data.frame(table(vertex_attr(g, "hcolor")))
  colouring[order(colouring$Freq, decreasing = TRUE),]
  top_colors <- head(colouring$Var1, 5)
  
  top_five <- list()
  
  for(i in 1:5){
  top_five[[top_colors[i]]] = induced_subgraph(g, match(top_colors[i], vertex_attr(g, "hcolor")))
  }
  
  return(list("Singapore" = sub_g_sing, "Philippines" = sub_g_phil, 
              "UK" = sub_g_uk, "Canada" = sub_g_canada, "Malaysia" = sub_g_mala, "Japan" = sub_g_japan, "Brazil" = sub_g_brazil,
              "LA" = sub_g_la,  "Texas" = sub_g_texas, "USA" = sub_g_US, "Colors" = top_five))
}

question8 <- function(){
  s <- question7()
  us <- s[[4]]
  z <- delete.vertices(us, which(degree(us)==0))
  plot(z, layout= layout.fruchterman.reingold(z, niter = 10000), 
       vertex.size=10,
       vertex.label=V(z)$hname,
       vertex.label.family="Times",
       vertex.label.dist=2,
       vertex.label.cex=0.6,
       edge.arrow.size=0,
       edge.width=1,
       edge.color=adjustcolor("Black", alpha.f=.5),
       vertex.color = adjustcolor("SkyBlue2", alpha.f = .5),
       main="Hamster Components Texas")
  mean(betweenness(z))
  mean(degree(z))

}

question9 <- function() {
  whole_subgraphs <- question7()
  loc_sub <- whole_subgraphs[1:7]
  for (i in 1:8) {
    loc_sub[[i]] <- delete.vertices(loc_sub[[i]], which(degree(loc_sub[[i]])==0))
  }
  number_of_entries <- 0
  big_sub_graph <- c()
  for (i in 1:8) {
    big_sub_graph <- union(big_sub_graph, vertex_attr(loc_sub[[i]], "ID"))
    number_of_entries = number_of_entries + length(vertex_attr(loc_sub[[i]], "ID"))
  }
  
  merge_sub_g <- induced_subgraph(question2(),big_sub_graph)
  merge_sub_g <- delete.vertices(merge_sub_g, which(degree(merge_sub_g)==0))
    
  plot(merge_sub_g, layout= layout.fruchterman.reingold(merge_sub_g, niter = 10000), 
       vertex.size=10,
       vertex.label="",
       vertex.label.family="Times",
       vertex.label.dist=2,
       vertex.label.cex=0.6,
       edge.arrow.size=0,
       edge.width=1,
       edge.color=adjustcolor("Black", alpha.f=.5),
       vertex.color = V(merge_sub_g$home),
       main="Hamster Components Hubs")
  return(big_sub_graph)
}

largestComp <- function() {
  g <- question2()
  dg <- decompose(g, mode = "strong", min.vertices = 0) # returns a list of three graphs
  plot(dg[[1]], layout = coords)
  return(dg[[1]])
}

largestERGM <- function(){
  library(ergm)
  g <- largestComp
  dfg <- as_data_frame(g, what = "edges")
  netE <- network(dfg, directed = FALSE)
  set.vertex.attribute(netE, "ID", ham_attr$ent)
  set.vertex.attribute(netE, "hname", as.character(ham_attr$name))
  set.vertex.attribute(netE, "join", as.character(ham_attr$joined))
  set.vertex.attribute(netE, "species", as.character(ham_attr$species))
  set.vertex.attribute(netE, "gender", as.character(ham_attr$gender))
  set.vertex.attribute(netE, "birth", as.character(ham_attr$birthday))
  set.vertex.attribute(netE, "age", as.character(ham_attr$age))
  set.vertex.attribute(netE, "home", as.character(ham_attr$hometown))
  set.vertex.attribute(netE, "ftoy", as.character(ham_attr$favorite_toy))
  set.vertex.attribute(netE, "facti", as.character(ham_attr$favorite_activity))
  set.vertex.attribute(netE, "ffood", as.character(ham_attr$favorite_food))
  set.vertex.attribute(netE, "home_country", as.character(ham_attr$home_country))
  
  
  summary(netE)
  plot(netE, displayisolates = FALSE, vertex.col='home')
  
  # Begin ERGM Models
  
  # What is the probability that a relationship between hamsters occurs based on the same homecountry? 
  ham_model1 <- ergm(netE ~ edges + nodematch("home_country"))
  summary(ham_model1)
  e <- ham_model1$coef[["edges"]]
  g <- ham_model1$coef[["nodematch.home_country"]]
  eg <- e + g
  prob <- exp(eg) / (1 + exp(eg))
  
  # What is the probability that a relationship forms between same hometown & join date? ANS: 11%
  ham_model2 <- ergm(netE ~ edges + nodematch("home") + nodematch("join"))
  e <- ham_model2$coef[["edges"]]
  g <- ham_model2$coef[["nodematch.home"]]
  j <- ham_model2$coef[["nodematch.join"]]
  egj <- e + g + j
  prob <- exp(egj) / (1 + exp(egj))
  
  # What is the probability that a relationship forms from the United States? ANS: TO BE CONTINUED
  
  summary(ham_model1)
  summary(ham_model2)
}