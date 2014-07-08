library(rgexf)
library(igraph)
library(stringi)
reseau <- read.graph("/media/Data/Dropbox/Thèse/counterpoints/complosphère/Hyphe/reseau.net", format = "pajek")
source("/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/clique.R")
cliques <- largest.cliques(reseau)

# communautés 
# centralité 
# connectivité
# URL
# nom 
# couleur
# thèmes



## merger : chantaldupille et r-sistons
## leschroniquesderorschach.blogspot.fr et leschroniquesderorschach.blogspot.com
## enlever : stcom.net
## enlever : vigilantcitizen.com
## enlever : countercurrents.org
## merger : infopalestine.eu et infopalestine.net
## enlever : infowars.com (Alex Jones)
## merger : oulala.net et oulala.info
## enlever presstv.ir (en anglais)
## enlever ria.ru
## merger u-p-r.fr et upr.fr
reseau <- contract.vertices(reseau,
#                  mapping = c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130),
                  mapping = match(c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130), unique(c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130))),
                  vertex.attr.comb = list(id = "first"))
V(reseau)$url <- gsub(" ", ".", V(reseau)$id)
V(reseau)$url <- stri_trans_tolower(V(reseau)$url)
reseau <- delete.vertices(reseau, v=grep("(stcom\\.net)|(vigilantcitizen)|(countercurrents)|(infowars)|(presstv\\.ir)|(www\\.ria\\.ru)",V(reseau)$url))
V(reseau)$betweenness<- betweenness(reseau)
V(reseau)$outcloseness <- closeness(reseau, mode = "out")
V(reseau)$incloseness <- closeness(reseau, mode = "in")
V(reseau)$indegree <- degree(reseau, mode = "in", normalized = TRUE)
V(reseau)$outdegree <- degree(reseau, mode = "out", normalized = TRUE)


write.csv(data.frame(url = V(reseau)$url), file="/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/reseau.csv", row.names=FALSE)
# on enrichit les données dans le CSV, et on le réimporte...
data <- read.csv("/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/reseau_enrichi.csv", sep=";", stringsAsFactor=FALSE)

V(reseau)$themes <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Themes"])
V(reseau)$description <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Description"])
V(reseau)$ressources <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Ressources"])
V(reseau)$alexa <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Alexa"])
V(reseau)$alexa <- 1 /log(as.integer(V(reseau)$alexa))
V(reseau)$alexa[is.na(V(reseau)$alexa)] <- 0.1
reseau <- remove.vertex.attribute(reseau, "z")
reseau2 <- reseau
V(reseau2)$x <- layout.fruchterman.reingold(reseau, weights=E(reseau)$weight)[,1]
V(reseau2)$y <- layout.fruchterman.reingold(reseau, weights=E(reseau)$weight)[,2]
reseau2 <- simplify(reseau2, remove.multiple = FALSE, remove.loops = TRUE)
plot(reseau2, vertex.size = V(reseau2)$indegree * 10, vertex.label=ifelse(V(reseau2)$indegree > 0.1, V(reseau2)$url,""), vertex.label.cex=V(reseau2)$indegree * 10, edge.width=log(E(reseau2)$weight)/4, edge.arrow.size=log(E(reseau2)$weight)/4)

write.gexf(nodes=data.frame(id =as.integer(V(reseau2)), label=V(reseau2)$url),
           edges = data.frame(source = get.edgelist(reseau2)[,1], target = get.edgelist(reseau2)[,2]),
           edgesWeight = E(reseau2)$weight,
           edgesVizAtt = list(size=log(E(reseau2)$weight)/4,
                              color=data.frame(r=rep(190, length(E(reseau2))), g=rep(190, length(E(reseau2))), b=rep(190, length(E(reseau2))), alpha=rep(0.9, length(E(reseau2))))), 
           nodesAtt = as.data.frame(vertex.attributes(reseau2), row.names = as.integer(V(reseau2))),
           nodesVizAtt = list(position=data.frame(
                                          x=V(reseau2)$x, y=V(reseau2)$y, z=NA),
                              size=V(reseau2)$indegree,
                              color=data.frame(r=rep(66, length(V(reseau2))), g=rep(66, length(V(reseau2))), b=rep(66, length(V(reseau2))), alpha=rep(0.9, length(V(reseau2))))),
           output="reseau2.gexf",
           defaultedgetype="directed")
           

reseau3 <- reseau
V(reseau3)$x <- layout.kamada.kawai(reseau)[,1]
V(reseau3)$y <- layout.kamada.kawai(reseau)[,2]
reseau3 <- simplify(reseau3, remove.multiple = FALSE, remove.loops = TRUE)
plot(reseau3, vertex.size = V(reseau3)$indegree * 10, vertex.label=ifelse(V(reseau3)$indegree > 0.1, V(reseau3)$url,""), vertex.label.cex=V(reseau3)$indegree * 10, edge.width=log(E(reseau3)$weight)/4, edge.arrow.size=log(E(reseau3)$weight)/4)

reseau3.gexf <- igraph.to.gexf(reseau3, position=data.frame(x=V(reseau3)$x, y=V(reseau3)$y, z=NA ))
