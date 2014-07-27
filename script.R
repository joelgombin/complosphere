


library(rgexf)
library(igraph)
library(stringi)
reseau <- read.gexf("/media/Data/Dropbox/Thèse/counterpoints/complosphère/Hyphe/juillet 2014/reseau.gexf")
reseau2 <- gexf.to.igraph(reseau)
E(reseau2)$weight <- reseau$edges$weight
reseau <- reseau2
# source("/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/clique.R")
# cliques <- largest.cliques(reseau)

# communautés 
# centralité 
# connectivité
# URL
# nom 
# couleur
# thèmes



# merger alterinfo.et et alterInfo.net 
# supprimer aporrea.org
# fusionner bruleparlesillumines.e-monsite.com et bruleparlesillumines.skyrock.com
# supprimer buzz.yahoo.com
# supprimer countercurrents.org
# merger fr.ria.ru, fr.rian.ru et ria.ru
# supprimer freetellafriend.com
# merger globalresearch.ca et mondialisation.ca
# supprimer independenza-webtv.com
# merger info-palestine.eu et info-palestine.net
# supprimer news.stcom.net
# merger oulala.info et oulala.net
# supprimer presstv.ir
# fusionner r-sistons.over-blog.com et chantaldupille.over-blog.com
# supprimer stcom.net
# merger u-p-r.fr et u-p-r.org

# supprimer acrimed.org
# merger agoravox.fr et agoravox.tv
# enlever bastamag.net
# enlever christianophobie.fr
# merger contre-info.com et contreinfo.info
# deux fois diktacratie.com ??
# enlever enquete-debat.fr ? 
# enlever fdesouche.com
# enlever metapedia.org
# enlever novopress.info
# merger fr.ria.ru et fr.rian.ru
# enlever france-irak-actualite.com
# enlever france-palestine.org
# enlever horizons-et-debats.ch
# merger info-palestine.net et info-palestine.eu (et enlever ?)
# enlever initiative-communiste.fr ?
# enlever ism-france.org
# jre2014 figure deux fois
# enlever la-dissidence.org
# enlever lecontrarien.com
# enlever lecourrierderussie.com
# enlever lejournaldepersonne
# enlever les-crises.fr
# fusionner leschroniquesderorschach.blogspot.com et leschroniquesderorschach.blogspot.fr
# enlever liesidotorg.wordpress.com
# enlever medias-presse.info
# michelcollon.info figure deux fois
# mondialisation.ca figure deux fois
# enlever nationspresse.info
# enlever ndf.fr
# newsoftomorrow.org figure deux fois
# enlever ojim.fr
# fusionner oulala.info et oulala.net
# enlever oumma.com
# enlever palestine-info.cc
# enlever panamza.com
# enlever piero.com
# enlever polemia.com
# enlever http://raimanet.wordpress.com/
# fusionner u-p-r.fr et upr.fr
# enlever ujfp.org
# 


reseau <- contract.vertices(reseau,
#                  mapping = c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130),
#                  mapping = match(c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130), unique(c(1:29,30,31:56,56,58:88,4,90:91,104,93:98,30,100:119,70,121:130))),
#                  mapping = match(c(1:12,14:19,21:22,22,24:30,4,33:37,39:51,53, 55,53,57:59,62:69,62,71:81,83:85,88:93,95:96, 98:99,101:105, 107:109,111:113,85,85,117,109,101,125,128:130,133:134,57,136,138:140,91,142, 108, 144, 65, 150:154), unique(c(1:12,14:19,21:22,22,24:30,4,33:37,39:51,53, 55,53,57:59,62:69,62,71:81,83:85,88:93,95:96, 98:99,101:105, 107:109,111:113,85,85,117,109,101,125,128:130,133:134,57,136,138:140,91,142, 108, 144, 65, 150:154))),
                  mapping = match(c(1:22,22,24:31,4,33:55,53,57:61, 70, 63:89,15,91:107,143,120,110:113,85,85,116:121,101,123,123,125:134,57,136:140,91,142:147,65,149:154), unique(c(1:22,22,24:31,4,33:55,53,57:61, 70, 63:89,15,91:107,143,120,110:113,85,85,116:121,101,123,123,125:134,57,136:140,91,142:147,65,149:154))),
                  vertex.attr.comb = list(name = "first"))
V(reseau)$url <- gsub("Www ", "", V(reseau)$name)
V(reseau)$url <- gsub(" ", ".", V(reseau)$url)
V(reseau)$url <- stri_trans_tolower(V(reseau)$url)
reseau <- delete.vertices(reseau, v=grep("(acrimed)|(bastamag)|(christianophobie)|(metapedia)|(france\\-palestine\\.org)|(info\\-palestine)|(initiative\\-communiste)|(lecontrarien)|(lecourrierderussie)|(lejournaldepersonne)|(liesidotorg\\.wordpress.\\com)|(medias\\-presse)|(nationspresse\\.info)|(ndf\\.fr)|(ojim\\.fr)|(palestine\\-info\\.cc)|(piero\\.com)|(polemia\\.com)|(raimanet\\.wordpress)|(ujfp\\.org)",V(reseau)$url))

write.csv(data.frame(url = V(reseau)$url), file="/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/urlsNettoyesJuillet2014.csv", row.names=FALSE)
# on enrichit les données dans le CSV, et on le réimporte...
data <- read.csv("/media/Data/Dropbox/Thèse/counterpoints/complosphère/web/reseauJuillet2014.csv", sep=",", stringsAsFactor=FALSE)

V(reseau)$themes <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Themes"])
V(reseau)$themes[is.na(V(reseau)$themes)] <- ""
V(reseau)$description <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Description"])
V(reseau)$description[is.na(V(reseau)$description)] <- ""
V(reseau)$ressources <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Ressources"])
V(reseau)$ressources[is.na(V(reseau)$ressources)] <- ""
V(reseau)$alexa <- whisker::whisker.escape(data[match(V(reseau)$url, data$url), "Alexa"])
V(reseau)$alexa <- 1 /log(as.integer(V(reseau)$alexa))
V(reseau)$alexa[is.na(V(reseau)$alexa)] <- 0.01
# reseau <- remove.vertex.attribute(reseau, "z")
reseau2 <- reseau
reseau2 <- simplify(reseau2, remove.multiple = FALSE, remove.loops = TRUE)
# V(reseau2)$x <- layout.fruchterman.reingold(reseau2, weights=E(reseau2)$weight)[,1]
# V(reseau2)$y <- layout.fruchterman.reingold(reseau2, weights=E(reseau2)$weight)[,2]
# # plot(reseau2)

set.seed(1)
coords <- layout.kamada.kawai(reseau2, weights=E(reseau2)$weight)
V(reseau2)$x <- coords[,1]
V(reseau2)$y <- coords[,2]

# on inverse les x 
# V(reseau2)$x <- - (V(reseau2)$x - mean(V(reseau2)$x))


# plot(reseau2)

V(reseau2)$betweenness<- betweenness(reseau2)
V(reseau2)$outcloseness <- closeness(reseau2, mode = "out")
V(reseau2)$incloseness <- closeness(reseau2, mode = "in")
V(reseau2)$indegree <- degree(reseau2, mode = "in", normalized = TRUE)
V(reseau2)$outdegree <- degree(reseau2, mode = "out", normalized = TRUE)
V(reseau2)$name <- V(reseau2)$url

# détection de communautés
reseau2EBC <- edge.betweenness.community(reseau2)
reseau2SC <- spinglass.community(reseau2, weights = E(reseau2)$weight, spin = 5)
V(reseau2)$EBComm <- reseau2EBC$membership
V(reseau2)$SBComm <- reseau2SC$membership




# plot(reseau2, vertex.size = V(reseau2)$indegree * 10, vertex.label=ifelse(V(reseau2)$indegree > 0.1, V(reseau2)$url,""), vertex.label.cex=V(reseau2)$indegree * 10, edge.width=log(E(reseau2)$weight)/4, edge.arrow.size=log(E(reseau2)$weight)/4, vertex.color = V(reseau2)$SBComm)

# plot(reseau2, vertex.size = V(reseau2)$indegree * 10, vertex.label=ifelse(V(reseau2)$indegree > 0.1, V(reseau2)$url,""), vertex.label.cex=V(reseau2)$indegree * 10, edge.width=log(E(reseau2)$weight)/4, edge.arrow.size=log(E(reseau2)$weight)/4)

V(reseau2)$pagerank <- page.rank(reseau2)$vector

library(RColorBrewer)


cols <- colorRampPalette(rev(brewer.pal(10, "RdGy")))
pal <- cols(length(unique(V(reseau2)$x)))
couleurs <- pal[match(V(reseau2)$x, sort(unique(V(reseau2)$x)))]
couleurs <- as.data.frame(t(col2rgb(couleurs, alpha = TRUE)))
couleurs$alpha <- 0.9
names(couleurs) <- c("r", "g", "b", "alpha")

# color.df <- data.frame(r = sapply(V(reseau2)$hclust, switch, "1" = 215, "2" = 255, "3" = 44), g = sapply(V(reseau2)$hclust, switch,  "1" = 25, "2" = 255, "3" = 123), b = sapply(V(reseau2)$hclust,switch,  "1" = 28, "2" = 191, "3" = 182), alpha = rep(0.9, length(V(reseau2))))

write.gexf(nodes=data.frame(id = V(reseau2)$url, label=V(reseau2)$url),
           edges = data.frame(source = get.edgelist(reseau2)[,1], target = get.edgelist(reseau2)[,2]),
           edgesWeight = E(reseau2)$weight,
           edgesVizAtt = list(size=log(E(reseau2)$weight)/4,
                              color=data.frame(r=rep(55, length(E(reseau2))), g=rep(55, length(E(reseau2))), b=rep(55, length(E(reseau2))), alpha=rep(0.8, length(E(reseau2))))), 
           nodesAtt = as.data.frame(vertex.attributes(reseau2), row.names = as.integer(V(reseau2))),
           nodesVizAtt = list(position=data.frame(
                                          x=V(reseau2)$x, y=V(reseau2)$y, z=as.integer(NA)),
                              size=as.numeric(V(reseau2)$alexa),
                              color=couleurs),
           output="reseauJuillet2014.gexf",
           defaultedgetype="mutual")
           


