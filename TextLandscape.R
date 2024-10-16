# Load required libraries ####
library(magrittr) # provides %>% pipe operator
library(data.table) # provides an efficient data.frame model and an interesting shorthand syntax
library(stringdist) # for detecting word inflections
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(ggplot2)
library(udpipe) # for lemmatization
library(igraph)
library(tidygraph) # allows for manipulation of igraph objects with a coherent syntax
library(ggraph)
library(particles) # for particle-based network layouts
library(RBioFabric) # must be installed by remotes::install_github("wjrl/RBioFabric")
library(vegan) # for efficient non-metric multidimensional scaling
library(text2vec)
library(fastcluster) # makes the hclust function much faster
library(kohonen) # Self-organizing maps
library(ggforce) # Allows to draw Voronoi tiles in any ggplot
library(akima) # Interpolates points to a raster, allowing to create a pseudo-DEM (digital elevation model)
library(rgl) # 3D graphics
library(rayshader) # Makes rendered 3D graphics
library(future)
library(future.apply)
ncores <- parallelly::availableCores() # determine the number of cores on your computer; we'll use this value in functions supporting multicore processing.
basepath <- rstudioapi::getActiveDocumentContext()$path %>% dirname


# Load language model for annotations ####
udmodel_english <- udpipe_load_model(file = 
                                       "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/english-ud-2.0-170801.udpipe")


# Read or prepare text ####
#text <- readtext(c(
#  "http://www.gutenberg.org/files/60850/60850-0.txt", 
#  "https://www.gutenberg.org/cache/epub/61697/pg61697.txt",
#  "https://www.gutenberg.org/cache/epub/40456/pg40456.txt"
#)) 


tidy_paragraphs <- readRDS('/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_pargraphs_ratings.rds')
df <- tidy_paragraphs

# Annotate text ####
#udmodelfile <- file.path(basepath,"french-gsd-ud-2.5-191206.udpipe")
#if (udmodelfile %>% file.exists){
#  udmodel<- udpipe_load_model(udmodelfile)
#} else {
#  udmodel <- udpipe_download_model(language = "french")
#}

# text.lemmas <- udpipe(text,object = udmodel_english, parallel.cores = ncores) %>% 
#  as.data.table
x <- udpipe_annotate(udmodel_english, 
                     x = df$cleaned_text, 
                     doc_id = df$doc_id, 
                     parser = "default", 
                     trace = FALSE)
x <- as.data.table(x)


x[,c("doc_id","sentence_id","token","lemma","upos")] %>% 
  head(30)

# Clean for this example - Delete later

#text.lemmas <- text.lemmas[
#  (doc_id =="60850-0.txt" & sentence_id > 16 & sentence_id < 1800) |
#    (doc_id =="pg61697.txt" & sentence_id > 21 & sentence_id < 1472) |
#    (doc_id =="pg40456.txt" & sentence_id > 18 & sentence_id < 219)
#] 
#text.lemmas[,c("sentence_id","token","lemma","upos")] %>% head(30) 
mystopwords <- c()

# Count Frequencies ####
tokens.selected <- x[upos %in% c("NOUN","PROPN","ADJ","VERB"), c("doc_id","lemma")][,.(text = paste(lemma, collapse=" ")), by = doc_id] %>% 
  corpus %>%
  tokens(remove_punct=TRUE) 

dfm.selected <- tokens.selected %>% dfm(tolower = FALSE) # lemmatization has already lower-cased what needs to be, thus tolower = FALSE
wordfreqs <- dfm.selected %>% 
  textstat_frequency %>% 
  as.data.table %>% 
  setorder(-frequency)

## Find and Group Word Inflections (only needed for small corpuses) ####
flexionsf <- function(words,d){ # word is a char vector, d is the maximum distance at which words are considered as synonyms.
  tokdist <- stringdist::stringdistmatrix(words,method="jw",nthread=ncores) %>% as.matrix %>% Matrix::forceSymmetric(., uplo="U")
  rownames(tokdist) <- words
  colnames(tokdist) <- words
  tokdist <- tokdist[grepl("^[[:lower:]]", rownames(tokdist)),] # Do not add word as key to the dictionary if it is capitalized
  cnslc <- tolower(colnames(tokdist))
  nchars <- nchar(colnames(tokdist))
  spldf <- split(tokdist %>% as.matrix %>% as.data.frame, seq(1, nrow(tokdist), by = floor(nrow(tokdist)/(ncores-1)))) # generates a warning, but is OK. Try c(1,2,3,4,5) %>% split(seq(1, 5, by = 2)) to understand why
  plan(multisession) # This needs parallel processing
  temp <- future.apply::future_lapply(spldf, function(x) {
    lapply(rownames(x), function(w) { # w is the current word
      selector <- unname(
        (x[w,] < d | w == cnslc | paste0(w,"s") == cnslc | paste0(w,"x") == cnslc) & 
          nchar(w) <= nchars # only add inflections that are longer than the current dictionary key
      )#[1,]
      if ( length(selector[selector]) > 1 ) { # only add the word if it has more than one inflection; selector[selector] gives the vector of TRUE elements
        return(colnames(x)[selector])
      } else {
        return(NULL)
      }
    })
  }) %>% unlist(recursive=FALSE)
  future:::ClusterRegistry("stop")
  temp <- temp[!sapply(temp, is.null)]
  names(temp) <- sapply(temp,function(i) i[1])
  temp <- temp[grepl("^[[:lower:]]", names(temp))] # why do I have to repeat this ?
  lapply(temp, function(i) i[-1]) # remove first entry to not repeat key in entries
}

options(future.globals.maxSize = 8000 * 1024^2) #Without this, there maybe an error message regarding future.apply()
flexions.list <- flexionsf(wordfreqs$feature,0.04) 
save(flexions.list,file=file.path(basepath,"flexions_list.RData"))

# Create Quanteda dictionary
load(file.path(basepath,"flexions_list.RData"))
flexions.dict <- flexions.list %>% dictionary(tolower=F)
tokens.selected <- tokens.selected %>% tokens_lookup(flexions.dict, exclusive=F, capkeys = F, case_insensitive = F)
dfm.selected <- tokens.selected %>% dfm(tolower = FALSE) # lemmatization has already lower-cased what needs to be, thus tolower = FALSE
wordfreqs <- dfm.selected %>% textstat_frequency %>% as.data.table %>% setorder(-frequency) # automatically sets the order in reverse

# Visualise Word Frequencies
ggplot(wordfreqs%>%head(50)) +
  geom_col(aes(y=frequency,x=reorder(feature,frequency),fill=docfreq)) +
  coord_flip()

# Create a map ####
# Calculate the FCM
fcm.selected <- fcm(tokens.selected, context="window", count="weighted", window=2) %>% fcm_remove(wordfreqs[frequency<5,feature],case_insensitive=F)
fcm.selected %>% head(10)
fcm.selected.symmetric <- Matrix::forceSymmetric(fcm.selected, uplo="U") # The matrix has to be made symmetric for igraph to be able to treat it as undirected
fcm.selected.weighted <- fcm.selected / rowSums(fcm.selected.symmetric)^0.9
fcm.selected.symmetric.weighted <- Matrix::forceSymmetric(fcm.selected.weighted, uplo="U")
fcm.selected.symmetric <- fcm.selected.weighted <- NULL ; gc() # free up some memory


## Plot with igraph as more control ####
g <- graph_from_adjacency_matrix(fcm.selected.symmetric.weighted, weighted=TRUE, diag = F, mode="undirected") %>% as_tbl_graph
# join frequency data from wordfreqs
setkey(wordfreqs,"feature")
V(g)$freq <- wordfreqs[V(g)$name,frequency]
# create groups
V(g)$group <- cluster_leiden(g) %>% membership %>% as.character

g.top <- g %>%  
  activate(nodes) %>% 
  arrange(-freq) %>% 
  slice(1:500) # we use only the top 500 nodes for the layout, we hide some afterwards
g.laidout <- g.top %>% create_layout(layout="igraph",algorithm="drl") # Distributed Recursive (Graph) Layout 2008 

mr <- max(g.laidout$x,g.laidout$y) / 60 # adapt the voronoi max radius size to the extent of the actual data
mw <- min(E(g)$weight) + (max(E(g)$weight) - min(E(g)$weight)) * 0.08
ggraph(g.laidout %>% slice(1:100)) +
  geom_edge_link(aes(width=ifelse(weight<mw,0,weight)),alpha=0.2,color="grey",show.legend = F) +
  scale_edge_width('Value', range = c(0.1, 1)) + 
  geom_node_voronoi(aes(fill = group), alpha=0.2, max.radius = mr, colour = "white",show.legend = F) + 
  geom_node_text(aes(label=name,size=log(freq),color=group),fontface = "bold",show.legend = F) +
  theme_void()

# None of igraph’s algorithms manage the collision force that prevents nodes from overlapping 
# and that leaves space for large nodes. Fortunately, the particles package allows you to do exactly that 
# by providing all forces from the wonderful D3 force-directed algorithm.

g.laidout2 <- g.top %>% 
  simulate() %>% 
  wield(link_force, strength=weight, distance=1/weight) %>% 
  wield(manybody_force) %>% 
  #wield(center_force) %>%
  wield(collision_force,radius=freq) %>%
  evolve() %>% 
  as_tbl_graph() %>% 
  create_layout(layout="manual",x=x, y=y) %>% # forward the words to the layout
  slice(5:150) # we show only a subset of words once layed out

mr <- max(g.laidout2$x,g.laidout2$y) / 20 # adapt the voronoi max radius size to the extent of the actual data
mw <- min(E(g)$weight) + (max(E(g)$weight) - min(E(g)$weight)) * 0.04
ggraph(g.laidout2) +
  geom_edge_link(aes(width=ifelse(weight<mw,0,weight)),alpha=0.2,color="grey",show.legend = F) +
  scale_edge_width('Value', range = c(0, 5)) + 
  geom_node_voronoi(aes(fill = group), alpha=0.2,colour = "white",show.legend = F, max.radius = mr) + # passes arguments to ggforce::geom_voronoi_tile , colour = "white"
  geom_node_text(aes(label=name,size=log(freq),color=group),fontface = "bold",show.legend = F) +
  theme_void()

## Create a 3 dimensional map ####
# Prepare raster heights 
xleft <- min(g.laidout2$x)-mr/2
xright <- max(g.laidout2$x)+mr/2
yupper <- max(g.laidout2$y)+mr/2
ylower <- min(g.laidout2$y)-mr/2
# interpolate the points to a a raster, with sampling frequency nx X ny ---
# Note that the original coordinates of the points are preserved 
pointheight.raster <- interp( 
  x = g.laidout2$x, 
  y = g.laidout2$y, 
  z = g.laidout2$freq,
  xo = seq(xleft,xright,by=(xright-xleft)/1500),
  yo = seq(ylower,yupper,by=(yupper-ylower)/3000),
  linear = FALSE,
  extrap = TRUE, # works for non-linear only; if false creates a convex hull that removes points at the edges
  duplicate = "strip"
)
pointheight.raster.dt <- interp2xyz(pointheight.raster) %>% as.data.table %>% setnames("z","freq")
if (nrow(pointheight.raster.dt[freq<0]) > 0) { # necessary if the interpolation has produced negative values	
  pointheight.raster.dt[,freq:=freq-min(freq,na.rm=TRUE)] # NB: -(-x) = +x
}
pointheight.raster.dt[is.na(freq),freq:=min(pointheight$freq)]

terrain_palette <- colorRampPalette(c("#f0f0f0","white"))(256)
gplot3d <- ggraph(g.laidout2) +
  geom_raster(data=pointheight.raster.dt, aes(x=x, y=y, fill=freq^0.5),interpolate=TRUE, show.legend = F) +
  scale_fill_gradientn(colours=terrain_palette) +
  geom_edge_link(aes(width=ifelse(weight<mw,0,weight)),alpha=0.2,color="grey",show.legend = F) +
  scale_edge_width('Value', range = c(0, 5)) + 
  geom_node_text(aes(label=name,size=log(freq),color=group),show.legend = F) +
  theme_void()

rayshader::plot_gg(
  gplot3d, width=15, height=10,
  shadow_intensity = 0.7,
  shadow = TRUE, 
  shadowcolor = "auto",
  shadow_darkness = 0.5,
  raytrace=TRUE, 
  sunangle = 315,
  anglebreaks = seq(30, 40, 0.1),
  scale = 500, # default is 150
  # preview = TRUE,
  windowsize = c(1600, 1000),
  fov = 70, zoom = 0.4, theta = -3, phi = 75
) 
rayshader::render_snapshot("network3d.png",width=3200, height=2000)
close3d()

# From the Feature-co-occurrence Matrix to a Positional Matrix with Multidimensional Scaling ####
# Calculating the Distance Matrix
g.dist <- 1/fcm.selected.symmetric.weighted
diag(g.dist) <- 0 # distance of a word to itself is always 0
g.dist[g.dist==Inf] <- max(fcm.selected) * 3

# Vegan MDS
options(mc.cores = ncores) 
mdsfit2 <- vegan::monoMDS(g.dist %>% as.dist,k=9) # non-metric

## Let's have a look at the first 2 dimensions ####
g.mds.points <- mdsfit2$points
g.mds.points.dt <- g.mds.points %>%
  as.data.table
g.mds.points.dt[,lemma:=rownames(g.mds.points)]
g.mds.points.dt[,freq:=wordfreqs[lemma,frequency]]
g.mds.points.dt[,clust:=kmeans(mdsfit2$points, 4)$cluster %>% as.character]
temp <- g.mds.points.dt %>% setorder(-freq) %>% head(200)
ggplot(temp) + geom_text(aes(x=MDS1,y=MDS2,label=lemma, size=freq,color=clust))

# Create a 3 dimensional map
par3d(windowRect = c(20, 30, 800, 800),zoom=0.7)
text3d(
  temp$MDS1,
  temp$MDS2,
  temp$MDS3,
  temp$lemma,
  color=temp$clust,
  cex=0.8*(temp$freq/min(temp$freq))^0.25,
  pos=4,
  offset=0.6
)
rgl.spheres(temp$MDS1,temp$MDS2,temp$MDS3,temp$lemma,color=temp$clust,
            r=0.01*(temp$freq/min(temp$freq))^0.25)
bg3d(color="white",
     fogtype="none")
movie3d(spin3d(axis = c(0.3, 0.3, 0.3)), movie="mdsscatter", 
        duration = 20, clean=T, fps=25, dir = basepath)
close3d()


# From MDS to SOM ####
g.somdata <- g.mds.points %>% as.data.frame
g.somgrid <- somgrid(xdim = 9, ydim=14, topo="hexagonal")
g.sommap <- som( 
  g.mds.points, 
  grid=g.somgrid, 
  rlen=600, 
  alpha=c(0.05,0.01)
)
g.sommap_dist <- getCodes(g.sommap) %>% dist
g.sommap_clusters <- fastcluster::hclust(g.sommap_dist)
g.somxydict <- data.table( # construct a dictionary
  pointid=1:nrow(g.sommap$grid$pts), 
  x=g.sommap$grid$pts[,1], 
  y=g.sommap$grid$pts[,2],
  groupe=cutree(g.sommap_clusters,9) %>% as.character
)
par(mfrow = c(1, 5))
for(x in 1:ncol(g.somdata)) {
  plot(
    g.sommap, 
    type = "property", 
    property = getCodes(g.sommap)[,x], 
    main=colnames(g.somdata)[x],
    shape="straight",
    palette.name = viridisLite::inferno
  ) 
  add.cluster.boundaries(g.sommap,g.somxydict$groupe,lwd=2)
}
par(mfrow = c(1, 1)) #reset par

g.somdata.result <- g.somdata %>% as.data.table
g.somdata.result[, lemma := rownames(g.somdata)]
g.somdata.result[, pointid := g.sommap$unit.classif]
setkey(wordfreqs,"feature")
g.somdata.result$freq <- wordfreqs[g.somdata.result$lemma,frequency]
setkey(g.somxydict,pointid)
setkey(g.somdata.result,pointid)
g.somdata.result <- g.somxydict[g.somdata.result]
setorder(g.somdata.result,-freq) # thanks to this order, .(lemmas=c(lemma)[1]) will give the highest element of group
g.somdata.result.aggreg <- lapply(1:3, function(i) g.somdata.result[,.(
  lemma=c(lemma)[i],
  x=c(x)[i],
  y=c(y)[i]-2/3+(i/3), # we want the middle term to be centered, thus -2/3 as starting point
  zlabel=(i),
  freq=c(freq)[i],
  groupe=c(groupe)[i]
),by="pointid"]) %>% rbindlist

ggplot() +
  geom_text(data=g.somdata.result.aggreg, aes(x=x,y=y,label=lemma,color=groupe),show.legend = F) +
  theme_void()

# The biofabric layout ####
cutoff <- E(g.top)$weight%>%min + (E(g.top)$weight%>%max - E(g.top)$weight%>%min) * 0.02
g.top.weightOver5 <- g.top %>% activate(edges) %>% filter(weight>cutoff)
g.top.weightOver5 <- delete.vertices(g.top.weightOver5,which(
  degree(g.top.weightOver5,mode="all") < cutoff/3	
)) %>% as_tbl_graph
bioFabric(g.top.weightOver5 %>% slice(1:20))

# Ussing ggplot for the bifabric layout
ggraph(g.top.weightOver5 %>% slice(1:20),layout="fabric", sort.by=node_rank_fabric()) + 
  geom_node_range(aes(colour=group),show.legend = F) +
  geom_edge_span(end_shape = "circle") +
  geom_node_label(aes(x=xmin-5,label=name, color=group),show.legend = F)
# remove most connected words to explore deeper connections
minedgeweightpct <- 0.014
minvertexdegreepct <- 0.3
cutoff <- E(g.top)$weight%>%min + (E(g.top)$weight%>%max - E(g.top)$weight%>%min) * minedgeweightpct
g.top.weightOver5 <- g.top %>% activate(edges) %>% filter(weight>cutoff)
g.top.weightOver5 <- delete.vertices(g.top.weightOver5,which(
  degree(g.top.weightOver5, mode="all") > (degree(g.top.weightOver5, mode="all") %>% max * minvertexdegreepct) 
  | degree(g.top.weightOver5, mode="all") < cutoff/3		
)) %>% as_tbl_graph
bioFabric(g.top.weightOver5 %>% slice(1:30))


# Word Embeddings
tokens.all <- x[, c("doc_id","lemma")][,.(text = paste(lemma, collapse=" ")), by = doc_id] %>% corpus %>% tokens

it <- itoken(tokens.all%>%as.list)
vocab <- create_vocabulary(it) # this generates a flat list of tokens
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5) # terms coocurrence matrix fast
glove = GlobalVectors$new(rank = 50, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 50, convergence_tol = 0.01, n_threads = ncores)
wv_context = glove$components
word_vectors = wv_main + t(wv_context) 
word_vectors.dt <- word_vectors %>% as.data.table
word_vectors.dt[,lemma:=vocab$term]
word_vectors.dt[,freq:=vocab$term_count]
setkey(word_vectors.dt,lemma)

# Only now that we have calculated the positions of words in 50-dimensional space 
# based on their complete embeddings, we can remove less interesting parts of speech 
# and stopwords:
text.lemmas.simplified <- x[,c("lemma","upos")] %>% as.data.table %>% unique %>% setkey(lemma)
word_vectors.dt <- text.lemmas.simplified[word_vectors.dt][
  upos %in% c("NOUN","PROPN","ADJ","VERB") & 
    !lemma %chin% mystopwords
  ,-c("upos")
] %>% unique
# Reduce dimensionality
word_vectors.simplified.df <- word_vectors.dt[,-c("lemma","freq")] %>% as.data.frame
rownames(word_vectors.simplified.df) <- word_vectors.dt$lemma
distance <- dist(word_vectors.simplified.df) # for the rest of the code, we only need the basic distance object
clusters <- hclust(distance) # uses fastcluster package when loaded

# Principal component analysis
pc <- prcomp(word_vectors.simplified.df)
screeplot(pc,type="lines",
          main="Global variance of the word2vec model\nkept in each principal component after the rotation",
          npcs=length(pc$sdev))
# Map words in two dimensions
pcx <- pc$x %>% as.data.table
pcx[,groupe9:=cutree(clusters,9)%>%as.character] 
pcx[,lemma:=word_vectors.dt$lemma]
pcx[,freq:=word_vectors.dt$freq]
setkey(pcx,"lemma")
pcx <- text.lemmas.simplified[pcx][
  upos %in% c("NOUN","PROPN","ADJ","VERB") & 
    !lemma %chin% mystopwords
  ,-c("upos")
] %>% unique

# Get the 30 most frequent words per group
pcx.grouptops <- copy(pcx)
setorder(pcx.grouptops,-freq)
pcx.grouptops <- lapply(1:9,function(i){
  pcx.grouptops[groupe9==i] %>% head(30)
}) %>% rbindlist

ggplot(pcx.grouptops) +
  geom_text(
    aes(label=lemma,x=PC1,y=PC2,colour=groupe9, size=log(freq)),
    alpha=0.8
  ) +
  facet_wrap("groupe9",scales = "free") +
  theme_light()
# Map the 200 most frequent words in the same space
ggplot(pcx %>% setorder(-freq) %>% head(150)) +
  geom_text(
    aes(label=lemma, x=PC1, y=PC2, colour=groupe9, size=log(freq)),
    alpha=0.8
  )

# Self-organising map
somdata <- pcx[,c(paste0("PC",1:10))] %>% as.data.frame
row.names(somdata) <- pcx$lemma
som_grid <- somgrid(xdim = 9, ydim=14, topo="hexagonal")
sommap <- som( 
  somdata %>% as.matrix, 
  grid=som_grid, 
  rlen=600, 
  alpha=c(0.05,0.01)
)
sommap_dist <- getCodes(sommap) %>% dist
sommap_clusters <- fastcluster::hclust(sommap_dist)
somxydict <- data.table( # construct a dictionary
  pointid=1:nrow(sommap$grid$pts), 
  x=sommap$grid$pts[,1], 
  y=sommap$grid$pts[,2],
  groupe5=cutree(sommap_clusters,5) %>% as.character
)
par(mfrow = c(2, 5))
for(x in 1:ncol(somdata)) {
  plot(
    sommap, 
    type = "property", 
    property = getCodes(sommap)[,x], 
    main=colnames(somdata)[x],
    shape="straight",
    palette.name = viridisLite::inferno
  ) 
  add.cluster.boundaries(sommap,somxydict$groupe5,lwd=2)
}
par(mfrow = c(1, 1)) #reset par

# Now the word map with the three most frequent words for each point ####
setkey(somxydict,pointid)
pcx[,pointid := sommap$unit.classif]
setkey(pcx,pointid)
pcx2 <- somxydict[pcx]
setorder(pcx2,-freq) # thanks to this order, .(lemmas=c(lemma)[1]) will give the highest element of group
pcx.aggreg <- lapply(1:3, function(i) pcx2[,.(
  lemma=c(lemma)[i],
  x=c(x)[i],
  y=c(y)[i]-2/3+(i/3), # we want the middle term to be centered, thus -2/3 as starting point
  zlabel=(i),
  freq=c(freq)[i],
  groupe5=c(groupe5)[i]
),by="pointid"]) %>% rbindlist

ggplot() +
  geom_text(data=pcx.aggreg, aes(x=x, y=y, label=lemma, color=groupe5), show.legend=F) +
  theme_void()

## Add third dimension to show frequency ####
# Prepare raster heights 
pointheight <- pcx2[,.(
  freq=sum(freq),
  x=c(x)[1],
  y=c(y)[1]
),by="pointid"]
xleft <- 0
xright <- max(pointheight$x)+0.8
yupper <- max(pointheight$y)+1
ylower <- 0
# interpolate the points to a raster, with sampling frequency nx X ny ---
# Note that the original coordinates of the points are preserved 
pointheight.raster2 <- akima::interp( 
  x = pointheight$x, 
  y = pointheight$y, 
  z = pointheight$freq,
  xo = seq(xleft,xright,by=(xright-xleft)/1500),
  yo = seq(ylower,yupper,by=(yupper-ylower)/3000),
  linear = FALSE,
  extrap = TRUE, # works for non-linear only; if false creates a convex hull that removes points at the edges
  duplicate = "strip"
)
pointheight.raster2.dt <- akima::interp2xyz(pointheight.raster2) %>% as.data.table %>% setnames("z","freq")
if (nrow(pointheight.raster2.dt[freq<0]) > 0) { 	
  # necessary if the interpolation has produced negative values
  pointheight.raster2.dt[,freq:=freq-min(freq,na.rm=TRUE)] # NB: -(-x) = +x
}
pointheight.raster2.dt[is.na(freq),freq:=min(pointheight$freq)]

# terrain_palette <- colorRampPalette(c("#6AA85B", "#D9CC9A", "#FFFFFF"))(256) %>% colorspace::lighten(0.9)
terrain_palette <- colorRampPalette(c("#f0f0f0","white"))(256)
pp <- ggplot() +
  geom_raster(data=pointheight.raster2.dt, aes(x=x, y=y, fill=freq^0.5),interpolate=TRUE,show.legend = F) +
  scale_fill_gradientn(colours=terrain_palette)+
  geom_text(data=pcx.aggreg, aes(x=x,y=y, label=lemma, color=groupe5), show.legend = F) +
  theme_void()
rayshader::plot_gg(
  pp, width=15, height=10,
  raytrace=TRUE, 
  scale = 500, 
  windowsize = c(1600, 1000),
  fov = 70, zoom = 0.4, theta = 0, phi = 90
) 
rayshader::render_snapshot("som3d.png",width=3200, height=2000)
close3d()
