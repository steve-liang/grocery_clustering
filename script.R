### follow along K-means + PCA
# https://www.business-science.io/business/2016/09/04/CustomerSegmentationPt1.html
# https://www.business-science.io/business/2016/09/04/CustomerSegmentationPt2.html
# https://github.com/JifuZhao/DS-Take-Home/blob/master/09.%20Clustering%20Grocery%20Items.ipynb


download.file('https://drive.google.com/uc?export=download&id=1mmaA7hTndumhyfC9xtafd2OrMAajO9Fr', destfile = 'grocery.zip')
unzip('grocery.zip')

library(readr)
library(tidyr)
library(recipes)
library(ggplot2)
p_history <- readr::read_csv('purchase_history.csv', col_types = cols(.default = col_character()))
item <- readr::read_csv('item_to_id.csv', col_types = cols(.default = col_character()))

p_hist <- separate_rows(p_history, id, sep=',', convert = F)

# create recipe

rec <- recipe( ~ ., data = p_hist)

r <- rec %>% step_dummy(id, one_hot = TRUE) %>% prep

d <- r$template

data <- d %>% group_by(user_id) %>% summarise_all(sum) %>% ungroup
# normalized <- bind_cols(data[,1], as.data.frame(prop.table(as.matrix(data[,-1]), margin = 2)))

data.t <- t(data[,-1])

# Running the k-means algorithm -------------------------------------------------
library(cluster) # Needed for silhouette function

# kmeansDat.t <- normalized[,-1]  # Extract only customer columns
# kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 30      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(20) # For reproducibility
  km.out[i] <- list(kmeans(data.t, centers = centr, nstart = 50))
  sil.out[i] <- list(cluster::silhouette(km.out[[i]][[1]], dist(data.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

plot(sil.out[[5]])

ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

# Get attributes of optimal k-means output
maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)

## PCA

pca <- prcomp(data.t, scale. = T, center = T) # Perform PCA
summary(pca)

library(ggfortify) # For fortify()

pca.fortify <- fortify(pca) # fortify() gets pca into usable format
pca.dat <- cbind(pca.fortify, group=km.out.best$cluster)

g <- ggplot(pca.dat) +
  geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca.dat)), size=2) +
  labs(title = "Visualizing K-Means Clusters Against First Two Principal Components") +
  scale_color_brewer(name="", palette = "Set1")

library(plotly)
# Use plotly for inteactivity
p <- plotly::ggplotly(g, tooltip = c("text", "x", "y")) %>%
  layout(legend = list(x=.9, y=.99))

