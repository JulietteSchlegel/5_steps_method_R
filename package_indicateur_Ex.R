library(igraph)
library(dplyr)
library(NMI)
library(mclust)
library(reshape2)
library(ggplot2)
library(sna)
library(concorR)

library(igraphdata) #for the Karate network data

# We show an example with algorithms using different heuristics, 
# the results might be strange on the karate network which is not 
# a policy network and whose links aren't related to coordination relationships 

#1. Donloawd or compute the network  -----------

data("karate")

Graph <- karate
Matrice <- as_adjacency_matrix(Graph, names=TRUE, sparse=FALSE, attr = "weight")

# transform to undirected with the method of your choice if using 
# certain algorithms that only work on undirected graphs

Graph_undirected <- as.undirected(Graph, mode = c("each"))


#2. Save algorithm selection partitioning -------------

# example with some modular, hierarchical and structural equivalence methods

Algo1 <- cluster_fast_greedy(Graph_undirected,  merges = TRUE,
                              modularity = TRUE,
                              membership = TRUE,
                              weights = NULL)  

Algo2 <- cluster_leading_eigen(Graph_undirected, steps = -1, weights = NULL) 

Algo3 <- cluster_louvain(Graph_undirected, weights = NULL, resolution = 1)

Algo4 <- cluster_walktrap(Graph,
                          weights = NULL,
                          merges = TRUE,
                          modularity = TRUE,
                          membership = TRUE)

Algo8 <- cluster_label_prop(Graph, weights = NULL)

#Algo 5

Dist_euc <- dist(Matrice, method="euclidean") 
dist_matrix <- as.matrix(Dist_euc)

hc_euc <- hclust(Dist_euc, method = "ward.D2" ) 
plot(hc_euc, hang = -1, cex = 0.6)

Algo5 <- cutree(hc_euc, k=3)  #set the number of communities manually 

#Algo 6

Dist_man <- dist(Matrice, method="manhattan") 

hc_man <- hclust(Dist_man, method = "ward.D2" )
plot(hc_man, hang = -1, cex = 0.6)

Algo6 <- cutree(hc_man, k=3)

#Algo 7

list1 = list(Matrice)
Algo7 <- concor(list1, 
                nsplit = 2, #adapt nsplit to the number of cluster pairs required
                self_ties = FALSE, 
                cutoff = .9999999, 
                max_iter = 50) 


#3. View the NMI matrix  ----------

# transform our various objects storing information on algorithm partioning
# into data frames for easier comparison

membership_A1 <- membership(Algo1)
df_algo1 <- data.frame(Actor = names(membership_A1), 
                       Cluster = as.integer(membership_A1))

membership_A2 <- membership(Algo2)
df_algo2 <- data.frame(Actor = names(membership_A2), 
                       Cluster = as.integer(membership_A2))

membership_A3 <- membership(Algo3)
df_algo3 <- data.frame(Actor = names(membership_A3), 
                       Cluster = as.integer(membership_A3))

membership_A4 <- membership(Algo4)
df_algo4 <- data.frame(Actor = names(membership_A4), 
                       Cluster = as.integer(membership_A4))

df_algo5 <- data.frame(Actor = names(Algo5), Cluster = as.integer(Algo5))

df_algo6 <- data.frame(Actor = names(Algo6), Cluster = as.integer(Algo6))

df_algo7 <- data.frame(Actor = Algo7$vertex,
                       Cluster = Algo7$block)

membership_A8 <- membership(Algo8)
df_algo8 <- data.frame(Actor = names(membership_A8), 
                       Cluster = as.integer(membership_A8))


### Compute the NMI matrix --------

matrix_NMI <- function(...) { # /!\ lfunction arguments must be data frames containing  :
                              # an “Actor” variable with the names/indice of the nodes
                              # a “Cluster” variable with the results of the algorithm 
  
  data_frames <- list(...)  # Get clusters as arguments
  n_algorithms <- length(data_frames)  
  
  algorithm_names <- sapply(substitute(list(...))[-1],deparse) # Retrieve algorithms names
  
  # Initialize the NMI matrix
  Nmi_matrice <- matrix(NA, nrow = n_algorithms, ncol = n_algorithms,
                        dimnames = list(algorithm_names,algorithm_names))
  
  diag(Nmi_matrice) <- 1  # NMI of a cluster with itself is 1
  
  # Loop to fill the matrix
  for (i in 1:(n_algorithms - 1)) {
    for (j in (i + 1):n_algorithms) {
      df_i <- data.frame(Actor = data_frames[[i]]$Actor,  
                         Cluster = as.integer(as.character(data_frames[[i]]$Cluster)))
      df_j <- data.frame(Actor = data_frames[[j]]$Actor, 
                         Cluster = as.integer(as.character(data_frames[[j]]$Cluster)))
      
      # Display debugging information
      cat("Comparing:", names(data_frames)[i], "and", names(data_frames)[j], "\n")
      cat("Number of actors in", names(data_frames)[i], ":", nrow(df_i), "\n")
      cat("Number of actors in", names(data_frames)[j], ":", nrow(df_j), "\n")
      
      # Compute NMI
      Nmi_result <- NMI(df_i, df_j)  
      
      # Extract NMI value
      Nmi_value <- Nmi_result$value 
      
      # Make sure the NMI value is a valid number before inserting it.
      cat("NMI value between", names(data_frames)[i], "and", names(data_frames)[j], ":", Nmi_value, "\n")
      
      if (!is.na(Nmi_value) && is.finite(Nmi_value)) {
        Nmi_matrice[i, j] <- Nmi_value  # Fill in the matrice
        Nmi_matrice[j, i] <- Nmi_value  # Symmetrical
      } else {
        cat("NMI value is not valid (NA or Inf) for", names(data_frames)[i], "and", names(data_frames)[j], "\n")
      }
    }
  }
  
  return(Nmi_matrice)  
}


Nmi_matrice <- matrix_NMI(df_algo1, df_algo2, df_algo3, df_algo4, df_algo5, df_algo6, df_algo7, df_algo8)

print(Nmi_matrice) 

### Display the matrix as a graph ----------

plot_NMI<- function(nmi_matrix) {
  nmi_frame <- as.data.frame(as.table(nmi_matrix))
  colnames(nmi_frame) <- c("Measure1", "Measure2", "NMI_value")
  
  nmi_plot <- ggplot(nmi_frame, aes(x = Measure1, y = Measure2, fill = NMI_value, label = round(NMI_value, 2))) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = NULL, y = NULL, fill = "NMI value", title = "NMI of the Partitionnements") +
    geom_text(color = "black", size = 4) + 
    theme_minimal() +  # Appliquer un thème minimal
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(nmi_plot)
}

plot_NMI <- plot_NMI(Nmi_matrice)

plot_NMI

#4. Calculate performance indicator ---------

##  calculation of the convergence of results for each algorithm with its “opponents

mean_NMI <- function(Nmi_matrice) {
  diag(Nmi_matrice) <- NA  
  
  mean_nmi <- rowMeans(Nmi_matrice, na.rm = TRUE)
  
  df_mean_nmi <- data.frame(Algorithm = rownames(Nmi_matrice), Mean_NMI = mean_nmi)
  
  return(df_mean_nmi)
}

mean_nmi <- mean_NMI(Nmi_matrice)

## calculation of mean absolute standard deviation (mean absolute difference between all pairs of NMI values)

eam_NMI <- function(Nmi_matrice) {
  
  diag(Nmi_matrice) <- NA  
  heterogeneity_values <- numeric(nrow(Nmi_matrice)) 

  # Loop over each algorithm (each row of the matrix)
  for (i in 1:nrow(Nmi_matrice)) {
    
    NMI_row <- Nmi_matrice[i, ]
    NMI_row <- NMI_row[!is.na(NMI_row)]# Retrieve NMIs from the algorithm along with the others
    combinaisons <- combn(NMI_row, 2)  # Generates all possible pairss
    abs_diff <- abs(combinaisons[1, ] - combinaisons[2, ]) #absolute difference for each pair
    mean_abs_diff <- mean(abs_diff)
    heterogeneity_values[i] <- mean_abs_diff
  }
  
  df_heterogeneity <- data.frame(Algorithm = rownames(Nmi_matrice), EAM = heterogeneity_values)
  
  return(df_heterogeneity)
}


eam_nmi <- eam_NMI(Nmi_matrice)  


Indicateur <- data.frame(
  row.names = mean_nmi$Algorithm,
  "Mean" = mean_nmi$Mean_NMI,
  "heterogeneity" = eam_nmi$EAM)

Indicateur$indicateur <- (mean_nmi$Mean_NMI * (1- eam_nmi$EAM))

# Open the Indicator data frame to see the performance of the algorithm selection  
# in the convergence of their partitioning.

# In the karate exemple, the algorithm Concor maximise the indicator.
# It is a blockmodelling algorithm whose heuristic use structural equivalence to compute the clusters

# Note that the result depend on the selection of algorithms, 
# the result would have been different with a different selection using different heuristics.

#viewing results
V(Graph)$cluster <- df_algo7$Cluster

plot.igraph(Graph, 
            vertex.color=ifelse(V(Graph)$cluster==1, "deepskyblue",
                            ifelse(V(Graph)$cluster == 2, "tomato", 
                               ifelse(V(Graph)$cluster == 3, "green", "gold"))),
            vertex.label.cex= 0.5,
            vertex.label.dist=0, 
            vertex.label.family="Helvetica",
            
            edge.arrow.size=0.2,
            edge.curved=0.4,
            layout=layout_nicely)

title( main = "Clusters formed by the algorithm maximizing the performance indicator", cex.main = 0.8)

