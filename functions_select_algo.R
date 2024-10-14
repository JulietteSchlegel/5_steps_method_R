library(igraph)
library(dplyr)
library(NMI)
library(mclust)
library(reshape2)
library(ggplot2)

#  Return the matrice of the NMI between each pair of algorithms tested. -------

matrix_NMI <- function(...) { # /!\ function arguments must be data frames containing  :
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
      
     
      # Compute NMI
      Nmi_result <- NMI(df_i, df_j)  
      
      # Extract NMI value
      Nmi_value <- Nmi_result$value 
         
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



# Plot the NMI matrice with a color gradient to show the stronger NMI.--------

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



# Compute the convergence of results for each algorithm with its "opponents"----------

mean_NMI <- function(Nmi_matrice) {
  diag(Nmi_matrice) <- NA  
  
  mean_nmi <- rowMeans(Nmi_matrice, na.rm = TRUE)
  
  df_mean_nmi <- data.frame(Algorithm = rownames(Nmi_matrice), Mean_NMI = mean_nmi)
  
  return(df_mean_nmi)
}



# Compute the mean absolute standard deviation for each aglorithm with its  "opponents" ----------

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


