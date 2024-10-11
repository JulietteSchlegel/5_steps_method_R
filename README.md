# Select_a_community_detection_algoritm
Provide functions on R to compare partitions and determine the "best algorithm" by maximizing a novel indicator based on convergence and homogeneity between algorithm results.

# Summary
Establishing sub-networks, clusters, blocks, cliques, or communities is one of the most critical problems in network analysis, studied by the block modeling and community detection literature. Methods for identifying efficient algorithms are emerging, but there is no clear-cut answer to the determination of the most efficient method for real-world networks, particularly in the absence of ground-based partition. According to existing methods, selecting the most efficient algorithm can produce varying results, depending on the type of network studied and its structure. In the case of policy networks whose subsystems contain a reduced number of nodes, whose community structures can fluctuate widely, and whose communities can have two definitions leading to very different heuristic methods (internal cohesion and/or structural equivalence), the final choice of algorithm can be somewhat arbitrary. 
To select a community detection algorithm that is precisely suited to a given policy network and in the absence of ground-truth communities, we developed a useful 5-Steps selection method:

  - (1) Test the algorithms frequently used in ACF studies
  - (2) Select those whose results are consistent in the context
  - (3) Compute the partitioning similarity between each selected algorithm
  - (4) Select the algorithm that maximizes the relevant performance indicator
  - (5) Perform a robustness test to check the number of clusters selected

In this file, you'll find the functions we've developed to compute the performance indicator as a function of the convergence and heterogeneity of the partitionings proposed by your selection of algorithms.

For details and application of the method, see "[Too many options: How to identify coalitions in a policy network?](https://www.sciencedirect.com/science/article/pii/S0378873324000376)", Thibaud Deguilhem, Juliette Schlegel, Jean-Philippe Berrou, Ousmane Djibo, Alain Piveteau. Social Networks, Volume 79, 2024, Pages 104-121, ISSN 0378-8733, [https://doi.org/10.1016/j.socnet.2024.06.005](https://doi.org/10.1016/j.socnet.2024.06.005).

# Functions
In the `functions_select_algo.R` file you will find the following functions : 

  - `matrix_NMI()` : Return the matrice of the NMI between each pair of algorithms tested.
  - `plot_NMI()` : Plot the NMI matrice with a color gradient to show the stronger NMI.
  - `mean_NMI()` : Compute the convergence of results for each algorithm with its "opponents".
  - `eam_NMI()` : Compute the mean absolute standard deviation for each aglorithm with its "opponents" (mean absolute difference between all pairs of NMI values)

The performance indicator for each algorithm can then be optain with the following equation : 
```
Indicator(n) = mean_NMI(n) * (1 - eam_NMI(n))
```

# Example
An example with an opensource network (`karate`) is available in the file `example_select_algo.R`.

# Citation
If you find those functions useful for your publication, please cite : 

```
Deguilhem, T., Schlegel, J., Berrou, J-P., Djibo, O., Piveteau, A. (2024). Too many options: How to identify coalitions in a policy network?, Social Networks, Volume 79, Pages 104-121, https://doi.org/10.1016/j.socnet.2024.06.005.
```
