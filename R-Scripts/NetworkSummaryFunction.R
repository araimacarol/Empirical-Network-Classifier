library(igraph)

###----Normalized Laplacina function--##
normalized_laplacian=function(Graphs){
  laplacian_matrix(Graphs,normalized = T)
}

###----Graph features
calcGraphFeatures <- function(Graphs=NULL) {
  
  features <- c(
    "order",                    # number of vertices
    "edges",                     # number of edges
    "connected",                # True / False
    "max_component",            # maximum component size (=order iff the graph is connected)
    "minDegree",                # minimum degree of any vertex
    "maxDegree",                # maximum degree of any vertex
    "mean_degree",                # average degree of any vertex
    "minCut",                   # minimum cut weight of the graph (might take a while to compute)
    "FiedlerValue",             # second-highest eigenvalue of the Laplacian matrix
    "Normalized_FiedlerValue",   # second-highest eigenvalue of the Normaized Laplacian matrix
    "closeness_centr",                # average inverse of distance between any pair of vertices
    "modularity",               # DEFINITION REQUIRED
    "diameter",                 # maximum distance between any two vertices (NAN if not connected)
    "betw_centr",              # max_{v} proportion of shortest paths going through vertex v
    "transitivity",             # aka Clustering Coefficient, is proportion of connected triples that form triangles: e.g., (a--b--c--a) when (a--b--c) is present.
    "threshold",                 # 1/max(eigen value of A)
    "spectral_radius"         # max (eigen value of A)
    
  )
  
  df <- as.data.frame(matrix(ncol=length(features),nrow=length(Graphs)))
  colnames(df)=features
  
  ###----Stuff that is simple to apply and needs no interim components:
  
  df$order = base::as.numeric(lapply(Graphs, gorder))
  df$edges = base::as.numeric(lapply(Graphs, gsize))
  df$connected = base::as.numeric(lapply(Graphs, is_connected))
  df$minCut = base::as.numeric(lapply(Graphs, min_cut))
  df$diameter = base::as.numeric(lapply(Graphs, diameter))
  df$transitivity = base::as.numeric(lapply(Graphs, transitivity))
  
  ###----stuff that needs interim things:
  degrees = lapply(Graphs,igraph::degree )
  df$minDegree = base::as.numeric(lapply(degrees, min))
  df$maxDegree = base::as.numeric(lapply(degrees, max))
  df$mean_degree = base::as.numeric(lapply(degrees, mean))
  
  ####----stuff that lapply doesn't like so has to be done in a loop:
  communities <-lapply(Graphs, cluster_walktrap) #lapply(Graphs, cluster_leading_eigen)
  Adj <- lapply(Graphs, as_adjacency_matrix)
  L <- lapply(Graphs, laplacian_matrix)
  Norm_Lap<-lapply(Graphs, normalized_laplacian)
  Fiedler.value=NULL
  norm.fiedler.value=NULL
  
  for (i in 1:length(Graphs)) {
    if (is.null(Graphs[[i]]$type)) { Graphs[[i]]$type = "untyped" }
    df$modularity[i] <- modularity(communities[[i]])
    df$spectral_radius[i] <- eigen(Adj[[i]], symmetric=TRUE, only.values=TRUE)$values[1]
    
    Fiedler.value[[i]]=eigen(L[[i]], symmetric=TRUE, only.values=TRUE)$values
    
    df$FiedlerValue[i] <- Fiedler.value[[i]][length(Fiedler.value[[i]])-1]
    
    norm.fiedler.value[[i]]=eigen(Norm_Lap[[i]], symmetric=TRUE, only.values=TRUE)$values
    
    df$Normalized_FiedlerValue[i] <- norm.fiedler.value[[i]][length(norm.fiedler.value[[i]])-1]
    
    df$eigen_centr[i] <- centr_eigen(Graphs[[i]])$centralization
    df$deg_centr[i] <- centr_degree(Graphs[[i]])$centralization
    
    df$betw_centr[i] <- centr_betw(Graphs[[i]])$centralization
    
    df$max_component[i] <- max(components(Graphs[[i]])$csize)
    df$mean_eccentr[i]<-mean(eccentricity(Graphs[[i]]))
    df$radius[i]<-radius(Graphs[[i]])
    df$mean_path_length[i]<-mean_distance(Graphs[[i]])
    #df$trace[i]<-sum(diag(Adj[[i]]))
    df$graph_energy[i]<-sum(abs(eigen(Adj[[i]], symmetric=TRUE, only.values=TRUE)$values))
    df$min_triangle[i]= min(count_triangles(Graphs[[i]]))
    df$mean_triangle[i]= mean(count_triangles(Graphs[[i]]))
    df$sd_triangle[i]= sd(count_triangles(Graphs[[i]]))
    df$max_triangle[i]= max(count_triangles(Graphs[[i]]))
    df$num_triangle[i]= sum(count_triangles(Graphs[[i]]))
    df$deg_assort_coef[i]=assortativity_degree(Graphs[[i]])
    
    df$threshold[i] <- 1/(df$spectral_radius[i])
    
    if (df$connected[i]==TRUE) {
      df$closeness_centr[i] = mean(closeness(Graphs[[i]]))
    } else { #----handle the case where G isn't connected
      df$closeness_centr[i] = -1
    }
  }
  return (df)
}



###----Finding largest component 
largest_comp=function(df){
  G=graph_from_data_frame(as.data.frame(df),directed=FALSE, vertices = NULL)
  df.graph=igraph::simplify(G,remove.multiple = T,remove.loops = T)
  Isolated = which(igraph::degree(df.graph)==0)
  Net=igraph::delete_vertices(df.graph, Isolated)
  components = igraph::components(Net, mode="weak")
  biggest_cluster_id = which.max(components$csize)
  vert_ids = V(df.graph)[components$membership== biggest_cluster_id]
  graph=igraph::induced_subgraph(df.graph, vert_ids)
  
  return(graph)
}

###----Create graph object
readtable=function(edgelist="aves-barn-swallow-non-physical.edges"){
  x=read.table(edgelist, 
               fill = TRUE , header = FALSE)
  return(x)
}



Network.Summary <- function(folder_path) {
  all_files <- list.files(path = folder_path, full.names = TRUE)
  
  # edge_files <- lapply(all_files, function(file) {
  #   sep_char <- ifelse(grepl(",", readLines(file, n = 1)), ",", " ")
  #   read.table(file, sep = sep_char, fill = TRUE, header = FALSE)
  # })
  edge_files <- lapply(all_files, function(file) {
    #cat("Reading file:", file, "\n")  # Debugging line
    # Read the first few lines to inspect the file format
    sample_lines <- readLines(file, n = 5)
    # Determine the separator based on the sample lines
    sep_char <- ifelse(grepl(",", sample_lines[1]), ",", ifelse(grepl("\t", sample_lines[1]), "\t", " "))
    # Read the file using the determined separator
    read.table(file, sep = sep_char, fill = TRUE, header = FALSE)
    #cat("Structure of the data frame:", str(df), "\n")  # Debugging line
    
  })
  
  #----Initialize a list to store network feature summaries
  GraphNames <- basename(all_files)
  
  G <- lapply(edge_files, largest_comp)
  features <- calcGraphFeatures(G)
  
  # Combine GraphNames and features into a data frame
  all_graphs <- data.frame(GraphNames = GraphNames, features)
  
  #----Return the list of feature summaries
  return(list(all_graphs, edge_files))
}

# folder_path="Collaboration-Network"
# z=Network.Summary(folder_path)[[1]]
# 
# 






# Network.Summary <- function(folder_path) {
#   all_files <- list.files(path = folder_path, full.names = TRUE)
# 
#   #----Filter only the .edge files
#   #edge_files <- lapply(all_files, read.table, fill = TRUE, header = FALSE)
#   #edge_files <- lapply(all_files, read.table, sep = ",", fill = TRUE, header = FALSE)
# 
#   #edge_files<- lapply(all_files, function(file) read.table(file, sep = ifelse(grepl(",", file), ",", " "), fill = TRUE, header = FALSE))
# 
#   edge_files <- lapply(all_files, function(file) {
#     sep_char <- ifelse(grepl(",", file), ",", " ")
#     read.table(file, sep = sep_char, fill = TRUE, header = FALSE)
#   })
# 
#   #----Initialize a list to store network feature summaries
#   GraphNames <- basename(all_files)
# 
#   G <- lapply(edge_files, largest_comp)
#   features <- calcGraphFeatures(G)
#   all_graphs <- cbind(GraphNames, features)
# 
#   #----Return the list of feature summaries
#   return(list(all_graphs,edge_files ))
# }


# Network.Summary <- function(folder_path) {
#   all_files <- list.files(path = folder_path, full.names = TRUE)
#   
#   edge_files <- lapply(all_files, function(file) read.table(file, sep = ifelse(grepl(",", file), ",", " "), fill = TRUE, header = FALSE))
#   
#   #----Initialize a list to store network feature summaries
#   GraphNames <- basename(all_files)
#   
#   G <- lapply(edge_files, largest_comp)
#   features <- calcGraphFeatures(G)
#   
#   # Combine GraphNames and features into a data frame
#   all_graphs <- data.frame(GraphNames = GraphNames, features)
#   
#   #----Return the list of feature summaries
#   return(list(all_graphs, edge_files))
# }



# folder="test2"
# z=Network.Summary(folder)
# z[[1]]
