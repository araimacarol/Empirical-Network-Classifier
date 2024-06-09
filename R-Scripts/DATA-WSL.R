setwd("C:/Users/rcappaw/OneDrive - University of Tasmania/Desktop/R/Workflow/igraphEpi-New")
library(igraph)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Data generation for the machine learning model
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###---Simulate--spatial--networks---##
# simulate.spatial<-function(N=50,radius=0.4,nsim=100){
#   spatial.graph=NULL
#   for (i in 1:nsim){
#     spatial.graph[[i]]=makeSpatialGraphs(node.size=N,Radius=radius)
#   }
#   return(spatial.graph)
# }

##########################################################################################################
# Input an igraph object from a file, treating it as a static graph
##########################################################################################################
getGraphFromFile <- function(file, simplify=TRUE, useBiggestComponent=TRUE, asUndirected=TRUE) {
  
  dat <- read.table(file) # just read static graph: ignoring third column
  G <- graph_from_data_frame(dat)
  if (asUndirected==TRUE) {
    G <- as.undirected(G, "collapse")
  }
  
  #  g_names <- gsub(".edges","",networks[i]) # one edge for each pair of connect vertices (not sure what this is for)
  
  if (useBiggestComponent==TRUE) {
    netclust <- components(G) #look for subgraphs
    gcc <- V(G)[netclust$membership == which.max(netclust$csize)]#select vertices from the largest sub-graph
    G <- induced.subgraph(G, gcc) #make it a igraph object.
  }
  if (simplify==TRUE) {
    G <- igraph::simplify(G, remove.multiple = TRUE, remove.loops = TRUE)
  }
  return(G)
}

###--Normalized Laplacina function--##
normalized_laplacian=function(Graphs){
  laplacian_matrix(Graphs,normalized = T)
}



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
  
  # Stuff that is simple to apply and needs no interim components:
  
  df$order = base::as.numeric(lapply(Graphs, gorder))
  df$edges = base::as.numeric(lapply(Graphs, gsize))
  df$connected = base::as.numeric(lapply(Graphs, is_connected))
  df$minCut = base::as.numeric(lapply(Graphs, min_cut))
  df$diameter = base::as.numeric(lapply(Graphs, diameter))
  df$transitivity = base::as.numeric(lapply(Graphs, transitivity))
  
  # stuff that needs interim things:
  degrees = lapply(Graphs,igraph::degree )
  df$minDegree = base::as.numeric(lapply(degrees, min))
  df$maxDegree = base::as.numeric(lapply(degrees, max))
  df$mean_degree = base::as.numeric(lapply(degrees, mean))
  
  # stuff that lapply doesn't like so has to be done in a loop:
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
    } else { # handle the case where G isn't connected
      df$closeness_centr[i] = -1
    }
  }
  return (df)
}




##----------- Graph Features----------#####
## Used to perform runs on multiple simulated graphs on any given network
RunSimOnGraphFeatures<-function(Graphs, nreps=nreps,output_file=NULL, seed=-1) {
  set.seed(1)
  # ### Definition and initialization of parameters for graphfeatures
  graphProperties=list()
  # ### Definition and initialization of Graph Prefix
  graphid=list();graphreplicate=list(); graphname=list(); GraphPrefix=list(); analysis=list()
  for (g in 1:length(Graphs)){
    for (reps in 1:nreps) {
      ### Calculate the graph features for each simulated graph of all the synthetic networks
      print(paste("Calculating graph features on", Graphs[[g]]$name))
      #graphProperties[[reps]] <- calcGraphFeatures(Graphs[g])
      graphProperties[[reps]] <- calcGraphFeatures(Graphs[g])
    }
    graphname[[g]]=Graphs[[g]]$type
    graphid[[g]]=Graphs[[g]]$id
    graphreplicate[[g]]=c(1:nreps)
    GraphPrefix=cbind(graphname[[g]],graphid[[g]],graphreplicate[[g]])
    colnames(GraphPrefix)=c("GraphName","GraphID","GraphReplicate")
    analysis[[g]]=as.data.frame(cbind(GraphPrefix,graphProperties[[reps]]))
    row.names(analysis[[g]])=1:nreps
  }
  All_results=do.call(rbind,analysis)
  # write.csv(All_results, file=output_file)
  return( All_results)
}


#+++++++++++++++++++++++++++++++++++++++++++++++
#-------MAKING OF SPATIAL GRAPH---
#+++++++++++++++++++++++++++++++++++++++++++++++

# Helper functions for spatial graphs to convert between (row, column) pairs and the index in a list of Cells.
 cellCoordsToIndex <- function(i, j, size) {
   return((i-1)*size+j)
 }

  indexToCellCoords <- function(idx, size) {
   j <- (idx-1) %% size + 1
   i <- (idx + size-1) %/% size
   return(c(i,j))
 }

# - end of Helper functions

#
# for (i in 1:4) {
#   for (j in 1:4) {
#     idx <- cellCoordsToIndex(i,j,4)
#     print(paste("i =", i, "; j = ", j , "index = " , idx ,
#                 "; reverse = ", indexToCellCoords(idx, 4)[1], indexToCellCoords(idx, 4)[2]))
#     print("")
#   }
# }

#########################################################################################
# Uncomment to generate data for various theoretical networks
#########################################################################################
#---Induce graph function---######
# induced.graph=function(Graphs){
#   initgraph=NULL;G=NULL
#   for (i in 1:length(Graphs)){
#     initgraph=Graphs[[i]]
#     components = igraph::components(initgraph , mode="weak")
#     biggest_cluster_id = which.max(components$csize)
#     # # ids
#     vert_ids = V(initgraph)[components$membership== biggest_cluster_id]
#     # # subgraph
#     G[[i]]=igraph::induced_subgraph(initgraph, vert_ids)
#   }
#   return(initgraph)
# }
#
#
# #+++++++++++++++++++++++++++++++++++++++++++
# # Function to make spatial network
# #+++++++++++++++++++++++++++++++++++++++++++++
fastSpatialNetwork <- function(n=100,r=0.1, makeConnected=FALSE, keepCellsSeparate=FALSE) {
  # Divide the grid into cells of diagonal length r (so side length r/sqrt(2))
  # All pairs of points in the same cell are within distance r of each other
  # Look around each cell at the points in the 8 adjacent cells
  # Test each for Euclidean distance.
  # Connect all that are close enough; note those that aren't for later addition

  # Set up the coordinates:
  v <- data.frame(matrix(nrow=n))
  v$x <- runif(n)
  v$y <- runif(n)
  v[,1] <- 1:n
  colnames(v) <- c("id","x","y")

  # put points into bins:
  cellSize <- r/sqrt(2)
  R2 <- r*r
  gridSize <- ceiling(sqrt(2)/r)
  # Create a grid of cells and assign vertices to them based on their coordinates:
  Cell <- vector(mode="list", gridSize*gridSize)
  for (i in 1:n) {
    #		rowNum <- floor(v$y[i]/cellSize) + 1
    #		colNum <- floor(v$x[i]/cellSize) + 1
    idx <- (floor(v$y[i]/cellSize))*gridSize + floor(v$x[i]/cellSize) + 1
    Cell[[idx]] <- cbind(Cell[[idx]], i)
  }
  E <- matrix(nrow=0, ncol=2, byrow=TRUE)

  # join all vertices that are in the same cell since they must be at most distance r apart
  for (cell in Cell) {
    if (length(cell) > 1) {
      E <- rbind(E, t(combn(cell, 2)))
    }
  }
  proximalPairs <- data.frame(matrix(5,ncol=3,nrow=1))
  colnames(proximalPairs) <- c("r","i","j")

  if (keepCellsSeparate == FALSE) {
    for (i in 2:gridSize) {
      # first column:
      # join any points in [ ]
      #                    [ ] that are close enough
      thisIdx <- cellCoordsToIndex(1,i, gridSize)
      aboveIdx <- cellCoordsToIndex(1,i-1, gridSize)
      for (a in Cell[[thisIdx]]) {
        for (b in Cell[[aboveIdx]]) {
          d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
          if (d < R2) {
            E <- as.matrix(rbind(E,c(a,b)))
          } else {
            proximalPairs <- rbind(proximalPairs, c(d, a, b))
          }
        }
      }
      # first row:
      # join any points in [ ][ ] that are close enough.
      thisIdx <- cellCoordsToIndex(i,1, gridSize)
      leftIdx <- cellCoordsToIndex(i-1,1, gridSize)
      for (a in Cell[[thisIdx]]) {
        for (b in Cell[[leftIdx]]) {
          d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
          if (d < R2) {
            E <- as.matrix(rbind(E,c(a,b)))
          } else {
            proximalPairs <- rbind(proximalPairs, c(d, a, b))
          }
        }
      }

      for (j in 2:gridSize) {
        # check all neighbours above and to the left
        #
        # aboveLeftIdx      aboveIdx
        #             XX       ||
        #     leftIdx    ==  thisIdx
        thisIdx <- cellCoordsToIndex(i,j,gridSize)
        aboveIdx <- cellCoordsToIndex(i,j-1,gridSize)

        # this + above:
        for (a in Cell[[thisIdx]]) {
          for (b in Cell[[aboveIdx]]) {
            d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
            if (d < R2) {
              E <- as.matrix(rbind(E,c(a,b)))
            } else {
              proximalPairs <- rbind(proximalPairs, c(d, a, b))
            }
          }
        }

        # this + left:
        leftIdx <- cellCoordsToIndex(i-1,j,gridSize)
        for (a in Cell[[thisIdx]]) {
          for (b in Cell[[leftIdx]]) {
            d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
            if (d < R2) {
              E <- as.matrix(rbind(E,c(a,b)))
            } else {
              proximalPairs <- rbind(proximalPairs, c(d, a, b))
            }
          }
        }

        # this + aboveLeft
        aboveLeftIdx <- cellCoordsToIndex(i-1,j-1,gridSize)
        for (a in Cell[[thisIdx]]) {
          for (b in Cell[[aboveLeftIdx]]) {
            d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
            if (d < R2) {
              E <- as.matrix(rbind(E,c(a,b)))
            } else {
              proximalPairs <- rbind(proximalPairs, c(d, a, b))
            }
          }
        }

        # left and above:
        for (a in Cell[[leftIdx]]) {
          for (b in Cell[[aboveIdx]]) {
            d <- (v[a,2]-v[b,2])^2 + (v[a,3]-v[b,3])^2
            if (d < R2) {
              E <- as.matrix(rbind(E,c(a,b)))
            } else {
              proximalPairs <- rbind(proximalPairs, c(d, a, b))
            }
          }
        }
      }
    }
  }

  G = make_empty_graph(n, directed=FALSE)
  G <- add_edges(G, t(E), directed=FALSE)
  vertex_attr(G)$id <- 1:n
  G$layout <- as.matrix(v[,2:3])
  G$name <- "Spatial"
  G$type <- "Spatial"
  G$id <- "1"
  if (makeConnected == F) {
    return(G)
  }
  if (is.connected(G)) {
    return(G)
  }
  # sort the pairs of coordinates in ascending order of distance apart
  proximalPairs <- proximalPairs[-1,]
  rownames(proximalPairs) <- seq(1,dim(proximalPairs)[1])
  indx <- order(proximalPairs$r)

  comps <- components(G, mode="weak")

  for (nu in indx) { # in increasing order of edge length
    i <- proximalPairs$i[nu]
    j <- proximalPairs$j[nu]

    # Consider vertices i and j for joining:
    if (comps$membership[i] != comps$membership[j]) { # if two vertices are not in the same component
      G <- G + edge(i, j, directed=F) # then join them
      comps <- components(G, mode="weak")
    } else {
      # i and j are in the same component.  Moving on...
    }
    if (is.connected(G) == T) {
      break # we are done!
    }
  }
  return(G)
}

#induced.graph(x)
#induced.graph(x[1])

# ###########################################################
# # Create spatial graphs for simulation experiment
# ###########################################################
makeSpatialGraphs<- function(n=25,r=0.8) {
  Graphs = list()#;G=list() # set up the list of Graphs first
  initgraph= list()
  i= 1
  print("Creating fastspatial networks")
  Graphs[[i]] = fastSpatialNetwork(n = n, r = r, makeConnected=T,keepCellsSeparate=FALSE)
  #Graphs[[i]]=induced.graph(G[i])
  # initgraph=Graphs[[i]]
  # components = igraph::components(initgraph , mode="weak")
  # biggest_cluster_id = which.max(components$csize)
  # # # ids
  # vert_ids = V(initgraph)[components$membership== biggest_cluster_id]
  # # subgraph
  #  Graphs[[i]]=igraph::induced_subgraph(initgraph, vert_ids)
  Graphs[[i]]$type = "Spatial"
  Graphs[[i]]$id = "1"
  Graphs[[i]]$name="Spatial"
  i <- i+1
  return(Graphs)
}
# 
sim.sp.net=function(r=c(0.3,0.4,0.6,0.05),n=100){
  net=NULL;data=NULL
  net=as.list(mapply(FUN =makeSpatialGraphs,n,r))
  #data= future.apply::future_lapply(list(net),RunSimOnGraphFeatures,nreps=1,future.seed = 0xBEEF)
  data= lapply(list(net),RunSimOnGraphFeatures,nreps=1)

  df=cbind(data.frame(r),data)
  return(df)
}
# 
# #sim.sp.net(r=rep(0.1,2),n=20)
# 
 plan(multisession)

# 
# # sim.sp.net=function(r=c(0.3,0.4,0.6,0.05),n=100){
# #   net=NULL;data=NULL
# #   k=1
# #   for (i in 1:length(r)){
# #     net[i]= makeSpatialGraphs(n,r[i])
# #     data=RunSimOnGraphFeatures(net,nreps = 1)
# #     k=k+1
# #   }
# #   df=cbind(r,data)
# #   return(df)
# # }

 # #sim.sp.net(r=rep(0.1,2),n=50)
 
 
 ###-----NOTE 2024----####
 # Generate 2139 instances instead of 250
 
### 50 nodes 
SP1.50=sim.sp.net(r=rep(0.1,250),n=50)
write.csv(SP1.50,"SP1.50.csv")
SP2.50=sim.sp.net(r=rep(0.2,250),n=50)
write.csv(SP2.50,"SP2.50.csv")
SP3.50=sim.sp.net(r=rep(0.3,250),n=50)
write.csv(SP3.50,"SP3.50.csv")
SP4.50=sim.sp.net(r=rep(0.4,250),n=50)
write.csv(SP4.50,"SP4.50.csv")
SP5.50=sim.sp.net(r=rep(0.5,250),n=50)
write.csv(SP5.50,"SP5.50.csv")
SP6.50=sim.sp.net(r=rep(0.6,250),n=50)
write.csv(SP6.50,"SP6.50.csv")
SP7.50=sim.sp.net(r=rep(0.7,250),n=50)
write.csv(SP7.50,"SP7.50.csv")
SP8.50=sim.sp.net(r=rep(0.8,250),n=50)
write.csv(SP8.50,"SP8.50.csv")
SP9.50=sim.sp.net(r=rep(0.9,250),n=50)
write.csv(SP9.50,"SP9.50.csv")
######## Saving all SP graphs 50 nodes data to csv
SP1=read.csv("SP1.50.csv");SP2=read.csv("SP2.50.csv");SP3=read.csv("SP3.50.csv")
SP4=read.csv("SP4.50.csv");SP5=read.csv("SP5.50.csv");SP6=read.csv("SP6.50.csv")
SP7=read.csv("SP7.50.csv");SP8=read.csv("SP8.50.csv");SP9=read.csv("SP9.50.csv")

df.SP.50=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.50,"df.SP.50.csv")

### 100 nodes
SP1.100=sim.sp.net(r=rep(0.1,250),n=100)
write.csv(SP1.100,"SP1.100.csv")
SP2.100=sim.sp.net(r=rep(0.2,250),n=100)
write.csv(SP2.100,"SP2.100.csv")
SP3.100=sim.sp.net(r=rep(0.3,250),n=100)
write.csv(SP3.100,"SP3.100.csv")
SP4.100=sim.sp.net(r=rep(0.4,250),n=100)
write.csv(SP4.100,"SP4.100.csv")
SP5.100=sim.sp.net(r=rep(0.5,250),n=100)
write.csv(SP5.100,"SP5.100.csv")
SP6.100=sim.sp.net(r=rep(0.6,250),n=100)
write.csv(SP6.100,"SP6.100.csv")
SP7.100=sim.sp.net(r=rep(0.7,250),n=100)
write.csv(SP7.100,"SP7.100.csv")
SP8.100=sim.sp.net(r=rep(0.8,250),n=100)
write.csv(SP8.100,"SP8.100.csv")
SP9.100=sim.sp.net(r=rep(0.9,250),n=100)
write.csv(SP9.100,"SP9.100.csv")
######## Saving all SP graphs 100 nodes data to csv
SP1=read.csv("SP1.100.csv");SP2=read.csv("SP2.100.csv");SP3=read.csv("SP3.100.csv")
SP4=read.csv("SP4.100.csv");SP5=read.csv("SP5.100.csv");SP6=read.csv("SP6.100.csv")
SP7=read.csv("SP7.100.csv");SP8=read.csv("SP8.100.csv");SP9=read.csv("SP9.100.csv")

df.SP.100=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.100,"df.SP.100.csv")

### 150 nodes
SP1.150=sim.sp.net(r=rep(0.1,250),n=150)
write.csv(SP1.150,"SP1.150.csv")
SP2.150=sim.sp.net(r=rep(0.2,250),n=150)
write.csv(SP2.150,"SP2.150.csv")
SP3.150=sim.sp.net(r=rep(0.3,250),n=150)
write.csv(SP3.150,"SP3.150.csv")
SP4.150=sim.sp.net(r=rep(0.4,250),n=150)
write.csv(SP4.150,"SP4.150.csv")
SP5.150=sim.sp.net(r=rep(0.5,250),n=150)
write.csv(SP5.150,"SP5.150.csv")
SP6.150=sim.sp.net(r=rep(0.6,250),n=150)
write.csv(SP6.150,"SP6.150.csv")
SP7.150=sim.sp.net(r=rep(0.7,250),n=150)
write.csv(SP7.150,"SP7.150.csv")
SP8.150=sim.sp.net(r=rep(0.8,250),n=150)
write.csv(SP8.150,"SP8.150.csv")
SP9.150=sim.sp.net(r=rep(0.9,250),n=150)
write.csv(SP9.150,"SP9.150.csv")
######## Saving all SP graphs 150 nodes data to csv
SP1=read.csv("SP1.150.csv");SP2=read.csv("SP2.150.csv");SP3=read.csv("SP3.150.csv")
SP4=read.csv("SP4.150.csv");SP5=read.csv("SP5.150.csv");SP6=read.csv("SP6.150.csv")
SP7=read.csv("SP7.150.csv");SP8=read.csv("SP8.150.csv");SP9=read.csv("SP9.150.csv")

df.SP.150=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.150,"df.SP.150.csv")

### 200 nodes
SP1.200=sim.sp.net(r=rep(0.1,250),n=200)
write.csv(SP1.200,"SP1.200.csv")
SP2.200=sim.sp.net(r=rep(0.2,250),n=200)
write.csv(SP2.200,"SP2.200.csv")
SP3.200=sim.sp.net(r=rep(0.3,250),n=200)
write.csv(SP3.200,"SP3.200.csv")
SP4.200=sim.sp.net(r=rep(0.4,250),n=200)
write.csv(SP4.200,"SP4.200.csv")
SP5.200=sim.sp.net(r=rep(0.5,250),n=200)
write.csv(SP5.200,"SP5.200.csv")
SP6.200=sim.sp.net(r=rep(0.6,250),n=200)
write.csv(SP6.200,"SP6.200.csv")
SP7.200=sim.sp.net(r=rep(0.7,250),n=200)
write.csv(SP7.200,"SP7.200.csv")
SP8.200=sim.sp.net(r=rep(0.8,250),n=200)
write.csv(SP8.200,"SP8.200.csv")
SP9.200=sim.sp.net(r=rep(0.9,250),n=200)
write.csv(SP9.200,"SP9.200.csv")
######## Saving all SP graphs 200 nodes data to csv
SP1=read.csv("SP1.200.csv");SP2=read.csv("SP2.200.csv");SP3=read.csv("SP3.200.csv")
SP4=read.csv("SP4.200.csv");SP5=read.csv("SP5.200.csv");SP6=read.csv("SP6.200.csv")
SP7=read.csv("SP7.200.csv");SP8=read.csv("SP8.200.csv");SP9=read.csv("SP9.200.csv")

df.SP.200=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.200,"df.SP.200.csv")

### 250 nodes
SP1.250=sim.sp.net(r=rep(0.1,250),n=250)
write.csv(SP1.250,"SP1.250.csv")
SP2.250=sim.sp.net(r=rep(0.2,250),n=250)
write.csv(SP2.250,"SP2.250.csv")
SP3.250=sim.sp.net(r=rep(0.3,250),n=250)
write.csv(SP3.250,"SP3.250.csv")
SP4.250=sim.sp.net(r=rep(0.4,250),n=250)
write.csv(SP4.250,"SP4.250.csv")
SP5.250=sim.sp.net(r=rep(0.5,250),n=250)
write.csv(SP5.250,"SP5.250.csv")
SP6.250=sim.sp.net(r=rep(0.6,250),n=250)
write.csv(SP6.250,"SP6.250.csv")
SP7.250=sim.sp.net(r=rep(0.7,250),n=250)
write.csv(SP7.250,"SP7.250.csv")
SP8.250=sim.sp.net(r=rep(0.8,250),n=250)
write.csv(SP8.250,"SP8.250.csv")
SP9.250=sim.sp.net(r=rep(0.9,250),n=250)
write.csv(SP9.250,"SP9.250.csv")
######## Saving all SP graphs 250 nodes data to csv
SP1=read.csv("SP1.250.csv");SP2=read.csv("SP2.250.csv");SP3=read.csv("SP3.250.csv")
SP4=read.csv("SP4.250.csv");SP5=read.csv("SP5.250.csv");SP6=read.csv("SP6.250.csv")
SP7=read.csv("SP7.250.csv");SP8=read.csv("SP8.250.csv");SP9=read.csv("SP9.250.csv")

df.SP.250=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.250,"df.SP.250.csv")

### 300 nodes
SP1.300=sim.sp.net(r=rep(0.1,250),n=300)
write.csv(SP1.300,"SP1.300.csv")
SP2.300=sim.sp.net(r=rep(0.2,250),n=300)
write.csv(SP2.300,"SP2.300.csv")
SP3.300=sim.sp.net(r=rep(0.3,250),n=300)
write.csv(SP3.300,"SP3.300.csv")
SP4.300=sim.sp.net(r=rep(0.4,250),n=300)
write.csv(SP4.300,"SP4.300.csv")
SP5.300=sim.sp.net(r=rep(0.5,250),n=300)
write.csv(SP5.300,"SP5.300.csv")
SP6.300=sim.sp.net(r=rep(0.6,250),n=300)
write.csv(SP6.300,"SP6.300.csv")
SP7.300=sim.sp.net(r=rep(0.7,250),n=300)
write.csv(SP7.300,"SP7.300.csv")
SP8.300=sim.sp.net(r=rep(0.8,250),n=300)
write.csv(SP8.300,"SP8.300.csv")
SP9.300=sim.sp.net(r=rep(0.9,250),n=300)
write.csv(SP9.300,"SP9.300.csv")
######## Saving all SP graphs 300 nodes data to csv
SP1=read.csv("SP1.300.csv");SP2=read.csv("SP2.300.csv");SP3=read.csv("SP3.300.csv")
SP4=read.csv("SP4.300.csv");SP5=read.csv("SP5.300.csv");SP6=read.csv("SP6.300.csv")
SP7=read.csv("SP7.300.csv");SP8=read.csv("SP8.300.csv");SP9=read.csv("SP9.300.csv")

df.SP.300=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.300,"df.SP.300.csv")

### 350 nodes
SP1.350=sim.sp.net(r=rep(0.1,250),n=350)
write.csv(SP1.350,"SP1.350.csv")
SP2.350=sim.sp.net(r=rep(0.2,250),n=350)
write.csv(SP2.350,"SP2.350.csv")
SP3.350=sim.sp.net(r=rep(0.3,250),n=350)
write.csv(SP3.350,"SP3.350.csv")
SP4.350=sim.sp.net(r=rep(0.4,250),n=350)
write.csv(SP4.350,"SP4.350.csv")
SP5.350=sim.sp.net(r=rep(0.5,250),n=350)
write.csv(SP5.350,"SP5.350.csv")
SP6.350=sim.sp.net(r=rep(0.6,250),n=350)
write.csv(SP6.350,"SP6.350.csv")
SP7.350=sim.sp.net(r=rep(0.7,250),n=350)
write.csv(SP7.350,"SP7.350.csv")
SP8.350=sim.sp.net(r=rep(0.8,250),n=350)
write.csv(SP8.350,"SP8.350.csv")
SP9.350=sim.sp.net(r=rep(0.9,250),n=350)
write.csv(SP9.350,"SP9.350.csv")
######## Saving all SP graphs 350 nodes data to csv
SP1=read.csv("SP1.350.csv");SP2=read.csv("SP2.350.csv");SP3=read.csv("SP3.350.csv")
SP4=read.csv("SP4.350.csv");SP5=read.csv("SP5.350.csv");SP6=read.csv("SP6.350.csv")
SP7=read.csv("SP7.350.csv");SP8=read.csv("SP8.350.csv");SP9=read.csv("SP9.350.csv")

df.SP.350=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.350,"df.SP.350.csv")

### 400 nodes
SP1.400=sim.sp.net(r=rep(0.1,250),n=400)
write.csv(SP1.400,"SP1.400.csv")
SP2.400=sim.sp.net(r=rep(0.2,250),n=400)
write.csv(SP2.400,"SP2.400.csv")
SP3.400=sim.sp.net(r=rep(0.3,250),n=400)
write.csv(SP3.400,"SP3.400.csv")
SP4.400=sim.sp.net(r=rep(0.4,250),n=400)
write.csv(SP4.400,"SP4.400.csv")
SP5.400=sim.sp.net(r=rep(0.5,250),n=400)
write.csv(SP5.400,"SP5.400.csv")
SP6.400=sim.sp.net(r=rep(0.6,250),n=400)
write.csv(SP6.400,"SP6.400.csv")
SP7.400=sim.sp.net(r=rep(0.7,250),n=400)
write.csv(SP7.400,"SP7.400.csv")
SP8.400=sim.sp.net(r=rep(0.8,250),n=400)
write.csv(SP8.400,"SP8.400.csv")
SP9.400=sim.sp.net(r=rep(0.9,250),n=400)
write.csv(SP9.400,"SP9.400.csv")
######## Saving all SP graphs 400 nodes data to csv
SP1=read.csv("SP1.400.csv");SP2=read.csv("SP2.400.csv");SP3=read.csv("SP3.400.csv")
SP4=read.csv("SP4.400.csv");SP5=read.csv("SP5.400.csv");SP6=read.csv("SP6.400.csv")
SP7=read.csv("SP7.400.csv");SP8=read.csv("SP8.400.csv");SP9=read.csv("SP9.400.csv")

df.SP.400=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.400,"df.SP.400.csv")

### 500 nodes
SP1.500=sim.sp.net(r=rep(0.1,250),n=500)
write.csv(SP1.500,"SP1.500.csv")
SP2.500=sim.sp.net(r=rep(0.2,250),n=500)
write.csv(SP2.500,"SP2.500.csv")
SP3.500=sim.sp.net(r=rep(0.3,250),n=500)
write.csv(SP3.500,"SP3.500.csv")
SP4.500=sim.sp.net(r=rep(0.4,250),n=500)
write.csv(SP4.500,"SP4.500.csv")
SP5.500=sim.sp.net(r=rep(0.5,250),n=500)
write.csv(SP5.500,"SP5.500.csv")
SP6.500=sim.sp.net(r=rep(0.6,250),n=500)
write.csv(SP6.500,"SP6.500.csv")
SP7.500=sim.sp.net(r=rep(0.7,250),n=500)
write.csv(SP7.500,"SP7.500.csv")
SP8.500=sim.sp.net(r=rep(0.8,250),n=500)
write.csv(SP8.500,"SP8.500.csv")
SP9.500=sim.sp.net(r=rep(0.9,250),n=500)
write.csv(SP9.500,"SP9.500.csv")
######## Saving all SP graphs 500 nodes data to csv
SP1=read.csv("SP1.500.csv");SP2=read.csv("SP2.500.csv");SP3=read.csv("SP3.500.csv")
SP4=read.csv("SP4.500.csv");SP5=read.csv("SP5.500.csv");SP6=read.csv("SP6.500.csv")
SP7=read.csv("SP7.500.csv");SP8=read.csv("SP8.500.csv");SP9=read.csv("SP9.500.csv")

df.SP.500=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.500,"df.SP.500.csv")

### 750 nodes
SP1.750=sim.sp.net(r=rep(0.1,250),n=750)
write.csv(SP1.750,"SP1.750.csv")
SP2.750=sim.sp.net(r=rep(0.2,250),n=750)
write.csv(SP2.750,"SP2.750.csv")
SP3.750=sim.sp.net(r=rep(0.3,250),n=750)
write.csv(SP3.750,"SP3.750.csv")
SP4.750=sim.sp.net(r=rep(0.4,250),n=750)
write.csv(SP4.750,"SP4.750.csv")
SP5.750=sim.sp.net(r=rep(0.5,250),n=750)
write.csv(SP5.750,"SP5.750.csv")
SP6.750=sim.sp.net(r=rep(0.6,250),n=750)
write.csv(SP6.750,"SP6.750.csv")
SP7.750=sim.sp.net(r=rep(0.7,250),n=750)
write.csv(SP7.750,"SP7.750.csv")
SP8.750=sim.sp.net(r=rep(0.8,250),n=750)
write.csv(SP8.750,"SP8.750.csv")
SP9.750=sim.sp.net(r=rep(0.9,250),n=750)
write.csv(SP9.750,"SP9.750.csv")
######## Saving all SP graphs 750 nodes data to csv
SP1=read.csv("SP1.750.csv");SP2=read.csv("SP2.750.csv");SP3=read.csv("SP3.750.csv")
SP4=read.csv("SP4.750.csv");SP5=read.csv("SP5.750.csv");SP6=read.csv("SP6.750.csv")
SP7=read.csv("SP7.750.csv");SP8=read.csv("SP8.750.csv");SP9=read.csv("SP9.750.csv")

df.SP.750=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)
write.csv(df.SP.750,"df.SP.750.csv")

### 1000 nodes
SP1.1000=sim.sp.net(r=rep(0.1,250),n=1000)
write.csv(SP1.1000,"SP1.1000.csv")
SP2.1000=sim.sp.net(r=rep(0.2,250),n=1000)
write.csv(SP2.1000,"SP2.1000.csv")
SP3.1000=sim.sp.net(r=rep(0.3,250),n=1000)
write.csv(SP3.1000,"SP3.1000.csv")
SP4.1000=sim.sp.net(r=rep(0.4,250),n=1000)
write.csv(SP4.1000,"SP4.1000.csv")
SP5.1000=sim.sp.net(r=rep(0.5,250),n=1000)
write.csv(SP5.1000,"SP5.1000.csv")
SP6.1000=sim.sp.net(r=rep(0.6,250),n=1000)
write.csv(SP6.1000,"SP6.1000.csv")
SP7.1000=sim.sp.net(r=rep(0.7,250),n=1000)
write.csv(SP7.1000,"SP7.1000.csv")
SP8.1000=sim.sp.net(r=rep(0.8,250),n=1000)
write.csv(SP8.1000,"SP8.1000.csv")

SP9.1000=sim.sp.net(r=rep(0.9,250),n=1000)
write.csv(SP9.1000,"SP9.1000.csv")
# ######## Saving all SP graphs 1000 nodes data to csv
SP1=read.csv("SP1.1000.csv");SP2=read.csv("SP2.1000.csv");SP3=read.csv("SP3.1000.csv")
SP4=read.csv("SP4.1000.csv");SP5=read.csv("SP5.1000.csv");SP6=read.csv("SP6.1000.csv")
SP7=read.csv("SP7.1000.csv");SP8=read.csv("SP8.1000.csv");SP9=read.csv("SP9.1000.csv")

df.SP.1000=rbind(SP1,SP2,SP3,SP4,SP5,SP6,SP7,SP8,SP9)

write.csv(df.SP.1000,"df.SP.1000.csv")
# 
# 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #                                    Erdos Renyi  data
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
MKERGraphs<- function(n=25,p=0.8) {
  Graphs = list()#;G=list() # set up the list of Graphs first
  initgraph= list()
  i= 1
  print("Creating ER Graphs")
  Graphs[[i]] = erdos.renyi.game(n,p,type = c("gnp"),directed = F,loops = F)

  Graphs[[i]]$type = "ER"
  Graphs[[i]]$id = i
  Graphs[[i]]$name="ER"
  i <- i+1
  return(Graphs)
}

sim.erdos.net=function(p=c(0.3,0.4,0.6,0.05),n=100){
net=NULL;data=NULL
net=as.list(mapply(FUN =MKERGraphs,n,p))
data= future.apply::future_lapply(list(net),RunSimOnGraphFeatures,nreps=1,future.seed = 0xBEEF)

df=cbind(data.frame(p),data)
return(df)
}
# # sim.erdos.net=function(p=c(0.3,0.4,0.6,0.05),n=100){
# #   net=NULL;data=NULL
# #   k=1
# #   for (i in 1:length(p)){
# #     net[i]= MKERGraphs(n,p[i])
# #     data=RunSimOnGraphFeatures(net,nreps = 1)
# #     k=k+1
# #   }
# #   df=cbind(p,data)
# #   return(df)
# # }
 plan(multisession, workers = 32)
 set.seed(0xBEEF)

###-----NOTE 2024----####
# Generate 2139 instances/reps instead of 250

# ### 50 nodes 
ER1.50=sim.erdos.net(p=rep(0.1,250),n=50)
write.csv(ER1.50,"ER1.50.csv")
ER2.50=sim.erdos.net(p=rep(0.2,250),n=50)
write.csv(ER2.50,"ER2.50.csv")
ER3.50=sim.erdos.net(p=rep(0.3,250),n=50)
write.csv(ER3.50,"ER3.50.csv")
ER4.50=sim.erdos.net(p=rep(0.4,250),n=50)
write.csv(ER4.50,"ER4.50.csv")
ER5.50=sim.erdos.net(p=rep(0.5,250),n=50)
write.csv(ER5.50,"ER5.50.csv")
ER6.50=sim.erdos.net(p=rep(0.6,250),n=50)
write.csv(ER6.50,"ER6.50.csv")
ER7.50=sim.erdos.net(p=rep(0.7,250),n=50)
write.csv(ER7.50,"ER7.50.csv")
ER8.50=sim.erdos.net(p=rep(0.8,250),n=50)
write.csv(ER8.50,"ER8.50.csv")
ER9.50=sim.erdos.net(p=rep(0.9,250),n=50)
write.csv(ER9.50,"ER9.50.csv")
######## Saving all ER graphs 50 nodes data to csv
ER1=read.csv("ER1.50.csv");ER2=read.csv("ER2.50.csv");ER3=read.csv("ER3.50.csv")
ER4=read.csv("ER4.50.csv");ER5=read.csv("ER5.50.csv");ER6=read.csv("ER6.50.csv")
ER7=read.csv("ER7.50.csv");ER8=read.csv("ER8.50.csv");ER9=read.csv("ER9.50.csv")

df.er.50=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.50,"df.er.50.csv")

### 100 nodes
ER1.100=sim.erdos.net(p=rep(0.1,250),n=100)
write.csv(ER1.100,"ER1.100.csv")
ER2.100=sim.erdos.net(p=rep(0.2,250),n=100)
write.csv(ER2.100,"ER2.100.csv")
ER3.100=sim.erdos.net(p=rep(0.3,250),n=100)
write.csv(ER3.100,"ER3.100.csv")
ER4.100=sim.erdos.net(p=rep(0.4,250),n=100)
write.csv(ER4.100,"ER4.100.csv")
ER5.100=sim.erdos.net(p=rep(0.5,250),n=100)
write.csv(ER5.100,"ER5.100.csv")
ER6.100=sim.erdos.net(p=rep(0.6,250),n=100)
write.csv(ER6.100,"ER6.100.csv")
ER7.100=sim.erdos.net(p=rep(0.7,250),n=100)
write.csv(ER7.100,"ER7.100.csv")
ER8.100=sim.erdos.net(p=rep(0.8,250),n=100)
write.csv(ER8.100,"ER8.100.csv")
ER9.100=sim.erdos.net(p=rep(0.9,250),n=100)
write.csv(ER9.100,"ER9.100.csv")
######## Saving all ER graphs 100 nodes data to csv
ER1=read.csv("ER1.100.csv");ER2=read.csv("ER2.100.csv");ER3=read.csv("ER3.100.csv")
ER4=read.csv("ER4.100.csv");ER5=read.csv("ER5.100.csv");ER6=read.csv("ER6.100.csv")
ER7=read.csv("ER7.100.csv");ER8=read.csv("ER8.100.csv");ER9=read.csv("ER9.100.csv")

df.er.100=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.100,"df.er.100.csv")

### 150 nodes
ER1.150=sim.erdos.net(p=rep(0.1,250),n=150)
write.csv(ER1.150,"ER1.150.csv")
ER2.150=sim.erdos.net(p=rep(0.2,250),n=150)
write.csv(ER2.150,"ER2.150.csv")
ER3.150=sim.erdos.net(p=rep(0.3,250),n=150)
write.csv(ER3.150,"ER3.150.csv")
ER4.150=sim.erdos.net(p=rep(0.4,250),n=150)
write.csv(ER4.150,"ER4.150.csv")
ER5.150=sim.erdos.net(p=rep(0.5,250),n=150)
write.csv(ER5.150,"ER5.150.csv")
ER6.150=sim.erdos.net(p=rep(0.6,250),n=150)
write.csv(ER6.150,"ER6.150.csv")
ER7.150=sim.erdos.net(p=rep(0.7,250),n=150)
write.csv(ER7.150,"ER7.150.csv")
ER8.150=sim.erdos.net(p=rep(0.8,250),n=150)
write.csv(ER8.150,"ER8.150.csv")
ER9.150=sim.erdos.net(p=rep(0.9,250),n=150)
write.csv(ER9.150,"ER9.150.csv")
######## Saving all ER graphs 150 nodes data to csv
ER1=read.csv("ER1.150.csv");ER2=read.csv("ER2.150.csv");ER3=read.csv("ER3.150.csv")
ER4=read.csv("ER4.150.csv");ER5=read.csv("ER5.150.csv");ER6=read.csv("ER6.150.csv")
ER7=read.csv("ER7.150.csv");ER8=read.csv("ER8.150.csv");ER9=read.csv("ER9.150.csv")

df.er.150=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.150,"df.er.150.csv")

### 200 nodes
ER1.200=sim.erdos.net(p=rep(0.1,250),n=200)
write.csv(ER1.200,"ER1.200.csv")
ER2.200=sim.erdos.net(p=rep(0.2,250),n=200)
write.csv(ER2.200,"ER2.200.csv")
ER3.200=sim.erdos.net(p=rep(0.3,250),n=200)
write.csv(ER3.200,"ER3.200.csv")
ER4.200=sim.erdos.net(p=rep(0.4,250),n=200)
write.csv(ER4.200,"ER4.200.csv")
ER5.200=sim.erdos.net(p=rep(0.5,250),n=200)
write.csv(ER5.200,"ER5.200.csv")
ER6.200=sim.erdos.net(p=rep(0.6,250),n=200)
write.csv(ER6.200,"ER6.200.csv")
ER7.200=sim.erdos.net(p=rep(0.7,250),n=200)
write.csv(ER7.200,"ER7.200.csv")
ER8.200=sim.erdos.net(p=rep(0.8,250),n=200)
write.csv(ER8.200,"ER8.200.csv")
ER9.200=sim.erdos.net(p=rep(0.9,250),n=200)
write.csv(ER9.200,"ER9.200.csv")
######## Saving all ER graphs 200 nodes data to csv
ER1=read.csv("ER1.200.csv");ER2=read.csv("ER2.200.csv");ER3=read.csv("ER3.200.csv")
ER4=read.csv("ER4.200.csv");ER5=read.csv("ER5.200.csv");ER6=read.csv("ER6.200.csv")
ER7=read.csv("ER7.200.csv");ER8=read.csv("ER8.200.csv");ER9=read.csv("ER9.200.csv")

df.er.200=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.200,"df.er.200.csv")

### 250 nodes
ER1.250=sim.erdos.net(p=rep(0.1,250),n=250)
write.csv(ER1.250,"ER1.250.csv")
ER2.250=sim.erdos.net(p=rep(0.2,250),n=250)
write.csv(ER2.250,"ER2.250.csv")
ER3.250=sim.erdos.net(p=rep(0.3,250),n=250)
write.csv(ER3.250,"ER3.250.csv")
ER4.250=sim.erdos.net(p=rep(0.4,250),n=250)
write.csv(ER4.250,"ER4.250.csv")
ER5.250=sim.erdos.net(p=rep(0.5,250),n=250)
write.csv(ER5.250,"ER5.250.csv")
ER6.250=sim.erdos.net(p=rep(0.6,250),n=250)
write.csv(ER6.250,"ER6.250.csv")
ER7.250=sim.erdos.net(p=rep(0.7,250),n=250)
write.csv(ER7.250,"ER7.250.csv")
ER8.250=sim.erdos.net(p=rep(0.8,250),n=250)
write.csv(ER8.250,"ER8.250.csv")
ER9.250=sim.erdos.net(p=rep(0.9,250),n=250)
write.csv(ER9.250,"ER9.250.csv")
######## Saving all ER graphs 250 nodes data to csv
ER1=read.csv("ER1.250.csv");ER2=read.csv("ER2.250.csv");ER3=read.csv("ER3.250.csv")
ER4=read.csv("ER4.250.csv");ER5=read.csv("ER5.250.csv");ER6=read.csv("ER6.250.csv")
ER7=read.csv("ER7.250.csv");ER8=read.csv("ER8.250.csv");ER9=read.csv("ER9.250.csv")

df.er.250=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.250,"df.er.250.csv")

### 300 nodes
ER1.300=sim.erdos.net(p=rep(0.1,250),n=300)
write.csv(ER1.300,"ER1.300.csv")
ER2.300=sim.erdos.net(p=rep(0.2,250),n=300)
write.csv(ER2.300,"ER2.300.csv")
ER3.300=sim.erdos.net(p=rep(0.3,250),n=300)
write.csv(ER3.300,"ER3.300.csv")
ER4.300=sim.erdos.net(p=rep(0.4,250),n=300)
write.csv(ER4.300,"ER4.300.csv")
ER5.300=sim.erdos.net(p=rep(0.5,250),n=300)
write.csv(ER5.300,"ER5.300.csv")
ER6.300=sim.erdos.net(p=rep(0.6,250),n=300)
write.csv(ER6.300,"ER6.300.csv")
ER7.300=sim.erdos.net(p=rep(0.7,250),n=300)
write.csv(ER7.300,"ER7.300.csv")
ER8.300=sim.erdos.net(p=rep(0.8,250),n=300)
write.csv(ER8.300,"ER8.300.csv")
ER9.300=sim.erdos.net(p=rep(0.9,250),n=300)
write.csv(ER9.300,"ER9.300.csv")
######## Saving all ER graphs 300 nodes data to csv
ER1=read.csv("ER1.300.csv");ER2=read.csv("ER2.300.csv");ER3=read.csv("ER3.300.csv")
ER4=read.csv("ER4.300.csv");ER5=read.csv("ER5.300.csv");ER6=read.csv("ER6.300.csv")
ER7=read.csv("ER7.300.csv");ER8=read.csv("ER8.300.csv");ER9=read.csv("ER9.300.csv")

df.er.300=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.300,"df.er.300.csv")

### 350 nodes
ER1.350=sim.erdos.net(p=rep(0.1,250),n=350)
write.csv(ER1.350,"ER1.350.csv")
ER2.350=sim.erdos.net(p=rep(0.2,250),n=350)
write.csv(ER2.350,"ER2.350.csv")
ER3.350=sim.erdos.net(p=rep(0.3,250),n=350)
write.csv(ER3.350,"ER3.350.csv")
ER4.350=sim.erdos.net(p=rep(0.4,250),n=350)
write.csv(ER4.350,"ER4.350.csv")
ER5.350=sim.erdos.net(p=rep(0.5,250),n=350)
write.csv(ER5.350,"ER5.350.csv")
ER6.350=sim.erdos.net(p=rep(0.6,250),n=350)
write.csv(ER6.350,"ER6.350.csv")
ER7.350=sim.erdos.net(p=rep(0.7,250),n=350)
write.csv(ER7.350,"ER7.350.csv")
ER8.350=sim.erdos.net(p=rep(0.8,250),n=350)
write.csv(ER8.350,"ER8.350.csv")
ER9.350=sim.erdos.net(p=rep(0.9,250),n=350)
write.csv(ER9.350,"ER9.350.csv")
######## Saving all ER graphs 350 nodes data to csv
ER1=read.csv("ER1.350.csv");ER2=read.csv("ER2.350.csv");ER3=read.csv("ER3.350.csv")
ER4=read.csv("ER4.350.csv");ER5=read.csv("ER5.350.csv");ER6=read.csv("ER6.350.csv")
ER7=read.csv("ER7.350.csv");ER8=read.csv("ER8.350.csv");ER9=read.csv("ER9.350.csv")

df.er.350=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.350,"df.er.350.csv")

### 400 nodes
ER1.400=sim.erdos.net(p=rep(0.1,250),n=400)
write.csv(ER1.400,"ER1.400.csv")
ER2.400=sim.erdos.net(p=rep(0.2,250),n=400)
write.csv(ER2.400,"ER2.400.csv")
ER3.400=sim.erdos.net(p=rep(0.3,250),n=400)
write.csv(ER3.400,"ER3.400.csv")
ER4.400=sim.erdos.net(p=rep(0.4,250),n=400)
write.csv(ER4.400,"ER4.400.csv")
ER5.400=sim.erdos.net(p=rep(0.5,250),n=400)
write.csv(ER5.400,"ER5.400.csv")
ER6.400=sim.erdos.net(p=rep(0.6,250),n=400)
write.csv(ER6.400,"ER6.400.csv")
ER7.400=sim.erdos.net(p=rep(0.7,250),n=400)
write.csv(ER7.400,"ER7.400.csv")
ER8.400=sim.erdos.net(p=rep(0.8,250),n=400)
write.csv(ER8.400,"ER8.400.csv")
ER9.400=sim.erdos.net(p=rep(0.9,250),n=400)
write.csv(ER9.400,"ER9.400.csv")
######## Saving all ER graphs 400 nodes data to csv
ER1=read.csv("ER1.400.csv");ER2=read.csv("ER2.400.csv");ER3=read.csv("ER3.400.csv")
ER4=read.csv("ER4.400.csv");ER5=read.csv("ER5.400.csv");ER6=read.csv("ER6.400.csv")
ER7=read.csv("ER7.400.csv");ER8=read.csv("ER8.400.csv");ER9=read.csv("ER9.400.csv")

df.er.400=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.400,"df.er.400.csv")

### 500 nodes
ER1.500=sim.erdos.net(p=rep(0.1,250),n=500)
write.csv(ER1.500,"ER1.500.csv")
ER2.500=sim.erdos.net(p=rep(0.2,250),n=500)
write.csv(ER2.500,"ER2.500.csv")
ER3.500=sim.erdos.net(p=rep(0.3,250),n=500)
write.csv(ER3.500,"ER3.500.csv")
ER4.500=sim.erdos.net(p=rep(0.4,250),n=500)
write.csv(ER4.500,"ER4.500.csv")
ER5.500=sim.erdos.net(p=rep(0.5,250),n=500)
write.csv(ER5.500,"ER5.500.csv")
ER6.500=sim.erdos.net(p=rep(0.6,250),n=500)
write.csv(ER6.500,"ER6.500.csv")
ER7.500=sim.erdos.net(p=rep(0.7,250),n=500)
write.csv(ER7.500,"ER7.500.csv")
ER8.500=sim.erdos.net(p=rep(0.8,250),n=500)
write.csv(ER8.500,"ER8.500.csv")
ER9.500=sim.erdos.net(p=rep(0.9,250),n=500)
write.csv(ER9.500,"ER9.500.csv")
######## Saving all ER graphs 500 nodes data to csv
ER1=read.csv("ER1.500.csv");ER2=read.csv("ER2.500.csv");ER3=read.csv("ER3.500.csv")
ER4=read.csv("ER4.500.csv");ER5=read.csv("ER5.500.csv");ER6=read.csv("ER6.500.csv")
ER7=read.csv("ER7.500.csv");ER8=read.csv("ER8.500.csv");ER9=read.csv("ER9.500.csv")

df.er.500=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.500,"df.er.500.csv")

### 750 nodes
ER1.750=sim.erdos.net(p=rep(0.1,250),n=750)
write.csv(ER1.750,"ER1.750.csv")
ER2.750=sim.erdos.net(p=rep(0.2,250),n=750)
write.csv(ER2.750,"ER2.750.csv")
ER3.750=sim.erdos.net(p=rep(0.3,250),n=750)
write.csv(ER3.750,"ER3.750.csv")
ER4.750=sim.erdos.net(p=rep(0.4,250),n=750)
write.csv(ER4.750,"ER4.750.csv")
ER5.750=sim.erdos.net(p=rep(0.5,250),n=750)
write.csv(ER5.750,"ER5.750.csv")
ER6.750=sim.erdos.net(p=rep(0.6,250),n=750)
write.csv(ER6.750,"ER6.750.csv")
ER7.750=sim.erdos.net(p=rep(0.7,250),n=750)
write.csv(ER7.750,"ER7.750.csv")
ER8.750=sim.erdos.net(p=rep(0.8,250),n=750)
write.csv(ER8.750,"ER8.750.csv")
ER9.750=sim.erdos.net(p=rep(0.9,250),n=750)
write.csv(ER9.750,"ER9.750.csv")
######## Saving all ER graphs 750 nodes data to csv
ER1=read.csv("ER1.750.csv");ER2=read.csv("ER2.750.csv");ER3=read.csv("ER3.750.csv")
ER4=read.csv("ER4.750.csv");ER5=read.csv("ER5.750.csv");ER6=read.csv("ER6.750.csv")
ER7=read.csv("ER7.750.csv");ER8=read.csv("ER8.750.csv");ER9=read.csv("ER9.750.csv")

df.er.750=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.750,"df.er.750.csv")

### 1000 nodes
ER1.1000=sim.erdos.net(p=rep(0.1,250),n=1000)
write.csv(ER1.1000,"ER1.1000.csv")
ER2.1000=sim.erdos.net(p=rep(0.2,250),n=1000)
write.csv(ER2.1000,"ER2.1000.csv")
ER3.1000=sim.erdos.net(p=rep(0.3,250),n=1000)
write.csv(ER3.1000,"ER3.1000.csv")
ER4.1000=sim.erdos.net(p=rep(0.4,250),n=1000)
write.csv(ER4.1000,"ER4.1000.csv")
ER5.1000=sim.erdos.net(p=rep(0.5,250),n=1000)
write.csv(ER5.1000,"ER5.1000.csv")
ER6.1000=sim.erdos.net(p=rep(0.6,250),n=1000)
write.csv(ER6.1000,"ER6.1000.csv")
ER7.1000=sim.erdos.net(p=rep(0.7,250),n=1000)
write.csv(ER7.1000,"ER7.1000.csv")
ER8.1000=sim.erdos.net(p=rep(0.8,250),n=1000)
write.csv(ER8.1000,"ER8.1000.csv")
ER9.1000=sim.erdos.net(p=rep(0.9,250),n=1000)
write.csv(ER9.1000,"ER9.1000.csv")
######## Saving all ER graphs 1000 nodes data to csv
ER1=read.csv("ER1.1000.csv");ER2=read.csv("ER2.1000.csv");ER3=read.csv("ER3.1000.csv")
ER4=read.csv("ER4.1000.csv");ER5=read.csv("ER5.1000.csv");ER6=read.csv("ER6.1000.csv")
ER7=read.csv("ER7.1000.csv");ER8=read.csv("ER8.1000.csv");ER9=read.csv("ER9.1000.csv")

df.er.1000=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.1000,"df.er.1000.csv")

### 1500 nodes
ER1.1500=sim.erdos.net(p=rep(0.1,250),n=1500)
write.csv(ER1.1500,"ER1.1500.csv")
ER2.1500=sim.erdos.net(p=rep(0.2,250),n=1500)
write.csv(ER2.1500,"ER2.1500.csv")
ER3.1500=sim.erdos.net(p=rep(0.3,250),n=1500)
write.csv(ER3.1500,"ER3.1500.csv")
ER4.1500=sim.erdos.net(p=rep(0.4,250),n=1500)
write.csv(ER4.1500,"ER4.1500.csv")
ER5.1500=sim.erdos.net(p=rep(0.5,250),n=1500)
write.csv(ER5.1500,"ER5.1500.csv")
ER6.1500=sim.erdos.net(p=rep(0.6,250),n=1500)
write.csv(ER6.1500,"ER6.1500.csv")
ER7.1500=sim.erdos.net(p=rep(0.7,250),n=1500)
write.csv(ER7.1500,"ER7.1500.csv")
ER8.1500=sim.erdos.net(p=rep(0.8,250),n=1500)
write.csv(ER8.1500,"ER8.1500.csv")
ER9.1500=sim.erdos.net(p=rep(0.9,250),n=1500)
write.csv(ER9.1500,"ER9.1500.csv")
######## Saving all ER graphs 1500 nodes data to csv
ER1=read.csv("ER1.1500.csv");ER2=read.csv("ER2.1500.csv");ER3=read.csv("ER3.1500.csv")
ER4=read.csv("ER4.1500.csv");ER5=read.csv("ER5.1500.csv");ER6=read.csv("ER6.1500.csv")
ER7=read.csv("ER7.1500.csv");ER8=read.csv("ER8.1500.csv");ER9=read.csv("ER9.1500.csv")

df.er.1500=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.1500,"df.er.1500.csv")

### 2000 nodes
ER1.2000=sim.erdos.net(p=rep(0.1,250),n=2000)
write.csv(ER1.2000,"ER1.2000.csv")
ER2.2000=sim.erdos.net(p=rep(0.2,250),n=2000)
write.csv(ER2.2000,"ER2.2000.csv")
ER3.2000=sim.erdos.net(p=rep(0.3,250),n=2000)
write.csv(ER3.2000,"ER3.2000.csv")
ER4.2000=sim.erdos.net(p=rep(0.4,250),n=2000)
write.csv(ER4.2000,"ER4.2000.csv")
ER5.2000=sim.erdos.net(p=rep(0.5,250),n=2000)
write.csv(ER5.2000,"ER5.2000.csv")
ER6.2000=sim.erdos.net(p=rep(0.6,250),n=2000)
write.csv(ER6.2000,"ER6.2000.csv")
ER7.2000=sim.erdos.net(p=rep(0.7,250),n=2000)
write.csv(ER7.2000,"ER7.2000.csv")
ER8.2000=sim.erdos.net(p=rep(0.8,250),n=2000)
write.csv(ER8.2000,"ER8.2000.csv")
ER9.2000=sim.erdos.net(p=rep(0.9,250),n=2000)
write.csv(ER9.2000,"ER9.2000.csv")
######## Saving all ER graphs 2000 nodes data to csv
ER1=read.csv("ER1.2000.csv");ER2=read.csv("ER2.2000.csv");ER3=read.csv("ER3.2000.csv")
ER4=read.csv("ER4.2000.csv");ER5=read.csv("ER5.2000.csv");ER6=read.csv("ER6.2000.csv")
ER7=read.csv("ER7.2000.csv");ER8=read.csv("ER8.2000.csv");ER9=read.csv("ER9.2000.csv")

df.er.2000=rbind(ER1,ER2,ER3,ER4,ER5,ER6,ER7,ER8,ER9)
write.csv(df.er.2000,"df.er.2000.csv")
# 
# 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #                                    Scale free  data
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
MKSFGraphs<- function(n=50, power=2, m=4,i) {
  Graphs = list()#;G=list() # set up the list of Graphs first
  initgraph= list()
  i= 1
  print("Creating SF Graphs")
  Graphs[[i]] = sample_pa(n=n, power=power, m=m, directed=FALSE, algorithm="psumtree")

  Graphs[[i]]$type = "SF"
  Graphs[[i]]$id = i
  Graphs[[i]]$name="SF"
  i <- i+1
  return(Graphs)
}
# 
# 
# # ParalleEpicSimOnGraphs<-function(Graphs, betaVals=betaVals,gammaVals=gammaVals, nreps=nreps,output_file="EpicSimOnGraphs.csv",report="s",nticks=10) {
# #   #  p = progressor(along=Graphs)
# #   pathogn=expand.grid(data.frame(betaVals,gammaVals))
# #   
# #   for (m in 1:nrow(pathogn)){## Counter to account for the cartesian product of all beta and gamma values
# #     results[[m]] = future_lapply(seq_along(Graphs), function(j) { 
# #       # Sys.sleep(6.0-j)
# #       #  p(sprintf("j=%g", j))
# #       data.frame(Run_EpicSim_And_Measures(Graphs[j], beta=pathogn$betaVals[m],gamma=pathogn$gammaVals[m],
# #                                           nticks=nticks,report=report,nreps = nreps))} 
# #       , future.seed = 0xBEEF, future.chunk.size=1)
# #     
# #   }}
# 
# 
sim.sf.net=function(power=powerVals,m=mVals,n=50,nreps=1){
  net=NULL;data=NULL
  dt=expand.grid(n,powerVals,mVals)
  colnames(dt)=c("nodes","powerVals","neiVals")
  #dt.split=setNames(split(dt, seq(nrow(dt))), rownames(dtname))
  net=as.list(mapply(FUN =MKSFGraphs,n=dt$nodes,power=dt$prewireVals,m=dt$neiVals,i=2))
  #k=1
 # start <- Sys.time()
  #net[i]= MKSFGraphs(n,power=dt$powerVals[i],m=dt$mVals[i])
  data= future.apply::future_lapply(list(net),RunSimOnGraphFeatures,nreps,future.seed = 0xBEEF)
  #k=k+1

  df=cbind(dt,data)

  return(df)
}
# 
# plan(multisession, workers = 6)
# set.seed(0xBEEF)
# #powerVals=1:14
# #mVals=5:15
# #x=sim.sf.net(power=powerVals,m=mVals,nreps = 50,n=50)
# powerVals=1:5
# mVals=1:50

# ### 50 nodes 
SF1.50=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=50)
write.csv(SF1.50,"SF1.50.csv")

### 100 nodes
SF1.100=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=100)
write.csv(SF1.100,"SF1.100.csv")

### 150 nodes
SF1.150=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=150)
write.csv(SF1.150,"SF1.150.csv")

### 200 nodes
SF1.200=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=200)
write.csv(SF1.200,"SF1.200.csv")

### 250 nodes
SF1.250=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=250)
write.csv(SF1.250,"SF1.250.csv")

### 300 nodes
SF1.300=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=300)
write.csv(SF1.300,"SF1.300.csv")

### 350 nodes
SF1.350=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=350)
write.csv(SF1.350,"SF1.350.csv")

### 400 nodes
SF1.400=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=400)
write.csv(SF1.400,"SF1.400.csv")

### 500 nodes
SF1.500=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=500)
write.csv(SF1.500,"SF1.500.csv")

### 750 nodes
SF1.750=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=750)
write.csv(SF1.750,"SF1.750.csv")

### 1000 nodes
SF1.1000=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=1000)
write.csv(SF1.1000,"SF1.1000.csv")

### 1500 nodes
SF1.1500=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=1500)
write.csv(SF1.1500,"SF1.1500.csv")

### 2000 nodes
SF1.2000=sim.sf.net(power=powerVals,m=mVals,nreps = 1200,n=2000)
write.csv(SF1.2000,"SF1.2000.csv")

######## Saving all SF graphs to csv
SF.50=read.csv("SF1.50.csv");SF.100=read.csv("SF1.100.csv");
SF.150=read.csv("SF1.150.csv")
SF.200=read.csv("SF1.100.csv");
SF.250=read.csv("SF1.250.csv");SF.300=read.csv("SF1.300.csv")
SF.350=read.csv("SF1.350.csv");SF.400=read.csv("SF1.400.csv")
SF.500=read.csv("SF1.500.csv");SF.750=read.csv("SF1.750.csv")
SF.1000=read.csv("SF1.1000.csv");SF.1500=read.csv("SF1.1500.csv")
SF.2000=read.csv("SF1.2000.csv")
df.sf=rbind(SF.50,SF.150,SF.200,SF.250,SF.300,SF.350,
            SF.400, SF.500,SF.750,SF.1000,SF.1500,
            SF.2000)

write.csv(df.sf,"df.sf.csv")
# 
# 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #                                    Small world  data
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
MKSWGraphs<- function(n=50,nei=6,p=0.2) {
  Graphs = list()#;G=list() # set up the list of Graphs first
  initgraph= list()
  i= 1
  print("Creating SW Graphs")
  Graphs[[i]] = sample_smallworld(dim=1, size=n, nei=nei, p=p)

  Graphs[[i]]$type = "SW"
  Graphs[[i]]$id = "1"
  Graphs[[i]]$name="SW"
  i <- i+1
  return(Graphs)
}


sim.sw.net=function(p=pVals,nei=neiVals,n=50,nreps=1){
  net=NULL;data=NULL
  dt=expand.grid(n,pVals,neiVals)
  colnames(dt)=c("nodes","prewireVals","neiVals")
  net=as.list(mapply(FUN =MKSWGraphs,n=dt$nodes,p=dt$prewireVals,nei=dt$neiVals))
  data= future.apply::future_lapply(list(net),RunSimOnGraphFeatures,nreps,future.seed = 0xBEEF)

  df=cbind(dt,data)
  return(df)
}
 
#plan(multisession, workers = 32)
set.seed(0xBEEF)

pVals=c(0.01,0.03,0.05,0.07,0.09,0.1,0.2,0.3,0.4)
neiVals=1:20
 
### 50 nodes
SW.50=sim.sw.net(p=pVals,nei=neiVals,n=50,nreps=1200)
write.csv(SW.50,"SW.50.csv")

### 100 nodes
SW.100=sim.sw.net(p=pVals,nei=neiVals,n=100,nreps=1200)
write.csv(SW.100,"SW.100.csv")

### 150 nodes
SW.150=sim.sw.net(p=pVals,nei=neiVals,n=150,nreps=1200)
write.csv(SW.150,"SW.150.csv")

### 200 nodes
SW.200=sim.sw.net(p=pVals,nei=neiVals,n=200,nreps=1200)
write.csv(SW.200,"SW.200.csv")

### 250 nodes
SW.250=sim.sw.net(p=pVals,nei=neiVals,n=250,nreps=1200)
write.csv(SW.250,"SW.250.csv")

### 300 nodes
SW.300=sim.sw.net(p=pVals,nei=neiVals,n=300,nreps=1200)
write.csv(SW.300,"SW.300.csv")

### 350 nodes
SW.350=sim.sw.net(p=pVals,nei=neiVals,n=350,nreps=1200)
write.csv(SW.350,"SW.350.csv")

### 400 nodes
SW.400=sim.sw.net(p=pVals,nei=neiVals,n=400,nreps=1200)
write.csv(SW.400,"SW.400.csv")

### 500 nodes
SW.500=sim.sw.net(p=pVals,nei=neiVals,n=500,nreps=1200)
write.csv(SW.500,"SW.500.csv")
# 
 ### 750 nodes 
 SW.750=sim.sw.net(p=pVals,nei=neiVals,n=750,nreps=1200)
 write.csv(SW.750,"SW.750.csv")
# 
# ### 1000 nodes 
 SW.1000=sim.sw.net(p=pVals,nei=neiVals,n=1000,nreps=1200)
 write.csv(SW.1000,"SW.1000.csv")

# 
# ### 1500 nodes 
# SW.1500=sim.sw.net(p=pVals,nei=neiVals,n=1500,nreps=1200)
# write.csv(SW.1500,"SW.1500.csv")
# 
# ### 2000 nodes 
# SW.2000=sim.sw.net(p=pVals,nei=neiVals,n=2000,nreps=1200)
# write.csv(SW.2000,"SW.2000.csv")
# 
# ######## Saving all SW graphs to csv
# SW.50=read.csv("SW.50.csv");SW.100=read.csv("SW.100.csv");
# SW.150=read.csv("SW.150.csv")
# SW.200=read.csv("SW.100.csv");
# SW.250=read.csv("SW.250.csv");SW.300=read.csv("SW.300.csv")
# SW.350=read.csv("SW.350.csv");SW.400=read.csv("SW.400.csv")
# SW.500=read.csv("SW.500.csv");SW.750=read.csv("SW.750.csv")
# SW.1000=read.csv("SW.1000.csv");SW.1500=read.csv("SW.1500.csv")
# SW.2000=read.csv("SW.2000.csv")
# 
# df.sw=rbind(SW.50,SW.150,SW.200,SW.250,SW.300,SW.350,
#             SW.400, SW.500,SW.750,SW.1000,SW.1500,
#             SW.2000)
# 
# write.csv(df.sw,"df.sw.csv")

 # #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 # #+ Stochastic block model
 # #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
 # #plot(g)
 
 MKSBMGraphs<- function(pm,n) {
   Graphs = list()#;G=list() # set up the list of Graphs first
   initgraph= list()
   i= 1
   print("Creating Stochastic block matrix")
   Graphs[[i]] = sbm_net(pm,n)
   Graphs[[i]]$type = "sbm"
   Graphs[[i]]$id = "1"
   Graphs[[i]]$name="sbm"
   i <- i+1
   return(Graphs)
 }
 
 # 
 sim.sbm.net=function(pm,n=50){
   net= lapply(pm, MKSBMGraphs,n)
   data=lapply(net,RunSimOnGraphFeatures,nreps=1)
   df <-  as.data.frame(do.call(rbind, data))
   return(df)
 }
 # 
 ## Two groups with not only few connection between groups
 #pm <- cbind(c(.05, .002), c(.002, .05))
 sbm_net <- function(pm,n){
   sbm.graph=sample_sbm(n, pref.matrix = pm, block.sizes = c(0.4*n, 0.6*n))
 }
 
 sbmfunc=function(ncomm=3){
   p_within <- runif(ncomm, 0.1, 0.12)#runif(ncomm, 0.5, 0.9)#
   p_between <- runif(ncomm, 0.007, 0.01)#runif(ncomm, 0.1, 0.4)#
   mat=matrix(0, nrow = ncomm, ncol = ncomm)
   diag(mat) <- p_within # populate diagonal with within-community probabilities
   mat[lower.tri(mat)] <- runif(1, min = min(p_between), max = max(p_between)) # populate lower triangle with between-community probabilities
   mat<- mat + t(mat) - diag(diag(mat)) # make matrix symmetric
   return(mat)
 }
 sbmfunc(ncomm=2)
 
 pm=sbmfunc(ncomm=2)
 
 
 ###-----NOTE 2024----####
 # Generate 19250 instances instead of 1500
 
 pm=list()
 for (i in 1:1500){
   pm[[i]]=sbmfunc(ncomm=2)
 }
 
 
 ### 50 nodes
 sbm.50=sim.sbm.net(pm,n=50)
 
 write.csv(sbm.50,"sbm.50.csv")
 
 ### 100 nodes
 sbm.100=sim.sbm.net(pm,n=100)
 write.csv(sbm.100,"sbm.100.csv")
 
 ### 150 nodes
 sbm.150=sim.sbm.net(pm,n=150)
 write.csv(sbm.150,"sbm.150.csv")
 
 ### 200 nodes
 sbm.200=sim.sbm.net(pm,n=200)
 write.csv(sbm.200,"sbm.200.csv")
 
 ### 250 nodes
 sbm.250=sim.sbm.net(pm,n=250)
 write.csv(sbm.250,"sbm.250.csv")
 
 ### 300 nodes
 sbm.300=sim.sbm.net(pm,n=300)
 write.csv(sbm.300,"sbm.300.csv")
 
 ### 350 nodes
 sbm.350=sim.sbm.net(pm,n=350)
 write.csv(sbm.350,"sbm.350.csv")
 
 ### 400 nodes
 sbm.400=sim.sbm.net(pm,n=400)
 write.csv(sbm.400,"sbm.400.csv")
 
 ### 500 nodes
 sbm.500=sim.sbm.net(pm,n=500)
 write.csv(sbm.500,"sbm.500.csv")
 
 ### 750 nodes
 sbm.750=sim.sbm.net(pm,n=750)
 write.csv(sbm.750,"sbm.750.csv")
 
 ### 1000 nodes
 sbm.1000=sim.sbm.net(pm,n=1000)
 write.csv(sbm.1000,"sbm.1000.csv")
 
 # ### 1500 nodes
 # sbm.1500=sim.sbm.net(pm,n=1500)
 # write.csv(sbm.1500,"sbm.1500.csv")
 # 
 # ### 2000 nodes
 # sbm.2000=sim.sbm.net(pm,n=2000)
 # write.csv(sbm.2000,"sbm.2000.csv")
 
 ######## Saving all sbm gradhs to csv
 sbm.50=read.csv("sbm.50.csv");sbm.100=read.csv("sbm.100.csv");
 sbm.150=read.csv("sbm.150.csv")
 sbm.200=read.csv("sbm.100.csv");
 sbm.250=read.csv("sbm.250.csv");sbm.300=read.csv("sbm.300.csv")
 sbm.350=read.csv("sbm.350.csv");sbm.400=read.csv("sbm.400.csv")
 sbm.500=read.csv("sbm.500.csv");sbm.750=read.csv("sbm.750.csv")
 sbm.1000=read.csv("sbm.1000.csv")
 # sbm.1500=read.csv("sbm.1500.csv")
 # sbm.2000=read.csv("sbm.2000.csv")
 
 df.sbm=rbind(sbm.50,sbm.100,sbm.150,sbm.200,sbm.250,sbm.300,sbm.350,
              sbm.400, sbm.500,sbm.750,sbm.1000)
 
 write.csv(df.sbm,"df.sbm.csv")
 
 # library(Matrix)
 # library(stats)
 # #library(spatstat)
 # library(igraph)
 # library(tidymodels) 

 
  
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+ Lattice graph
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #Function to generate a single lattice graph
# #make_lattice(length = ceiling(sqrt(order)),dim=dim.lat,nei = nei.lat)
# lattice_net<-function(n,nei){
#   lattice_graph=make_lattice(length=floor(sqrt(n)), dim = 2,nei = nei,directed= F, mutual= T, circular = F)
# }
# 
# #Function for Replicating the lattice network N times
# 
# # lattice_graph=list()
# # Lattice_net<-function(d,n,nei,N){
# #   for (i in 1:N){
# #     lattice_graph[[i]]=lattice_net(d,n,nei)
# #   }
# #   return(lattice_graph)
# # }
# 
# ##---Test case----
# #f3=Lattice(2,5)
# #plot(f3)
# 
# MKLATGraphs<- function(n=50,nei=3) {
#   Graphs = list()#;G=list() # set up the list of Graphs first
#   initgraph= list()
#   i= 1
#   print("Creating LAT Graphs they are all the same")
#   Graphs[[i]] = lattice_net(n,nei)
#   Graphs[[i]]$type = "LAT"
#   Graphs[[i]]$id = "1"
#   Graphs[[i]]$name="LAT"
#   i <- i+1
#   return(Graphs)
# }
# 
# 
# sim.lat.net=function(n=30,nei=c(1,2,4)){
#   net=NULL;data=NULL
#   for (i in 1:length(nei)){
#     net[i]= MKLATGraphs(n,nei=nei[i])
#     data=RunSimOnGraphFeatures(net,nreps=1)
#   }
#   df=cbind(nei,data)  
#   return(df)
# }
# 
# nei=1:20
# ### 50 nodes 
# lat.50=sim.lat.net(nei,n=50)
# 
# write.csv(lat.50,"lat.50.csv")
# 
# ### 100 nodes 
# lat.100=sim.lat.net(nei,n=100)
# 
# write.csv(lat.100,"lat.100.csv")
# 
# ### 150 nodes 
# lat.150=sim.lat.net(nei,n=150)
# write.csv(lat.150,"lat.150.csv")
# 
# ### 200 nodes 
# lat.200=sim.lat.net(nei,n=200)
# write.csv(lat.200,"lat.200.csv")
# 
# ### 250 nodes 
# lat.250=sim.lat.net(nei,n=250)
# write.csv(lat.250,"lat.250.csv")
# 
# ### 300 nodes 
# lat.300=sim.lat.net(nei,n=300)
# write.csv(lat.300,"lat.300.csv")
# 
# ### 350 nodes 
# lat.350=sim.lat.net(nei,n=350)
# write.csv(lat.350,"lat.350.csv")
# 
# ### 400 nodes 
# lat.400=sim.lat.net(nei,n=400)
# write.csv(lat.400,"lat.400.csv")
# 
# ### 500 nodes 
# lat.500=sim.lat.net(nei,n=500)
# write.csv(lat.500,"lat.500.csv")
# 
# ### 750 nodes 
# lat.750=sim.lat.net(nei,n=750)
# write.csv(lat.750,"lat.750.csv")
# 
# ### 1000 nodes 
# lat.1000=sim.lat.net(nei,n=1000)
# write.csv(lat.1000,"lat.1000.csv")
# 
# ### 1500 nodes 
# lat.1500=sim.lat.net(nei,n=1500)
# write.csv(lat.1500,"lat.1500.csv")
# 
# ### 2000 nodes 
# lat.2000=sim.lat.net(nei,n=2000)
# write.csv(lat.2000,"lat.2000.csv")
# 
# ######## Saving all lat gradhs to csv
# lat.50=read.csv("lat.50.csv");lat.100=read.csv("lat.100.csv");
# lat.150=read.csv("lat.150.csv")
# lat.200=read.csv("lat.100.csv");
# lat.250=read.csv("lat.250.csv");lat.300=read.csv("lat.300.csv")
# lat.350=read.csv("lat.350.csv");lat.400=read.csv("lat.400.csv")
# lat.500=read.csv("lat.500.csv");lat.750=read.csv("lat.750.csv")
# lat.1000=read.csv("lat.1000.csv");lat.1500=read.csv("lat.1500.csv")
# lat.2000=read.csv("lat.2000.csv")
# 
# df.lat=rbind(lat.50,lat.150,lat.200,lat.250,lat.300,lat.350,
#              lat.400, lat.500,lat.750,lat.1000,lat.1500,
#              lat.2000)
# 
# write.csv(df.lat,"df.lat.csv")
# 

# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+ Data generation for the machine learning model
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ###---Simulate--spatial--networks---##
# # simulate.spatial<-function(N=50,radius=0.4,nsim=100){
# #   spatial.graph=NULL
# #   for (i in 1:nsim){
# #     spatial.graph[[i]]=makeSpatialGraphs(node.size=N,Radius=radius)
# #   }
# #   return(spatial.graph)
# # }
# 
# ##########################################################################################################
# # Input an igraph object from a file, treating it as a static graph
# ##########################################################################################################
# getGraphFromFile <- function(file, simplify=TRUE, useBiggestComponent=TRUE, asUndirected=TRUE) {
#   
#   dat <- read.table(file) # just read static graph: ignoring third column
#   G <- graph_from_data_frame(dat)
#   if (asUndirected==TRUE) {
#     G <- as.undirected(G, "collapse")
#   }
#   
#   #  g_names <- gsub(".edges","",networks[i]) # one edge for each pair of connect vertices (not sure what this is for)
#   
#   if (useBiggestComponent==TRUE) {
#     netclust <- components(G) #look for subgraphs
#     gcc <- V(G)[netclust$membership == which.max(netclust$csize)]#select vertices from the largest sub-graph
#     G <- induced.subgraph(G, gcc) #make it a igraph object.
#   }
#   if (simplify==TRUE) {
#     G <- igraph::simplify(G, remove.multiple = TRUE, remove.loops = TRUE)
#   }
#   return(G)
# }
# 
###--Normalized Laplacina function--##
normalized_laplacian=function(Graphs){
  laplacian_matrix(Graphs,normalized = T)
}
# 
# 
# 
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

  # Stuff that is simple to apply and needs no interim components:

  df$order = base::as.numeric(lapply(Graphs, gorder))
  df$edges = base::as.numeric(lapply(Graphs, gsize))
  df$connected = base::as.numeric(lapply(Graphs, is_connected))
  df$minCut = base::as.numeric(lapply(Graphs, min_cut))
  df$diameter = base::as.numeric(lapply(Graphs, diameter))
  df$transitivity = base::as.numeric(lapply(Graphs, transitivity))

  # stuff that needs interim things:
  degrees = lapply(Graphs,igraph::degree )
  df$minDegree = base::as.numeric(lapply(degrees, min))
  df$maxDegree = base::as.numeric(lapply(degrees, max))
  df$mean_degree = base::as.numeric(lapply(degrees, mean))

  # stuff that lapply doesn't like so has to be done in a loop:
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
    # df$deg_centr[i] <- centr_degree(Graphs[[i]])$centralization

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
    } else { # handle the case where G isn't connected
      df$closeness_centr[i] = -1
    }
  }
  return (df)
}

# #calcGraphFeatures(x)
# ##----------- Graph Features----------#####
# ## Used to perform runs on multiple simulated graphs on any given network
# RunSimOnGraphFeatures<-function(Graphs, nreps=nreps,output_file=NULL, seed=-1) {
#   set.seed(1)
#   # ### Definition and initialization of parameters for graphfeatures
#   graphProperties=list()
#   # ### Definition and initialization of Graph Prefix
#   graphid=list();graphreplicate=list(); graphname=list(); GraphPrefix=list(); analysis=list()
#   for (g in 1:length(Graphs)){
#     for (reps in 1:nreps) {
#       ### Calculate the graph features for each simulated graph of all the synthetic networks
#       print(paste("Calculating graph features on", Graphs[[g]]$name))
#       #graphProperties[[reps]] <- calcGraphFeatures(Graphs[g])
#       graphProperties[[reps]] <- calcGraphFeatures(Graphs[g])
#     }
#     graphname[[g]]=Graphs[[g]]$type
#     graphid[[g]]=Graphs[[g]]$id
#     graphreplicate[[g]]=c(1:nreps)
#     GraphPrefix=cbind(graphname[[g]],graphid[[g]],graphreplicate[[g]])
#     colnames(GraphPrefix)=c("GraphName","GraphID","GraphReplicate")
#     analysis[[g]]=as.data.frame(cbind(GraphPrefix,graphProperties[[reps]]))
#     row.names(analysis[[g]])=1:nreps
#   }
#   All_results=do.call(rbind,analysis)
#   # write.csv(All_results, file=output_file)
#   return( All_results)
# }
# 
# 
MKSFGraphs<- function(n=50, power=2, m=4) {
  Graphs = list()#;G=list() # set up the list of Graphs first
  initgraph= list()
  i= 1
  print("Creating SF Graphs")
  Graphs[[i]] = sample_pa(n, power, m, directed=FALSE, algorithm="psumtree")

  Graphs[[i]]$type = "SF"
  Graphs[[i]]$id = "1"
  Graphs[[i]]$name="SF"
  i <- i+1
  return(Graphs)
}


sim.sf.net=function(power=powerVals,m=mVals,n=500,nreps=1){
  net=NULL;data=NULL
  dt=expand.grid(powerVals,mVals)
  colnames(dt)=c("powerVals","mVals")
  for (i in 1:nrow(dt)){
    net[i]= MKSFGraphs(n,power=dt$powerVals[i],m=dt$mVals[i])
    data=RunSimOnGraphFeatures(net,nreps)
  }
  df=cbind(power,m,data)
  return(df)
}
powerVals=1:10
mVals=1:100
# ### 50 nodes 
# SF1.50=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=50)
# write.csv(SF1.50,"SF1.50.csv")
# 
# ### 100 nodes 
# SF1.100=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=100)
# write.csv(SF1.100,"SF1.100.csv")
# 
# ### 150 nodes 
# SF1.150=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=150)
# write.csv(SF1.150,"SF1.150.csv")
# 
# ### 200 nodes 
# SF1.200=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=200)
# write.csv(SF1.200,"SF1.200.csv")
# 
# ### 250 nodes 
# SF1.250=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=250)
# write.csv(SF1.250,"SF1.250.csv")
# 
# ### 300 nodes 
# SF1.300=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=300)
# write.csv(SF1.300,"SF1.300.csv")
# 
# ### 350 nodes 
# SF1.350=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=350)
# write.csv(SF1.350,"SF1.350.csv")
# 
# ### 400 nodes 
# SF1.400=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=400)
# write.csv(SF1.400,"SF1.400.csv")
# 
# ### 500 nodes 
# SF1.500=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=500)
# write.csv(SF1.500,"SF1.500.csv")
# 
# ### 750 nodes 
# SF1.750=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=750)
# write.csv(SF1.750,"SF1.750.csv")
# 
# ### 1000 nodes 
# SF1.1000=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=1000)
# write.csv(SF1.1000,"SF1.1000.csv")
# 
# ### 1500 nodes 
# SF1.1500=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=1500)
# write.csv(SF1.1500,"SF1.1500.csv")
# 
# ### 2000 nodes 
# SF1.2000=sim.sf.net(power=powerVals,m=mVals,nreps = 100,n=2000)
# write.csv(SF1.2000,"SF1.2000.csv")
# 
# ######## Saving all SF graphs to csv
# SF.50=read.csv("SF1.50.csv");SF.100=read.csv("SF1.100.csv");
# SF.150=read.csv("SF1.150.csv")
# SF.200=read.csv("SF1.100.csv");
# SF.250=read.csv("SF1.250.csv");SF.300=read.csv("SF1.300.csv")
# SF.350=read.csv("SF1.350.csv");SF.400=read.csv("SF1.400.csv")
# SF.500=read.csv("SF1.500.csv");SF.750=read.csv("SF1.750.csv")
# SF.1000=read.csv("SF1.1000.csv");SF.1500=read.csv("SF1.1500.csv")
# SF.2000=read.csv("SF1.2000.csv")
# df.sf=rbind(SF.50,SF.150,SF.200,SF.250,SF.300,SF.350,
#             SF.400, SF.500,SF.750,SF.1000,SF.1500,
#             SF.2000)
# 
# write.csv(df.sf,"df.sf.csv")
#########################################################################################
# Uncomment the above to generate data for various theoretical networks
#########################################################################################







# n <- 50
# m <- 520
# 
# # Work out how theye divide into each other
# rem <- m %% n
# div <- m %/% n
# 
# set.seed(123)
# if(rem != 0) {
#   g <- sample_smallworld(1, n, div+1, p = 0.001)
#   # Randomly delete the unwanted edges. Should be quite homegenous
#   g <- delete_edges(g, sample(1:gsize(g), size = gsize(g) - m))
# } else {
#   g <- sample_smallworld(1, n, div, p = 0.001)
# }
# 
# 
# 
# lat=make_lattice(dimvector=1000, nei=2, circular=T)
# plot(lat)
# 
# g <- sample_smallworld(dim=1, size=172, nei=6, p=0.05)
# g.new=rewire(make_lattice(dimvector=500, nei=3, circular=T), with=each_edge(p=0.9, loops=F))
# plot(g.new)



###############################################################################################
# other codes
###############################################################################################
#   
# # conda activate tf
# # conda create -y --name tf tensorflow-gpu python=3.6.8
# # conda activate tf
# 
# # dplyr::select(c(GraphName,order,edges,mean_eccentr,
# #                 radius,mean_path_length,graph_energy,min_triangle,mean_triangle,
# #                 sd_triangle,modularity,diameter,betw_centr,transitivity,threshold,
# #                 spectral_radius,eigen_centr,deg_centr,
# #                 mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
# #                 closeness_centr),max_triangle,num_triangle,deg_assort_coef)%>%
# #   mutate_if(is.character,factor)
# 
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# #+ Data processing and cleaning
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # df1=read.csv("NEW-DATA/df.lat.csv")%>%
# # filter(order<=1000)
# # 
# # df1=df1%>%dplyr::select(c(GraphName,order,edges,
# #                           mean_eccentr,mean_path_length,graph_energy,
# #                           modularity,diameter,betw_centr,transitivity,
# #                           spectral_radius,eigen_centr,deg_centr,
# #                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
# #                           closeness_centr,deg_assort_coef))%>%
# #   mutate_if(is.character,factor)
# 
#  # df2=rbind(read.csv("sbm.50.csv",sep = ",", header = T),
#  #           read.csv("sbm.150.csv",sep = ",", header = T),
#  #           read.csv("sbm.200.csv",sep = ",", header = T),
#  #           read.csv("sbm.250.csv",sep = ",", header = T),
#  #           read.csv("sbm.300.csv",sep = ",", header = T),
#  #           read.csv("sbm.350.csv",sep = ",", header = T),
#  #           read.csv("sbm.400.csv",sep = ",", header = T),
#  #           read.csv("sbm.500.csv",sep = ",", header = T),
#  #           read.csv("sbm.750.csv",sep = ",", header = T),
#  #           read.csv("sbm.1000.csv",sep = ",", header = T))
#  #       #    read.csv("sbm.750.csv",sep = ",", header = T))
# 
# 
# df2=read.csv("NEW-DATA/df.sbm.csv")
# df2=df2%>%
#   dplyr::filter(connected==1)
#    # filter(order<=250 & connected==1)
#  
# # df2=read.csv("NEW-DATA/df.sbm.csv")%>%
# #   filter(order<=1000 & connected==1)
# 
# df2=df2%>%dplyr::select(c(GraphName,order,edges,
#                           mean_eccentr,mean_path_length,graph_energy,
#                           modularity,diameter,betw_centr,transitivity,
#                           spectral_radius,eigen_centr,deg_centr,
#                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
#                           closeness_centr,deg_assort_coef))%>%
#   mutate_if(is.character,factor)
# 
# #write.csv(df3,"Erdos.Renyi.data.csv")
# 
# df3=read.csv("NEW-DATA/Erdos.Renyi.data.csv")%>%
#   dplyr::filter(connected==1)
# df3=df3%>%dplyr::select(c(GraphName,order,edges,
#                           mean_eccentr,mean_path_length,graph_energy,
#                           modularity,diameter,betw_centr,transitivity,
#                           spectral_radius,eigen_centr,deg_centr,
#                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
#                           closeness_centr,deg_assort_coef))%>%
#   mutate_if(is.character,factor)
# 
# # 
# # df4=rbind(read.csv("NEW-DATA/df.SP.50.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.100.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.150.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.200.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.250.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.300.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.350.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.400.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.500.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.750.csv",sep = ",", header = T),
# #           read.csv("NEW-DATA/df.SP.1000.csv",sep = ",", header = T))
# # 
# # write.csv(df4,"Spatial.data.csv")
# 
# df4=read.csv("NEW-DATA/Spatial.data.csv")%>%
#   dplyr::filter(connected==1)
#  # filter(r %in% c("0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9"))# & GraphReplicate <= 250)
# 
# df4=df4%>%dplyr::select(c(GraphName,order,edges,
#                           mean_eccentr,mean_path_length,graph_energy,
#                           modularity,diameter,betw_centr,transitivity,
#                           spectral_radius,eigen_centr,deg_centr,
#                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
#                           closeness_centr,deg_assort_coef))%>%
#   mutate_if(is.character,factor)
# 
# # df5=rbind(read.csv("sfnet.50.csv",sep = ",", header = T),
# #           read.csv("sfnet.100.csv",sep = ",", header = T),
# #           read.csv("sfnet.150.csv",sep = ",", header = T),
# #           read.csv("sfnet.200.csv",sep = ",", header = T),
# #           read.csv("sfnet.250.csv",sep = ",", header = T),
# #           read.csv("sfnet.300.csv",sep = ",", header = T),
# #           read.csv("sfnet.350.csv",sep = ",", header = T),
# #           read.csv("sfnet.400.csv",sep = ",", header = T),
# #           read.csv("sfnet.500.csv",sep = ",", header = T),
# #           read.csv("sfnet.750.csv",sep = ",", header = T),
# #           read.csv("sfnet.1000.csv",sep = ",", header = T))
# # 
# # 
# # write.csv(df5,"Scale-free.data.csv")
# 
# df5=read.csv("NEW-DATA/Scale-free.data.csv")%>%
#   dplyr::filter(connected==1)
# 
# df5=df5%>%
#   dplyr::filter(power %in% c("1","2"))#,"3"))# & m %in% c(1:30) )
#   
# # df5=df5%>%
# #   dplyr::filter(GraphReplicate <=70)
# 
# df5=df5%>%dplyr::select(c(GraphName,order,edges,
#                           mean_eccentr,mean_path_length,graph_energy,
#                           modularity,diameter,betw_centr,transitivity,
#                           spectral_radius,eigen_centr,deg_centr,
#                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
#                           closeness_centr,deg_assort_coef))%>%
#   mutate_if(is.character,factor)
# 
# 
# # df6=rbind(read.csv("swnet.50.csv",sep = ",", header = T),
# #           read.csv("swnet.100.csv",sep = ",", header = T),
# #           read.csv("swnet.150.csv",sep = ",", header = T),
# #           read.csv("swnet.200.csv",sep = ",", header = T),
# #           read.csv("swnet.250.csv",sep = ",", header = T),
# #           read.csv("swnet.300.csv",sep = ",", header = T),
# #           read.csv("swnet.350.csv",sep = ",", header = T),
# #            read.csv("swnet.400.csv",sep = ",", header = T),
# #            read.csv("swnet.500.csv",sep = ",", header = T),
# #            read.csv("swnet.750.csv",sep = ",", header = T),
# #            read.csv("swnet.1000.csv",sep = ",", header = T))
# 
# # write.csv(df6,"Small-world.data.csv")
# 
# 
# df6=read.csv("NEW-DATA/Small-world.data.csv")%>%
#   dplyr::filter(connected==1)
# 
# df6=df6%>%
#   dplyr::filter(prewire %in% c("0.1","0.2"))# & GraphReplicate <= 40)
# 
# #df6[df6$GraphReplicate <= 250,]
# df6=df6%>%dplyr::select(c(GraphName,order,edges,
#                           mean_eccentr,mean_path_length,graph_energy,
#                           modularity,diameter,betw_centr,transitivity,
#                           spectral_radius,eigen_centr,deg_centr,
#                           mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
#                           closeness_centr,deg_assort_coef))%>%
#   mutate_if(is.character,factor)
# 
# 
# Data=rbind(df2,df3,df4,df5,df6)# we drop lattice networks
# 
# write.csv(Data,"model.data.csv")





####----[3] Neural Network----####
# nnet.tune.df <- mlp(hidden_units = tune(), penalty = tune(), activation = "relu") %>%
#   set_engine("keras") %>%
#   set_mode("classification")
#
# nnet.grid <- nnet.tune.df %>%
#   parameters() %>%
#   grid_max_entropy(size = 10)
#
# nnet.wflow <- workflow() %>%
#   add_recipe(df.prep) %>%
#   add_model(nnet.tune.df)