library(shiny)
library(network)
library(splancs)
library(spatial)
library(sna)
library(DT)
library(igraph)

options(digits=2)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Get the selected dataaset and create a graph object
  ds_list = as.character(dir(".", ".*\\.txt"))
  
  getEdgeData <- reactive({
    if (is.null(input$dataset)) return(NULL)
    
    # else ...
    # Read it in as a matrix.
    m <- as.matrix(read.table(input$dataset$datapath))
    
    # Does it look like an adjacency matrix?  Convert it to an edgelist.
    dm <- dim(m)
    if (dm[1] == dm[2] && dm[1] > 0) {
      edges <- as.edgelist.sna(as.network.matrix(m, directed=FALSE), directed=FALSE)[,c(1,2)]
    }
    else {
      edges <- as.matrix(m)[,c(1,2)]
    }
    
    t(edges)
  })
  
  getDeleteList <- reactive({
    as.integer(input$table_rows_selected)
  })
  
  get_iGraph <- reactive({
    edge_data <- getEdgeData()
    if (is.null(edge_data)) return(make_empty_graph())
    
    g <- simplify(make_undirected_graph(edge_data), remove.multiple = input$simplifyGraph)
    
    E(g)$color <- 'blue'
    dels <- getDeleteList()
    if (is.null(dels)) return(g) 
    
    b <- calcCentrality()
    node <- c(b$Node[dels])
    edges <- E(g)[from(node)]
    igraph::delete.edges(g, edges)
  })
 
  calcCentrality <- reactive({
    
    edge_data <- getEdgeData()
    
    # Handle "no data" case (i.e. startup)
    if (is.null(edge_data)) {
      df <- data.frame(t(c("No", "Data")))
      colnames(df) <- c("", "")
      return(df)
    }
    
    # Make the igraph
    g <- t(make_undirected_graph(edge_data))
    
    # Call the selected centrality metric
    if (input$centr_meas == "1") {
      deg <- igraph::centr_betw(g)
      dname = "Betweenness"
    }
    else if (input$centr_meas == "2") {
      deg <- igraph::centr_degree(g)
      dname = "Degree"
    }
    else if (input$centr_meas == "3") {
      deg <- igraph::centr_clo(g)
      dname = "Closeness"
    }
    else if (input$centr_meas == "4") {
      deg <- igraph::centr_eigen(g)
      dname = "Eigenvalue"
    }
    
    # Pretty up the result
    deg.df <- data.frame(seq(length(deg[[1]])), deg[[1]])
    colnames(deg.df) <- c("Node", dname)
    deg.df <- deg.df[order(-deg.df[,2]),]
    deg.df[,2] <- round(deg.df[,2], digits=2)
    deg.df
  })
  
  # Plot the graph
  output$graphPlot <- renderPlot({
    b <- calcCentrality()
    if ((b[1] == c("No", "data"))[1]) return(NULL)
    plot(get_iGraph(), mark.groups=c(b$Node[1]), vertex.size=10)
  })
  
  # Display centrality as a table
  output$table <- DT::renderDataTable({
    calcCentrality()
  }, rownames=F, filter="none", options=list(pageLength=5, dom="tip", digits=3))
  
  # Calculate and display average path length
  output$avgPathLength <- renderText({
    g <- get_iGraph()
    if (is.null(g))
      paste(c("No", "dataset"))
    else
      as.character(round(mean_distance(get_iGraph(), unconnected=T), 4))
  })
})