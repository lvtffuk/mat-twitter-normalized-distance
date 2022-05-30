options(rgl.useNULL=TRUE)
options(shiny.trace=TRUE)
options(shiny.maxRequestSize=30*1024^2)

library(psych)
library(rjson)
library(ca)
library(nFactors)
library(ape)
library(wordcloud)
library(qgraph)
library(polycor)
library(bipartite)
library(FactoMineR)
library(png)
library(vegan3d)
library(ade4)
library(vegan)
library(fpc)
library(cluster) 
library(network) 
library(corrplot) 
library(smacof)
library(e1071)
library(mclust)
library(MASS)
library(ape)
library(igraph)
library(plotrix)
# library(shinyRGL)
# library(rgl)


# TODO:

# - pridelat odstranovani stranek

 


# Define server logic required to draw a histogram
shinyServer(function(input, output) {



  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot


iniTable <- function(matrixFile) {

    # matrixFile <- "muslim-gamers_distance_matrix.csv"  


   if (is.null(matrixFile)){
      return(NULL)
    
      }
    
    tbl <- read.csv(matrixFile,   sep=";")


tbl[tbl == "NaN"] <- 1.9
    tbl[is.na(tbl)] <- 0

    names <- tbl$X
    tbl$X <- NULL
    rownames(tbl) = make.names(names, unique=TRUE)
    rownames(tbl) = names
    colnames(tbl) = rownames(tbl)

    tmp <- tbl[, colSums(is.na(tbl)) != nrow(tbl)]
    tmp <- as.data.frame(t(tmp))
    tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]
    tbl <- tmp

  tbl[tbl=="Inf"]<-1.9

    tbl <- as.matrix(tbl)
    pages <- rownames(tbl)
    mode(tbl) <- 'numeric' 
    mydata <- tbl

  }

  iniTablePlus <- function(matrixFile) {

    # matrixFile <- "muslim-gamers_distance_matrix.csv"  


   if (is.null(matrixFile)){
      return(NULL)
    
      }
    
    tbl <- read.csv(matrixFile,  sep=";")


tbl[tbl == "NaN"] <- 1.9
    tbl[is.na(tbl)] <- 0

    names <- tbl$X
    tbl$X <- NULL
    rownames(tbl) = make.names(names, unique=TRUE)
    rownames(tbl) = names
    colnames(tbl) = rownames(tbl)

    tmp <- tbl[, colSums(is.na(tbl)) != nrow(tbl)]
    tmp <- as.data.frame(t(tmp))
    tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]
    tbl <- tmp


    tbl <- as.matrix(tbl)
    pages <- rownames(tbl)
    mode(tbl) <- 'numeric' 
    mydata <- tbl

   myPoints  <- as.vector(input$targetSelector)

    mydata_cleaned <- mydata
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- mydata_cleaned[!(colnames(mydata_cleaned) %in% myPoints), ]


    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- t(mydata_cleaned)

    mydata_cleaned <- mydata_cleaned[!(rownames(mydata_cleaned) %in% myPoints), ]
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- t(mydata_cleaned)
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata <- mydata_cleaned

  }
  
    


output$referenceSelector <- renderUI({

    matrix <- input$matrix
    mydata <- iniTable(matrix$datapath)
    pages <- rownames(mydata) 
   selectizeInput("referenceSelector", "Reference:", as.list(pages)) 
})

output$targetSelector <- renderUI({
    matrix <- input$matrix
    mydata <- iniTable(matrix$datapath)
    pages <- rownames(mydata) 
   checkboxGroupInput("targetSelector", "Target:", as.list(pages)) 
})

output$value <- renderPrint({ input$referenceSelector })

output$FuzzyOutput <- renderImage({
    
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)

    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")
    
    result <- cmeans(mydata, input$numberClusters, 100, m=2, method="cmeans")


    png(outfile, width=width, height=height)
      plot(mydata[,1], mydata[,2], col=result$cluster, type="n")
      points(result$centers[,c(1,2)], col=1:3, pch=8, cex=2)
      text(mydata[,1], mydata[,2], labels = row.names(mydata), cex=.7,col=result$cluster)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$CAOutput <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTable(matrix$datapath)


    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

    fit <- ca(mydata) 

    png(outfile, width=width, height=height)
      plot(fit)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)


output$kmeansOutput <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)




    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

 fit <- kmeans(mydata, input$numberClusters)
 
    png(outfile, width=width, height=height)
      clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)


  output$sctPlot <- renderImage({
        matrix <- input$matrix
        mydata <- iniTablePlus(matrix$datapath)


          ord <- rda(mydata)
          cl <- kmeans(mydata,input$numberClusters)
          barva <- as.vector(cl$cluster)

          barva <- replace(barva, barva==1, "red")
          barva <- replace(barva, barva==2, "green")
          barva <- replace(barva, barva==3, "blue")
          barva <- replace(barva, barva==4, "black")
          barva <- replace(barva, barva==5, "burlywood4")
          barva <- replace(barva, barva==6, "cadetblue")
          barva <- replace(barva, barva==7, "chocolate4")
          barva <- replace(barva, barva==8, "darkgoldenrod1")


    width  <- 1024
    height <- 768
     outfile <- tempfile(fileext = ".png")


    png(outfile, width=width, height=height)
          pl <- ordiplot3d(ord, scaling = 3, angle=15, type="n")
          points(pl, "points", pch=16, col=barva, cex = 0.7)
          text(pl, col=barva, pos=1, cex = 1)    
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)


output$DHierarClust <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)




    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")



d <- vegdist(mydata)
m <- metaMDS(d)
cl <- hclust(d, "aver")
 
    png(outfile, width=width, height=height)
        orditree3d(m, cl, pch=16, col=cutree(cl, input$numberClusters), type = "t")
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$igraphOutput <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)

    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

      tbl <- as.matrix(mydata)


      threshold <- input$minDistance
      tbl[tbl > threshold] <-  0
      tbl[tbl <= threshold & tbl > 0] <- 1

      g  <- graph.adjacency(tbl,mode="undirected")
      g <- simplify(g)

 
    png(outfile, width=width, height=height)
      plot(g, vertex.size=3, vertex.label=V(g)$name, vertex.label.cex=1.5, vertex.label.dist=0.4, layout=layout.fruchterman.reingold)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)



  network <- reactive({
       matrix <- input$matrix
       mydata <- iniTablePlus(matrix$datapath)

      tbl <- as.matrix(mydata)

      threshold <- input$minDistance
      tbl[tbl > threshold] <-  0
      tbl[tbl <= threshold & tbl > 0] <- 1

      g  <- graph.adjacency(tbl,mode="undirected")
      g <- simplify(g)
      g
  })



  output$downloadData <- downloadHandler(
     
    


    filename = function() { "data.graphml" },
    content = function(file) {
      write.graph(network(), file, format="graphml")
    }
  )



output$HierarClustOutput <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)


    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

    d <- dist(mydata, method = "euclidean") # distance matrix
    fit <- hclust(d, method="ward") 

 
    png(outfile, width=width, height=height)
      plot(fit,cex=1.5) # display dendogram
      groups <- cutree(fit, k=input$numberClusters) # cut tree into 5 clusters
      # draw dendogram with red borders around the 5 clusters 
      rect.hclust(fit, k=input$numberClusters, border="red")
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$SummaryOutput <- renderImage({
 
    matrix <- input$matrix
    mydata <- iniTable(matrix$datapath)




    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")


 
    png(outfile, width=width, height=height)
      hist(mydata)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$MDSOutput <- renderImage({


    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)
 
    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

d <- dist(mydata) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]

 
    png(outfile, width=width, height=height)

      plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric  MDS",  type="n")
      text(x, y, labels = row.names(mydata), cex=1)

    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)



output$PCAFactor <- renderImage({
    matrix <- input$matrix
    mydata <- iniTablePlus(matrix$datapath)

    width  <- 1024
    height <- 768
    
    outfile <- tempfile(fileext = ".png")

    png(outfile, width=width, height=height)
      res.pca = PCA(mydata, scale.unit=TRUE, ncp=5, graph=T)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$PCADiagram <- renderImage({
      matrix <- input$matrix
      mydata <- iniTablePlus(matrix$datapath)
       fit <- principal(cor(mydata), input$numberClusters)


    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")
    

    png(outfile, width=width, height=height)
      fa.diagram(fit, cex=1.2)
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)

output$caption <- renderPrint({

       matrix <- input$matrix
      mydata <- iniTablePlus(matrix$datapath)
       fit <- principal(cor(mydata), input$numberClusters)


      fit
  })


output$RadialOutput <- renderImage({

    matrix <- input$matrix
    mydata <- iniTable(matrix$datapath)
 
    width  <- 1024
    height <- 768

    outfile <- tempfile(fileext = ".png")

    reference <- as.vector(input$referenceSelector)
    mydataTarget <- reference
    mydata_cleaned <- mydata[colnames(mydata) %in% mydataTarget, ]
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- t(mydata_cleaned)
    mydata_cleaned <- as.data.frame(mydata_cleaned)




    myPoints  <- as.vector(input$targetSelector)

    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- subset(mydata_cleaned, select=myPoints)
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    mydata_cleaned <- t(mydata_cleaned)

    mydata_cleaned <- mydata_cleaned[!colnames(mydata_cleaned) %in% reference, ]
    mydata_cleaned <- as.data.frame(mydata_cleaned)
    # mydata_cleaned$mydata_cleaned <- 1 - mydata_cleaned$mydata_cleaned
    mydata_cleaned <- t(mydata_cleaned)
    mydata_cleaned <- as.data.frame(mydata_cleaned)

 
    png(outfile, width=width, height=height)

      radial.plot(mydata_cleaned,labels=colnames(mydata_cleaned),
      rp.type="p",main=reference[1],
      radial.lim=c(0,1.2), show.grid.labels=1)
 
    dev.off()
    
    list(src = outfile,
         contentType = "image/png",
         width = width,
         height = height,
         alt = "This is alternate text")

  }, deleteFile = TRUE)


output$facebooktable <- renderDataTable({ 
    filename <- "hernistranky_matrix_details.csv"  

    toDF <- read.csv(filename)
    toDF$id <- as.character(toDF$id)
    toDF$X <- NULL
    toDF
 })

})