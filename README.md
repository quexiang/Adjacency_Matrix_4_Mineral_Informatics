### About

Adjacency matrix for quick exploring hidden patterns in mineral data. Here we present an R Shiny application [Adjacency matrix 4 Mineral Informatics](https://quexiang.shinyapps.io/Adjacency_Matrix_4_Mineral_Informatics/) to show co-relationships in mineral data.  

- This application provides interactive browsing of adjacency matrices for many other datasets in the four use cases. With the integration of **[<font color= #00000>Interactive Periodic Table in JavaScript version 1.0</font>](https://www.codedrome.com/interactive-periodic-table-in-javascript/)** in our shiny application, we can easily organize and quickly display the adjacency matrix by selecting chemical elements. We also used other lists and controls on the graphical interface, users can quickly make a selection to obtain the adjacency matrix and then analyze. The clear layout of cells in the adjacency matrix can effectively avoid the node overlapping and edge crossings in the network visualization especially when the network becomes dense. Moreover, it provides a variety of interactive operations for rearranging the matrix (e.g., using communities, closeness, degree, betweenness, and eigen), allowing users to observe changes and patterns in the adjacency matrix from different perspectives.

- Our work illustrates that many interesting patterns can be found in the large element-based mineral count datasets and the location-based mineral co-existence datasets through the many community detection algorithms and visualization techniques in adjacency matrices.

- It is still in the preliminary stage and has some limitations that can be address in the futrue work.

  - (1) The current application does not support automatic data update.The Mindat API was able to provide most of the datasets mentioned in our study. With the progress of the [mindat API](https://api.mindat.org/) and the gradual implementation of OpenMindat's [R Package](https://github.com/quexiang/OpenMindat) and Python package (plan to do),we believe that an automatic data pipeline can be built for automatic update the adjacency matrices in our R Shiny application. 

  - (2) The current application lacks rich filtering rules and operations to assist in browsing the adjacency matrices.A potential update is to make the matrix and data more interactive, such as using some visualization packages in JavaScript. 
  
  - (3) The current adjacency matrices are two-dimensional, and there should be ways to build three-dimensional adjacency matrices.

