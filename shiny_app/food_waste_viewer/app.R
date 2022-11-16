#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Final project
### Author: Italo Duran,.....,.....,.....,.....
### Email: duran01@bu.edu

#if (!requireNamespace("BiocManager", quietly= TRUE))
#install.packages("BiocManager")
library(BiocManager)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(colourpicker)# you might need to install this.
#library(DT)# you might need to install this.
#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)# you might need to install this.
#BiocManager::install("DESeq2")
#library(igraph)# you might need to install this.
#library(bslib)


###############################################################
#####Visual interface - Front End Part#########################
ui = fluidPage(theme = shinythemes::shinytheme("journal"),
               titlePanel("Final Project"),
               h3("By: Italo Duran"),
               h4(HTML("<b>Visualization analysis of:</b><br> mRNA-Seq Expression profiling of human post-mortem BA9 brain tissue for<br>Huntington's Disease and neurologically normal individuals data set...")),
               h4(HTML("<b>Citation:</b> Labadorf A, Hoss AG, Lagomarsino V, Latourelle JC et al.<br>RNA Sequence Analysis of Human Huntington Disease Brain Reveals an<br>Extensive Increase in Inflammatory and Developmental Gene Expression.<br>PLoS One 2015;10(12):e0143563. PMID: 26636579")),
               tags$h4(HTML("To use this application, download the <b>.csv files</b> from the data folder in my github:")),
               tags$a(href="https://github.com/imd9/Final-Project-BF591-R",target="_blank",rel="noopener noreferrer","Click here for 'My Github' CSV sample files!"), br(),br(),
               #This part is where we build the page layout and how it's going to look
               tabsetPanel(
                 tabPanel("Sample", fluid = TRUE,
                          h3("Sample Data Analysis"),
                          HTML(paste(rep("In this section you will be able to visualize distinct values and distributions of the samples."), collapse = "")),br(),br(),
                          sidebarLayout(sidebarPanel(fileInput("meta_data_samples", "Load sample Data", accept = ".csv", placeholder = "Sample_data.csv"),
                                                     HTML(paste(rep("Download the <b>Sample1_data.csv</b> from the GitHub link above."), collapse = "")),br(),br(),
                                                     HTML(paste(rep("<b>Summary:</b> It shows the different data types, Identifier values and Mean standard deviation.<br>
                                                        <b>Table:</b> Shows the samples that can be sorted by selecting the header names, chose the number of samples entries,
                                                        and use the search bar to look for specific values from the table.<br>
                                                        <b>Plots of Samples:</b> Histogram of the normal samples vs. with Huntington's, with different types of parameters to chose from."), collapse = ""))),
                                        mainPanel(tabsetPanel(tabPanel("Summary", fluid = TRUE,tableOutput(outputId = 'sample_summary')),
                                                              tabPanel("Data", fluid = TRUE,dataTableOutput(outputId = "samples_mdata")),
                                                              tabPanel("Plots",plotOutput(outputId = 'p.m.i.'),plotOutput(outputId = 'r.i.n.'),plotOutput(outputId = 'a.o.d.')))))
                 ),
                 ################################################################################################################################################################################################################
                 tabPanel("Counts",fluid = TRUE,
                          h3("Counts Matrix Data Analysis"),
                          HTML(paste(rep("In this section you will be able to visualize the normalized count data."), collapse = "")),br(),br(),
                          sidebarLayout(sidebarPanel(fileInput(inputId = 'sample_counts_file', "Load sample Data", accept = ".csv", placeholder = "norm_counts.csv"),
                                                     HTML(paste(rep("Download the <b>norm_counts.csv</b> from the GitHub link above."))),br(),br(),
                                                     HTML(paste(rep("<b>Summary:</b> It shows the different data types, Identifier values and Mean standard deviation.<br>
                                                        <b>Diagnostic plot_sampscatter plots:</b> Shows the samples that can be sorted by selecting the header names, chose the number of samples entries,
                                                        and use the search bar to look for specific values from the table.<br>
                                                        <b>Plots of Samples:</b> Histogram of the normal samples vs. with Huntington's, with different types of parameters to chose from."))),br(),br(),
                                                     sliderInput(inputId = 'percent_varians', label = 'Select a threshold for percent variance:',min=0,max=100,value=50,step=1),
                                                     sliderInput(inputId = 'sample_non_zero', label = 'Select a threshold for number of genes that are non-zero:',min=0,max=100,value=50,step=1),
                                                     submitButton("Plot", icon("r-project",class="fab fa-r-project fa-1x"), width = '100%')),
                                        mainPanel(tabsetPanel(tabPanel("Summary",tableOutput(outputId='sample_normalz_summry')),
                                                              tabPanel("Diagnostic plot_sampscatter Plots",plotOutput(outputId='sample_median_varians'),plotOutput(outputId='samp_median_zero')),
                                                              tabPanel("Clustered Heatmap",plotOutput(outputId = "samp_heat_map")),
                                                              tabPanel("P.C.A.",selectInput(inputId="var_pca1",label="Select a PCA for the X-axis:",choices=c("PC1","PC2","PC3","PC4","PC5","PC5","PC6","PC7","PC8","PC9","PC10"),selected="PC1"),
                                                                       selectInput(inputId="var_pca2",label="Select a PCA for the Y-axis:",choices=c("PC1","PC2","PC3","PC4","PC5","PC5","PC6","PC7","PC8","PC9","PC10"),selected="PC10"),
                                                                       plotOutput(outputId="PCAplot")))))           
                 ),
                 #############################################################################################################################
                 tabPanel("Differential Expression",fluid = TRUE,
                          h3("Differential Expression Analysis"),
                          HTML(paste(rep("In this section you will be able to visualize differential expression data."),collapse = "")),br(),br(),
                          sidebarLayout(sidebarPanel(fileInput(inputId ='file1_de',"Load differential expression results",accept = ".csv",placeholder="deseq_res.csv"),
                                                     HTML(paste(rep("Download the <b>ndeseq_diff_exp_res.csv</b> from the GitHub link above."))),br(),br(),
                                                     HTML(paste(rep("<p>A volcano plot can be generated with <b>'log<sub>2</sub> fold-change'</b> on the x-axis and <b>'p-adjusted'</b> on the y-axis.</p>"),collapse="")),
                                                     radioButtons(inputId='xaxis',label='Choose the column for the x-axis',choices=c('baseMean','log2FoldChange', 'lfcSE','stat','pvalue','padj'),selected = 'log2FoldChange'),
                                                     radioButtons(inputId='yaxis',label='Choose the column for the x-axis',choices=c('baseMean','log2FoldChange', 'lfcSE','stat','pvalue','padj'),selected = 'padj'),
                                                     colourpicker::colourInput(inputId='base_de_color',label="Base point color" ,"#138086"),
                                                     colourpicker::colourInput(inputId='high_de_color',label="Highlight point color","#EEB462"),
                                                     sliderInput(inputId = 'samp_padjad',min=-300,max=0,label="Select the magnitude of the p adjusted coloring:",value=-16,step=1),
                                                     submitButton("Plot", icon("r-project",class="fab fa-r-project fa-1x"), width = '100%')),
                                        mainPanel(tabsetPanel(tabPanel('Data Table',dataTableOutput("samp_sumr_dattable")),
                                                              tabPanel('Filtered Table',dataTableOutput("filtered_de_table")),
                                                              tabPanel('Volcano Plot',plotOutput("volcano")))))
                 ),
                 #####################################################################################################################
                 tabPanel("G.S.E.A.",fluid = TRUE,
                          h3("Gene Set Enrichment Analysis"),
                          HTML(paste(rep("In this section you will be able to visualize<br>Gene Set Enrichment Analysisdata for each gene."),collapse = "")),br(),br(),
                          sidebarLayout(sidebarPanel(HTML(paste(rep("Download the <b>Sample1_data.csv</b> from the GitHub link above."), collapse = "")),br(),
                                                     fileInput(inputId = 'samp_gsea_data', label = 'Load sample information matrix CSV',accept = ".csv",placeholder="Sample1_data.csv"),
                                                     HTML(paste(rep("Download the <b>norm_counts.csv</b> from the GitHub link above."))),br(),
                                                     fileInput(inputId = 'samp_count_gsea', label = 'Load normalized counts matrix',accept = ".csv",placeholder="norm_counts.csv"),
                                                     selectInput("metachoice", choices = c("PMI"="PMI","RIN"="RIN","Diagnosis"="Diagnosis","Sequence Reads"="Seq_reads","Age of death"="Age_of_death"),label="Select Data Parameter",selected="PMI"),
                                                     HTML(paste(rep("E.g. of gene names:<br><b>ENSG00000069011.10, ENSG00000170689.8<br>ENSG00000180818.4, ENSG00000128710.5</b>"))),br(),br(),br(),
                                                     textInput("gene", label = "Search for gene:", placeholder = "ENSG00000069011.10"),
                                                     submitButton("Plot", icon("r-project",class="fab fa-r-project fa-1x"), width = '100%')),mainPanel(plotOutput("distroplot"))))
                 
                 
               )
)
##################################server##############################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
