################################################################################
# FPDS Contracting app - October 2017
# final version
# server.R
################################################################################

require(shiny)
library(shinyjs)
library(shinyBS)
require(ggplot2)
require(dplyr)
require(scales)
require(plotly)
require(grid)
require(gridExtra)
library(forcats)

# server function starts

shinyServer <- function(input, output, session){
  
  ################################################################################
  # Read in and clean up data
  ################################################################################
  # read in data            
  FullData <- read.csv("data2.csv")
  
  # make Big Five the first category (so it displays at the top of the legend)
  # FullData$VendorSize <- relevel(FullData$VendorSize, "Big Five")

  # rename some column names
  # names(FullData)[9] <- "Contract_Type"
  # names(FullData)[10] <- "Vendor_Size"
  # names(FullData)[6] <- "Platform_Portfolio"
  # names(FullData)[8] <- "Competition"
  
  ################################################################################
  # Subset data based on user input
  ################################################################################
  
  dataset <- reactive({
    
    if(input$color_var == 'Category') {
      FullData <- filter(FullData, Category != "NULL")
    } else if(input$color_var == 'Customer') {
      FullData <- filter(FullData, Customer != "NULL")
    } else if(input$color_var == 'Contract_Type') {
      FullData <- filter(FullData, Customer != "NA")
      FullData <- filter(FullData, Contract_Type != "#N/A")
      FullData <- filter(FullData, Contract_Type != "Unlabeled")
      FullData <- filter(FullData, Contract_Type != "Other")
      FullData <- filter(FullData, Category != "NA") 
      FullData$Customer <- factor(FullData$Customer, 
                                  levels = c("Army", "Navy", "Air Force", "DLA", "Other DoD"))
      FullData$Category <- factor(FullData$Category, 
                                  levels = c("Products", "Services", "R&D"))
    # } else if(input$color_var == 'Competition') {
    #   FullData$Competition <- fct_recode(
    #     FullData$Competition,
    #     "Comp. w/ One Offer" = "Competition with single offer")
    } else if(input$color_var == 'Platform_Portfolio') {
      #FullData$Amount <- FullData$Amount / 1000000000
      FullData <- filter(FullData, Platform_Portfolio != "NULL" & !is.null(Platform_Portfolio))
      FullData$Platform_Portfolio <- fct_drop(FullData$Platform_Portfolio)
    } else if(input$color_var == 'Vendor_Size') {
      
    }
    
    ## subset by year, based on year slider ##
    
    # input$Yr[1] is the user-selected minimum year
    # input$Yr[2] is the user-selected maximum year
    # as.numeric(levels(FY))[FY] is just FY, converted from a factor to
    # a numeric variable
    shown <- filter(FullData, FY >= input$Yr[1] & FY <= input$Yr[2])
    
    ## subset data based on which categories the user selected ##
    
    # the selectInput widget holds the selected choices as a vector of
    # strings. This code checks whether the each observation is in the
    # selected categories, and discards it if isn't in all three.  The %in%
    # operator is a nice way to avoid typing lots of conditional tests all
    # strung together 
    if(input$color_var != 'Category') {
      shown <- filter(shown, Category %in% input$Cat)
    }
    
    if(input$color_var != 'Customer') {
      shown <- filter(shown, Customer %in% input$Customer)
    }
    
    if(input$color_var != 'Contract_Type') {
      shown <- filter(shown, `Contract_Type` %in% input$Contract)
    }
    
    if(input$color_var != 'Competition') {
      shown <- filter(shown, Competition %in% input$Classification)
    }
    
    if(input$color_var != 'Platform_Portfolio') {
      shown <- filter(shown, `Platform_Portfolio` %in% input$Portfolio)
    }
    
    if(input$color_var != 'Vendor_Size') {
      shown <- filter(shown, `Vendor_Size` %in% input$VS)
    }
    
    if(nrow(shown) != 0){
      # aggregate amount by (Fiscal Year x Category)
      shown <- shown %>%
        group_by(.dots = c( "FY", input$color_var)) %>%
        summarise(Amount = sum(Amount) / 1e9)
      
      # create 'Percent' variable as the percent of each FY's obligations that
      # went to that var
      shown <- shown %>%
        group_by(FY) %>%
        mutate(Percent = Amount / sum(Amount)) 
      
      shown <- shown %>%
        filter(!is.na(input$color_var))
      
      # let the NaN percent value be 0
      shown["Percent"][is.na(shown["Percent"])] <- 0
      
      # reorder by final year percent
      if(input$color_var == 'Category') {
        shown$Category <- fct_reorder(
          shown$Category,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      } else if(input$color_var == 'Customer') {
        shown$Customer <- fct_reorder(
          shown$Customer,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      } else if(input$color_var == 'Contract_Type') {
        shown$Contract_Type <- fct_reorder(
          shown$Contract_Type,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      } else if(input$color_var == 'Competition') {
        shown$Competition <- fct_reorder(
          shown$Competition,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      } else if(input$color_var == 'Platform_Portfolio') {
        shown$Platform_Portfolio <- fct_reorder(
          shown$Platform_Portfolio,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      } else if(input$color_var == 'Vendor_Size') {
        shown$Vendor_Size <- fct_reorder(
          shown$Vendor_Size,
          (shown$Percent * (shown$FY == input$Yr[2])) ,
          mean,
          na.rm = TRUE,
          .desc = TRUE)
      }
      
    }
    
    
    # return the subsetted dataframe to whatever called dataset()
    return(shown)
    # end of dataset() function      
  })
  
  ################################################################################
  # Set colors  
  ################################################################################
  colorsetting <- reactive({
    if(input$color_var == 'Category') {
      colorset <- 
        c("Products" = "#CE884E", 
          "Services" = "#63c5b8", 
          "R&D" = "#628582")
    } else if(input$color_var == 'Customer') {
      colorset <- 
        c("Air Force" = "#63c5b8", 
          "Army" = "#CE884E", 
          "Navy" = "#554449", 
          # "MDA" = "#008e9d", 
          "DLA" = "#36605a", 
          "Other DoD" = "#AD4545")
    } else if(input$color_var == 'Contract_Type') {
      colorset <- 
        c("Combination (two or more)" = "#554449", 
          "Cost No Fee" = "#7C3772", 
          "Cost Plus Award Fee" = "#36605a", 
          "Cost Plus Fixed Fee" = "#AD4545", 
          "Cost Plus Incentive" = "#008e9d", 
          "Cost Sharing" = "#599a9e", 
          "Firm Fixed Price" = "#CE884E",
          "Fixed Price Award Fee" = "#554449", 
          "Fixed Price Incentive" = "#7C3772", 
          "Fixed Price Level of Effort" = "#36605a", 
          "Fixed Price Redetermination" = "#AD4545", 
          "Fixed Price with Economic Price Adjustment" = "#008e9d", 
          "Labor Hours" = "#599a9e", 
          "Unlabeled" = "#CE884E",
          "Not Reported" = "#554449", 
          "NULL" = "#7C3772", 
          "Order Dependent (IDV only)" = "#36605a", 
          "Time and Materials" = "#AD4545")
      
    } else if(input$color_var == 'Competition') {
      colorset <- 
        c("Competition with single offer" = "#554449", 
          "Competition with single offer (Overrode blank Fair Opportunity)" = "#7C3772", 
          "Full Competition (Multiple Offers)" = "#36605a", 
          "Limited Competition with multiple offers" = "#AD4545", 
          "Limited Competition with multiple offers (Overrode blank Fair Opportunity)" = "#008e9d", 
          "No Competition (Follow on to competed action)" = "#599a9e", 
          "No Competition (Only One Source Exception)" = "#CE884E",
          "No Competition (Only One Source Exception; Overrode blank Fair Opportunity)" = "#554449", 
          "No Competition (Other Exception)" = "#7C3772", 
          "No Competition (Unlabeled Exception)" = "#36605a", 
          "No Competition (Unlabeled Exception; Overrode blank Fair Opportunity)" = "#AD4545", 
          "Unlabeled: Blank Extent Competed" = "#008e9d", 
          "Unlabeled: Competition; Zero Offers" = "#599a9e", 
          "Unlabeled: No competition; multiple offers" = "#CE884E")
      

    } else if(input$color_var == 'Platform_Portfolio') {
      colorset <- 
        ("Aircraft" = "#554449")
    } else if(input$color_var == 'Vendor_Size') {
      colorset <- 
        # c("Big Five" = "#C74F4F",
        #   "Large" =  "#5F597C",
        #   "Medium" = "#599a9e",
        #   "Small" = "#84B564")
      
      c("Large" = "#554449", 
        "Large: Big 6" = "#7C3772", 
        "Large: Big 6 JV" = "#36605a", 
        "Medium <1B" = "#AD4545", 
        "Medium >1B" = "#008e9d", 
        "Small" = "#599a9e", 
        "Unlabeled" = "#CE884E")
    }
    
    DIIGcolors <- scale_color_manual(values = colorset, name = NULL)
    return(DIIGcolors)
  })
  
  #DIIGcolors <- scale_color_manual(values = colorsetting(), name = NULL)
  
  ################################################################################
  # Set fill colors  
  ################################################################################
  fillcolorsetting <- reactive({
    if(input$color_var == 'Category') {
      colorset <- 
        c("Products" = "#CE884E", 
          "Services" = "#63c5b8", 
          "R&D" = "#628582")
    } else if(input$color_var == 'Customer') {
      colorset <- 
        c("Air Force" = "#63c5b8", 
          "Army" = "#CE884E", 
          "Navy" = "#554449", 
          # "MDA" = "#008e9d", 
          "DLA" = "#36605a", 
          "Other DoD" = "#AD4545")
    } else if(input$color_var == 'Contract_Type') {
      colorset <- 
        c("Combination (two or more)" = "#554449", 
          "Cost No Fee" = "#7C3772", 
          "Cost Plus Award Fee" = "#36605a", 
          "Cost Plus Fixed Fee" = "#AD4545", 
          "Cost Plus Incentive" = "#008e9d", 
          "Cost Sharing" = "#599a9e", 
          "Firm Fixed Price" = "#CE884E",
          "Fixed Price Award Fee" = "#554449", 
          "Fixed Price Incentive" = "#7C3772", 
          "Fixed Price Level of Effort" = "#36605a", 
          "Fixed Price Redetermination" = "#AD4545", 
          "Fixed Price with Economic Price Adjustment" = "#008e9d", 
          "Labor Hours" = "#599a9e", 
          "Unlabeled" = "#CE884E",
          "Not Reported" = "#554449", 
          "NULL" = "#7C3772", 
          "Order Dependent (IDV only)" = "#36605a", 
          "Time and Materials" = "#AD4545")
    } else if(input$color_var == 'Competition') {
      colorset <- 
        c("Competition with single offer" = "#554449", 
          "Competition with single offer (Overrode blank Fair Opportunity)" = "#7C3772", 
          "Full Competition (Multiple Offers)" = "#36605a", 
          "Limited Competition with multiple offers" = "#AD4545", 
          "Limited Competition with multiple offers (Overrode blank Fair Opportunity)" = "#008e9d", 
          "No Competition (Follow on to competed action)" = "#599a9e", 
          "No Competition (Only One Source Exception)" = "#CE884E",
          "No Competition (Only One Source Exception; Overrode blank Fair Opportunity)" = "#554449", 
          "No Competition (Other Exception)" = "#7C3772", 
          "No Competition (Unlabeled Exception)" = "#36605a", 
          "No Competition (Unlabeled Exception; Overrode blank Fair Opportunity)" = "#AD4545", 
          "Unlabeled: Blank Extent Competed" = "#008e9d", 
          "Unlabeled: Competition; Zero Offers" = "#599a9e", 
          "Unlabeled: No competition; multiple offers" = "#CE884E")
    } else if(input$color_var == 'Platform_Portfolio') {
      colorset <- 
        ("Aircraft" = "#554449")
    } else if(input$color_var == 'Vendor_Size') {
      colorset <- 
        c("Large" = "#554449", 
          "Large: Big 6" = "#7C3772", 
          "Large: Big 6 JV" = "#36605a", 
          "Medium <1B" = "#AD4545", 
          "Medium >1B" = "#008e9d", 
          "Small" = "#599a9e", 
          "Unlabeled" = "#CE884E")
    }
    DIIGfillcolors <- scale_fill_manual(values = colorset)
    return(DIIGfillcolors)
  })

  ################################################################################
  # Build the plot for output
  ################################################################################
  
  lineplotsetting <- reactive({
   
    mainplot <- ggplot(data = dataset(),aes(x=FY, y=Percent))
    mainplot <- mainplot + 
      geom_line(aes_q(
        color=as.name(input$color_var)
        ,group=as.name(input$color_var)
        #,fill =as.name(input$color_var)
      ),
      size = 1.5) +

      ggtitle(paste("Contract Obligations by", input$color_var)) + 
     
      colorsetting()+
      
    ########################################################################################share below
    # diigtheme1:::diiggraph()+ 
    theme(plot.title = element_text(
      family = "Open Sans", color = "#554449", size = 25, face="bold",
      margin=margin(20,0,30,0), hjust = 0.5)) +
      
      theme(panel.border = element_blank(),
            panel.background = element_rect(fill = "#FCFCFC", color="#FCFCFC"),
            plot.background = element_rect(fill = "#FCFCFC", color="#FCFCFC"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(size=.1, color="lightgray"),
            panel.grid.minor.y = element_line(size=.1, color="lightgray")) +
      
      scale_y_continuous(labels=percent) +
      scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 1),
                         labels = function(x) {substring(as.character(x), 3, 4)}) +
      
      theme(legend.position = "right") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size = 18, color="#554449")) +
      theme(legend.key = element_rect(fill="#FCFCFC")) +
      theme(legend.background = element_rect(fill="#FCFCFC")) + 
      theme(legend.key.width = unit(3,"line")) +
      theme(axis.text.x = element_text(size = 11, color="#554449", margin=margin(-5,0,0,0))) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(size = 11, color="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
      theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
      
      xlab("Fiscal Year") +
      ylab("Share of Contract Obligations") +
      theme(plot.caption = element_text(
        size = 12, face = "bold", color = "#554449", family = "Open Sans"
      )) +
      labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans")
    #######################################################################################
    # set the y limits according
    if(min(dataset()$Percent > 0)) {
      mainplot <- mainplot + 
        coord_cartesian(ylim = c(0, 1.05*max(dataset()$Percent)))
    } else {
      mainplot <- mainplot +
        coord_cartesian(ylim = c(1.05*min(dataset()$Percent), 1.05*max(dataset()$Percent)))
    }
    ########################################################################################share above
    if(input$color_var == 'Platform_Portfolio'){
      mainplot + 
        facet_wrap(~ Platform_Portfolio, nrow = 2, scales="free_x", drop = FALSE) +
        theme(panel.spacing.y = unit(1, "lines")) +
        theme(legend.position = "none") + 
        scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
                           labels = function(x) {substring(as.character(x), 3, 4)}) +
        theme(strip.background = element_rect(color = "gray95", fill=c("#fcfcfc"))) +
        theme(strip.text.x = element_text(family = "Open Sans",
                                          size = rel(1.2),
                                          color = "#554449"))
    } else {mainplot}

  })
  
  barplotsetting <- reactive({
    #set the number of rows
    if(input$color_var == 'Category') {
      numrow <- 1
    } else {
      numrow <- 2
    } 
    
    mainplot <- ggplot(data = dataset(),aes(x=FY,weight=Amount))
    mainplot <- mainplot + 
      geom_bar(aes_q(
        group=as.name(input$color_var)
        ,fill =as.name(input$color_var)
      ),
      width=.7) +
      
      facet_wrap(as.formula(paste0("~ `",input$color_var, "`")), 
                 nrow = numrow, scales="free_x", drop = TRUE) + 
      
      theme(panel.spacing.y = unit(1, "lines")) +
      
      ggtitle(paste("Contract Obligations by", input$color_var)) + 
      
    fillcolorsetting() + 
      
    ##############################################################################facet below 
      theme(plot.title = element_text(
        family = "Arial", color = "#554449", size = 25, face="bold",
        margin=margin(20,0,30,0), hjust = 0.5)) +
      
      theme(panel.border = element_blank(),
            panel.background = element_rect(fill = "#FCFCFC"),
            plot.background = element_rect(fill = "#FCFCFC", color="#FCFCFC"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(size=.1, color="lightgray"),
            panel.grid.minor.y = element_line(size=.1, color="lightgray")) +
      
      scale_x_continuous(breaks = seq(input$Yr[1], input$Yr[2], by = 2),
                         labels = function(x) {substring(as.character(x), 3, 4)}) +
      #coord_cartesian(ylim = c(min(dataset()$Amount), 1.05*max(dataset()$Amount))) +
      
      theme(legend.position="none") +
      theme(strip.background = element_rect(color = "gray95", fill=c("#fcfcfc"))) +
      theme(strip.text.x = element_text(family = "Open Sans",
                                        size = rel(1.7),
                                        color = "#554449")) +
      theme(legend.text = element_text(size = 18, color="#554449")) +
      theme(legend.title = element_text(size = 18, face = "bold", color="#554449")) +
      theme(legend.key = element_rect(fill="#fcfcfc")) +
      theme(legend.key.width = unit(3,"line")) +
      theme(axis.text.x = element_text(size = 12, color="#554449", margin=margin(-5,0,0,0))) +
      theme(axis.ticks.length = unit(.00, "cm")) +
      theme(axis.text.y = element_text(size = 12, color="#554449", margin=margin(0,5,0,0))) +
      theme(axis.title.x = element_text(size = 16, face = "bold", color="#554449", margin=margin(15,0,0,0))) +
      theme(axis.title.y = element_text(size = 16, face = "bold", color="#554449", margin=margin(0,15,0,0))) +
      
      xlab("Fiscal Year") +
      ylab("Constant 2016 $ Billion") +
      theme(plot.caption = element_text(
        size = 12, face = "bold", color = "#554449", family = "Open Sans"
      )) +
      labs(caption = "Source: FPDS; CSIS analysis", size = 30, family= "Open Sans") 
    ############################################################################
    # set the y limits according

    ##############################################################################facet above
    if(input$color_var == 'Platform_Portfolio'){
      mainplot + 
        theme(strip.text.x = element_text(family = "Open Sans",
                                          size = rel(1.2),
                                          color = "#554449"))
    } else {mainplot}  
    
  })
  
  ################################################################################
  # Run download buttons
  ################################################################################
  
  # run csv download button
  output$CSVDownloadBtn <- downloadHandler(
    filename = paste('CSIS-Contract-Obligations-Data', Sys.Date(),'.csv', sep=''),
    content = function(file) {
      writedata <- dataset()
      writedata$Percent <- writedata$Percent * 100
      write.csv(writedata, file)
    }
  )
  
  ################################################################################
  # Run hover section
  ################################################################################
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    if(is.null(hover)) return(NULL)
    
    switch(
      input$Chart,
      "Line" = {
        point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Percent",
                            threshold = (150 / (input$Yr[2] - input$Yr[1])) + 10,
                            maxpoints = 1, addDist = TRUE)
      },
      "Bar" = {
        point <- nearPoints(dataset(), hover, xvar = "FY", yvar = "Amount",
                            threshold = 200,
                            maxpoints = 1, addDist = TRUE)
      }
    )
    
    positiondf <- as.data.frame(point)

    if(nrow(point) == 0) return(NULL)
    
    if(input$Chart == "Bar"){
      year <- round(hover$x)
      if(year < input$Yr[1] | year > input$Yr[2]) return(NULL)
      hov_amount <- point$Amount
      hov_percent <- point$Percent
      colorvariable <- names(positiondf)[2]
      colorcat <- sapply(positiondf[2], as.character) 
      if(hover$y > 0 & hover$y > hov_amount) return(NULL)
      # end of bar
    } else {
      year <- point$FY
      hov_amount <- point$Amount
      hov_percent <- point$Percent
      colorvariable <- names(positiondf)[2]
      colorcat <- sapply(positiondf[2], as.character) 
      #end of line
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) /
      (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) /
      (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct *
      (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct *
      (hover$range$bottom - hover$range$top)
    
    # Use HTML/CSS to change style of tooltip panel here
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Fiscal Year: </b>", year, "<br/>",
                    "<b>", colorvariable,": </b>", colorcat, "<br/>",
                    "<b> Share: </b>", round(hov_percent*100,2), "%<br/>",
                    "<b> Amount: </b> $",
                    round(hov_amount,4),  " Billion")))
    )
  })
  
  
  ################################################################################
  # Output the built plot and start the app
  ################################################################################

  output$PlotPart <- renderUI({
    shown <- dataset()
    if(nrow(shown) == 0){
      h4("ERROR: There is no data under selected categories.")
    } else{
      plotOutput("plot", 
                 hover = hoverOpts(id = "plot_hover", delay = 20))
    }
  })
  
  output$plot <- renderPlot({
    (switch(input$Chart, 
            `Line` = lineplotsetting(), 
            `Bar`  = barplotsetting()))
  }, height = 700) 
  
  observeEvent(input$reset_input, {
    shinyjs::reset("selectinput")
  })

}