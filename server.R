#
# This is the server logic of a Shiny web application that calculates the optimal
# inventory level for a retail item with a short life.
#

library(shiny)
library(ggplot2)

# Define server logic 
shinyServer(function(input, output) {
    parms <- reactive({
        # Create input list for testing - commented out for production
        #input <- list(Margin = 60, DailyDemand = 1, Variability = 40,
        #              SeasWks = 4,
        #              CPDays = 7)
        
        itemPrice <- 10
        
        # Get the input values into variables
        itemMargin <- input$Margin/100
        dailyDemand <- input$DailyDemand
        variability <- input$Variability/100
        seaswks <- input$SeasWks
        cpdays <- input$CPRP + input$CPLT
        #print(cpdays)
        output$cpdays <- renderText(paste0("Total coverage period = ",cpdays, " days"))
        
        # Now calculate all of the parameters we need for the gamma distribution functions
        
        itemCost <- (1 - itemMargin)*itemPrice
        cpdemand <- dailyDemand * cpdays
        seasdemand <- dailyDemand * seaswks * 7
        cpstdev <- variability * cpdemand
        seasstdev <- variability * seasdemand
        cpvar <- cpstdev^2
        seasvar <- seasstdev^2
        alpha <- (cpdemand/cpstdev)^2 # alpha is the same for both cp and season
        betaCP <- cpvar/cpdemand
        betaSeas <- seasvar/seasdemand
        
        segmentsperunit <- 10
        units <- seq(from = 0, to = seasdemand, by = 1/segmentsperunit)
        CPPDF <- dgamma(units, shape = alpha, scale = betaCP)
        CPCDF <- pgamma(units, shape = alpha, scale = betaCP)
        CPCDFp1 <- pgamma(units, shape = alpha+1, scale = betaCP)
        SeasPDF <- dgamma(units, shape = alpha, scale = betaSeas)
        SeasCDF <- pgamma(units, shape = alpha, scale = betaSeas)
        SeasCDFp1 <- pgamma(units, shape = alpha+1, scale = betaSeas)
        
        # Calculate the costs
        LSUnits <- cpdemand*(1-CPCDFp1) - units*(1-CPCDF)
        LSCost <- LSUnits*(itemMargin*itemPrice)
        
        obsUnits <- units*SeasCDF - seasdemand*SeasCDFp1
        obsCost <- obsUnits*itemCost
        
        TotCost <- LSCost + obsCost
        
        minCost <- min(TotCost)

        OptInvLevel <- round(units[TotCost == minCost],0)

        # Return a list object with all of the values needed elsewhere
        
        list(itemMargin=itemMargin,
             dailyDemand=dailyDemand,
             variability=variability,
             seaswks=seaswks,
             cpdays=cpdays,
             itemCost=itemCost,
             cpdemand=cpdemand,
             seasdemand=seasdemand,
             cpstdev=cpstdev,
             seasstdev=seasstdev,
             cpvar=cpvar,
             seasvar=seasvar,
             alpha=alpha,
             betaCP=betaCP,
             betaSeas=betaSeas,
             segmentsperunit=segmentsperunit,
             units=units,
             CPPDF=CPPDF,
             CPCDF=CPCDF,
             CPCDFp1=CPCDFp1,
             SeasPDF=SeasPDF,
             SeasCDF=SeasCDF,
             SeasCDFp1=SeasCDFp1,
             LSUnits=LSUnits,
             LSCost=LSCost,
             obsUnits=obsUnits,
             obsCost=obsCost,
             TotCost=TotCost,
             minCost=minCost,
             OptInvLevel=OptInvLevel )
        
    })
    
  output$pdfPlots <- renderPlot({
            
    
    
    linewidth = 4
    
    x <- parms()$units
    yCP <- parms()$CPPDF
    ySeas <- parms()$SeasPDF
    optInv <- parms()$OptInvLevel

    transval <- 0.333
    aucCPCol <- "maroon"
    aucSeasCol <- "blue3"

    aucCPRGB <- col2rgb(aucCPCol)/255
    aucSeasRGB <- col2rgb(aucSeasCol)/255
    aucCPColor <- rgb(aucCPRGB[1],aucCPRGB[2],aucCPRGB[3], transval)
    aucSeasColor <- rgb(aucSeasRGB[1],aucSeasRGB[2],aucSeasRGB[3], transval)
    chartcols <- c("maroon","blue3","black", aucCPColor, aucSeasColor)
    
    
    plot(x=x, y=yCP, type = "l", col = chartcols[1], lwd = linewidth,
         xlab = "Demand (Units)",
         ylab = "Probability",
         main = "Demand Probability For Next Coverage Period and Remainder of Season",
         cex.main = 1.5,
         cex.axis = 1.25,
         cex.lab = 1.5,
         bty = "n")
    lines(x=x, y=ySeas, type = "l", col = chartcols[2], lwd = linewidth)
    abline(v=optInv, col = chartcols[3], lwd = linewidth, lty = 3)
    polygon(c( x[x>=optInv], optInv ),  c(yCP[x>=optInv],0 ), col = aucCPColor)
    polygon(c( x[x<=optInv], optInv), c(ySeas[x<=optInv],0), col = aucSeasColor)
    text(x=optInv, 
         y = max(yCP)*.5, 
         cex =1.5,
         labels = paste("You should stock\n",
                        formatC(as.integer(optInv), digits = 0),
                        "units"),
         pos = 4)
    legend("topright", c("Next Coverage Period",
                         "Remainder of Season",
                         "Optimal Inventory Level",
                         "Probability of Lost Sales",
                         "Probability of Stranded Units"),
           col = chartcols,
           cex=1.25,
           lty = c(1,1,3,NA,NA), lwd = linewidth, bty = "n")
    
    legend("topright", c("Next Coverage Period",
                         "Remainder of Season",
                         "Optimal Inventory Level",
                         "Probability of Lost Sales",
                         "Probability of Stranded Units"),
           col = chartcols,
           cex=1.25,
           pch = c(NA, NA, NA, 15, 15), pt.cex = 3, bty = "n")

  })
  
  output$CostPlots <- renderPlot({
      #str(parms())
      LSRows <- data.frame(units = (parms()$units), 
                      metric = rep("LS",length(parms()$units)), 
                      value = (parms()$LSCost))
      
      ObsRows <- data.frame(units = as.numeric(parms()$units), 
                       metric = rep("Obs",length(parms()$units)), 
                       value = as.numeric(parms()$obsCost))

      plotTable <- rbind(LSRows, ObsRows)

      plotTable$metric <- factor(plotTable$metric, levels = c("LS","Obs"),
                                    labels = c("Lost Sales","Obsolescence"))
      
      
      #str(plotTable)
      #fill <- c("lightslategrey", "indianred")
      fill <- c("#5F9EA0", "#E1B378")
      
      ggplot() + geom_col(data = plotTable, 
                          aes(x = units, y = value, fill = metric)) +
          geom_point(aes(x=parms()$OptInvLevel, y = parms()$minCost),
                     size = 6, color = "red") +
          theme_bw() +
          theme(panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text=element_text(size=14),
                axis.title=element_text(size=18),
                axis.ticks.length = unit(.25,"cm"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position=c(0.5,0.5), 
                legend.direction="horizontal", 
                legend.title = element_blank(), 
                legend.text=element_text(size=18),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
          scale_fill_manual(values=fill) +
          labs(x="Units", y="Cost") +
          ggtitle("Inventory Costs vs. Stocking Level")
  })
  
  output$help <- renderUI({
      helptext <- HTML('<h1>Inventory Allocation Optimizer</h1>
                        <h3>This tool will tell you the most profitable level of inventory to allocate to a store. "Most profitable" is
                            defined as the level that minimizes the sum of two costs: lost margin due to stockouts, and obsolescence
                            cost at the end of the season due to unsold units.<br><br>
                            Use the sliders to adjust various parameters used to determine the optimal answer:</h3>
                        <ul style="font-size:20px">
                           <li>Retail Margin %: What portion of the retail price of the item represents gross profit?</li>
                           <li>Average Daily Demand: the average number of units sold per day at this location</li>
                           <li>Variability of Demand: the degree to which demand varies from day-to-day (as a % of demand)</li>
                           <li>Time Remaining in Season: the number of weeks left before this item will be taken off the shelf</li>
                           <li>Shipping Frequency: the number of days between arrival of shipments to this location</li>
                           <li>Shipping Time: number of days it takes for ordered goods to arrive at this location from the distribution center</li>
                        </ul>
                        <h3>Assumptions:</h3>
                        <ul style="font-size:20px">
                           <li>Average demand rate is constant over the remainder of the season</li>
                           <li>At the end of the season, any remaining units are discarded, so the obsolescence cost is 100% of the unit cost</li>
                        </ul>
                        <h3>Notes:</h3>
                        <ul style="font-size:20px">
                           <li>There is an interesting interplay between margin % and variability of demand.
                               If you set a very high margin %, then as you increase variability you will see 
                               the recommended stocking level increase. On the other hand, if you set the margin % low,
                               the recommended stocking level will go down as variability goes up. Why do you think that is?</li>
                           <li>The recommended stocking level is not an "order" - what you would order is the difference
                               between the recommended level and the number of units currently held at the store.</li>
                        </ul>
                       
                      ')
  })
})
