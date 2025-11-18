library(shiny)
library(ggplot2)
library(ggiraph)
# library(cusum)
library(bslib)
library(DT)
library(dplyr)
library(shinycssloaders)
library(Rcpp)

eg_mort <- readRDS("data/eg_mort.rds")
Rcpp::sourceCpp(file="./ewmaRcpp.cpp")

# todo dynamic limits available with racusum_limit_dpcl()

hlim <- 4.6
odds <- 2
d.fmt <- "%d/%m/%Y"

cs <- cusumRcpp(eg_mort$risk,
                     eg_mort$died > 0,
                     odds,
                     hlim)
cs$vlad <- cumsum(eg_mort$risk - eg_mort$died)
cs$risk <- eg_mort$risk
cs$died <- eg_mort$died
cs$vlad_l <- cs$vlad + (cs$doubling.finish - hlim) / log(odds)
cs$vlad_u <- cs$vlad + (cs$halving.finish + hlim) / log(odds)
triggers <- list(
  up = cs[cs$halving.finish <= -hlim, c("sequence", "vlad_u")],
  down = cs[cs$doubling.finish >= hlim,  c("sequence", "vlad_l")]
)

cs$id <- eg_mort$Id
cs$descr <- with(eg_mort, 
                   ifelse(died, 
                          paste0(
                            "<em>ID:</em>", Id,
                            "<br><em>Admit:</em>", format(icu_admit, d.fmt),
                            "<br><em>Died:</em>", format(icu_disch, d.fmt),
                            "<br><small>",dx ,"</small>",
                            "<br><small>(risk of death ", scales::label_percent(accuracy=1)(risk),")</small>"
                        ),
                        NA_character_)
                  )


filterRange <- function(selectedVars) {
  selectedVars <- as.integer(selectedVars)
  return(
    eg_mort %>% 
      select(Id, Risk = risk, Admitted = icu_admit, Died = icu_disch, Diagnosis = dx) %>% 
      filter(Id %in% selectedVars)
  )
}


# Define UI for application that draws a histogram
ui <- page_sidebar(
    
    # Application title
    title = "CUSUM For Investigation",

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(
        sliderInput("p_display",
                    "Risk of death less than (%):",
                    min = 1,
                    max = 90,
                    value = 50),
        sliderInput("caseRng", "Case Range:", 
                       min = 1, 
                       max = nrow(cs),
                       value = c(1,nrow(cs))),
        sliderInput("lambda", "Exponential weight λ:", 
                    min = 0.001, 
                    max = 0.05,
                    value = 0.01)
    ),
    navset_card_underline(
      title = "Visualisations",
      nav_panel("CUSUM",
                shinycssloaders::withSpinner(
                  girafeOutput("cusumPlot"))
                ),
      nav_panel("Selected Cases",
                shinycssloaders::withSpinner(
                  DTOutput("selectedCases"))
                ),
      nav_panel("EWMA",
                shinycssloaders::withSpinner(
                  plotOutput("ewmaPlot"))
                ),
      nav_panel("VLAD",
                shinycssloaders::withSpinner(
                  plotOutput("vladPlot"))
                )
    )
)

server <- function(input, output) {
    filteredCUSUMData <- reactive({
       dta <- cs[input$caseRng[1]:input$caseRng[2],]
       mark <- dta$died & (dta$risk <= input$p_display / 100.0)
       dta$grp <- as.factor(ifelse(mark, cumsum(mark), 0))
       return(dta)
    })
    
    filteredTriggers <- reactive({
      return(list(
        up = triggers$up[triggers$up$sequence >=  input$caseRng[1] & triggers$up$sequence <=  input$caseRng[2],],
        down = triggers$down[triggers$down$sequence >=  input$caseRng[1] & triggers$down$sequence <=  input$caseRng[2],]
      ))
    })

    filteredEWMAData <- reactive({
      runIn <- 500 # about 500 admissions every 6 months
      minDisplay <- 25
      start <- max(runIn + 1, min(input$caseRng[1], nrow(eg_mort) - minDisplay))
      end <- max(input$caseRng[2], start + minDisplay)
      priorMort <- mean(eg_mort$died[(start - runIn):start])
      subset <- eg_mort[start:end, c("died", "risk", "t")]
      pred <- c(priorMort, subset$risk)
      pred <- ewmaRcpp(pred, input$lambda)
      sePred <- ewmaSERcpp(pred * (1 - pred), input$lambda)
      data.frame(
        observed = ewmaRcpp(c(priorMort, subset$died), input$lambda),
        ci99ub = pred + 2.576 * sePred,
        ci95ub = pred + 1.96 * sePred,
        ci95lb = pmax(0, pred - 1.96 * sePred),
        ci99lb = pmax(0, pred - 2.576 * sePred),
        sequence = c(start-1, subset$t)
      )
    })
    output$cusumPlot <- renderGirafe({
      dt <- filteredCUSUMData()
      csum.plot <- ggplot(dt, mapping = aes(x=sequence, xend=sequence+1, y=doubling.start, yend=doubling.finish, data_id=id,
                                        tooltip=descr)) + 
        geom_segment(data = dt[dt$grp=="0",],  color = "black") +
        geom_hline(yintercept = hlim, colour = "red", linewidth=1) +
        geom_hline(yintercept = 2.9, colour = "orange", linewidth=0.75) +
        theme(aspect.ratio=0.33333) +
        labs(x = "case number", y = "cumulative log likelihood ratio", 
             caption="*alternating pink & blue upticks are to deliniate deaths in proximity and have no other meaning")
      
      for (l in levels(dt$grp)[-1]) {
        indx <- which(dt$grp==l)
        colour <- c("deeppink3", "blue")[(as.integer(l) %% 2) + 1]
        csum.plot <- csum.plot + 
          geom_segment_interactive(data=dt[indx,], color=colour, hover_nearest = TRUE)
      }
      
    # from girafe help: width_svg, height_svg: The width and height of the graphics region in inches. The default values are 6 and 5 inches. 
    # This will define the aspect ratio of the graphic as it will be used to define viewbox attribute of the SVG result.
    # we want the width the same but smaller height to fit a useful aspect ratio for a wide plot such as this
    girafe(ggobj = csum.plot, height_svg=3.05)
    })
    output$selectedCases <- renderDT({
      DT::datatable(filterRange(input$cusumPlot_selected), 
        class = 'cell-border stripe',
        rownames = FALSE,
        extensions = c("Buttons", "Select"),
        selection = 'none',
        options = 
          list(
            select = TRUE,
            dom = "Bt",  ##  remove f to remove search  ## Brftip
            buttons = list(
              list(
                extend = "copy",
                text = 'Copy'#,
                #exportOptions = list(modifier = list(selected = TRUE))
              )
            )
          )) %>% formatStyle(
            0,
            target = "row",
            fontWeight = styleEqual(1, "bold")
          )  %>% 
       formatRound(columns = 'Risk', digits = 3) #  %>% 
      #  formatDate(columns = c('Admitted', 'Died'))
      # data.frame(ID = rs$id, Admission = format(rs$admit, d.fmt), Discharge = format(rs$disch, d.fmt), Risk = rs$p)
    }, server = FALSE)
    output$ewmaPlot <- renderPlot(
      ggplot(data = filteredEWMAData()) + 
        geom_line(aes(x=sequence, y = ci95lb), colour="#d7d7d7") +
        geom_line(aes(x=sequence, y = ci95ub), colour="#d7d7d7") +
        geom_line(aes(x=sequence, y = ci99lb), colour="#2f2f7e") +
        geom_line(aes(x=sequence, y = ci99ub), colour="#2f2f7e") +
        geom_line(aes(x=sequence, y = observed), linewidth=1, colour="#ff0000") +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'))
    )
    output$vladPlot <- renderPlot({
      trigs <- filteredTriggers()
      ggplot(data = filteredCUSUMData()) +
        geom_line(aes(x=sequence, y=vlad), color="black") +
        geom_line(aes(x=sequence, y=vlad_l), colour="red") +
        geom_point(data= trigs$down, aes(x=sequence, y=vlad_l), colour = "red") +
        geom_line(aes(x=sequence, y=vlad_u), colour="green") +
        geom_point(data= trigs$up, aes(x=sequence, y=vlad_u), colour = "green") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




