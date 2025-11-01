library(shiny)
library(ggplot2)
library(ggiraph)
library(cusum)
library(bslib)
library(DT)
library(dplyr)

eg_mort <- readRDS("data/eg_mort.rds")

# note dynamic with racusum_limit_dpcl()

hlim <- 4.6
d.fmt <- "%d/%m/%Y"

cs <- cusum::racusum(eg_mort$risk,
                     eg_mort$died,
                     limit = hlim)

cs$start <- c(0, cs$ct[-nrow(cs)])
cs$ct[cs$signal == 1] <- with(cs[cs$signal == 1,], start + log(2) - log(1 + p))
cs$id <- eg_mort$Id
cs$descr <- with(eg_mort, 
                   ifelse(died, 
                          paste0(
                            "<strong>ID:</strong>", Id,
                            "<br><strong>Admit:</strong>", format(icu_admit, d.fmt),
                            "<br><strong>Died:</strong>", format(icu_disch, d.fmt),
                            "<br><small>(risk of death ", scales::label_percent(accuracy=1)(risk),")</small>"
                        ),
                        NA_character_)
                  )


filterRange <- function(selectedVars) {
  selectedVars <- as.integer(selectedVars)
  return(
    eg_mort %>% 
      select(Id, Risk = risk, Admitted = icu_admit, Discharged = icu_disch) %>% 
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
                       value = c(1,nrow(cs)))
    ),
    navset_card_underline(
      title = "Visualisations",
      nav_panel("CUSUM",girafeOutput("cusumPlot")),
      nav_panel("Selected Cases",DTOutput("selectedCases"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    filteredData <- reactive({
       dta <- cs[input$caseRng[1]:input$caseRng[2],]
       mark <- dta$ct - dta$start >= log(2) - log(1 + input$p_display / 100.0)
       dta$grp <- as.factor(ifelse(mark, cumsum(mark), 0))
       return(dta)
    })
    output$cusumPlot <- renderGirafe({
      dt <- filteredData()
      csum.plot <- ggplot(dt, mapping = aes(x=t, xend=t+1, y=start, yend=ct, data_id=id,
                                        tooltip=descr)) + 
        geom_segment(data = dt[dt$grp=="0",],  color = "black") +
        geom_hline(yintercept = hlim, colour = "red", linewidth=1) +
        theme(aspect.ratio=0.33333) +
        labs(x = "case number", y = "cumulative log likelihood ratio", 
             caption="*alternating pink & blue upticks are to deliniate deaths in proximity and have no other meaning")
      
      for (l in levels(dt$grp)[-1]) {
        indx <- which(dt$grp==l)
        colour <- c("deeppink3", "blue")[(as.integer(l) %% 2) + 1]
        csum.plot <- csum.plot + 
          geom_segment_interactive(data=dt[indx,], color=colour, hover_nearest = TRUE)
      }
      # hover_nearest = TRUE
      
      girafe(ggobj = csum.plot)
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
          ) %>% formatRound(columns = 'Risk', digits = 3)
      # data.frame(ID = rs$id, Admission = format(rs$admit, d.fmt), Discharge = format(rs$disch, d.fmt), Risk = rs$p)
    }, server = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
