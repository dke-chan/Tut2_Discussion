#
# Start flipping a coin. Note the results (heads or tails) in the order they appear.
# Keep a tally of the total number of heads and tails you get.
# Stop when you get 1 head and 4 tails. Note that there are three possible scenarios:
#  - You get 1 head and 4 tails in your first 5 flips of the coin
#  - You have more than 1 head and stop as soon as you get the 4th tail
#  - You have more than 4 tails and stop as soon as you get the first head.
# Will you have more heads than tails when you stop?
# (Assume that you will stop flipping the coin as soon as you reach the goal.)
#

library(shiny); library(ggplot2); library(dplyr); library(magrittr)

ui <- fixedPage(
    titlePanel("Tutorial 2 - Discussion Question"),

    sidebarPanel(
        ## Input for the number of simulations
        numericInput("N_sim", "Number of Simulations:", min = 1, value = 100, step = 10),
        ##
        checkboxInput("toAnimate", "Animate"),
        ##
        actionButton("singlePlay", "Simulate", class = "btn btn-primary"),
        hr(),
        ##
        sliderInput("animationSpeed", "Animation Speed (s)", 0.1, 2, 1, round = TRUE, ticks = FALSE)
    ),

    mainPanel(
        h3("Results"),
        plotOutput("donutPlot"),
        hr(),
        div(
            align = "center",
            plotOutput("currentFlip", height = "100px", width = "400px")
        )
    )
)

simulate.experiment <- function(sample.size, no.of.tails) {
    no.of.tails <- no.of.tails[1]
    sample.size <- sample.size[1]

    no.of.heads <- rnbinom(sample.size, no.of.tails, 0.5) # Flips for 'no.of.tails' tails ~ Counting heads

    no.of.tails.ex <- rgeom(sample.size, 0.5) # Flip until the first head ~ Counting tails

    tibble(
        numHeads = ifelse(no.of.heads == 0, 1, no.of.heads),
        numTails = ifelse(no.of.heads == 0, no.of.tails + no.of.tails.ex, no.of.tails))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##
    logfile <- reactiveValues(
        previous.N_sim = 100,
        simulateData = simulate.experiment(0, 4),
        currentlyAnimating = FALSE,
        currentOb = 0,
        targetOb = 0
    )

    ##
    observe(priority = 9000, {
        ##
        invalidateLater(1000 * input$animationSpeed)

        ##
        isolate({
            ##
            req(logfile$currentlyAnimating & logfile$currentOb < logfile$targetOb)

            ##
            logfile$currentOb <- logfile$currentOb + 1

            ##
            if (logfile$currentOb >= logfile$targetOb) {
                logfile$currentlyAnimating <- FALSE
            }
        })
    })

    ##
    output$currentFlip <- renderPlot({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)

        ## Set-up plot area
        par(mar = rep(0, 4), xaxs = "i", yaxs = "i")

        ## Draw the plot "empty"
        plot(0, type = "n", axes = FALSE, xlim = c(0, 5), ylim = c(0, 1))

        ##
        if (logfile$currentlyAnimating) {
            ##
            currentFlip <- slice(logfile$simulateData, logfile$currentOb)
        } else {
            ##
            currentFlip <- slice(logfile$simulateData, n())
        }

        ##
        text(
            x = c(0.5, 2.9), y = rep(0.5, 2), labels = paste(c("Heads:", "Tails:"), currentFlip), cex = 3, pos = 4
        )
    })

    ## Ensure sensible values in the "N_sim" element
    observeEvent(input$N_sim, {
        ## Ensure that the field isn't blank
        if (isTruthy(input$N_sim)) {
            if (input$N_sim < 1 | !is.numeric(input$N_sim)) {
                ## Update to the last sensible value
                updateNumericInput(session, "N_sim", value = logfile$previous.N_sim)
            } else {
                ## Update the last sensible value
                logfile$previous.N_sim <- input$N_sim
            }
        }
    })

    ## Generate data via the "singlePlayer" element
    observeEvent(input$singlePlay, {
        ## Only generate data if we are not in an animation loop
        if (!logfile$currentlyAnimating) {
            ## Check whether we have the animation flag active, if so, turn on the animation functionality and disable input$singlePlay
            if (input$toAnimate) {
                logfile$currentlyAnimating <- TRUE
                logfile$currentOb <- nrow(logfile$simulateData)
                logfile$targetOb <- logfile$currentOb + input$N_sim
            }

            ## Generate the experiment "input$N_sim" times
            simulate.experiment(input$N_sim, 4) %>%
                ## Bind together the newly generated data to the current "logfile$simulateData" object
                {bind_rows(logfile$simulateData, .)} ->
                logfile$simulateData
        }
    })

    ## Visualise the donut plot of the discussion question
    output$donutPlot <- renderPlot({
        ## Only print if we actually simulate data!
        req(nrow(logfile$simulateData) > 0)

        ## Load in the data
        if (logfile$currentlyAnimating) {
            ##
            toPlot <- slice(logfile$simulateData , 1:logfile$currentOb)
        } else {
            ##
            toPlot <- logfile$simulateData
        }

        toPlot %<>% mutate(headsGreater = if_else(numHeads > numTails, "B", "A")) %>%
            group_by(headsGreater) %>%
            summarise(count = n(), .groups = "drop") %>%
            mutate(fraction = count/sum(count),
                   ymax = cumsum(fraction))

        ##
        if (nrow(toPlot) < 2) {
            if (toPlot$headsGreater[1] == "A") {
                toPlot %<>% add_row(headsGreater = "B", count = 0, fraction = 0, ymax = 1, .after = 1)
            } else {
                toPlot %<>% add_row(headsGreater = "A", count = 0, fraction = 0, ymax = 0, .before = 1)
            }
        }

        toPlot$ymin = c(0, head(toPlot$ymax, n = -1))
        toPlot$labelPosition <- (toPlot$ymax + toPlot$ymin) / 2

        ## Plot the two probabilities as a donut plot
        ggplot(toPlot, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = headsGreater)) +
            theme_void() +
            theme(legend.position = "none") +
            geom_rect() +
            labs(caption = "Flip a coin until we observe at least one head and four tails.\nLet X be the event where we observe more heads than tails.") +
            coord_polar(theta = "y") +
            scale_fill_brewer(palette = "Set1") +
            annotate("text", x = 1.75, y = c(0.23, 0.77), label = c("Rho(X^c)", "Rho(X)"), size = 6, parse = TRUE) +
            annotate("text", x = 1.75, y = c(0.28, 0.72), label = as.character(round(toPlot$count/sum(toPlot$count), 4)), size = 6, parse = TRUE) +
            xlim(c(-1, 4))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
