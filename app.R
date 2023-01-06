# Libraries -------------------------------------------------------------------

library("shiny")
library("ggplot2")
library("dplyr")
library("glue")
library("forcats")
library("stringr")
library("Cairo")

# -----------------------------------------------------------------------------




# Setup -----------------------------------------------------------------------

options(shiny.usecairo = T)

# Load data and relevel model id
merged <- readRDS("merged_posterior_shiny.rds") %>%
  mutate(
    model_id = fct_reorder(model_id, post_mean),
    compelling = if_else(
      compelling == "not compelling",
      true = "Not compelling",
      false = "Compelling"
      ),
    outcome = str_to_title(outcome),
    temporal_window = str_to_sentence(temporal_window),
    operationalisation = str_to_sentence(operationalisation),
    typicality = str_to_sentence(typicality)
  )

# Shorten figure caption
fig_capl1 <- "Posterior estimates from meta-analytic model (color),\n"
fig_capl2 <- "and raw estimates extracted from teams' models (grey)\n"

# Set msa themes
msa_theme <- function() {
  list(
    theme_classic(base_size = 20),
    theme(
      legend.position = "top",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line = element_blank()
    ),
    guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1)))
  )
}

msa_scatter_theme <- function() {
  list(
    theme_minimal(base_size = 20),
    theme(
      legend.position = "top",
      axis.title.y = element_text(size = rel(.9), hjust = 0.95),
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15)
    ),
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))
  )
}

# -----------------------------------------------------------------------------






# Define UI for application that draws a histogram
ui <- navbarPage(
  title = div(img(src = "logo.png", height = 50, style = "margin-top:-20px")),
  fluid = T, windowTitle = "MSA",
  header = tags$head(
    tags$style(
      HTML(
       '.navbar-default .navbar-nav > .active {background-color:white;}
        .navbar-default .navbar-nav > .active > a {background-color: white;}
        .navbar-default .navbar-nav > .active > a:hover {background-color: white;}
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > li > a:hover {
           color:black;
           background-color:white;
           text-decoration
        }
        .navbar {
          background-color: #fff;
          min-height:60px !important;
          padding-top:20px !important;
          padding-bottom:10px !important;
          }'))
  ),

  tabPanel("Overview",
    fluidRow(
      column(1, p("")),
      column(10,
        h3("Description"),
        p("The purpose of this application is to...
          The source code is available on github (see below).
          Lorem ipsum dolor sit amet, consectetur adipisicing
          elit, sed do eiusmod tempor incididunt ut labore et
          dolore magna aliqua. Ut enim ad minim veniam, quis
          nostrud exercitation ullamco laboris nisi ut aliquip
          ex ea commodo consequat. Duis aute irure dolor in
          reprehenderit in voluptate velit esse cillum dolore
          eu fugiat nulla pariatur. Excepteur sint occaecat
          cupidatat non proident, sunt in culpa qui officia
          deserunt mollit anim id est laborum."),
        div(style = "float:right",
          img(src = "ForkingPaths.png",
              width = 500, style = "padding-left:20px")
        ),
        p("The purpose of this application is to...
          The source code is available on github (see below).
          Lorem ipsum dolor sit amet, consectetur adipisicing
          elit, sed do eiusmod tempor incididunt ut labore et
          dolore magna aliqua. Ut enim ad minim veniam, quis
          nostrud exercitation ullamco laboris nisi ut aliquip
          ex ea commodo consequat. Duis aute irure dolor in
          reprehenderit in voluptate velit esse cillum dolore
          eu fugiat nulla pariatur. Excepteur sint occaecat
          cupidatat non proident, sunt in culpa qui officia
          deserunt mollit anim id est laborum.")
      ),
      column(1, p(""))
    ),
    br(), br(), br(),
    fluidRow(
      column(1, p("")),
      column(10,
        wellPanel(
          p(strong("Created by:"),
            tags$a("Joseph V. Casillas", href="https://www.jvcasillas.com"),
            br(),
            strong("Source code:"),
            tags$a("Github", href="https://github.com/jvcasillas/msa/")
          )
        )
      ),
      column(1, p(""))
    )
  ),

  tabPanel("Outcomes",
    fluidRow(
      column(2, p("")),
      column(3,
        div(align = "left",
           selectInput(
             inputId = "color",
             label = "Choose a factor:",
             choices = c(
               "Compelling outcome" = "compelling",
               "Outcome measure" = "outcome",
               "Temporal window" = "temporal_window",
               "Operationalization" = "operationalisation",
               "Typicality" = "typicality"
             )
           )
        )
      ),
      column(3,
        div(align = "left",
          selectInput(
            inputId = "framework",
            label = "Choose a framework:",
            choices = c("Any", "Frequentist", "Bayesian")
          )
        )
      ),
      column(3,
        div(align = "left",
          checkboxInput(
            inputId = "include_submitted",
            label = "Plot submitted effect",
            value = TRUE
          ),
          checkboxInput(
            inputId = "hide_intervals",
            label = "Hide CI",
            value = FALSE
          )
        )
      ),
      column(1, p(""))
    ),
    fluidRow(
      column(1, p("")),
      column(10,
        div(align = "center",
          plotOutput("postPlot", width = "100%", height = "500px")
        )
      ),
      column(1, p(""))
    )
  ),

  tabPanel("Scatterplots",
    fluidRow(
      column(2, p("")),
      column(2,
        selectInput(
          inputId = "sp_y_var",
          label = "Choose effect",
          choices = c(
            "Meta-analytic effect" = "post_mean",
            "Submitted effect" = "estimate"
          )
        )
      ),
      column(3,
        selectInput(
          inputId = "sp_x_var",
          label = "Choose x variable",
          choices = c(
            "Years after PhD" = "years_from_phd",
            "Prior belief" = "prior_belief",
            "Peer rating (acoustic analysis)" = "phon_rating",
            "Peer rating (statistical analysis)" = "stat_rating",
            "Peer rating (overall)" = "all_rating"
          )
        )
      ),
      column(2,
        selectInput(
          inputId = "sp_color_var",
          label = "Choose factor",
          choices = c(
            "None" = "none",
            "Compelling outcome" = "compelling",
            "Outcome measure" = "outcome",
            "Temporal window" = "temporal_window",
            "Operationalization" = "operationalisation",
            "Typicality" = "typicality"
          )
        )
      ),
      column(3,
        div(align = "left",
          checkboxInput(
            inputId = "std_vars",
            label = "Standardize predictor",
            value = TRUE
          ),
          checkboxInput(
            inputId = "add_regression",
            label = "Add regression line",
            value = FALSE
          )
        )
      )
      #column(1, p(""))
    ),
    fluidRow(
      column(1, p("")),
      column(10,
        div(align = "center",
          plotOutput("scatterPlot", width = "100%", height = "500px")
        )
      ),
      column(1, p(""))
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {

  data_post <- reactive({
    if (input$framework == 'Any') {
      merged %>%
        select(post_mean, model_id, estimate, se, lower95, higher95,
               color_var = input$color) %>%
        filter(!is.na(color_var))
    } else if (input$framework == 'Frequentist') {
      merged %>%
        filter(framework == "frequentist") %>%
        select(post_mean, model_id, estimate, se, lower95, higher95,
               color_var = input$color) %>%
        filter(!is.na(color_var))
    } else {
      merged %>%
        filter(framework == "bayesian") %>%
        select(post_mean, model_id, estimate, se, lower95, higher95,
               color_var = input$color) %>%
        filter(!is.na(color_var))
    }
  })

  point_size <- reactive({
    2.90 - (nrow(data_post()) / 100)
  })

  line_size <- reactive({
    2.75 - (nrow(data_post()) / 100)
  })

  stroke_size <- reactive({
    2.50 - (nrow(data_post()) / 100)
  })

  include_submitted <- reactive({
    input$include_submitted
  })

  x_lab <- reactive({
    if (input$framework == "Any") {
      "All models"
    } else if (input$framework == "Frequentist") {
      "Frequentist models"
    } else {
      "Bayesian models"
    }
  })

  output$postPlot <- renderPlot({

    if (include_submitted() == TRUE) {

    ggplot(data_post()) +
      aes(x = model_id, y = post_mean) +
      geom_hline(yintercept = 0, lty = 3, alpha = 0.5) +
      geom_segment(lineend = "round", linewidth = line_size(), alpha = 0.1,
        aes(xend = model_id, y = estimate-1.96*se, yend = estimate+1.96*se)) +
      geom_point(aes(y = estimate), pch = 17, alpha = 0.1, size = point_size()) +
      geom_segment(lineend = "round", linewidth = line_size(), alpha = 0.5,
        aes(y = lower95, yend = higher95, xend = model_id, color = color_var)) +
      geom_point(aes(color = color_var), show.legend = F,
        size = point_size(), pch = 21, stroke = line_size()) +
      scale_x_discrete(expand =  c(0.01,0.01)) +
      labs(caption = glue("{fig_capl1}{fig_capl2}"),
           x = x_lab(),
           y = "Posterior effect size\n") +
      scale_color_viridis_d(name = NULL, begin = 0.15, end = 0.85) +
      msa_theme()
  } else {
    ggplot(data_post()) +
      aes(x = model_id, y = post_mean) +
      geom_hline(yintercept = 0, lty = 3, alpha = 0.5) +
      geom_segment(lineend = "round", linewidth = line_size(), alpha = 0.5,
        aes(y = lower95, yend = higher95, xend = model_id, color = color_var)) +
      geom_point(aes(color = color_var),
        size = point_size(), show.legend = F, pch = 21, stroke = line_size()) +
      scale_x_discrete(expand =  c(0.01,0.01)) +
      labs(x = x_lab(), y = "Posterior effect size") +
      scale_color_viridis_d(name = NULL, begin = 0.15, end = 0.85) +
      msa_theme()
    }
  })

  data_scatter <- reactive({
    if (input$std_vars == FALSE) {
      if (input$sp_color_var == "none") {
        merged %>%
          select(y = input$sp_y_var, x = input$sp_x_var)
      } else {
      merged %>%
        select(y = input$sp_y_var, x = input$sp_x_var,
               color_var = input$sp_color_var) %>%
        filter(!is.na(color_var))
      }
    } else {
      if (input$sp_color_var == "none") {
        merged %>%
          select(y = input$sp_y_var, x = input$sp_x_var) %>%
          mutate(x = (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
      } else {
      merged %>%
        select(y = input$sp_y_var, x = input$sp_x_var,
               color_var = input$sp_color_var) %>%
        filter(!is.na(color_var)) %>%
        mutate(x = (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
      }
    }
  })

  sp_y_lab <- reactive({
    if (input$sp_y_var == "post_mean") {
      "Meta-analytic effect"
    } else {
      "Submitted effect"
    }
  })

  sp_x_lab <- reactive({
    if (input$sp_x_var == "years_from_phd") {
      "Years after PhD"
    } else if (input$sp_x_var == "prior_belief") {
      "Prior belief"
    } else if (input$sp_x_var == "phon_rating") {
      "Peer rating\n(acoustic analysis)"
    } else if (input$sp_x_var == "stat_rating") {
      "Peer rating\n(statistical analysis)"
    } else {
      "Peer rating\n(overall)"
    }
  })

  output$scatterPlot <- renderPlot({

    if (input$add_regression == FALSE) {
      if (input$sp_color_var == "none") {
        ggplot(data_scatter()) +
        aes(x = x, y = y) +
        geom_point(alpha = 0.3, size = 4) +
        labs(y = sp_y_lab(), x = sp_x_lab()) +
        msa_scatter_theme()
      } else {
        ggplot(data_scatter()) +
        aes(x = x, y = y, color = color_var) +
        geom_point(alpha = 0.3, size = 4) +
        labs(y = sp_y_lab(), x = sp_x_lab()) +
        scale_color_viridis_d(name = NULL, begin = 0.15, end = 0.85) +
        msa_scatter_theme()
      }
    } else {
      if (input$sp_color_var == "none") {
        ggplot(data_scatter()) +
        aes(x = x, y = y) +
        geom_point(alpha = 0.3, size = 4) +
        geom_smooth(method = "lm", formula = "y ~ x") +
        labs(y = sp_y_lab(), x = sp_x_lab()) +
        msa_scatter_theme()
      } else {
        ggplot(data_scatter()) +
        aes(x = x, y = y) +
        geom_point(aes(color = color_var), alpha = 0.3, size = 4) +
        geom_smooth(method = "lm", formula = "y ~ x") +
        labs(y = sp_y_lab(), x = sp_x_lab()) +
        scale_color_viridis_d(name = NULL, begin = 0.15, end = 0.85) +
        msa_scatter_theme()
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
