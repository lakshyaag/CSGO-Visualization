library(shiny)
library(bslib)
library(shinycssloaders)

ui <-
    bootstrapPage(
        theme = bs_theme(
            version = 5,
            base_font = font_google("Lato"),
            primary = "#00796B",
            bootswatch = "zephyr"
        ) %>% bs_add_rules(
            "
            .shiny-input-container {margin: auto}
            .progress {height: 1rem}
            .col-sm-8 {margin-right: auto !important; margin-left: auto !important; width: 83.33% !important}
            "
        ),
        h1(class = "text-center mt-2", "CS:GO grenade visualization"),
        h5(
            class = "text-center mt-2",
            "Upload the file containing grenade data to view the trajectory maps and breakdown."
        ),
        div(
            class = "container-fluid col-6 justify-content-center p-3",
            div(
                class = "card shadow",
                div(
                    class = "card-body text-center d-flex",
                    fileInput(
                        "dataUpload", "Upload .csv file",
                        accept = c(".csv")
                    )
                )
            )
        ),
        div(
            class = "container-fluid col-12",
            mainPanel(div(
                class = "col-12",
                tabsetPanel(
                    id = "output",
                    type = "pills",
                    tabPanel(
                        "Grenade trajectories",
                        fluidRow(
                            column(width = 6, uiOutput("selectMap_2")),
                            column(width = 6, uiOutput("selectPlayer"))
                        ),
                        hr(),
                        plotOutput("grenadeTrajectory", width = "100%") %>% withSpinner()
                    ),
                    tabPanel(
                        "Grenade breakdown",
                        fluidRow(
                            column(width = 6, uiOutput("selectMap_1")),
                            column(width = 6, uiOutput("selectTeam"))
                        ),
                        hr(),
                        plotOutput("matchGrenadesThrown", width = "100%") %>% withSpinner()
                    )
                )
            ))
        ),
        HTML(
            '
            <footer class="footer mt-auto py-2 bg-light">
                <div class="container">
                    <div class="row">
                        <div class="col-6">
                            <span class="text-muted">
                                <p class="text-start">Created by <a href="https://twitter.com/lakshyaag">Lakshya Agarwal</a></p>
                            </span>
                        </div>
                        <div class="col-6">
                            <span class="text-muted">
                                <p class="text-end">Credits to <a href="https://www.hltv.org/">HTLV</a>, <a
                                        href="https://readtldr.gg/simpleradar-download">SimpleRadar</a> and the <a
                                        href="https://github.com/pnxenopoulos/csgo">CSGO parser</a>
                            </span>
                        </div>
                    </div>
                </div>
            </footer>
             '
        )
    )