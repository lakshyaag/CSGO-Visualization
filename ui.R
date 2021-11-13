library(magrittr)
library(shiny)
library(bslib)
library(shinycssloaders)

options(spinner.color = "#009688")
options(spinner.type = 7)

ui <-
    navbarPage(
        title = "CSGO Visualization",
        position = "static-top",
        collapsible = TRUE,
        inverse = TRUE,
        windowTitle = "CS:GO Visualization",
        theme = bs_theme(
            version = 5,
            base_font = font_google("Questrial"),
            primary = "#009688",
            bootswatch = "zephyr",
            "navbar-light-bg" = "#009688"
        ) %>% bs_add_rules(
            "
            .shiny-input-container {margin: auto}
            .progress {height: 1rem}
            .col-sm-8 {margin-right: auto !important; margin-left: auto !important; width: 83.33% !important}
            .navbar {font-size: 1rem !important}
            img {padding-bottom: 5rem !important}
            @media (min-width: 992px) { img {padding-bottom: 3rem !important} }
            "
        ),
        
        tabPanel(
            title = "Grenades",
            icon = icon("bomb"),
            div(
                class = "container-fluid col-12",
                h2(class = "text-center mt-2", "CS:GO grenade visualization"),
                h5(
                    class = "text-center mt-2",
                    "Upload the file containing grenade data to view the trajectory maps and breakdown."
                ),
                div(class = "container-fluid col-6 justify-content-center p-3",
                    div(
                        class = "card shadow",
                        div(class = "card-body text-center d-flex",
                            fileInput(
                                "dataUploadGrenades", "Upload .xlsx file",
                                accept = c(".xlsx")
                            ))
                    )),
                div(class = "container-fluid col-12",
                    mainPanel(div(
                        class = "col-12",
                        tabsetPanel(
                            id = "output",
                            type = "pills",
                            tabPanel(
                                "Grenade trajectories",
                                fluidRow(column(width = 6, uiOutput("selectMap_2") %>% withSpinner()),
                                         column(width = 6, uiOutput("selectPlayer") %>% withSpinner())),
                                hr(),
                                fluidRow(column(width = 4, htmlOutput("flashNumEnemy") %>% withSpinner()),
                                         column(width = 4, htmlOutput("flashDuration") %>% withSpinner()),
                                         column(width = 4, htmlOutput("grenadeDmg") %>% withSpinner())),
                                hr(),
                                plotOutput("grenadeTrajectory", width = "100%") %>% withSpinner()
                            ),
                            tabPanel(
                                "Grenade breakdown",
                                fluidRow(column(width = 6, uiOutput("selectMap_1") %>% withSpinner()),
                                         column(width = 6, uiOutput("selectTeam") %>% withSpinner())),
                                hr(),
                                plotOutput("matchGrenadesThrown", width = "100%") %>% withSpinner()
                            )
                        )
                    )))
            )
        ),
        tabPanel(
            title = "Weapons",
            icon = icon("skull"),
            div(
                class = "container-fluid col-12",
                h2(class = "text-center mt-2", "CS:GO weapon heatmap"),
                h5(
                    class = "text-center mt-2",
                    "Upload the file containing kills data to view the heatmaps."
                ),
                div(class = "container-fluid col-6 justify-content-center p-3",
                    div(
                        class = "card shadow",
                        div(class = "card-body text-center d-flex",
                            fileInput(
                                "dataUploadKills", "Upload .csv file",
                                accept = c(".csv")
                            ))
                    )),
                div(class = "container-fluid col-12",
                    mainPanel(div(
                        class = "col-12",
                        tabsetPanel(
                            id = "output",
                            type = "pills",
                            tabPanel(
                                "Weapon heatmap",
                                fluidRow(column(width = 4, uiOutput("selectMap_3") %>% withSpinner()),
                                         column(width = 4, uiOutput("selectWeapon") %>% withSpinner()),
                                         column(width = 4, uiOutput("selectPlayer_2") %>% withSpinner())),
                                hr(),
                                plotOutput("weaponHeatmap", width = "100%") %>% withSpinner()
                            )
                        )
                    )))
            )
        ),
        footer =
            HTML(
                '
            <footer class="footer mt-auto fixed-bottom bg-light">
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
