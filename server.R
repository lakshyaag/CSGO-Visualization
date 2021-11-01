library(shiny)
library(thematic)

library(tidyverse)
library(ggplot2)
library(scales)
library(janitor)
library(glue)
library(bbplot)
library(ggthemes)
library(patchwork)
library(ggtext)
library(png)
library(ggpubr)


# windowsFonts(Helvetica = "Product Sans")

theme_lox <- function() {
    theme(
        panel.grid.major.x = element_line(size = 0.3, colour = "#cbcbcb"),
        panel.grid.major.y = element_line(size = 0.3, colour = "#cbcbcb"),
        plot.title = element_markdown(
            family = "Helvetica",
            size = 22,
            face = "bold",
            color = "#222222"
        ),
        plot.subtitle = element_text(
            family = "Helvetica",
            size = 16,
            margin = margin(2, 0, 2, 0)
        ),
        plot.caption = element_text(family = "Helvetica", face = "bold"),
        axis.text = element_text(
            family = "Helvetica",
            size = 12,
            color = "#222222"
        ),
        axis.title = element_text(
            family = "Helvetica",
            size = 14,
            color = "#222222"
        ),
        legend.text = element_text(family = "Helvetica", size = 12),
        legend.title = element_text(
            family = "Helvetica",
            size = 14,
            face = "bold"
        ),
        legend.position = "right",
        strip.text = element_text(
            family = "Helvetica",
            size = 12,
            hjust = 0.5
        )
    )
}

caption <- "Data: Demofile | Graphic: @lakshyaag"

# Mapping helper functions ----

mapCoordinates <-
    jsonlite::read_json("maps/map_coords.json", simplifyVector = T)

translateScaleX <- function(usr_x, map_x, scale) {
    x_trans <- (usr_x - map_x) / scale

    return(abs(x_trans))
}

translateScaleY <- function(usr_y, map_y, scale) {
    y_trans <- (map_y - usr_y) / scale

    return(abs(y_trans))
}

grenade_colors <-
    c(
        "Flashbang" = "#5778a4",
        "Smoke Grenade" = "#b8b0ac",
        "HE Grenade" = "#6a9f58",
        "Molotov" = "#d1615d",
        "Decoy Grenade" = "#967662"
    )

grenade_colors_map <-
    c(
        "Flashbang" = "#2c77db",
        "Smoke Grenade" = "#e3e3e3",
        "HE Grenade" = "#4cdb1d",
        "Molotov" = "#c9221c",
        "Decoy Grenade" = "#78513a"
    )


# Plotting functions ----
throwsPlot <- function(map_focus, team, data) {
    throws <- data %>%
        filter(mapName == map_focus) %>%
        filter(throwerTeam == team) %>%
        count(throwerName, grenadeType, sort = TRUE) %>%
        group_by(throwerName) %>%
        mutate(pct_n = prop.table(n)) %>%
        ungroup()

    throws_total <- throws %>%
        group_by(throwerName) %>%
        summarise(total_nades = sum(n)) %>%
        ungroup()

    plot <- throws %>%
        ggplot(aes(y = throwerName, x = pct_n)) +
        geom_col(aes(fill = grenadeType), position = position_stack()) +
        geom_label(
            data = throws_total,
            aes(x = 1, y = throwerName, label = total_nades),
            nudge_x = 0.01,
            family = "Helvetica",
            fontface = "bold"
        ) +
        scale_x_continuous(labels = label_percent()) +
        scale_fill_manual(values = grenade_colors) +
        labs(x = "% of grenades", y = "", fill = "Type of grenade") +
        bbc_style() +
        theme_lox() +
        theme(
            axis.text.y = element_text(face = "bold"),
            legend.position = "bottom"
        )

    return(plot)
}

grenadeBreakdownPlot <- function(teams, map_focus, team, data) {
    if (team == "All") {
        team1 <-
            throwsPlot(map_focus, teams[1], data) + ggtitle(glue("{teams[1]}"))
        team2 <-
            throwsPlot(map_focus, teams[2], data) + ggtitle(glue("{teams[2]}"))

        plot <-
            wrap_plots(
                team1,
                team2,
                guides = "collect",
                nrow = 1,
                ncol = 2
            )
        plot <- plot +
            plot_annotation(
                title = glue(
                    "Grenades thrown on <span style='color:#c44037;'>{map_focus}</span>"
                ),
                # subtitle = subtitle,
                caption = caption,
                theme = theme_lox() + theme(legend.position = "bottom")
            )

        return(plot)
    } else {
        plot <- throwsPlot(map_focus, team, data) +
            plot_annotation(
                title = glue(
                    "Grenades thrown by {team} on <span style='color:#c44037;'>{map_focus}</span>"
                ),
                # subtitle = subtitle,
                caption = caption,
                theme = theme_lox()
            )

        return(plot)
    }
}

grenadeTrajectoryPlot <-
    function(map_focus, player, data) {
        map_x <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(x)
        map_y <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(y)
        scale <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(scale)

        map_image <- readPNG(glue("maps/{map_focus}_light.png"), native = TRUE)

        throw_land_coords <- data %>%
            filter(mapName == map_focus) %>%
            select(starts_with(c("thrower", "grenade"))) %>%
            mutate(throwerMapX = translateScaleX(throwerX, map_x, scale)) %>%
            mutate(throwerMapY = translateScaleY(throwerY, map_y, scale)) %>%
            mutate(grenadeMapX = translateScaleX(grenadeX, map_x, scale)) %>%
            mutate(grenadeMapY = translateScaleY(grenadeY, map_y, scale)) %>%
            mutate(
                throwerSide = case_when(
                    throwerSide == "CT" ~ "Counter-Terrorist",
                    throwerSide == "T" ~ "Terrorist",
                    TRUE ~ throwerSide
                )
            )

        player_grenades_map <- ggplot(
            throw_land_coords %>% filter(throwerName == player),
            aes(
                x = throwerMapX,
                y = throwerMapY,
                xend = grenadeMapX,
                yend = grenadeMapY,
                colour = grenadeType
            )
        ) +
            background_image(map_image) +
            geom_segment(
                arrow = arrow(length = unit(0.15, "cm")),
                lineend = "round",
                linejoin = "mitre",
                size = 1.1,
                alpha = 0.7
            ) +
            scale_color_manual(values = grenade_colors_map) +
            scale_x_continuous(limits = c(0, 1024), expand = c(0, 0)) +
            scale_y_reverse(limits = c(1024, 0), expand = c(0, 0)) +
            labs(color = "Type of grenade") +
            theme_void() +
            theme(
                legend.text = element_text(family = "Helvetica", size = 12),
                legend.title = element_text(
                    family = "Helvetica",
                    size = 14,
                    face = "bold"
                ),
                legend.position = "bottom",
                strip.text = element_text(
                    family = "Helvetica",
                    color = "white",
                    size = 12,
                    hjust = 0.5,
                    vjust = 0.5,
                    face = "bold"
                ),
                strip.background = element_rect(fill = "grey5", colour = NA)
            ) +
            facet_wrap(~throwerSide, scales = "free") +
            plot_annotation(
                title = glue(
                    "Grenade trajectories of <span style='color:#c44037;'>{player}</span> on {map_focus}"
                ),
                # subtitle = subtitle,
                caption = caption,
                theme = theme_lox()
            )

        return(player_grenades_map)
    }





# Server functions start here ----

shinyServer(function(input, output) {
    thematic_shiny()
    
    data <- reactive({
        req(input$dataUpload)
        read_csv(input$dataUpload$datapath) %>%
            mutate(grenadeType = replace(
                grenadeType,
                grenadeType == "Incendiary Grenade",
                "Molotov"
            )) %>%
            filter(grenadeType != "Decoy Grenade")
    })

    maps <-
        reactive({
            data() %>%
                select(mapName) %>%
                unique() %>%
                pull()
        })
    teams <-
        reactive({
            data() %>%
                select(throwerTeam) %>%
                unique() %>%
                pull()
        })
    players <-
        reactive({
            data() %>%
                select(throwerName) %>%
                unique() %>%
                pull()
        })


    output$selectMap_1 <- renderUI({
        req(input$dataUpload)
        selectInput("mapNum_1",
            "Select map",
            maps(),
            selectize = T,
            width = "100%"
        )
    })

    output$selectTeam <- renderUI({
        req(input$dataUpload)
        selectInput(
            "teamNum",
            "Select team",
            c("All", teams()),
            selectize = T,
            width = "100%"
        )
    })


    output$matchGrenadesThrown <- renderPlot({
        req(input$dataUpload)
        grenadeBreakdownPlot(teams(), input$mapNum_1, input$teamNum, data())
    })

    output$selectMap_2 <- renderUI({
        req(input$dataUpload)
        selectInput("mapNum_2",
            "Select map",
            maps(),
            selectize = T,
            width = "100%"
        )
    })

    output$selectPlayer <- renderUI({
        req(input$dataUpload)
        selectInput(
            "playerName",
            "Select player",
            players(),
            selectize = T,
            width = "100%"
        )
    })

    output$grenadeTrajectory <- renderPlot({
        req(input$dataUpload)
        grenadeTrajectoryPlot(input$mapNum_2, input$playerName, data())
    })
})