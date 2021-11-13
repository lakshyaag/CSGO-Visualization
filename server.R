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
library(RColorBrewer)
library(paletteer)
library(ggpubr)
library(stringr)
library(readxl)

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

getSubtitle <- function(data) {
    matchId <- (data %>% select(matchID) %>% unique() %>% pull())[1]
    subtitleList <-
        str_split(str_remove(matchId, "(-m\\d.*)"), "-", simplify = TRUE)
    subtitle <-
        glue("{str_to_upper(subtitleList[1])} vs. {str_to_upper(subtitleList[3])}")
    return (subtitle)
}

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
        theme(axis.text.y = element_text(face = "bold"),
              legend.position = "bottom")
    
    return(plot)
}

grenadeBreakdownPlot <-
    function(teams, map_focus, team, subtitleVar, data) {
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
                    subtitle = subtitleVar,
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
                    subtitle = subtitleVar,
                    caption = caption,
                    theme = theme_lox()
                )
            
            return(plot)
        }
    }

grenadeTrajectoryPlot <-
    function(map_focus, player, subtitleVar, data) {
        map_x <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(x)
        map_y <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(y)
        scale <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(scale)
        
        map_image <-
            readPNG(glue("maps/{map_focus}_light.png"), native = TRUE)
        
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
            facet_wrap( ~ throwerSide, scales = "free") +
            plot_annotation(
                title = glue(
                    "Grenade trajectories of <span style='color:#c44037;'>{player}</span> on {map_focus}"
                ),
                subtitle = subtitleVar,
                caption = caption,
                theme = theme_lox()
            )
        
        return(player_grenades_map)
    }


flashStats <- function(map_focus, player_focus, data) {
    flashData <- data %>%
        filter(mapName == map_focus) %>%
        filter(attackerName == player_focus) %>%
        group_by(tick, attackerTeam, playerTeam) %>%
        summarise(n = n(), flashDur = sum(flashDuration)) %>%
        filter(attackerTeam != playerTeam) %>%
        ungroup() %>%
        summarise(n = sum(n), flashDur = sum(flashDur))
    
    return(flashData)
}

dmgStats <- function(map_focus, player_focus, data) {
    dmgData <- data %>%
        filter(mapName == map_focus) %>%
        filter(attackerName == player_focus) %>%
        filter(weapon %in% c("HE Grenade", "Molotov", "Incendiary Grenade")) %>%
        summarise(dmg = sum(hpDamageTaken))
    
    return(dmgData)
}

weaponHeatmapPlot <-
    function(map_focus,
             weapon_inspect,
             player_name,
             subtitleVar,
             data) {
        map_image = readPNG(glue("maps/{map_focus}_light.png"), native = TRUE)
        
        map_x <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(x)
        map_y <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(y)
        scale <- mapCoordinates %>%
            filter(map_name == map_focus) %>%
            pull(scale)
        
        if (player_name == "All") {
            kills_weapon <- data %>%
                filter(weapon == weapon_inspect &
                           mapName == map_focus)
        } else {
            kills_weapon <- data %>%
                filter(weapon == weapon_inspect &
                           mapName == map_focus &
                           attackerName == player_name)
        }
        
        kills_coords <- kills_weapon %>%
            select(attackerX, attackerY, mapName, roundNum) %>%
            mutate(attackerX = translateScaleX(attackerX, map_x, scale)) %>%
            mutate(attackerY = translateScaleY(attackerY, map_y, scale))
        
        
        killer_heatmap <- ggplot(kills_coords,
                                 aes(x = attackerX,
                                     y = attackerY)) +
            background_image(map_image) +
            stat_density_2d_filled(
                geom = "polygon",
                contour = TRUE,
                contour_var = 'ndensity',
                h = c(32, 32),
                n = 250,
                aes(fill = (..level..)),
                breaks = seq(0.15, 1, length.out = 7),
                alpha = 0.9
            ) +
            scale_fill_manual(values = c(colorRampPalette(
                paletteer_d(
                    "RColorBrewer::RdYlGn",
                    n = 11,
                    direction = -1
                )
            )(7))) +
            scale_x_continuous(limits = c(0, 1024), expand = c(0, 0)) +
            scale_y_reverse(limits = c(1024, 0), expand = c(0, 0)) +
            guides(fill = F) +
            theme_void() +
            plot_annotation(
                title = if_else(
                    player_name == "All",
                    glue(
                        "Kill heatmap using <span style='color:#c44037;'>{weapon_inspect}</span> on *{map_focus}*"
                    ),
                    glue(
                        "Kill heatmap for <span style='color:#c44037;'>{player_name}</span> using <span style='color:#c44037;'>{weapon_inspect}</span> on *{map_focus}*"
                    )
                ),
                subtitle = subtitleVar,
                caption = caption,
                theme = theme_lox()
            )
        
        return(killer_heatmap)
    }


# Server functions start here ----

shinyServer(function(input, output) {
    # Grenade visualization ----
    
    grenadeData <- reactive({
        req(input$dataUploadGrenades)
        read_xlsx(input$dataUploadGrenades$datapath, sheet = "grenades") %>%
            mutate(grenadeType = replace(
                grenadeType,
                grenadeType == "Incendiary Grenade",
                "Molotov"
            )) %>%
            filter(grenadeType != "Decoy Grenade")
    })
    
    dmgData <- reactive({
        req(input$dataUploadGrenades)
        read_xlsx(input$dataUploadGrenades$datapath, sheet = "damages")
    })
    
    flashData <- reactive({
        req(input$dataUploadGrenades)
        read_xlsx(input$dataUploadGrenades$datapath, sheet = "flashes")
    })
    
    maps <-
        reactive({
            grenadeData() %>%
                select(mapName) %>%
                unique() %>%
                pull()
        })
    teams <-
        reactive({
            grenadeData() %>%
                select(throwerTeam) %>%
                unique() %>%
                pull()
        })
    players <-
        reactive({
            grenadeData() %>%
                select(throwerName) %>%
                unique() %>%
                pull()
        })
    
    subtitleMatchGrenade <- reactive({
        getSubtitle(grenadeData())
    })
    
    
    output$selectMap_1 <- renderUI({
        req(input$dataUploadGrenades)
        selectInput("mapNum_1",
                    "Select map",
                    maps(),
                    selectize = T,
                    width = "100%")
    })
    
    output$selectTeam <- renderUI({
        req(input$dataUploadGrenades)
        selectInput(
            "teamNum",
            "Select team",
            c("All", teams()),
            selectize = T,
            width = "100%"
        )
    })
    
    
    output$matchGrenadesThrown <- renderPlot({
        req(input$dataUploadGrenades)
        
        grenadeBreakdownPlot(
            teams(),
            input$mapNum_1,
            input$teamNum,
            subtitleMatchGrenade(),
            grenadeData()
        )
    }, height = 600)
    
    output$selectMap_2 <- renderUI({
        req(input$dataUploadGrenades)
        selectInput("mapNum_2",
                    "Select map",
                    maps(),
                    selectize = T,
                    width = "100%")
    })
    
    output$selectPlayer <- renderUI({
        req(input$dataUploadGrenades)
        selectInput(
            "playerName",
            "Select player",
            players(),
            selectize = T,
            width = "100%"
        )
    })
    
    output$grenadeTrajectory <- renderPlot({
        req(input$dataUploadGrenades)
        grenadeTrajectoryPlot(input$mapNum_2,
                              input$playerName,
                              subtitleMatchGrenade(),
                              grenadeData())
    }, height = 600)
    
    flashStatsOutput <- reactive({
        flashStats(input$mapNum_2,
                   input$playerName,
                   flashData())
    })
    
    dmgStatsOutput <- reactive({
        dmgStats(input$mapNum_2,
                   input$playerName,
                   dmgData())
    })
    
    output$flashNumEnemy <- renderUI({
        req(flashData())
        HTML(glue(
            '
                <div class="card text-white bg-primary mx-1">
                        <div class="card-body">
                                <p class="card-text display-6">{flashStatsOutput()$n}</p>
                                <p class="card-text">Enemies flashed</p>
                        </div>
                    </div>
             ')
            )
        
    })
    
    output$flashDuration <- renderUI({
        req(flashData())
        HTML(glue(
            '
                <div class="card text-white bg-info mx-1">
                        <div class="card-body">
                                <p class="card-text display-6">{round(flashStatsOutput()$flashDur, 1)}</p>
                                <p class="card-text">Total duration flashed (seconds)</p>
                        </div>
                    </div>
             ')
        )
        
    })
    
    output$grenadeDmg <- renderUI({
        req(dmgData())
        HTML(glue(
            '
                <div class="card text-white bg-danger mx-1">
                        <div class="card-body">
                                <p class="card-text display-6">{dmgStatsOutput()$dmg}</p>
                                <p class="card-text">Total grenade damage</p>
                        </div>
                    </div>
             ')
        )
        
    })
    
    # Weapon heatmap ----
    
    weaponData <- reactive({
        req(input$dataUploadKills)
        read_csv(input$dataUploadKills$datapath)
    })
    
    weaponMaps <-
        reactive({
            weaponData() %>%
                select(mapName) %>%
                unique() %>%
                pull()
        })
    
    output$selectMap_3 <- renderUI({
        req(input$dataUploadKills)
        selectInput(
            "mapNum_3",
            "Select map",
            weaponMaps(),
            selectize = T,
            width = "100%"
        )
    })
    
    weaponList <-
        reactive({
            weaponData() %>%
                filter(mapName == input$mapNum_3) %>%
                select(weapon) %>%
                unique() %>%
                pull()
        })
    
    output$selectWeapon <- renderUI({
        req(input$dataUploadKills)
        selectInput(
            "weaponName",
            "Select weapon",
            weaponList(),
            selectize = T,
            width = "100%"
        )
    })
    
    playerList <-
        reactive({
            weaponData() %>%
                filter(mapName == input$mapNum_3 &
                           weapon == input$weaponName) %>%
                select(attackerName) %>%
                unique() %>%
                pull()
        })
    
    output$selectPlayer_2 <- renderUI({
        req(input$dataUploadKills)
        selectInput(
            "playerName_2",
            "Select player",
            c("All", playerList()),
            selectize = T,
            width = "100%"
        )
    })
    
    subtitleMatchWeapons <- reactive({
        getSubtitle(weaponData())
    })
    
    output$weaponHeatmap <- renderPlot({
        req(input$dataUploadKills)
        weaponHeatmapPlot(
            input$mapNum_3,
            input$weaponName,
            input$playerName_2,
            subtitleMatchWeapons(),
            weaponData()
        )
    }, height = 600)
    
    
    
})