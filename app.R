options(shiny.suppressBootstrapModalPadding = TRUE)  
options(shiny.maxRequestSize = 200 * 1024^2)

library(shiny)
library(shinyWidgets)
library(DBI)
library(RMySQL)
library(jsonlite)
library(RMariaDB)

source("login.R")
source("top_rated_page.R")


DB_HOST <- Sys.getenv("DB_HOST")
DB_PORT <- as.integer(Sys.getenv("DB_PORT", "3306"))
DB_USER <- Sys.getenv("DB_USER")
DB_PASS <- Sys.getenv("DB_PASS")
DB_NAME <- Sys.getenv("DB_NAME")
DB_SSL_CA <- Sys.getenv("DB_SSL_CA")

con <- tryCatch(
  {
    dbConnect(
      RMariaDB::MariaDB(),
      host = DB_HOST,
      port = DB_PORT,
      user = DB_USER,
      password = DB_PASS,
      dbname = DB_NAME,
      ssl.ca = DB_SSL_CA,
      ssl.verify.server.cert = TRUE
    )
  },
  error = function(e) {
    stop(paste0("❌ Failed to connect to Aiven MySQL: ", conditionMessage(e)))
  }
)

cat("✅ Connected to Aiven MySQL successfully\n")

# ======================================================
# UI
# ======================================================
main_ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    

  tags$script(src = "app.js")),
  
  div(
    id = "app-root",
    
    div(
      class = "app-header",
      
      div(
        class = "header-title",
        HTML("🎬 <span>PLAYLIX</span>")
      ),
      
      actionButton(
        "logout_btn",
        "Logout",
        class = "logout-btn"
      )
    ),
    
    div(style = "height: 78px;"),
    div(
      class = "page-nav",
      
      actionButton("go_home", "🏠 Home", class = "page-btn"),
      actionButton("go_top_rated", "⭐ Top Rated", class = "page-btn")
    ),
  
  ),
  uiOutput("add_button_ui"),
  div(
    id = "page-content-wrapper",
    uiOutput("page_content")
  ),
  uiOutput("player_overlay")
)
ui <- fluidPage(
  uiOutput("app_page")
)

# ======================================================
# SERVER
# ======================================================
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE) 
  
  login_server(input, output, session, logged_in)
  output$app_page <- renderUI({
    if (logged_in()) {
      main_ui
    } else {
      login_ui
    }
  })
  
  refresh_trigger <- reactiveVal(0)
  
  current_detail_id <- reactiveVal(NULL)
  current_playing_id <- reactiveVal(NULL)
  current_tv_data    <- reactiveVal(NULL)
  current_season_idx <- reactiveVal(NULL)
  current_episode_idx<- reactiveVal(NULL)
  play_request <- reactiveVal(NULL)
  player_open <- reactiveVal(FALSE)
  resuming_tv <- reactiveVal(FALSE)
  current_page_view <- reactiveVal("home")
  
  # ================= CLICK GUARD (STOP DUPLICATE RUNS) =================
  click_guard <- reactiveValues(store = new.env(parent = emptyenv()))
  
  allow_click <- function(key, cooldown_ms = 800) {
    now <- as.numeric(Sys.time()) * 1000
    
    last <- if (exists(key, envir = click_guard$store, inherits = FALSE)) {
      get(key, envir = click_guard$store, inherits = FALSE)
    } else {
      NA_real_
    }
    
    if (!is.na(last) && (now - last) < cooldown_ms) return(FALSE)
    
    assign(key, now, envir = click_guard$store)
    TRUE
  }
  
  # ================= PAGINATION =================
  items_per_page <- 15
  current_page <- reactiveVal(1)
  
  output$add_button_ui <- renderUI({
    if (current_page_view() == "home") {
      actionButton(
        "add_movie_btn",
        "➕ Add Movie / TV Show",
        class = "add-button"
      )
    } else {
      NULL
    }
  })
  
  fallback_poster <- function(path) {
    if (is.null(path) || !nzchar(trimws(path))) {
      "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRoWcWg0E8pSjBNi0TtiZsqu8uD2PAr_K11DA&s"
    } else {
      path
    }
  }
  
  show_details_modal <- function(mid) {
    
    movies <- load_movies()
    m <- dbGetQuery(
      con,
      paste0("SELECT * FROM movies WHERE id = ", mid)
    )
    req(nrow(m) == 1)
    
    yt_id <- ""
    if (nzchar(m$youtube_trailer)) {
      yt_id <- sub(".*v=([^&]+).*", "\\1", m$youtube_trailer)
    }
    
    showModal(
      modalDialog(
        size = "l",
        easyClose = FALSE,
        footer = modalButton("Dismiss"),
        
        div(
          class = "modal-banner",
          style = paste0(
            "background-image:url('", fallback_poster(m$poster_path), "');",
            "background-position:center top;"
          )
        ),
        
        div(
          class = "modal-body",
          style = "position:relative;",
          
          div(
            class = "details-row",
            
            tags$img(src = fallback_poster(m$poster_path), class = "details-poster"),
            
            div(
              tags$h2(m$title),
              tags$p(paste(m$genre, "•", m$year_released)),
              tags$p(m$description),
              
              actionButton(
                paste0("play_movie_", m$id),
                ifelse(m$type == "Movie", "▶ Play Movie", "▶ Play TV Show"),
                class = "play-btn",
                onclick = "pauseTrailer();"
              ),
              
              actionButton(
                paste0("favorite_", m$id),
                HTML(ifelse(m$favorite == 1, "&#10084;", "&#9825;")),
                class = paste(
                  "favorite-btn",
                  ifelse(m$favorite == 1, "active", "")
                ),
                style = "margin-top: 15px;",   # 👈 adjust ONLY the heart
                onclick = "toggleFavorite(this)"
              )
            )
          ),
          
          if (nzchar(yt_id))
            div(
              class = "trailer-wrapper",
              tags$iframe(
                id = paste0("trailer_iframe_", m$id),
                src = paste0(
                  "https://www.youtube.com/embed/",
                  yt_id,
                  "?enablejsapi=1",
                  "&autoplay=1",
                  "&controls=0",
                  "&rel=0",
                  "&loop=1",
                  "&playlist=", yt_id
                ),
                allow = "autoplay; encrypted-media",
                allowfullscreen = TRUE
              )
            ),
          
          checkboxInput(
            "detail_finished",
            "Finished Watching?",
            value = isTRUE(m$finished == 1)
          ),
          
          conditionalPanel(
            condition = "input.detail_finished == true",
            numericInput(
              "detail_rating",
              "Your Rating (0 – 10)",
              value = ifelse(is.na(m$rating), 0, m$rating),
              min = 0,
              max = 10,
              step = 0.1
            ) |>
              tagAppendAttributes(
                readonly = "readonly",
                onkeydown = "return false;"
              )          ),
          
          div(
            style = "margin-top:30px; display:flex; gap:10px;",
            
            actionButton(
              paste0("edit_movie_", m$id),
              "✏ Edit",
              class = "play-btn",
              style = "background:#444;",
              onclick = "event.stopPropagation();"
            ),
            
            actionButton(
              paste0("delete_movie_", m$id),
              "🗑 Delete",
              class = "play-btn",
              style = "background:#8b0000;",
              onclick = "event.stopPropagation();"
            )
          )
        )
      )
    )
  }
  
  show_select_episode_modal <- function(m, tv_data) {
    
    current_tv_data(tv_data)
    
    showModal(modalDialog(
      title = paste("Select Episode –", m$title),
      size = "m",
      easyClose = TRUE,
      
      selectInput(
        "tv_season_select",
        "Season",
        choices = seq_along(tv_data),
        selected = 1
      ),
      
      selectInput(
        "tv_episode_select",
        "Episode",
        choices = setNames(
          seq_along(tv_data[[1]]$episodes),
          sapply(tv_data[[1]]$episodes, function(e) e$title)
        ),
        selected = 1
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("play_selected_episode", "▶ Play Episode", class = "play-btn")
      )
    ))
  }
  
  force_reopen_player <- function() {
    current_player(NULL)
    selected_episode(NULL)
    invalidateLater(50, session)
  }
  
  load_movies <- reactive({
    refresh_trigger()
    dbGetQuery(con,"SELECT * FROM movies ORDER BY id DESC")
    
  })
  top_rated_server(input, output, session, load_movies)
  
  filtered_movies <- reactive({
    df <- load_movies()
    if (nrow(df) == 0) return(df)
    
    # 🔍 SEARCH
    if (nzchar(input$search_title)) {
      df <- df[grepl(input$search_title, df$title, ignore.case = TRUE), , drop = FALSE]
    }
    
    # 🎬 TYPE
    if (input$filter_type != "All") {
      df <- df[df$type == input$filter_type, , drop = FALSE]
    }
    
    # 🎭 GENRE
    if (input$filter_genre != "All") {
      df <- df[grepl(input$filter_genre, df$genre, ignore.case = TRUE), , drop = FALSE]
    }
    
    # 📅 YEAR
    if (input$filter_year != "All") {
      df <- df[df$year_released == as.numeric(input$filter_year), , drop = FALSE]
    }
    
    # 📌 STATUS (FILTER FIRST)
    if (input$filter_status != "All") {
      
      if (input$filter_status == "Favorites") {
        df <- df[df$favorite == 1, , drop = FALSE]
      }
      
      if (input$filter_status == "Finished Watching") {
        df <- df[df$finished == 1, , drop = FALSE]
      }
      
      if (input$filter_status == "Currently Watching") {
        df <- df[df$currently_watching == 1 & df$finished == 0, , drop = FALSE]
      }
      
      if (input$filter_status == "Unwatched") {
        df <- df[df$finished == 0 & df$currently_watching == 0, , drop = FALSE]
      }
    }
    
    # 🔀 SORT (ALWAYS LAST)
    if (input$sort_order == "year_desc") {
      df <- df[order(df$year_released, decreasing = TRUE), , drop = FALSE]
    }
    
    if (input$sort_order == "year_asc") {
      df <- df[order(df$year_released), , drop = FALSE]
    }
    
    if (input$sort_order == "title_asc") {
      df <- df[order(tolower(df$title)), , drop = FALSE]
    }
    
    if (input$sort_order == "title_desc") {
      df <- df[order(tolower(df$title), decreasing = TRUE), , drop = FALSE]
    }
    
    df
  })
  
  paginated_movies <- reactive({
    df <- filtered_movies()
    if (nrow(df) == 0) return(df)
    
    start <- (current_page() - 1) * items_per_page + 1
    end   <- min(start + items_per_page - 1, nrow(df))
    
    df[start:end, , drop = FALSE]
  })
  
  observeEvent(logged_in(), {
    if (logged_in()) {
      
      # 🔥 RESET ALL PLAYER STATE
      current_detail_id(NULL)
      current_tv_data(NULL)
      current_season_idx(NULL)
      current_episode_idx(NULL)
      selected_episode(NULL)
      current_player(NULL)
      resuming_tv(FALSE)
      
    }
  }, ignoreInit = TRUE)
  
  observeEvent(refresh_trigger(), {
    
    req(logged_in())
    
    movies <- load_movies()
    if (nrow(movies) == 0) return()
    
    # ----- GENRES -----
    genres <- unique(unlist(strsplit(movies$genre, ",")))
    genres <- trimws(genres)
    genres <- genres[genres != ""]
    
    updatePickerInput(
      session,
      "filter_genre",
      choices = c("All", sort(unique(genres))),
      selected = if (!is.null(input$filter_genre)) input$filter_genre else "All"
    )
    
    # ----- YEARS -----
    years <- sort(unique(movies$year_released), decreasing = TRUE)
    
    updatePickerInput(
      session,
      "filter_year",
      choices = c("All", years),
      selected = if (!is.null(input$filter_year)) input$filter_year else "All"
    )
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$logout_btn, {
    
    # 🔥 clear all modal-related state
    current_detail_id(NULL)
    current_player(NULL)
    selected_episode(NULL)
    current_tv_data(NULL)
    current_season_idx(NULL)
    current_episode_idx(NULL)
    
    
    logged_in(FALSE)
    
    showNotification("Logged out successfully", type = "message")
  })
  

  normalize_drive_url <- function(url) {
    if (grepl("drive.google.com", url)) {
      id <- sub(".*?/d/([^/]+).*", "\\1", url)
      return(paste0("https://drive.google.com/file/d/", id, "/preview"))
    }
    url
  }
  
  save_tv_progress <- function(mid = current_playing_id(),
                               s   = current_season_idx(),
                               e   = current_episode_idx()) {
    if (is.null(mid) || is.null(s) || is.null(e)) return()
    
    dbExecute(
      con,
      paste0(
        "UPDATE movies SET ",
        "last_season = ", as.integer(s), ", ",
        "last_episode = ", as.integer(e), ", ",
        "currently_watching = 1 ",
        "WHERE id = ", as.integer(mid), " ",
        "AND finished = 0"
      )
    )
  }
  
  observe({
    session$onSessionEnded(function() {
      current_detail_id(NULL)
    })
  })
  
  observeEvent(current_page_view(), {
    session$sendCustomMessage(
      "pageView",
      current_page_view()
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$go_home, {
    current_page_view("home")
  })
  
  observeEvent(input$go_top_rated, {
    current_page_view("top_rated")
  })
  
  observeEvent(input$close_player, {
    current_detail_id(NULL)
  }, ignoreInit = TRUE)
  
  
  observe({
    movies <- load_movies()
    
    lapply(movies$id, function(mid) {
      observeEvent(input[[paste0("finish_movie_", mid)]], {
        
        showModal(modalDialog(
          title = "Rate This Title",
          easyClose = TRUE,
          
          numericInput(
            "rating_input",
            "Your Rating (0 – 10)",
            value = 8.0,
            min = 0,
            max = 10,
            step = 0.1
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_rating", "Save Rating", class = "btn-success")
          )
        ))
        
        observeEvent(input$save_rating, {
          if (!allow_click(paste0("save_rating_", mid))) return()
          
          rating <- round(as.numeric(input$rating_input), 1)
          
          if (is.na(rating) || rating < 0 || rating > 10) {
            showNotification("Rating must be between 0 and 10", type = "error")
            return()
          }
          
          
          
          dbExecute(
            con,
            paste0(
              "UPDATE movies 
              SET finished = 1,
                rating = ", rating, ",
                currently_watching = 0
              WHERE id = ", mid
            )
          )
          
          removeModal()
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Marked as finished!", type = "message")
          
        }, once = TRUE)
        
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$detail_finished, {
    
    if (!input$detail_finished) {
      updateNumericInput(session, "detail_rating", value = 0)
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$prev_episode, {
    tv <- current_tv_data()
    s  <- current_season_idx()
    e  <- current_episode_idx() - 1
    
    req(tv, s, e >= 1)
    
    ep <- tv[[s]]$episodes[[e]]
    
    current_episode_idx(e)
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
    
    # ✅ updates DB correctly (uses current_playing_id)
    save_tv_progress()
  }, ignoreInit = TRUE)
  
  observeEvent(input$next_episode, {
    tv <- current_tv_data()
    s  <- current_season_idx()
    e  <- current_episode_idx() + 1
    
    req(tv, s, e <= length(tv[[s]]$episodes))
    
    ep <- tv[[s]]$episodes[[e]]
    
    current_episode_idx(e)
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
    
    # ✅ updates DB correctly (uses current_playing_id)
    save_tv_progress()
  }, ignoreInit = TRUE)
  
  # 🔄 Change season inside player
  observeEvent(input$player_season_select, {
    req(current_tv_data())
    
    # 🛑 BLOCK AUTO-RESET DURING RESUME
    if (isTRUE(resuming_tv())) {
      resuming_tv(FALSE)
      return()
    }
    
    s <- as.numeric(input$player_season_select)
    
    current_season_idx(s)
    
    ep <- current_tv_data()[[s]]$episodes[[1]]
    
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
  })
  
  # 🔄 Change episode inside player
  observeEvent(input$player_episode_select, {
    req(current_tv_data(), current_season_idx())
    
    e <- as.numeric(input$player_episode_select)
    
    current_episode_idx(e)
    
    ep <- current_tv_data()[[current_season_idx()]]$episodes[[e]]
    
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
  })
  
  
  
  # 🎬 UPDATE EPISODE LIST WHEN SEASON CHANGES
  output$modal_episode_select_ui <- renderUI({
    req(input$modal_season_select, current_tv_data())
    
    s <- as.numeric(input$modal_season_select)
    episodes <- current_tv_data()[[s]]$episodes
    
    selectInput(
      "modal_episode_select",
      "Episode",
      choices = setNames(
        seq_along(episodes),
        sapply(episodes, function(e) e$title)
      ),
      selected = if (s == current_season_idx())
        current_episode_idx()
      else
        1
    )
  })
  
  # ▶ PLAY SELECTED EPISODE
  observeEvent(input$play_modal_episode, {
    
    req(
      input$modal_season_select,
      input$modal_episode_select,
      current_tv_data()
    )
    
    s <- as.numeric(input$modal_season_select)
    e <- as.numeric(input$modal_episode_select)
    
    ep <- current_tv_data()[[s]]$episodes[[e]]
    
    current_season_idx(s)
    current_episode_idx(e)
    
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
    
    removeModal()
  })
  
  observeEvent(input$panel_season_select, {
    req(current_tv_data())
    
    # 🛑 BLOCK AUTO-RESET DURING RESUME
    if (isTRUE(resuming_tv())) {
      resuming_tv(FALSE)
      return()
    }
    
    s <- as.numeric(input$panel_season_select)
    
    current_season_idx(s)
    current_episode_idx(current_episode_idx())
  })
  
  observeEvent(input$tv_season_select, {
    
    tv <- current_tv_data()
    req(tv, input$tv_season_select)
    
    s <- as.numeric(input$tv_season_select)
    
    updateSelectInput(
      session,
      "tv_episode_select",
      choices = setNames(
        seq_along(tv[[s]]$episodes),
        sapply(tv[[s]]$episodes, function(e) e$title)
      ),
      selected = 1
    )
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$play_selected_episode, {
    
    req(
      input$tv_season_select,
      input$tv_episode_select,
      current_tv_data()
    )
    
    mid <- current_detail_id()
    key <- paste0("play_selected_episode_", mid, "_s", input$tv_season_select, "_e", input$tv_episode_select)
    if (!allow_click(key)) return()
    
    s <- as.numeric(input$tv_season_select)
    e <- as.numeric(input$tv_episode_select)
    
    tv <- current_tv_data()
    ep <- tv[[s]]$episodes[[e]]
    
    # ✅ Update reactive state
    current_season_idx(s)
    current_episode_idx(e)
    
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
    
    current_playing_id(mid)
    save_tv_progress(mid, s, e)
    
    # 🔒 IMPORTANT: DO NOT UNFINISH A FINISHED SHOW
    mid <- current_detail_id()
    m   <- load_movies()[load_movies()$id == mid, ]
    
    if (nrow(m) == 1 && !isTRUE(m$finished == 1)) {
      save_tv_progress()    
      }
    
    refresh_trigger(refresh_trigger() + 1)
    
    # ▶ Open player
    player_open(TRUE)
    
    play_request(NULL)
    invalidateLater(10, session)
    
    play_request(list(
      type = "tv",
      tv_data = tv,
      season = s,
      episode = e,
      episode_data = ep
    ))
    
    removeModal()
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$play_panel_episode, {
    req(
      input$panel_season_select,
      input$panel_episode_select,
      current_tv_data()
    )
    
    # ✅ CLICK GUARD (locks this panel selection)
    mid <- current_detail_id()
    key <- paste0("play_panel_episode_", mid, "_s", input$panel_season_select, "_e", input$panel_episode_select)
    if (!allow_click(key)) return()
    
    s <- as.numeric(input$panel_season_select)
    e <- as.numeric(input$panel_episode_select)
    
    ep <- current_tv_data()[[s]]$episodes[[e]]
    
    current_season_idx(s)
    current_episode_idx(e)
    
    selected_episode(list(
      title = ep$title,
      url   = ep$url
    ))
    current_playing_id(mid)
    save_tv_progress(mid, s, e)
  })
  
  # 🔴 TITLE REQUIRED MESSAGE
  output$new_title_error <- renderUI({
    if (!is.null(input$save_new_movie) &&
        input$save_new_movie > 0 &&
        !is.null(input$new_title) &&
        !nzchar(trimws(input$new_title))) {
      
      div(
        style = "color:#e50914; font-weight:bold; margin-bottom:10px;",
        "⚠ Title is required"
      )
    }
  })
  
  observeEvent(play_request(), {
    
    req(play_request())
    pr <- play_request()
    
    # 🚫 NEVER auto-open TV player here
    if (pr$type == "tv" && isTRUE(resuming_tv())) {
      return()
    }
    
    if (pr$type == "movie") {
      current_player(pr$data)
    }
    
  }, ignoreInit = TRUE)
  
  
  observe({
    movies <- load_movies()
    
    lapply(movies$id, function(mid) {
      observeEvent(input[[paste0("favorite_", mid)]], {
        
        if (!allow_click(paste0("favorite_", mid))) return()
        
        m <- movies[movies$id == mid, ]
        
        new_val <- ifelse(m$favorite == 1, 0, 1)
        
        dbExecute(
          con,
          paste0(
            "UPDATE movies SET favorite = ",
            new_val,
            " WHERE id = ",
            mid
          )
        )
        
        refresh_trigger(refresh_trigger() + 1)
        
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$open_detail, {
    mid <- as.numeric(input$open_detail)
    req(mid)

    if (!allow_click(paste0("open_detail_", mid))) return()
    
    m <- load_movies()[load_movies()$id == mid, ]
    req(nrow(m) == 1)
    
    current_detail_id(mid)
    
    # 🔥 PRELOAD TV DATA HERE (NOT IN MODAL)
    if (m$type == "TV Show" && nzchar(m$video_path)) {
      tv_data <- jsonlite::fromJSON(m$video_path, simplifyVector = FALSE)
      current_tv_data(tv_data)
    } else {
      current_tv_data(NULL)
    }
    
    removeModal()
    show_details_modal(mid)
    
  }, ignoreInit = TRUE)
  
  observeEvent(
    list(
      input$search_title,
      input$filter_type,
      input$filter_genre,
      input$filter_year,
      input$filter_status,
      input$sort_order
    ),
    {
      current_page(1)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$prev_page, {
    current_page(max(1, current_page() - 1))
  })
  
  observeEvent(input$next_page, {
    total_pages <- ceiling(nrow(filtered_movies()) / items_per_page)
    current_page(min(total_pages, current_page() + 1))
  })
  
  output$page_content <- renderUI({
    
    if (current_page_view() == "home") {
      
      tagList(
        
        # 🔍 HOME FILTERS (ONLY HERE)
        div(
          class = "filter-wrapper",
          
          div(class = "filter-title", "Browse Library"),
          
          div(
            class = "filter-search",
            textInput(
              "search_title",
              label = NULL,
              placeholder = "🔍 Search movies or TV shows...",
              width = "100%"
            )
          ),
          
          div(
            class = "filter-row",
            
            div(
              class = "filter-block",
              tags$label("TYPE", class = "filter-label"),
              pickerInput(
                "filter_type",
                label = NULL,
                choices = c("All", "Movie", "TV Show"),
                selected = "All",
                width = "100%"
              )
            ),
            
            div(
              class = "filter-block",
              tags$label("GENRE", class = "filter-label"),
              pickerInput(
                "filter_genre",
                label = NULL,
                choices = "All",
                selected = "All",
                width = "100%"
              )
            ),
            
            div(
              class = "filter-block",
              tags$label("YEAR", class = "filter-label"),
              pickerInput(
                "filter_year",
                label = NULL,
                choices = "All",
                selected = "All",
                width = "100%"
              )
            ),
            
            div(
              class = "filter-block",
              tags$label("SORT", class = "filter-label"),
              pickerInput(
                "sort_order",
                label = NULL,
                choices = c(
                  "Newest → Oldest" = "year_desc",
                  "Oldest → Newest" = "year_asc",
                  "Title A–Z" = "title_asc",
                  "Title Z–A" = "title_desc"
                ),
                selected = "year_desc",
                width = "100%"
              )
            ),
            
            div(
              class = "filter-block",
              tags$label("STATUS", class = "filter-label"),
              pickerInput(
                "filter_status",
                label = NULL,
                choices = c(
                  "All",
                  "Favorites",
                  "Finished Watching",
                  "Currently Watching",
                  "Unwatched"
                ),
                selected = "All",
                width = "100%"
              )
            )
          )
        ),
        
        # 🎬 CONTENT
        uiOutput("poster_grid"),
        uiOutput("pagination_ui")
      )
    }
    
    else if (current_page_view() == "top_rated") {
      top_rated_ui()
    }
  })
  
  # ---------------- POSTER GRID (UNCHANGED) ----------------
  output$poster_grid <- renderUI({
    
    movies <- paginated_movies()
    div(class="movie-grid",
        lapply(seq_len(nrow(movies)), function(i){
          m <- movies[i,]
          tags$div(
            class = "movie-card",
            onclick = sprintf(
              "Shiny.setInputValue('open_detail', %d, {priority: 'event'})",
              m$id
            ),
            tagList(
              tags$img(src = fallback_poster(m$poster_path), class = "movie-poster"),
              
              tagList(
                
                # 👀 WATCHING (HIGHEST PRIORITY)
                if (isTRUE(m$currently_watching == 1) && !isTRUE(m$finished == 1)) {
                  
                  div(
                    class = "watching-badge",
                    "👀 WATCHING"
                  )
                  
                  # ✅ FINISHED
                } else if (isTRUE(m$finished == 1)) {
                  
                  div(
                    class = "finished-badge",
                    HTML(paste0(
                      "✓ FINISHED ⭐ ", m$rating, " /10"
                    ))
                  )
                  
                  # ❤️ FAVORITE ONLY (NOT WATCHING, NOT FINISHED)
                } else if (isTRUE(m$favorite == 1)) {
                  
                  div(
                    class = "favorite-only-badge",
                    HTML("&#10084; FAVORITE")
                  )
              
                  
                },
                
                # ❤️ FLOATING FAVORITE HEART (BOTTOM RIGHT)
                if (isTRUE(m$favorite == 1)) {
                  div(
                    class = "poster-heart",
                    HTML("&#10084;")
                  )
                },
                
                
                # 🎬 MOVIE OVERLAY (UNCHANGED)
                div(
                  class = "movie-overlay",
                  div(class = "movie-title", m$title),
                  div(class = "movie-meta", paste(m$type, "•", m$year_released))
                )
              )
            )
            
          )
          
        })
    )
  })
  
  observeEvent(
    list(input$detail_finished, input$detail_rating),
    {
      mid <- current_detail_id()
      req(mid)
      
      if (isTRUE(input$detail_finished)) {
        
        rating <- as.numeric(input$detail_rating)
        rating_sql <- if (is.na(rating)) "NULL" else round(rating, 1)
        
        dbExecute(
          con,
          paste0(
            "UPDATE movies SET
            finished = 1,
            rating = ", rating, ",
            currently_watching = 0,
            last_season = NULL,
            last_episode = NULL
           WHERE id = ", mid
          )
        )
        
      } else {
        
        dbExecute(
          con,
          paste0(
            "UPDATE movies 
           SET finished = 0,
               rating = NULL
           WHERE id = ", mid
          )
        )
      }
      
      refresh_trigger(refresh_trigger() + 1)
    },
    ignoreInit = TRUE
  )
  
  # ================= DELETE MOVIE =================
  observe({
    movies <- load_movies()
    
    lapply(movies$id, function(mid){
      observeEvent(input[[paste0("delete_movie_", mid)]], {
        
        showModal(modalDialog(
          title = "Confirm Delete",
          "Are you sure you want to delete this title?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_delete_movie", "Delete", class="btn-danger")
          )
        ))
        
        observeEvent(input$confirm_delete_movie, {
          if (!allow_click(paste0("confirm_delete_", mid))) return()
          dbExecute(con, paste0("DELETE FROM movies WHERE id = ", mid))
          removeModal()
          removeModal()
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Deleted successfully", type="message")
        }, once = TRUE)
        
      }, ignoreInit = TRUE)
    })
  })
  
  # ================= EDIT MOVIE (MATCHES ADD UI) =================
  observe({
    movies <- load_movies()
    
    lapply(movies$id, function(mid){
      observeEvent(input[[paste0("edit_movie_", mid)]], {
        
        m <- movies[movies$id == mid, ]
        
        removeModal()  # close Details modal first
        
        showModal(
          modalDialog(
            title = paste("✏ Edit –", m$title),
            size = "l",
            easyClose = TRUE,
            
            fluidRow(
              column(
                6,
                div(
                  class = "modal-section",
                  div(class = "section-title", "📄 Details"),
                  uiOutput("edit_title_error"),
                  textInput("edit_title", "Title", m$title),
                  selectInput("edit_type", "Type", c("Movie", "TV Show"), selected = m$type),
                  numericInput("edit_year", "Year Released", m$year_released, 1900, 2035),
                  textInput("edit_genre", "Genre", m$genre),
                  textAreaInput("edit_desc", "Synopsis", m$description, height = "140px")
                )
              ),
              
              column(
                6,
                div(
                  class = "modal-section",
                  div(class = "section-title", "🎞 Media"),
                  textInput("edit_poster", "Poster Image URL", m$poster_path),
                  uiOutput("edit_poster_preview"),
                  textInput("edit_trailer", "YouTube Trailer URL", m$youtube_trailer),
                  uiOutput("edit_movie_ui"),
                  uiOutput("edit_tvshow_ui")
                )
              )
            ),
            
            uiOutput("edit_save_btn")
          )
        )
        
        
        # 📌 STORE ORIGINAL VALUES
        original_edit <- reactiveValues(
          title  = m$title,
          year   = m$year_released,
          genre  = m$genre,
          type   = m$type,
          desc   = m$description,
          poster = m$poster_path,
          trailer= m$youtube_trailer
        )
        
        edit_changed <- reactive({
          trimws(input$edit_title)  != trimws(original_edit$title)  ||
            input$edit_year           != original_edit$year           ||
            trimws(input$edit_genre)  != trimws(original_edit$genre)  ||
            input$edit_type           != original_edit$type           ||
            trimws(input$edit_desc)   != trimws(original_edit$desc)   ||
            trimws(input$edit_poster) != trimws(original_edit$poster) ||
            trimws(input$edit_trailer)!= trimws(original_edit$trailer)
        })
        
        is_tv <- m$type == "TV Show"
        tv_data <- NULL
        if (isTRUE(is_tv) &&
            !is.null(m$video_path) &&
            nzchar(trimws(m$video_path))) {
          tv_data <- tryCatch(
            jsonlite::fromJSON(m$video_path, simplifyVector = FALSE),
            error = function(e) NULL
          )
        }
        
        output$edit_save_btn <- renderUI({
          if (edit_changed()) {
            actionButton(
              "save_edit_movie_full",
              "💾 Save Changes",
              class = "save-btn"
            )
          } else {
            actionButton(
              "save_edit_movie_full",
              "💾 Save Changes",
              class = "save-btn disabled"
            )
          }
        })
        
        
        output$edit_poster_preview <- renderUI({
          req(input$edit_poster)
          if (!nzchar(input$edit_poster)) return(NULL)
          tags$img(src = input$edit_poster, class = "poster-preview",
                   onerror = "this.style.display='none'")
        })
        
        output$edit_title_error <- renderUI({
          if (!is.null(input$save_edit_movie_full) &&
              input$save_edit_movie_full > 0 &&
              !is.null(input$edit_title) &&
              !nzchar(trimws(input$edit_title))) {
            
            div(
              style = "color:#e50914; font-weight:bold; margin-bottom:10px;",
              "⚠ Title is required"
            )
          }
        })
        
        output$edit_movie_ui <- renderUI({
          if (input$edit_type != "Movie") return(NULL)
          textInput("edit_movie_url", "Movie Video URL", m$video_path)
        })
        
        output$edit_tvshow_ui <- renderUI({
          if (input$edit_type != "TV Show") return(NULL)
          seasons_n <- if (!is.null(tv_data)) length(tv_data) else 1
          tagList(
            numericInput("edit_season_count","Number of Seasons", seasons_n, min=1),
            uiOutput("edit_seasons_ui")
          )
        })
        
        output$edit_seasons_ui <- renderUI({
          req(input$edit_season_count)
          lapply(seq_len(input$edit_season_count), function(s){
            
            ep_n <- if (!is.null(tv_data) && length(tv_data) >= s)
              length(tv_data[[s]]$episodes) else 1
            
            tagList(
              tags$hr(),
              tags$h4(paste("Season", s)),
              numericInput(paste0("edit_season_",s,"_episodes"), "Number of Episodes", ep_n, min=1),
              uiOutput(paste0("edit_season_",s,"_episodes_ui"))
            )
          })
        })
        
        observe({
          req(input$edit_season_count)
          
          for (s in seq_len(input$edit_season_count)) {
            local({
              season <- s
              output[[paste0("edit_season_",season,"_episodes_ui")]] <- renderUI({
                ep_n <- input[[paste0("edit_season_",season,"_episodes")]]
                
                lapply(seq_len(ep_n), function(e){
                  
                  title_val <- ""
                  url_val <- ""
                  
                  if (!is.null(tv_data) &&
                      length(tv_data) >= season &&
                      length(tv_data[[season]]$episodes) >= e) {
                    title_val <- tv_data[[season]]$episodes[[e]]$title
                    url_val <- tv_data[[season]]$episodes[[e]]$url
                  }
                  
                  tagList(
                    textInput(paste0("edit_s",season,"_e",e,"_title"),
                              paste("Episode",e,"Title"), title_val),
                    textInput(paste0("edit_s",season,"_e",e,"_url"),
                              paste("Episode",e,"Video URL"), url_val)
                  )
                })
              })
            })
          }
        })
        
        observeEvent(input$save_edit_movie_full, {
          
          # 🚫 STOP IF NOTHING CHANGED
          if (!edit_changed()) return()
          
          # 🚫 BLOCK SAVE IF TITLE EMPTY
          if (!nzchar(trimws(input$edit_title))) return()
          
          video_db <- NULL
          
          if (input$edit_type == "Movie") {
            video_db <- input$edit_movie_url
          }
          
          if (input$edit_type == "TV Show") {
            seasons <- list()
            for (s in seq_len(input$edit_season_count)) {
              eps <- list()
              ep_n <- input[[paste0("edit_season_", s, "_episodes")]]
              for (e in seq_len(ep_n)) {
                eps[[e]] <- list(
                  title = input[[paste0("edit_s", s, "_e", e, "_title")]],
                  url   = input[[paste0("edit_s", s, "_e", e, "_url")]]
                )
              }
              seasons[[s]] <- list(season = s, episodes = eps)
            }
            video_db <- jsonlite::toJSON(seasons, auto_unbox = TRUE)
          }
          
          dbExecute(con, paste0(
            "UPDATE movies SET ",
            "title=", dbQuoteString(con, input$edit_title), ",",
            "year_released=", input$edit_year, ",",
            "genre=", dbQuoteString(con, input$edit_genre), ",",
            "type=", dbQuoteString(con, input$edit_type), ",",
            "description=", dbQuoteString(con, input$edit_desc), ",",
            "poster_path=", dbQuoteString(con, input$edit_poster), ",",
            "video_path=", dbQuoteString(con, video_db), ",",
            "youtube_trailer=", dbQuoteString(con, input$edit_trailer),
            " WHERE id=", mid
          ))
          
          removeModal()
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Updated successfully", type = "message")
          
        }, once = TRUE)
        
        
      }, ignoreInit = TRUE)
    })
  })
  
  # ================= FULLSCREEN PLAYER OVERLAY =================
  current_player <- reactiveVal(NULL)
  selected_episode <- reactiveVal(NULL)
  
  observe({
    movies <- load_movies()
    
    lapply(movies$id, function(mid){
      observeEvent(input[[paste0("play_movie_", mid)]], {
        
        if (!allow_click(paste0("play_movie_", mid))) return()
        
        m <- movies[movies$id == mid, ]
        
        current_detail_id(mid)
        current_playing_id(mid) 
        
        # 🚫 CONTENT NOT AVAILABLE
        if (
          is.null(m$video_path) ||
          !nzchar(trimws(m$video_path))
        ) {
          showNotification(
            "❌ Content is not yet available",
            type = "warning",
            duration = 3
          )
          return()
        }
        
        # 🎬 MOVIE → OPEN PLAYER ONLY (DO NOT CLOSE DETAIL MODAL)
        if (m$type == "Movie") {
          
          # 🔒 DO NOT override finished movies
          if (!isTRUE(m$finished == 1)) {
            dbExecute(
              con,
              paste0(
                "UPDATE movies SET
         currently_watching = 1
         WHERE id = ", mid
              )
            )
          }
          
          refresh_trigger(refresh_trigger() + 1)
          
          # ✅ CLEAR TV STATE
          current_tv_data(NULL)
          current_season_idx(NULL)
          current_episode_idx(NULL)
          selected_episode(NULL)
          
          player_open(TRUE)
          
          play_request(NULL)
          invalidateLater(10, session)
          
          play_request(list(
            type = "movie",
            data = m
          ))
          
          return()
        }
        
        
        # 📺 TV SHOW
        # 📺 TV SHOW
        tv_data <- jsonlite::fromJSON(m$video_path, simplifyVector = FALSE)
        
        # 🔴 FINISHED → ALWAYS SELECT EPISODE
        if (isTRUE(m$finished == 1)) {
          
          current_tv_data(tv_data)
          show_select_episode_modal(m, tv_data)
          return()
        }
        
        # 🔵 HAS PROGRESS → ALWAYS ASK CONTINUE
        if (!is.na(m$last_season) && !is.na(m$last_episode)) {
          
          showModal(modalDialog(
            title = "Continue Watching?",
            easyClose = TRUE,
            
            div(
              style = "margin-bottom:15px;",
              paste0(
                "Resume from Season ",
                m$last_season,
                ", Episode ",
                m$last_episode,
                "?"
              )
            ),
            
            footer = tagList(
              actionButton(paste0("choose_episode_", mid), "No, choose episode"),
              actionButton(paste0("continue_tv_", mid), "▶ Continue", class = "play-btn")
            )
          ))
          
          # ▶ CONTINUE
          observeEvent(input[[paste0("continue_tv_", mid)]], {
            
            if (!allow_click(paste0("continue_tv_", mid))) return()
            
            resuming_tv(TRUE)
            
            s <- m$last_season
            e <- m$last_episode
            ep <- tv_data[[s]]$episodes[[e]]
            
            current_tv_data(tv_data)
            current_season_idx(s)
            current_episode_idx(e)
            
            selected_episode(list(title = ep$title, url = ep$url))
            
            current_playing_id(mid)
            save_tv_progress(mid, s, e)
            
            player_open(TRUE)
            
            play_request(NULL)
            invalidateLater(10, session)
            
            play_request(list(
              type = "tv",
              tv_data = tv_data,
              season = s,
              episode = e,
              episode_data = ep
            ))
            
            removeModal()
          }, once = TRUE)
          
          # ❌ USER CHOSE MANUAL SELECTION
          observeEvent(input[[paste0("choose_episode_", mid)]], {
            
            # ✅ CLICK GUARD
            if (!allow_click(paste0("choose_episode_", mid))) return()
            
            # 🔴 RESET RESUME INTENT — THIS IS THE FIX
            resuming_tv(FALSE)
            
            removeModal()
            show_select_episode_modal(m, tv_data)
            
          }, once = TRUE)
          
          return()
        }
        
        # 🟢 FIRST TIME EVER → SELECT EPISODE
        show_select_episode_modal(m, tv_data)
        
      }, ignoreInit = TRUE)
      
    })
  })
  
  output$player_overlay <- renderUI({
    
    m  <- current_player()
    ep <- selected_episode()
    
    if (is.null(m) && is.null(ep)) return(NULL)
    
    video_url <- if (!is.null(ep))
      normalize_drive_url(ep$url)
    else
      normalize_drive_url(m$video_path)
    
    div(
      id = "player-overlay",
      style = "position:fixed; inset:0; background:black; z-index:99999;",
      
      # 🎬 Episode title
      if (!is.null(ep)) {
        div(
          class = "episode-title hidden",
          paste0(
            "Episode ",
            current_episode_idx(),
            " • ",
            ep$title
          )
        )
      },
      
      # ⏮ 📺 ⏭ Controls
      if (!is.null(current_tv_data()) &&
          !is.null(current_season_idx()) &&
          !is.null(current_episode_idx())) {
        
        div(
          class = "episode-nav hidden",
          
          actionButton(
            "prev_episode",
            HTML("&#9198;"),
            class = "episode-btn",
            disabled = isTRUE(current_episode_idx() <= 1)
          ),
          
          actionButton(
            "open_episode_modal",
            "Select Ep",
            class = "select-ep-btn hidden"
          ),
          
          actionButton(
            "next_episode",
            HTML("&#9197;"),
            class = "episode-btn",
            disabled = isTRUE(
              current_episode_idx() >=
                length(current_tv_data()[[current_season_idx()]]$episodes)
            )
          )
        )
      },
      
      # 🎯 Select Episode button (TOP RIGHT)
      if (!is.null(current_tv_data())) {
        div(
          class = "select-ep-wrapper hidden",
          actionButton(
            "open_episode_modal",
            "Select Ep",
            class = "select-ep-btn"
          )
        )
      },
      
      
      # ⬅ Back button
      div(
        class = "play-header",
        actionButton("close_player", HTML("&#8592;"), class = "back-btn")
      ),
      
      # 📺 Episode selector panel (INSIDE player overlay)
      if (!is.null(current_tv_data()) &&
          !is.null(current_season_idx()) &&
          !is.null(current_episode_idx())) {
        
        div(
          id = "episode-panel",
          class = "episode-panel hidden",
          
          div(class = "episode-panel-title", "Select Episode"),
          
          selectInput(
            "panel_season_select",
            "Season",
            choices = seq_along(current_tv_data()),
            selected = current_season_idx(),
            width = "100%"
          ),
          
          selectInput(
            "panel_episode_select",
            "Episode",
            choices = setNames(
              seq_along(current_tv_data()[[current_season_idx()]]$episodes),
              sapply(
                current_tv_data()[[current_season_idx()]]$episodes,
                function(e) e$title
              )
            ),
            selected = current_episode_idx(),
            width = "100%"
          ),
          
          actionButton(
            "play_panel_episode",
            "▶ Play",
            class = "play-btn"
          )
        )
      },
      
      
      # 🎥 Video
      div(
        class = "video-wrapper",
        tags$iframe(
          src = video_url,
          allowfullscreen = TRUE,
          style = "width:100%; height:100%; border:none;"
        )
      )
    )
  })
  
  observeEvent(input$close_player, {
    # ✅ Only refresh list if a TV show was active (has tv_data + indices)
    was_tv <- !is.null(current_tv_data()) &&
      !is.null(current_season_idx()) &&
      !is.null(current_episode_idx())
    
    player_open(FALSE)
    current_player(NULL)
    selected_episode(NULL)
    current_tv_data(NULL)
    current_season_idx(NULL)
    current_episode_idx(NULL)
    current_playing_id(NULL)
    
    # ✅ TV-only refresh (movies won't trigger this)
    if (was_tv) {
      refresh_trigger(refresh_trigger() + 1)
    }
    
  }, ignoreInit = TRUE)
  
  
  output$pagination_ui <- renderUI({
    total_items <- nrow(filtered_movies())
    if (total_items == 0) return(NULL)
    
    total_pages <- ceiling(total_items / items_per_page)
    
    div(
      style = "
    display:flex;
    justify-content:center;
    align-items:center;
    gap:12px;
    margin:30px 0;
  ",
      
      actionButton(
        "prev_page",
        "◀ Prev",
        class = "play-btn",
        disabled = current_page() <= 1
      ),
      
      span(
        style = "
      color:white;
      font-weight:bold;
      line-height:1;
      padding:0 6px;
    ",
        paste("Page", current_page(), "of", total_pages)
      ),
      
      actionButton(
        "next_page",
        "Next ▶",
        class = "play-btn",
        disabled = current_page() >= total_pages
      )
    )
  })
  
  
  # ================= ADD MOVIE MODAL =================
  observeEvent(input$add_movie_btn,{
    showModal(modalDialog(
      size="l", easyClose=TRUE,
      
      fluidRow(
        column(
          6,
          div(class="modal-section",
              div(class="section-title","📄 Details"),
              uiOutput("new_title_error"),
              textInput("new_title","Title"),
              selectInput("new_type","Type",c("Movie","TV Show")),
              numericInput("new_year","Year Released",2025,1900,2035),
              textInput("new_genre","Genre"),
              textAreaInput("new_desc","Synopsis",height="140px")
          )
        ),
        
        column(
          6,
          div(class="modal-section",
              div(class="section-title","🎞 Media"),
              textInput("poster_url","Poster Image URL"),
              uiOutput("poster_url_preview"),
              textInput("new_trailer","YouTube Trailer URL"),
              uiOutput("movie_upload_ui"),
              uiOutput("tvshow_section_ui")
          )
        )
      ),
      
      actionButton("save_new_movie","💾 Save Media",class="save-btn")
    ))
  })
  
  output$poster_url_preview <- renderUI({
    req(input$poster_url)
    if (!nzchar(input$poster_url)) return(NULL)
    
    tags$img(
      src = input$poster_url,
      class = "poster-preview",
      onerror = "this.style.display='none'"
    )
  })
  
  output$movie_upload_ui <- renderUI({
    if (is.null(input$new_type) || input$new_type != "Movie") return(NULL)
    textInput("movie_url","Movie Video URL")
  })
  
  output$tvshow_section_ui <- renderUI({
    if (is.null(input$new_type) || input$new_type != "TV Show") return(NULL)
    
    tagList(
      numericInput("season_count","Number of Seasons",1,min=1),
      uiOutput("seasons_ui")
    )
  })
  
  output$seasons_ui <- renderUI({
    req(input$season_count)
    
    lapply(seq_len(input$season_count), function(s){
      tagList(
        tags$hr(),
        tags$h4(paste("Season", s)),
        numericInput(paste0("season_",s,"_episodes"), "Number of Episodes", 1, min=1),
        uiOutput(paste0("season_",s,"_episodes_ui"))
      )
    })
  })
  
  observe({
    req(input$season_count)
    
    for (s in seq_len(input$season_count)) {
      local({
        season <- s
        output[[paste0("season_",season,"_episodes_ui")]] <- renderUI({
          req(input[[paste0("season_",season,"_episodes")]])
          
          lapply(seq_len(input[[paste0("season_",season,"_episodes")]]), function(e){
            tagList(
              textInput(paste0("s",season,"_e",e,"_title"), paste("Episode",e,"Title")),
              textInput(paste0("s",season,"_e",e,"_url"), paste("Episode",e,"Video URL"))
            )
          })
        })
      })
    }
  })
  
  observeEvent(input$save_new_movie,{
    if (!allow_click("save_new_movie")) return()
    
    # 🚫 BLOCK SAVE IF TITLE EMPTY
    if (!nzchar(trimws(input$new_title))) {
      return()
    }
    
    video_db <- NULL
    
    if (input$new_type == "Movie") video_db <- input$movie_url
    
    if (input$new_type == "TV Show") {
      seasons <- list()
      for (s in seq_len(input$season_count)) {
        eps <- list()
        ep_count <- input[[paste0("season_",s,"_episodes")]]
        for (e in seq_len(ep_count)) {
          eps[[e]] <- list(
            title = input[[paste0("s",s,"_e",e,"_title")]],
            url   = input[[paste0("s",s,"_e",e,"_url")]]
          )
        }
        seasons[[s]] <- list(season = s, episodes = eps)
      }
      video_db <- jsonlite::toJSON(seasons, auto_unbox = TRUE)
    }
    
    dbExecute(
      con,
      paste0(
        "INSERT INTO movies (title,year_released,genre,type,description,finished,rating,poster_path,video_path,youtube_trailer) VALUES(",
        dbQuoteString(con, input$new_title), ",",
        input$new_year, ",",
        dbQuoteString(con, input$new_genre), ",",
        dbQuoteString(con, input$new_type), ",",
        dbQuoteString(con, input$new_desc), ",",
        "0,",            # finished = FALSE by default
        "NULL,",         # rating = NULL by default
        dbQuoteString(con, input$poster_url), ",",
        dbQuoteString(con, video_db), ",",
        dbQuoteString(con, input$new_trailer),
        ")"
      )
    )
    
    removeModal()
    refresh_trigger(refresh_trigger()+1)
    showNotification("Saved successfully!",type="message")
  })
}

shinyApp(ui, server)