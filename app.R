library(shiny)
library(bslib)
library(dplyr)
library(stringr)

source("scraping.R")

# ==============================================================================
# 1. UI
# ==============================================================================

theme_dfco <- bs_theme(
  bg = "#07101f",        
  fg = "#F8F9FA", 
  "navbar-bg" = "#d40028",
  primary = "#07101f",   # Le rouge du logo du DFCO
  secondary = "#d40028", # 
  success = "#00C853",   # Vert vif pour les stats positives
  danger = "#FF1744",    # Rouge vif pour les alertes
  base_font = font_google("Montserrat"),    # Police
  heading_font = font_google("Montserrat")  # Police
)

ui <- page_navbar(
  
  theme = theme_dfco,
  id = "mes_onglets",
  title = tags$span(
    tags$img(
      src = "https://files.memberz.fr/dfco/logo_starter.png", 
      height = "80px", 
      style = "margin-right: 10px;"
    ), 
    "Scout FRANCE Data Center | DFCO"
  ),
  
  # --- ONGLET 1 : RECHERCHE AVANCEE  ---
  nav_panel("🔍 Recherche Avancée",
            
            # 1. Injection du CSS pour le design des cartes (les "bulles")
            tags$head(tags$style(HTML("
    /* La grille qui s'adapte à la taille de l'écran */
    .grid-container {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
      gap: 20px;
      padding: 10px;
    }
    /* Le design de la carte joueur */
    .player-card {
      background-color: #f8f9fa;
      border: 1px solid #e0e0e0;
      border-radius: 12px;
      padding: 15px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.05);
      transition: transform 0.2s;
    }
    .player-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.1);
    }
    /* L'en-tête de la carte (Photo/Initiales + Nom) */
    .player-header {
      display: flex;
      align-items: center;
      margin-bottom: 15px;
    }
    .player-initials {
      background-color: #d40028; /* Rouge DFCO */
      color: white;
      border-radius: 50%;
      width: 45px;
      height: 45px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-weight: bold;
      font-size: 1.1em;
      margin-right: 15px;
    }
    .player-info h5 { margin: 0; font-weight: bold; color: #333; font-size: 1.1em; }
    .player-info p { margin: 0; color: #6c757d; font-size: 0.85em; }
    .club-badge {
      background-color: #e9ecef;
      color: #07101f;
      padding: 3px 8px;
      border-radius: 4px;
      font-size: 0.8em;
      font-weight: bold;
      display: inline-block;
      margin-top: 5px;
    }
    /* La petite barre de stats en bas de la carte */
    .player-stats {
      display: flex;
      justify-content: space-between;
      background-color: #ffffff;
      border-radius: 8px;
      padding: 10px;
      border: 1px solid #eeeeee;
    }
    .stat-box { text-align: center; width: 25%; }
    .stat-box strong { display: block; font-size: 1em; color: #212529; }
    .stat-box span { font-size: 0.7em; color: #6c757d; text-transform: uppercase; }
  "))),
            
            # 2. Le Layout (Barre de filtres à gauche, Résultats à droite)
            sidebarLayout(
              
              sidebarPanel(
                width = 3,
                h4("Filtres", style="margin-bottom: 20px; font-weight: bold;"),
                
                
                selectInput("filtre_poste", "POSTE PRINCIPAL", 
                            choices = c("Tous"), 
                            selected = "Tous"),
                
                selectInput("filtre_niveau", "NIVEAU (Saison Actuelle)", 
                            choices = c("Tous","N2", "N3", "U19 NAT"), 
                            selected = "Tous"),
                
                sliderInput("filtre_age", "TRANCHE D'ÂGE", 
                            min = 15, max = 40, value = c(16, 40), step = 1),
                
                sliderInput("filtre_minutes", "MINUTES JOUÉES (Saison Actuelle)", 
                            min = 0, max = 3500, value = 900, step = 50,post = "min"),
                
                sliderInput("filtre_titu", "TAUX DE TITULARISATIONS (%)", 
                            min = 0, max = 100, value = c(0, 100), post = "%")
              ),
              
              mainPanel(
                width = 9,
                # Un petit texte pour afficher le nombre de résultats trouvés
                h4(textOutput("nb_resultats_texte"), style="margin-bottom: 20px; color: #f8f9fa;"),
                
                # C'est ici que la grille de cartes HTML va s'afficher
                uiOutput("grille_joueurs_ui")
              )
            )
  ),
  
  # --- ONGLET 2 : FICHE JOUEUR  ---
  nav_panel("👤 Fiche Joueur",
            sidebarLayout(
              sidebarPanel(
                hr(),
                h4("Rechercher"),
                selectizeInput("choix_joueur", "Sélectionnez un joueur :", choices = NULL, options = list(placeholder = 'Tapez un nom...'))
              ),
              
              mainPanel(
                uiOutput("fiche_joueur_ui")
              )
            )
  )
)

# ==============================================================================
# 2. SERVEUR 
# ==============================================================================

server <- function(input, output, session) {
  
  df <- read.csv2("https://raw.githubusercontent.com/WaImad/Scouting_France/refs/heads/main/resultats_scraping.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  df$Nom_Complet <- paste(df$PRENOMS, df$NOMS)
  
  ## Remplir le menu déroulant (Fiche Joueur)
  observe({
    updateSelectizeInput(session, "choix_joueur", choices = df$Nom_Complet, server = TRUE)
  })
  
  # ============================================================================
  # MOTEUR DE L'ONGLET 1 : RECHERCHE AVANCÉE (GRILLE)
  # ============================================================================
  
  # 1. Filtrage dynamique de la base de données
  donnees_filtrees <- reactive({
    req(df)
    
    donnees <- df
    
    # Création d'une colonne Age via l'année de naissance
    annee_naissance <- as.numeric(str_extract(donnees$Date_Naissance, "[0-9]{4}"))
    donnees$Age <- as.numeric(format(Sys.Date(), "%Y")) - annee_naissance
    
    # Filtre Poste
    if (input$filtre_poste != "Tous") {
      donnees <- donnees |> filter(grepl(input$filtre_poste, Position_Principale, ignore.case = TRUE))
    }
    
    # Filtre Niveau (Basé sur la saison 25_26 par exemple)
    if (input$filtre_niveau != "Tous") {
      donnees <- donnees |> filter(Division_En_Cours == input$filtre_niveau)
    }
    
    # Filtre Âge
    donnees <- donnees |> filter(Age >= input$filtre_age[1] & Age <= input$filtre_age[2] | is.na(Age))
    
    # Filtre Minutes
    donnees <- donnees |> filter(Temps_de_jeu_En_Cours >= input$filtre_minutes)
    
    # Filtre Taux Titularisation (Transformation du % en valeur de 0 à 100)
    donnees <- donnees |> filter(
      (Pourcentage_titu_En_Cours * 100) >= input$filtre_titu[1] & 
        (Pourcentage_titu_En_Cours * 100) <= input$filtre_titu[2] | is.na(Pourcentage_titu_En_Cours)
    )
    
    return(donnees)
  })
  
  # 2. Affichage du nombre de résultats
  output$nb_resultats_texte <- renderText({
    paste(nrow(donnees_filtrees()), "joueurs correspondants")
  })
  
  # 3. Création des cartes HTML
  output$grille_joueurs_ui <- renderUI({
    data_grid <- donnees_filtrees()
    
    if (nrow(data_grid) == 0) {
      return(h4("Aucun joueur ne correspond à vos critères de recherche.", style="color:red;"))
    }
    
    cartes <- lapply(1:nrow(data_grid), function(i) {
      joueur <- data_grid[i, ]
      
      # On vérifie si la photo existe et n'est pas "Non trouvée"
      photo_valide <- !is.na(joueur$PHOTOS) && joueur$PHOTOS != "" && !grepl("https://img.a.transfermarkt.technology/portrait/header/default.jpg?lm=1", joueur$PHOTOS, ignore.case = TRUE)
      
      if (photo_valide) {
        # Si on a une vraie photo, on génère la balise <img>
        visuel_html <- paste0('<img src="', joueur$PHOTOS, '" class="player-photo">')
      } else {
        # Si pas de photo, on génère la bulle rouge avec les initiales
        init_prenom <- ifelse(is.na(joueur$PRENOMS) | joueur$PRENOMS == "", "", substr(joueur$PRENOMS, 1, 1))
        init_nom <- ifelse(is.na(joueur$NOMS) | joueur$NOMS == "", "", substr(joueur$NOMS, 1, 1))
        initiales <- toupper(paste0(init_prenom, init_nom))
        if(initiales == "") initiales <- "?"
        
        visuel_html <- paste0('<div class="player-initials">', initiales, '</div>')
      }
      
      # Nettoyage affichage
      age_display <- ifelse(is.na(joueur$Age), "Âge inconnu", paste(joueur$Age, "ans"))
      buts_display <- ifelse(is.na(joueur$Buts_En_Cours), 0, joueur$Buts_En_Cours)
      passes_display <- ifelse(is.na(joueur$Passes_En_Cours), 0, joueur$Passes_En_Cours)
      
      # Formatage du pourcentage pour l'affichage (ex: 0.75 -> 75%)
      titu_display <- ifelse(is.na(joueur$Pourcentage_titu_En_Cours), "0%", paste0(round(joueur$Pourcentage_titu_En_Cours * 100), "%"))
      
      HTML(paste0('
        <div class="player-card">
          <div class="player-header">
            ', visuel_html, ' <div class="player-info">
              <h5>', joueur$PRENOMS, ' ', joueur$NOMS, '</h5>
              <p>', joueur$Position_Complete, ' | ', age_display, '</p>
              <div class="club-badge">', joueur$Club_actuel, ' - ', ifelse(is.na(joueur$Division_En_Cours) | joueur$Division_En_Cours == "", "N/A", joueur$Division_En_Cours), '</div>
            </div>
          </div>
          <div class="player-stats">
            <div class="stat-box"><strong>', titu_display, '</strong><span>Titu</span></div>
            <div class="stat-box"><strong>', joueur$Temps_de_jeu_En_Cours, '</strong><span>Minutes</span></div>
            <div class="stat-box"><strong>', buts_display, '</strong><span>Buts</span></div>
            <div class="stat-box"><strong>', passes_display, '</strong><span>Passes D</span></div>
          </div>
        </div>
      '))
    })
    
    div(class = "grid-container", cartes)
  })
  observe({
    # 1. On récupère tous les postes uniques de la base de données
    postes_uniques <- unique(df$Position_Principale)
    
    
    # 2. On nettoie : on enlève les cases vides ou les NA s'il y en a
    postes_uniques <- postes_uniques[!is.na(postes_uniques) & postes_uniques != ""]
    
    # 3. On trie par ordre alphabétique pour que ce soit joli
    postes_uniques <- sort(postes_uniques)
    
    # 4. On met à jour le menu déroulant dans l'UI en ajoutant "Tous" tout en haut
    updateSelectInput(session, "filtre_poste", 
                      choices = c("Tous", postes_uniques), 
                      selected = "Tous")
  })
  
  ## Remplir le menu déroulant (Fiche Joueur)
  observe({
    updateSelectizeInput(session, "choix_joueur", choices = df$Nom_Complet, server = TRUE)
  })
  
  # ============================================================================
  # MOTEUR DE L'ONGLET 2 : FICHE JOUEUR
  # ============================================================================
  
  output$fiche_joueur_ui <- renderUI({
    req(input$choix_joueur)
    
    joueur <- df[df$Nom_Complet == input$choix_joueur, ]
    
    # 1. STYLE CSS SUR-MESURE (Pour recréer le style Excel)
    css_excel <- tags$style("
        .excel-table { width: 100%; border-collapse: collapse; font-family: 'Montserrat', sans-serif; font-size: 13px; background-color: white; }
        .excel-table td { border: 1px solid #d3d3d3; padding: 4px 8px; vertical-align: middle; text-align: center; color: black; }
        .excel-bold { font-weight: bold; }
        .bg-red { background-color: #d40028; color: white; }
        .bg-yellow { background-color: #ffff00; font-weight: bold; }
        .bg-red-alert { background-color: #ff0000; color: white; font-weight: bold; }
        .border-dark-thick { border: 2px solid #007bff !important; }
        .text-blue-italic { color: #007bff; font-style: italic; }
      ")
    
    tagList(
      css_excel,
      
      # =========================================================
      # BLOC 1 : L'EN-TÊTE ROUGE (Photo, Noms, Infos Club)
      # =========================================================
      div(style = "display: flex; background-color: #d40028; color: white; border: 2px solid #007bff; margin-bottom: 0px;",
          
          # Gauche : Photo (Fond blanc)
          div(style = "width: 10%; background-color: white; border-right: 2px solid #007bff; display: flex; justify-content: center; align-items: center;",
              tags$img(src = joueur$PHOTOS, style = "width: 100%; max-height: 150px; object-fit: cover;")
          ),
          
          # Centre : Noms et Logo
          div(style = "width: 70%; position: relative; padding: 10px;",
              h4(joueur$PRENOMS, style = "margin: 0; padding-left: 10%; font-weight: normal;"),
              div(style = "background-color: #a8001e; margin-top: 10px; padding: 5px 10%; width: 85%;",
                  h2(tags$b(toupper(joueur$NOMS)), style = "margin: 0; letter-spacing: 1px;")
              ),
              tags$img(src = joueur$Photo_Club, style = "position: absolute; top: 20px; right: 20px; height: 80px; background: white; padding: 2px; border-radius: 5px;")
          ),
          
          # Droite : Infos Club
          div(style = "width: 30%; border-left: 1px solid #a8001e; padding: 10px;",
              tags$table(style = "width: 100%; color: white; font-size: 14px; font-weight: bold;",
                         tags$tr(tags$td("Club"), tags$td(joueur$Club_actuel, align="center")),
                         tags$tr(tags$td("Génération"), tags$td(joueur$Date_Naissance, align="center")), 
                         tags$tr(tags$td("Position"), tags$td(joueur$Position_Principale, align="center")), 
                         tags$tr(tags$td("Pied"), tags$td(joueur$Pied_Fort, align="center")),      
                         tags$tr(tags$td("Taille"), tags$td(joueur$Taille, align="center"))    
              )
          )
      ),
      
      # =========================================================
      # BLOC 2 : BANDEAU INFORMATIONS
      # =========================================================
      div(class = "bg-red border-black-thick", style = "text-align: center; font-weight: bold; padding: 3px; font-size: 15px; margin-bottom: 0; border-top: none !important;", "INFORMATIONS"),
      
      # =========================================================
      # BLOC 3 : LA GRILLE EXCEL
      # =========================================================
      tags$table(class = "excel-table border-black-thick", style = "border-top: none !important;",
                 
                 tags$tr(
                   tags$td(colspan = 2, tags$b("Agent"), br(), span(class = "text-blue-italic", joueur$Agent)),
                   tags$td(colspan = 4, style="padding: 0;",
                           tags$table(style="width: 100%; height: 100%; text-align: left; border-collapse: collapse;",
                                      tags$tr(tags$td(tags$b("Nationalité"), style="border:none;"), tags$td(joueur$NATIONALITE, style="border:none;")),
                                      tags$tr(tags$td(tags$b("Formé à"), style="border:none; border-top: 1px solid #d3d3d3;"), tags$td(joueur$Forme_A, style="border:none; border-top: 1px solid #d3d3d3;"))
                           )
                   ),
                 ),
                 
                 tags$tr(
                   tags$td(colspan = 2, tags$b("contact")),
                   tags$td(colspan = 8)
                 ),
                 
                 tags$tr(
                   tags$td(tags$b("Prétention")), tags$td(tags$b("Valeur K€")),
                   tags$td(colspan = 8, tags$b("Temps de jeu selon les divisions (min)"))
                 ),
                 
                 tags$tr(
                   tags$td(""), tags$td(joueur$Valeur_Marchande),
                   tags$td(tags$b("L1")), tags$td(tags$b("L2")), tags$td(tags$b("N1")),
                   tags$td(tags$b("N2")), tags$td(tags$b("N3")), tags$td(tags$b("CDF")),
                   tags$td(tags$b("ETR/AUTRE")), tags$td("")
                 ),
                 
                 tags$tr(
                   tags$td(tags$b("Type contrat")), tags$td(tags$b("Fin contrat")),
                   tags$td(joueur$Temps_L1), tags$td(joueur$Temps_L2), tags$td(joueur$Temps_N1), 
                   tags$td(joueur$Temps_N2), tags$td(joueur$Temps_N3), tags$td(joueur$Temps_Coupe_France), 
                   tags$td(joueur$Temps_Etranger_Jeunes), tags$td("")                             
                 ),
                 
                 tags$tr(
                   tags$td(""), tags$td(joueur$Fin_contrat), 
                   tags$td(colspan = 3, class = "bg-yellow", "Cartons jaunes"),
                   tags$td(joueur$Cartons_jaunes), 
                 ),
                 
                 tags$tr(
                   tags$td(""), tags$td(""),
                   tags$td(colspan = 3, class = "bg-red-alert", "Cartons rouges"),
                   tags$td(joueur$Cartons_rouges + joueur$Dont_Second_jaunes), 
                 ),
                 
                 tags$td(
                   div(class = "bg-red border-black-thick", style = "text-align: center; font-weight: bold; padding: 3px; font-size: 15px; margin-bottom: 0; border-top: none !important;", "SAISONS")
                 ),
                 
                 tags$tr(
                   tags$td("Saison"), tags$td("Club"), tags$td("Pays"),tags$td("Division"), tags$td("Groupe"), tags$td("Feuille de match"), tags$td("Titu"), tags$td("% Titu"), tags$td("Minutes jouées"), tags$td("Buts"), tags$td("Passes D")
                 ),
                 tags$tr(
                   tags$td("Actuel"),tags$td(joueur$Club_actuel),tags$td(joueur$Pays_En_Cours), tags$td(joueur$Division_En_Cours),tags$td(joueur$Groupes_En_Cours) , tags$td(joueur$Apparition_Groupe_En_Cours) ,tags$td((joueur$Titu_En_Cours)), tags$td(joueur$Pourcentage_titu_En_Cours*100),tags$td(joueur$Temps_de_jeu_En_Cours),tags$td(joueur$Buts_En_Cours),tags$td(joueur$Passes_En_Cours)
                 ),
                 tags$tr(
                   tags$td("N-1"),tags$td(joueur$Club_N_1),tags$td(joueur$Pays_N_1), tags$td(joueur$Division_N_1),tags$td(joueur$Groupes_N_1) , tags$td(joueur$Apparition_Groupe_N_1) ,tags$td(joueur$Titu_N_1), tags$td(joueur$Pourcentage_titu_N_1*100),tags$td(joueur$Temps_de_jeu_N_1),tags$td(joueur$Buts_N_1),tags$td(joueur$Passes_N_1)
                 ),
                 tags$tr(
                   tags$td("N-2"),tags$td(joueur$Club_N_2),tags$td(joueur$Pays_N_2), tags$td(joueur$Division_N_2),tags$td(joueur$Groupes_N_2) , tags$td(joueur$Apparition_Groupe_N_2) ,tags$td(joueur$Titu_N_2), tags$td(joueur$Pourcentage_titu_N_2*100),tags$td(joueur$Temps_de_jeu_N_2),tags$td(joueur$Buts_N_2),tags$td(joueur$Passes_N_2)
                 ),
                 tags$tr(
                   tags$td("N-3"),tags$td(joueur$Club_N_3),tags$td(joueur$Pays_N_3), tags$td(joueur$Division_N_3),tags$td(joueur$Groupes_N_3) , tags$td(joueur$Apparition_Groupe_N_3) ,tags$td(joueur$Titu_N_3), tags$td(joueur$Pourcentage_titu_N_3*100),tags$td(joueur$Temps_de_jeu_N_3),tags$td(joueur$Buts_N_3),tags$td(joueur$Passes_N_3)
                 )
      )
    )
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)