library(shiny)
library(bslib)

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
    "Scout Data Center | DFCO"
  ),
  
  # --- ONGLET 1 : L'USINE ---
  nav_panel("Actualisation des données",
            sidebarLayout(
              sidebarPanel(
                h4("1. Importer le fichier de donnée"),
                helpText("⚠️ ATTENTION ce fichier doit contenir une colonne 'NOMS' et 'PRENOMS' exactement écrit de la sorte"),
                fileInput("fichier_entree", "Sélectionnez votre .csv", accept = ".csv"),
                
                h4("2. Lancer la mise à jour des données"),
                actionButton("btn_lancer", "Lancer l'extraction", class = "btn-primary", style = "width: 100%;"),
                br(), br(),
                
                h4("3. Récupérer les données"),
                downloadButton("btn_telecharger", "Télécharger le résultat", class = "btn-success", style = "width: 100%;"),
                br(), br(),
                
                h4("4. Envoyer le fichier sur le Google Sheets (DFCO FICHE OFF)"),
                actionButton(
                  inputId = "btn_envoyer_gsheets", 
                  label = "Envoyer les données sur DFCO FICHE OFF", 
                  icon = icon("cogs"),
                  class = "btn-success", # Le met en bleu
                  width = "100%"
                )
              ),
              
              mainPanel(
                h3("Statut de l'opération"),
                # Une boîte grise pour afficher les messages
                verbatimTextOutput("statut_texte"),
                
                # Un petit mot d'explication pour l'utilisateur
                helpText("💡 Info : Pendant le traitement, l'application peut sembler figée. Regardez la bulle en bas pour voir l'avancée joueur par joueur !")
              )
            )
  ),
  
  # --- ONGLET 2 : FICHE JOUEUR  ---
  # nav_panel("👤 Fiche Joueur",
  #           sidebarLayout(
  #             sidebarPanel(
  #               h4("1. Charger la base de données"),
  #               fileInput("fichier_base", "Importez votre fichier CSV final", accept = ".csv"),
  #               hr(),
  #               h4("2. Rechercher"),
  #               selectizeInput("choix_joueur", "Sélectionnez un joueur :", choices = NULL, options = list(placeholder = 'Tapez un nom...'))
  #             ),
  #             
  #             mainPanel(
  #               uiOutput("fiche_joueur_ui")
  #             )
  #           )
  # )
)

# ==============================================================================
# 2. SERVEUR 
# ==============================================================================

server <- function(input, output, session) {
  
  # Variable qui va stocker le texte à afficher à l'écran
  message_statut <- reactiveVal("En attente d'un fichier CSV...")
  output$statut_texte <- renderText({ message_statut() })
  
  # ---------------------------------------------------------
  # ACTION : Quand l'utilisateur clique sur "Lancer l'extraction"
  # ---------------------------------------------------------
  observeEvent(input$btn_lancer, {
    
    # Sécurité : on vérifie qu'un fichier a bien été mis dans la boîte
    req(input$fichier_entree) 
    
    # On met à jour l'écran
    message_statut("⏳ Extraction en cours...\nNe fermez pas cette fenêtre.")
    
    # Nom du fichier temporaire qui sera créé en coulisses
    fichier_temporaire <- "resultat_temporaire_shiny.csv"
    
    # On lance NOTRE fonction de scraping dans un bloc sécurisé (tryCatch)
    tryCatch({
      
      withProgress(message = 'Extraction en cours...', value = 0, {
        
        # On crée une petite fonction "facteur" qui fera le lien
        fonction_pont <- function(avancement, texte_detail) {
          incProgress(avancement, detail = texte_detail)
        }
        
        # Le paramètre 'datapath' est le chemin temporaire où Shiny a rangé le fichier uploadé
        lancer_scraping(
          chemin_entree = input$fichier_entree$datapath, 
          chemin_sortie = fichier_temporaire,
          mise_a_jour_barre = fonction_pont
        )
      })
      
      # Si on arrive ici, c'est que la fonction s'est terminée 
      message_statut("✅ Extraction terminée avec succès !\nVous pouvez cliquer sur le bouton vert pour télécharger.")
      
    }, error = function(e) {
      # S'il y a un plantage, on l'affiche à l'écran
      message_statut(paste("❌ Une erreur critique est survenue :", e$message))
    })
  })
  
  # ---------------------------------------------------------
  # ACTION : Quand l'utilisateur clique sur "Télécharger"
  # ---------------------------------------------------------
  
  output$btn_telecharger <- downloadHandler(
    filename = function() {
      # On crée un beau nom de fichier avec la date du jour
      paste0("DFCO_Base_Actualisee_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # On prend le fichier temporaire qu'on a créé et on l'envoie à l'utilisateur
      file.copy("resultat_temporaire_shiny.csv", file)
    }
  )
  
  observeEvent(input$btn_envoyer_gsheets, {
    
    # 1. On vérifie que le fichier de scraping existe bien
    if (!file.exists("resultat_temporaire_shiny.csv")) {
      showNotification("Erreur : Aucun fichier à envoyer. Veuillez lancer le scraping d'abord.", type = "error")
      return() 
    }
    
    id_notif <- showNotification("Envoi en cours vers Google Sheets...", type = "message", duration = 10)
    
    tryCatch({
      
      df_final <- read.csv2("resultat_temporaire_shiny.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      
      url_google_sheets <- "https://docs.google.com/spreadsheets/d/12Xcyag-nM-xup10svbYfuT2_aMZ1YYFxk0hmldOv6u0/edit"
      
      df_final <- df_final |>
        mutate(
          A_VERIFIER = case_when(
            URL_Profil == "Non trouvée" ~ "⚠️",
            (Temps_L1 + Temps_L2 + Temps_N1 + Temps_N2 + Temps_N3 + Temps_Coupe_France + Temps_Etranger_Jeunes) == 0 ~ "⚠️",
            #pas de club actuel trouvé
            (Club_actuel == "")  ~ "⚠️",
            TRUE ~ "✅ OK"
          )
        ) %>%
        #On place cette colonne tout au début du tableau pour la voir de suite
        relocate(A_VERIFIER, .before = 1)
      
      range_write(
        ss = url_google_sheets,
        data = df_final,
        sheet = "BASE_COMPLETE",
        range = "A1", # On commence à coller en haut à gauche
        reformat = FALSE
      )
      
      showNotification("✅ Données écrites avec succès dans l'onglet BASE_COMPLETE !")
    }, error = function(e) {
      showNotification("❌ Erreur lors de l'écriture sur Google Sheets : ", e$message)
    })
  })
  
  ## FICHE JOUEUR 
  donnees_joueurs <- reactive({
    # On attend que l'utilisateur dépose le fichier dans l'onglet 2
    req(input$fichier_base) 
    
    # On lit le fichier
    df <- read.csv(input$fichier_base$datapath, sep = ";", stringsAsFactors = FALSE)
    df$Nom_Complet <- paste(df$PRENOMS, df$NOMS)
    
    return(df)
  })
  
  ## Remplir le menu déroulant 
  observe({
    # Cette action ne se déclenche que si le fichier est chargé
    req(input$fichier_base) 
    
    df <- donnees_joueurs()
    
    # On injecte la liste des joueurs dans le menu déroulant
    updateSelectizeInput(session, "choix_joueur", choices = df$Nom_Complet, server = TRUE)
  })
  
  output$fiche_joueur_ui <- renderUI({
    req(input$choix_joueur)
    
    df <- donnees_joueurs()
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
              tags$img(src = joueur$PHOTOS, style = "width: 100%; max-height: 150px; object-fit: cover;") # Remplacer par joueur$Photo
          ),
          
          # Centre : Noms et Logo
          div(style = "width: 70%; position: relative; padding: 10px;",
              h4(joueur$PRENOMS, style = "margin: 0; padding-left: 10%; font-weight: normal;"),
              div(style = "background-color: #a8001e; margin-top: 10px; padding: 5px 10%; width: 85%;",
                  h2(tags$b(toupper(joueur$NOMS)), style = "margin: 0; letter-spacing: 1px;")
              ),
              # Logo du club en haut à droite du bloc central
              tags$img(src = joueur$Photo_Club, style = "position: absolute; top: 20px; right: 20px; height: 80px; background: white; padding: 2px; border-radius: 5px;") # Remplacer par Logo_Club
          ),
          
          # Droite : Infos Club
          div(style = "width: 30%; border-left: 1px solid #a8001e; padding: 10px;",
              tags$table(style = "width: 100%; color: white; font-size: 14px; font-weight: bold;",
                         tags$tr(tags$td("Club"), tags$td(joueur$Club, align="center")),
                         tags$tr(tags$td("Génération"), tags$td(joueur$Date_Naissance, align="center")), # ex: joueur$Annee
                         tags$tr(tags$td("Position"), tags$td(joueur$Position_Principale, align="center")), # ex: joueur$Poste
                         tags$tr(tags$td("Pied"), tags$td(joueur$Pied_Fort, align="center")),     # ex: joueur$Pied
                         tags$tr(tags$td("Taille"), tags$td(joueur$Taille, align="center"))    # ex: joueur$Taille
              )
          )
      ),
      
      # =========================================================
      # BLOC 2 : BANDEAU INFORMATIONS
      # =========================================================
      div(class = "bg-red border-black-thick", style = "text-align: center; font-weight: bold; padding: 3px; font-size: 15px; margin-bottom: 0; border-top: none !important;", "INFORMATIONS"),
      
      # =========================================================
      # BLOC 3 : LA GRILLE EXCEL (Le grand tableau du bas)
      # =========================================================
      tags$table(class = "excel-table border-black-thick", style = "border-top: none !important;",
                 
                 # --- Ligne 1 : Agent, Nationalité, Besoc, FBDB ---
                 tags$tr(
                   tags$td(colspan = 2, tags$b("Agent"), br(), span(class = "text-blue-italic", joueur$Agent)), 
                   tags$td(colspan = 4, style="padding: 0;",
                           tags$table(style="width: 100%; height: 100%; text-align: left; border-collapse: collapse;",
                                      tags$tr(tags$td(tags$b("Nationalité"), style="border:none;"), tags$td(joueur$NATIONALITE, style="border:none;")),
                                      tags$tr(tags$td(tags$b("Sélection"), style="border:none; border-top: 1px solid #d3d3d3;"), tags$td("", style="border:none; border-top: 1px solid #d3d3d3;")),
                                      tags$tr(tags$td(tags$b("Formé à"), style="border:none; border-top: 1px solid #d3d3d3;"), tags$td("", style="border:none; border-top: 1px solid #d3d3d3;"))
                           )
                   ),
                 ),
                 
                 # --- Ligne 2 : Contact ---
                 tags$tr(
                   tags$td(colspan = 2, tags$b("contact")),
                   tags$td(colspan = 8)
                 ),
                 
                 # --- Ligne 3 : Prétention / Temps de jeu (Titres) ---
                 tags$tr(
                   tags$td(tags$b("prétention")), tags$td(tags$b("Valeur K€")),
                   tags$td(colspan = 8, tags$b("Temps de jeu selon les divisions (min)"))
                 ),
                 
                 # --- Ligne 4 : Divisions (L1, L2, N1...) ---
                 tags$tr(
                   tags$td(""), tags$td(joueur$Valeur_Marchande),
                   tags$td(tags$b("L1")), tags$td(tags$b("L2")), tags$td(tags$b("N1")),
                   tags$td(tags$b("N2")), tags$td(tags$b("N3")), tags$td(tags$b("CDF")),
                   tags$td(tags$b("ETR/AUTRE")), tags$td("")
                 ),
                 
                 # --- Ligne 5 : Les minutes ---
                 tags$tr(
                   tags$td(tags$b("Type contrat")), tags$td(tags$b("fin contrat")),
                   tags$td(joueur$Temps_L1), tags$td(joueur$Temps_L2), tags$td(joueur$Temps_N1), # Temps L1, L2, N1
                   tags$td(joueur$Temps_N2), tags$td(joueur$Temps_N3), tags$td(joueur$Temps_Coupe_France), # Temps N2, N3, CDF
                   tags$td(joueur$Temps_Etranger_Autres), tags$td("")                             # Temps ETR
                 ),
                 
                 # --- Ligne 6 : Cartons Jaunes ---
                 tags$tr(
                   tags$td(""), tags$td(joueur$Fin_contrat), # Valeurs contrat
                   tags$td(colspan = 3, class = "bg-yellow", "Cartons jaunes"),
                   tags$td(joueur$Cartons_jaunes), # Valeur cartons jaunes (ex: joueur$Cartons_J)
                 ),
                 
                 # --- Ligne 7 : Cartons Rouges ---
                 tags$tr(
                   tags$td(""), tags$td(""),
                   tags$td(colspan = 3, class = "bg-red-alert", "Cartons rouges"),
                   tags$td(joueur$Cartons_rouges + joueur$Dont_Second_jaunes), # Valeur cartons rouges
                   
                 )
      )
    )
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
