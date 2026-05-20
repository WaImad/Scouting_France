# On installe pacman s'il n'est pas là
if (!require("pacman")) install.packages("pacman")

# installe les packages si absents et les charge
pacman::p_load(shiny, dplyr, rvest, httr, stringr, googlesheets4, jsonlite)

#Début de la fonction
lancer_scraping <- function(chemin_entree, chemin_sortie, mise_a_jour_barre = NULL) {
  
  message("Démarrage de l'extraction...")
  
  ## Scrapping de la page de transfermarkt
  url_transfermarkt <- "https://www.transfermarkt.fr/"
  url_club_passe_base = "https://www.transfermarkt.fr/m/startseite/verein/"
  
  # --- DÉTECTION AUTOMATIQUE DU SÉPARATEUR CSV ---
  premiere_ligne <- readLines(chemin_entree, n = 1, encoding = "UTF-8")
  
  # Si on trouve un point-virgule, on utilise read.csv2 (format Français)
  if (grepl(";", premiere_ligne)) {
    df <- read.csv2(chemin_entree, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else {
    df <- read.csv(chemin_entree, stringsAsFactors = FALSE, sep = ",", fileEncoding = "UTF-8")
  }
  
  # Configuration du Headers
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
    `Accept-Language` = "fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7"
  )
  
  resultats <- list()
  
  # =======================================================================
  # CALCUL DES SAISONS DYNAMIQUES (Pour Transfermarkt ET BeSoccer)
  # =======================================================================
  annee_actuelle <- as.numeric(format(Sys.Date(), "%Y"))
  mois_actuel <- as.numeric(format(Sys.Date(), "%m"))
  debut_saison_actuelle <- ifelse(mois_actuel >= 7, annee_actuelle, annee_actuelle - 1)
  
  saisons_cibles <- c(
    debut_saison_actuelle,       
    debut_saison_actuelle - 1,   
    debut_saison_actuelle - 2,   
    debut_saison_actuelle - 3    
  )
  noms_saisons <- c("En_Cours", "N_1", "N_2", "N_3")
  
  # Formatage pour BeSoccer (ex: "2025/26")
  bs_en_cours <- paste0(debut_saison_actuelle, "/", substr(debut_saison_actuelle + 1, 3, 4))
  bs_n_1 <- paste0(debut_saison_actuelle - 1, "/", substr(debut_saison_actuelle, 3, 4))
  bs_n_2 <- paste0(debut_saison_actuelle - 2, "/", substr(debut_saison_actuelle - 1, 3, 4))
  bs_n_3 <- paste0(debut_saison_actuelle - 3, "/", substr(debut_saison_actuelle - 2, 3, 4))
  
  #Boucler sur chaque joueur deja présent dans le fichier
  for (i in 1:nrow(df)) {
    nom <- df$NOMS[i]
    prenom <- df$PRENOMS[i]
    message(sprintf("\n[%d/%d] Traitement de : %s %s", i, nrow(df), prenom, nom))
    
    if (is.function(mise_a_jour_barre)) {
      pas_avancement <- 1 / nrow(df)
      texte_info <- sprintf("%s %s (%d/%d)", prenom, nom, i, nrow(df))
      mise_a_jour_barre(pas_avancement, texte_info)
    }
    
    # --- INITIALISATION DE TOUTES LES VARIABLES ---
    url_profil <- "Non trouvée"
    photo_profil <- ""
    nationalité <- ""
    agent <- ""
    date_naissance <- ""
    position_principale <- ""
    position_secondaire <- ""
    position_complete <- ""
    club_jeune <- ""
    pied_fort <- ""
    taille <- ""
    club <- ""
    photo_club <- ""
    pays_club_actuel <- ""
    valeur_marchande <- ""
    fin_contrat <- ""
    
    Temps_L1 <- 0; Temps_L2 <- 0; Temps_N1 <- 0; Temps_N2 <- 0; Temps_N3 <- 0
    Temps_Coupe_France <- 0; Temps_Etranger_Autres <- 0
    cartons_jaunes <- 0; cartons_jaunes_rouges <- 0; cartons_rouges <- 0
    
    # NOUVELLES SAISONS RELATIVES
    Buts_En_Cours <- 0; Passes_En_Cours <- 0; Division_En_Cours <- ""; Pays_En_Cours <- ""; Titularisations_En_Cours <- 0; Groupes_En_Cours <- ""; Pourcentage_titu_En_Cours <- 0; Nombre_Matchs_Equipe_En_Cours = 0; Club_En_Cours = ""; Temps_de_jeu_En_Cours = 0
    Buts_N_1 <- 0; Passes_N_1 <- 0; Division_N_1 <- ""; Pays_N_1 <- ""; Titularisations_N_1 <- 0; Groupes_N_1 <- ""; Pourcentage_titu_N_1 <- 0; Nombre_Matchs_Equipe_N_1 = 0; Club_N_1 = ""; Temps_de_jeu_N_1 = 0
    Buts_N_2 <- 0; Passes_N_2 <- 0; Division_N_2 <- ""; Pays_N_2 <- ""; Titularisations_N_2 <- 0; Groupes_N_2 <- ""; Pourcentage_titu_N_2 <- 0; Nombre_Matchs_Equipe_N_2 = 0; Club_N_2 = ""; Temps_de_jeu_N_2 = 0
    Buts_N_3 <- 0; Passes_N_3 <- 0; Division_N_3 <- ""; Pays_N_3 <- ""; Titularisations_N_3 <- 0; Groupes_N_3 <- ""; Pourcentage_titu_N_3 <- 0; Nombre_Matchs_Equipe_N_3 = 0; Club_N_3 = ""; Temps_de_jeu_N_3 = 0
    
    tryCatch({
      # --- ÉTAPE A : RECHERCHE TRANSFERMARKT ---
      terme_recherche <- paste0(nom, ",", prenom)
      url_recherche <- paste0("https://www.transfermarkt.fr/schnellsuche/ergebnis/schnellsuche?query=", URLencode(terme_recherche))
      rep_recherche <- GET(url_recherche, add_headers(.headers = headers))
      
      if (status_code(rep_recherche) == 200) {
        page_recherche <- read_html(rep_recherche)
        lien_relatif <- page_recherche |> html_element("#player-grid .odd:nth-child(1) .hauptlink a") |> html_attr("href")
        
        if (!is.na(lien_relatif)) {
          url_profil <- paste0("https://www.transfermarkt.fr", lien_relatif)
          message(sprintf("  -> URL du profil trouvée : %s", url_profil))
          Sys.sleep(runif(1, min = 2, max = 4))
          
          rep_profil <- GET(url_profil, add_headers(.headers = headers))
          
          if (status_code(rep_profil) == 200) {
            page_profil <- read_html(rep_profil)
            
            # --- INFOS BASIQUES (Mis à jour avec position secondaire) ---
            photo_profil_noeud <- page_profil |> html_element(".data-header__profile-image") |> html_attr("src")
            if (!is.na(photo_profil_noeud)) photo_profil <- photo_profil_noeud
            
            club_noeud <- page_profil |> html_element(".data-header__club a") |> html_text(trim = TRUE)
            if (!is.na(club_noeud)) club <- club_noeud
            
            photo_club_noeud <- page_profil |> html_element(".data-header__box__club-link img") |> html_attr("srcset")
            if (!is.na(photo_club_noeud)) photo_club <- trimws(strsplit(photo_club_noeud, " 1x")[[1]][1])
            
            pays_club_noeud <- page_profil |> html_element(".data-header__club-info .flaggenrahmen") |> html_attr("alt")
            if (!is.na(pays_club_noeud)) pays_club_actuel <- pays_club_noeud
            
            nationalite_noeud <- page_profil |> html_element(".data-header__items .data-header__label~ .data-header__label+ .data-header__label .data-header__content") |> html_text(trim = TRUE)
            if (!is.na(nationalite_noeud)) nationalité <- nationalite_noeud 
            
            date_naissance_noeud <- page_profil |> html_element(".data-header__items:nth-child(1) .data-header__label:nth-child(1) .data-header__content") |> html_text(trim = TRUE)
            if (!is.na(date_naissance_noeud)) date_naissance <- date_naissance_noeud
            
            valeur_brute <- page_profil |> html_element("a.data-header__market-value-wrapper") |> html_text(trim = TRUE)
            if (!is.na(valeur_brute)) valeur_marchande <- paste0(trimws(strsplit(valeur_brute, "€")[[1]][1]), " €")
            
            taille_noeud <- page_profil |> html_element(".data-header__items:nth-child(2) .data-header__label:nth-child(1) .data-header__content") |> html_text(trim = TRUE)
            if (!is.na(taille_noeud) && grepl("m$", taille_noeud)) taille <- taille_noeud
            
            club_jeune_brut <- page_profil |> html_element("h2+ .content") |> html_text(trim = TRUE)
            if(is.na(club_jeune_brut) || length(club_jeune_brut) == 0 || club_jeune_brut == "") {
              club_jeune <- "" 
            } else {
              club_jeune_net <- str_replace_all(club_jeune_brut, "[\r\n\t]", " ")
              liste_clubs <- str_split(club_jeune_net, ",")[[1]]
              dernier_club <- liste_clubs[length(liste_clubs)]
              dernier_club <- str_replace_all(dernier_club, "\\(.*?\\)", "")
              club_jeune <- str_trim(dernier_club)
            }
            
            # --- NOUVELLE GESTION DES POSTES ---
            position_principale_noeud <- page_profil |> html_element(".detail-position__inner-box .detail-position__position") |> html_text(trim = TRUE)
            if (!is.na(position_principale_noeud)) position_principale <- position_principale_noeud
            
            position_secondaire_noeud <- page_profil |> html_element(".detail-position__position .detail-position__position") |> html_text(trim = TRUE)
            position_secondaire <- if (!is.na(position_secondaire_noeud)) position_secondaire_noeud else ""
            position_complete <- paste0(position_principale, " / ", position_secondaire)
            
            fin_contrat_noeud <- page_profil |> html_element(".data-header__club-info .data-header__label~ .data-header__label+ .data-header__label .data-header__content") |> html_text(trim = TRUE)
            fin_contrat <- if (!is.na(fin_contrat_noeud) || fin_contrat_noeud != "-") fin_contrat_noeud else "Non trouvée"
            
            pied_fort_noeud <- page_profil |> html_element(xpath = "//span[contains(@class, 'info-table__content--regular') and contains(text(), 'Pied')]/following-sibling::span[1]") |> html_text(trim = TRUE)
            if (length(pied_fort_noeud) > 0 && !is.na(pied_fort_noeud)) pied_fort <- pied_fort_noeud
            
            agent_noeud <- page_profil |> html_element(xpath = "//span[contains(@class, 'info-table__content--regular') and contains(text(), 'Agent')]/following-sibling::span[1]") |> html_text(trim = TRUE)
            if (length(agent_noeud) > 0 && !is.na(agent_noeud)) agent <- agent_noeud
            
            # --- API TRANSFERMARKT (STATS) ---
            id_joueur <- str_extract(url_profil,"[0-9]+")
            url_api <- paste0("https://tmapi-alpha.transfermarkt.technology/player/", id_joueur, "/performance-game")
            headers_api <- c(`User-Agent` = "Mozilla/5.0", `Accept` = "application/json")
            
            tentatives <- 0; max_tentatives <- 3; code_statut <- 0
            while (tentatives < max_tentatives) {
              rep_api <- GET(url_api, add_headers(.headers = headers_api))
              code_statut <- status_code(rep_api)
              if (code_statut < 500) break
              tentatives <- tentatives + 1
              Sys.sleep(5)
            }
            
            if (code_statut == 200) {
              json_text <- content(rep_api, as = "text", encoding = "UTF-8")
              data_api <- fromJSON(json_text, flatten = TRUE) 
              tableau_stats <- if ("data" %in% names(data_api)) data_api$data$performance else data_api
              
              if (is.data.frame(tableau_stats)) {
                
                # Temps de jeu globaux
                for (j in 1:nrow(tableau_stats)) {
                  col_jaune <- "statistics.cardStatistics.yellowCard.minute"
                  if (col_jaune %in% names(tableau_stats) && length(tableau_stats[[col_jaune]][j]) > 0 && !is.na(tableau_stats[[col_jaune]][j])) cartons_jaunes <- cartons_jaunes + 1
                  
                  col_rouge <- "statistics.cardStatistics.redCard.minute"
                  if (col_rouge %in% names(tableau_stats) && length(tableau_stats[[col_rouge]][j]) > 0 && !is.na(tableau_stats[[col_rouge]][j])) cartons_rouges <- cartons_rouges + 1
                  
                  col_jr <- "statistics.cardStatistics.yellowRedCard.minute"
                  if (col_jr %in% names(tableau_stats) && length(tableau_stats[[col_jr]][j]) > 0 && !is.na(tableau_stats[[col_jr]][j])) cartons_jaunes_rouges <- cartons_jaunes_rouges + 1
                  
                  competition_id <- tableau_stats$gameInformation.competitionId[j]
                  minutes <- tableau_stats$statistics.playingTimeStatistics.playedMinutes[j]
                  
                  if (is.na(competition_id) || is.na(minutes)) next
                  comp_id_lower <- tolower(as.character(competition_id))
                  
                  if (grepl("fr1", comp_id_lower)) Temps_L1 <- Temps_L1 + minutes
                  else if (grepl("fr2", comp_id_lower)) Temps_L2 <- Temps_L2 + minutes
                  else if (comp_id_lower == "fr3") Temps_N1 <- Temps_N1 + minutes
                  else if (grepl("^cn2", comp_id_lower)) Temps_N2 <- Temps_N2 + minutes
                  else if (grepl("^c3[a-z]+|^cn3", comp_id_lower)) Temps_N3 <- Temps_N3 + minutes
                  else if (comp_id_lower == "frc") Temps_Coupe_France <- Temps_Coupe_France + minutes
                  else Temps_Etranger_Autres <- Temps_Etranger_Autres + minutes
                }
                
                # --- NOUVELLE BOUCLE DES SAISONS DYNAMIQUES ---
                for (k in 1:length(saisons_cibles)) {
                  id_saison_actuel <- saisons_cibles[k]
                  suffixe <- noms_saisons[k]
                  
                  if ("gameInformation.season.id" %in% names(tableau_stats)) {
                    # Le nouveau filtre pour écarter les matchs hors-groupe
                    stats_saison <- tableau_stats |> filter(
                      gameInformation.season.id == id_saison_actuel & 
                        !statistics.generalStatistics.participationState %in% c("injured", "absent", "not in squad", "NOT_IN_SQUAD")
                    )
                  } else {
                    stats_saison <- data.frame() 
                  }
                  
                  titularisations <- 0; saison_buts <- 0; saison_passes <- 0; division <- ""; pays_club_saison <- ""; groupes <- ""
                  nbr_matchs <- 0; pourcentage_titu <- 0; nom_club <- ""; club_id <- ""; minutes_saison <- 0
                  
                  if (nrow(stats_saison) > 0) {
                    competition_id <- stats_saison$gameInformation.competitionId[1]
                    comp_id_lower <- tolower(as.character(competition_id))
                    
                    if (grepl("^fr|^cn|^c3|^f19", comp_id_lower)) pays_club_saison <- "France"
                    else if (grepl("^gb", comp_id_lower)) pays_club_saison <- "Angleterre"
                    else if (grepl("^es", comp_id_lower)) pays_club_saison <- "Espagne"
                    else if (grepl("^it", comp_id_lower)) pays_club_saison <- "Italie"
                    else if (grepl("^l[1-9]", comp_id_lower)) pays_club_saison <- "Allemagne"
                    else if (grepl("^nl", comp_id_lower)) pays_club_saison <- "Pays-Bas"
                    else if (grepl("^be", comp_id_lower)) pays_club_saison <- "Belgique"
                    else if (grepl("^pt", comp_id_lower)) pays_club_saison <- "Portugal"
                    else if (grepl("^lux", comp_id_lower)) pays_club_saison <- "Luxembourg"
                    else if (grepl("c1|c2", comp_id_lower)) pays_club_saison <- "Suisse"
                    else if (grepl("fs", comp_id_lower)) { pays_club_saison <- nationalité; division <- "Sélection Nationale" } 
                    else pays_club_saison <- "Étranger"
                    
                    if (grepl("1$", comp_id_lower)) division <- "L1"
                    else if (grepl("2$", comp_id_lower)) division <- "L2"
                    else if (comp_id_lower == "fr3" ||grepl("3$", comp_id_lower)) division <- "N1/L3"
                    else if (grepl("^cn2", comp_id_lower)) { division <- "N2"; groupes <- toupper(sub("^cn2","",comp_id_lower)) }
                    else if (grepl("^c3[a-z]+|^cn3", comp_id_lower)) { division <- "N3"; groupes <- toupper(sub("^c3|^cn3", "", comp_id_lower)) }
                    else if (grepl("^f19", comp_id_lower)) { division <- "U19 NAT"; groupes <- toupper(sub("^f19", "", comp_id_lower)) }
                    else division <- "Division étrangère"
                    
                    if ("statistics.goalStatistics.goalsScoredTotal" %in% names(stats_saison)) saison_buts <- sum(stats_saison$statistics.goalStatistics.goalsScoredTotal, na.rm = TRUE)
                    if ("statistics.goalStatistics.assists" %in% names(stats_saison)) saison_passes <- sum(stats_saison$statistics.goalStatistics.assists, na.rm = TRUE)
                    if ("statistics.playingTimeStatistics.isStarting" %in% names(stats_saison)) titularisations <- sum(stats_saison$statistics.playingTimeStatistics.isStarting, na.rm = TRUE)
                    
                    if ("gameInformation.gameId" %in% names(stats_saison)) { 
                      nbr_matchs <- n_distinct(stats_saison$gameInformation.gameId)
                      if(nbr_matchs > 0) pourcentage_titu <- round((titularisations/nbr_matchs),2)
                    }
                    
                    if ("statistics.playingTimeStatistics.playedMinutes" %in% names(stats_saison)) {
                      minutes_saison <- sum(stats_saison$statistics.playingTimeStatistics.playedMinutes, na.rm = TRUE)
                    }
                    
                    # Récupération API du club
                    if ("clubsInformation.club.clubId" %in% names(stats_saison)) {
                      club_id <- stats_saison$clubsInformation.club.clubId[1]
                      if (length(club_id) > 0 && !is.na(club_id)) {
                        url_club_passe <- paste0(url_club_passe_base, club_id)
                        rep_club_passe <- GET(url_club_passe, add_headers(.headers = headers))
                        if (status_code(rep_club_passe) == 200) {
                          page_club_passe <- read_html(rep_club_passe)
                          nom_club <- page_club_passe |> html_element(".data-header__headline-wrapper--oswald") |> html_text(trim = TRUE)
                        }
                      }
                    }
                    
                    if (suffixe == "En_Cours") {
                      Buts_En_Cours <- saison_buts; Passes_En_Cours <- saison_passes; Division_En_Cours <- division; Pays_En_Cours <- pays_club_saison; Titularisations_En_Cours <- titularisations; Groupes_En_Cours <- groupes; Pourcentage_titu_En_Cours <- pourcentage_titu; Nombre_Matchs_Equipe_En_Cours = nbr_matchs;  Club_En_Cours = nom_club; Temps_de_jeu_En_Cours = minutes_saison
                    } else if (suffixe == "N_1") {
                      Buts_N_1 <- saison_buts; Passes_N_1 <- saison_passes; Division_N_1 <- division; Pays_N_1 <- pays_club_saison; Titularisations_N_1 <- titularisations;Groupes_N_1 <- groupes; Pourcentage_titu_N_1 <- pourcentage_titu; Nombre_Matchs_Equipe_N_1 = nbr_matchs; Club_N_1 = nom_club; Temps_de_jeu_N_1 = minutes_saison
                    } else if (suffixe == "N_2") {
                      Buts_N_2 <- saison_buts; Passes_N_2 <- saison_passes; Division_N_2 <- division; Pays_N_2 <- pays_club_saison; Titularisations_N_2 <- titularisations; Groupes_N_2 <- groupes; Pourcentage_titu_N_2 <- pourcentage_titu; Nombre_Matchs_Equipe_N_2 = nbr_matchs; Club_N_2 = nom_club; Temps_de_jeu_N_2 = minutes_saison
                    } else if (suffixe == "N_3") {
                      Buts_N_3 <- saison_buts; Passes_N_3 <- saison_passes; Division_N_3 <- division; Pays_N_3 <- pays_club_saison; Titularisations_N_3 <- titularisations; Groupes_N_3 <- groupes; Pourcentage_titu_N_3 <- pourcentage_titu; Nombre_Matchs_Equipe_N_3 = nbr_matchs; Club_N_3 = nom_club; Temps_de_jeu_N_3 = minutes_saison
                    }
                  }
                }
              }
            } else message(sprintf("  -> API inaccessible (Code %s)", code_statut))
          }
        } else {
          message("  -> ❌ Non trouvé sur TM. Lancement du scrapping BeSoccer ...")
          
          # --- PLAN B : RECHERCHE BESOCCER (AVEC SAISONS DYNAMIQUES) ---
          nom_clean <- str_replace_all(tolower(nom), "[ ']", "-")
          prenom_clean <- str_replace_all(tolower(prenom), "[ ']", "-")
          url_recherche_bs <- paste0("https://www.besoccer.com/search/", nom_clean, "-", prenom_clean)
          rep_bs <- GET(url_recherche_bs, add_headers(.headers = headers))
          
          if (status_code(rep_bs) == 200) {
            page_bs <- read_html(rep_bs)
            lien_bs <- page_bs |> html_element(".info:nth-child(1) .block") |> html_attr("href")
            premiere_lettre <- substr(prenom_clean, 1, 1)
            
            if (grepl(paste0(premiere_lettre, "-", nom_clean), lien_bs) || grepl(paste0(prenom_clean, "-", nom_clean), lien_bs)) {
              url_profil <- lien_bs
              message(sprintf("  -> ✅ Profil BeSoccer trouvé via recherche : %s", url_profil))
              
              page_profil_bs <- read_html(GET(url_profil, add_headers(.headers = headers)))
              
              nationalité <- page_profil_bs |> html_element(".stat:nth-child(1) .mb5+ .small-row") |> html_text(trim = TRUE)
              club <- page_profil_bs |> html_element(".mb5 b") |> html_text(trim = TRUE)
              photo_club <- page_profil_bs |> html_element(".team img") |> html_attr("src")
              photo_profil <- page_profil_bs |> html_element(".img-wrapper img") |> html_attr("src")
              if (is.na(photo_profil)) photo_profil <- ""
              
              position_principale <- page_profil_bs |> html_element(".rol4 b") |> html_text(trim = TRUE)
              position_complete <- position_principale # BeSoccer ne donne pas tjrs le secondaire
              
              date_naissance <- page_profil_bs |> html_element("#mod_player_stats .color-grey2") |> html_text(trim = TRUE)
              date_naissance <- sub("Born on ", "", date_naissance)
              
              unité_vm <- page_profil_bs |> html_element(".stat:nth-child(4) .big-row+ .small-row") |> html_text(trim = TRUE)
              valeur_vm <- page_profil_bs |> html_element(".stat:nth-child(4) .big-row") |> html_text(trim = TRUE)
              if (length(valeur_vm) > 0 && !is.na(valeur_vm)) valeur_marchande <- paste(valeur_vm, unité_vm)
              
              url_carriere <- str_replace(url_profil, "/player/", "/player/career-path/")
              page_carriere <- read_html(GET(url_carriere, add_headers(.headers = headers)))
              table_perf <- page_carriere |> html_element("#mod_trajectory > div > div:nth-child(1) > div.panel-body.table-list.team-result > table") |> html_table()
              
              if(is.data.frame(table_perf)) {
                table_perf <- table_perf[1:(nrow(table_perf)-1), c(1,2,3,4,5,6,7,9,11)] 
                colnames(table_perf) <- c("Club","Saison", "MP", "Buts", "Passes.D", "Cartons_jaunes", "Cartons_rouges", "Titularisations","Minutes")
                
                cartons_jaunes <- sum(as.numeric(table_perf$Cartons_jaunes), na.rm = TRUE)
                cartons_rouges <- sum(as.numeric(table_perf$Cartons_rouges), na.rm = TRUE)
                
                minutes_propres <- as.numeric(gsub("[^0-9]", "", table_perf$Minutes))
                
                if ("National 3" %in% table_perf$Club) Temps_N3 <- sum(minutes_propres[table_perf$Club == "National 3"], na.rm = TRUE)
                if ("National 2" %in% table_perf$Club) Temps_N2 <- sum(minutes_propres[table_perf$Club == "National 2"], na.rm = TRUE)
                if ("National 1" %in% table_perf$Club) Temps_N1 <- sum(minutes_propres[table_perf$Club == "National 1"], na.rm = TRUE) 
                if ("French League U19" %in% table_perf$Club) Temps_Etranger_Autres <- Temps_Etranger_Autres + sum(minutes_propres[table_perf$Club == "French League U19"], na.rm = TRUE)
                if ("Liga Francesa Sub 17" %in% table_perf$Club) Temps_Etranger_Autres <- Temps_Etranger_Autres + sum(minutes_propres[table_perf$Club == "Liga Francesa Sub 17"], na.rm = TRUE)
                
                # NOUVEAU : EXTRACTION BESOCCER AVEC L'ANCIENNE MÉTHODE (Dates écrites en dur)
                
                # === SAISON EN COURS (2025/26) ===
                if ("2025/26" %in% table_perf$Saison) {
                  Buts_En_Cours <- sum(as.numeric(table_perf$Buts[table_perf$Saison == "2025/26"]), na.rm = TRUE)
                  Passes_En_Cours <- sum(as.numeric(table_perf$Passes.D[table_perf$Saison == "2025/26"]), na.rm = TRUE)
                  Titularisations_En_Cours <- sum(as.numeric(table_perf$Titularisations[table_perf$Saison == "2025/26"]), na.rm = TRUE)
                  Nombre_Matchs_Equipe_En_Cours <- sum(as.numeric(table_perf$MP[table_perf$Saison == "2025/26"]), na.rm = TRUE)
                  Temps_de_jeu_En_Cours <- sum(as.numeric(gsub("[^0-9]", "", table_perf$Minutes[table_perf$Saison == "2025/26"])), na.rm = TRUE)
                  if(Nombre_Matchs_Equipe_En_Cours > 0) {
                    Pourcentage_titu_En_Cours <- round(Titularisations_En_Cours / Nombre_Matchs_Equipe_En_Cours, 2)
                  }
                  Club_En_Cours <- table_perf$Club[table_perf$Saison == "2025/26"][1]
                }
                
                # === SAISON N-1 (2024/25) ===
                if ("2024/25" %in% table_perf$Saison) {
                  Buts_N_1 <- sum(as.numeric(table_perf$Buts[table_perf$Saison == "2024/25"]), na.rm = TRUE)
                  Passes_N_1 <- sum(as.numeric(table_perf$Passes.D[table_perf$Saison == "2024/25"]), na.rm = TRUE)
                  Titularisations_N_1 <- sum(as.numeric(table_perf$Titularisations[table_perf$Saison == "2024/25"]), na.rm = TRUE)
                  Nombre_Matchs_Equipe_N_1 <- sum(as.numeric(table_perf$MP[table_perf$Saison == "2024/25"]), na.rm = TRUE)
                  Temps_de_jeu_N_1 <- sum(as.numeric(gsub("[^0-9]", "", table_perf$Minutes[table_perf$Saison == "2024/25"])), na.rm = TRUE)
                  if(Nombre_Matchs_Equipe_N_1 > 0) {
                    Pourcentage_titu_N_1 <- round(Titularisations_N_1 / Nombre_Matchs_Equipe_N_1, 2)
                  }
                  Club_N_1 <- table_perf$Club[table_perf$Saison == "2024/25"][1]
                }
                
                # === SAISON N-2 (2023/24) ===
                if ("2023/24" %in% table_perf$Saison) {
                  Buts_N_2 <- sum(as.numeric(table_perf$Buts[table_perf$Saison == "2023/24"]), na.rm = TRUE)
                  Passes_N_2 <- sum(as.numeric(table_perf$Passes.D[table_perf$Saison == "2023/24"]), na.rm = TRUE)
                  Titularisations_N_2 <- sum(as.numeric(table_perf$Titularisations[table_perf$Saison == "2023/24"]), na.rm = TRUE)
                  Nombre_Matchs_Equipe_N_2 <- sum(as.numeric(table_perf$MP[table_perf$Saison == "2023/24"]), na.rm = TRUE)
                  Temps_de_jeu_N_2 <- sum(as.numeric(gsub("[^0-9]", "", table_perf$Minutes[table_perf$Saison == "2023/24"])), na.rm = TRUE)
                  if(Nombre_Matchs_Equipe_N_2 > 0) {
                    Pourcentage_titu_N_2 <- round(Titularisations_N_2 / Nombre_Matchs_Equipe_N_2, 2)
                  }
                  Club_N_2 <- table_perf$Club[table_perf$Saison == "2023/24"][1]
                }
                
                # === SAISON N-3 (2022/23) ===
                if ("2022/23" %in% table_perf$Saison) {
                  Buts_N_3 <- sum(as.numeric(table_perf$Buts[table_perf$Saison == "2022/23"]), na.rm = TRUE)
                  Passes_N_3 <- sum(as.numeric(table_perf$Passes.D[table_perf$Saison == "2022/23"]), na.rm = TRUE)
                  Titularisations_N_3 <- sum(as.numeric(table_perf$Titularisations[table_perf$Saison == "2022/23"]), na.rm = TRUE)
                  Nombre_Matchs_Equipe_N_3 <- sum(as.numeric(table_perf$MP[table_perf$Saison == "2022/23"]), na.rm = TRUE)
                  Temps_de_jeu_N_3 <- sum(as.numeric(gsub("[^0-9]", "", table_perf$Minutes[table_perf$Saison == "2022/23"])), na.rm = TRUE)
                  if(Nombre_Matchs_Equipe_N_3 > 0) {
                    Pourcentage_titu_N_3 <- round(Titularisations_N_3 / Nombre_Matchs_Equipe_N_3, 2)
                  }
                  Club_N_3 <- table_perf$Club[table_perf$Saison == "2022/23"][1]
                }
              }
            } else {
              message("  -> Introuvable même sur BeSoccer.")
            }
          }
        }
      }
    }, error = function(e) {
      message(sprintf("  -> Erreur : %s", e$message))
    })
    
    # ENREGISTREMENT FINAL 
    resultats[[i]] <- data.frame(
      NOMS = nom,
      PRENOMS = prenom,
      PHOTOS = photo_profil,
      NATIONALITE = nationalité,
      Temps_L1 = Temps_L1,
      Temps_L2 = Temps_L2,
      Temps_N1 = Temps_N1,
      Temps_N2 = Temps_N2,
      Temps_N3 = Temps_N3,
      Temps_Coupe_France = Temps_Coupe_France,
      Temps_Etranger_Jeunes = Temps_Etranger_Autres,
      Cartons_jaunes = cartons_jaunes,
      Dont_Second_jaunes = cartons_jaunes_rouges,
      Cartons_rouges = cartons_rouges,
      Agent = agent,
      Valeur_Marchande = valeur_marchande,
      URL_Profil = url_profil,
      Date_Naissance = date_naissance,
      Position_Principale = position_principale,
      Position_Secondaire = position_secondaire,
      Position_Complete = position_complete,
      Forme_A = club_jeune,
      Pied_Fort = pied_fort,
      Taille = taille,
      Club_actuel = club,
      Photo_Club = photo_club,
      Pays_Club_actuel = pays_club_actuel,
      
      # Saison en cours
      Club_En_Cours = Club_En_Cours,
      Titu_En_Cours = Titularisations_En_Cours,
      Apparition_Groupe_En_Cours = Nombre_Matchs_Equipe_En_Cours,
      Pourcentage_titu_En_Cours = Pourcentage_titu_En_Cours,
      Temps_de_jeu_En_Cours = Temps_de_jeu_En_Cours,
      Buts_En_Cours = Buts_En_Cours,
      Passes_En_Cours = Passes_En_Cours,
      Division_En_Cours = Division_En_Cours,
      Groupes_En_Cours = Groupes_En_Cours,
      Pays_En_Cours = Pays_En_Cours,
      
      # Saison N-1
      Club_N_1 = Club_N_1,
      Titu_N_1 = Titularisations_N_1,
      Apparition_Groupe_N_1 = Nombre_Matchs_Equipe_N_1,
      Pourcentage_titu_N_1 = Pourcentage_titu_N_1,
      Temps_de_jeu_N_1 = Temps_de_jeu_N_1,
      Buts_N_1 = Buts_N_1,
      Passes_N_1 = Passes_N_1,
      Division_N_1 = Division_N_1,
      Groupes_N_1 = Groupes_N_1,
      Pays_N_1 = Pays_N_1,
      
      # Saison N-2
      Club_N_2 = Club_N_2,
      Titu_N_2 = Titularisations_N_2,
      Apparition_Groupe_N_2 = Nombre_Matchs_Equipe_N_2,
      Pourcentage_titu_N_2 = Pourcentage_titu_N_2,
      Temps_de_jeu_N_2 = Temps_de_jeu_N_2,
      Buts_N_2 = Buts_N_2,
      Passes_N_2 = Passes_N_2,
      Division_N_2 = Division_N_2,
      Groupes_N_2 = Groupes_N_2,
      Pays_N_2 = Pays_N_2,
      
      # Saison N-3
      Club_N_3 = Club_N_3,
      Titu_N_3 = Titularisations_N_3,
      Apparition_Groupe_N_3 = Nombre_Matchs_Equipe_N_3,
      Pourcentage_titu_N_3 = Pourcentage_titu_N_3,
      Temps_de_jeu_N_3 = Temps_de_jeu_N_3,
      Buts_N_3 = Buts_N_3,
      Passes_N_3 = Passes_N_3,
      Division_N_3 = Division_N_3,
      Groupes_N_3 = Groupes_N_3,
      Pays_N_3 = Pays_N_3,
      
      Fin_contrat = fin_contrat,
      stringsAsFactors = FALSE
    )
    
    Sys.sleep(runif(1, min = 3, max = 6))
  }
  
  df_final <- bind_rows(resultats)
  write.csv2(df_final, chemin_sortie, row.names = FALSE, fileEncoding = "UTF-8", sep = ',')
  
  message("\n✅ Terminé !")
  return(chemin_sortie)
}

#lancer_scraping("test.csv","resultats_scraping.csv")
#df_test = read.csv2("resultats_scraping.csv", fileEncoding = "UTF-8")