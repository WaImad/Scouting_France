# On installe pacman s'il n'est pas là
if (!require("pacman")) install.packages("pacman")

# installe les packages si absents et les charge
pacman::p_load(shiny, dplyr, rvest, httr, stringr, googlesheets4,jsonlite)

lancer_scraping <- function() {
  
  headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
    `Accept-Language` = "fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7"
  )
  
  ## Scrapping de la page de transfermarkt
  url_simple = "https://www.transfermarkt.fr"
  url_transfermarkt <- "https://www.transfermarkt.fr/ligue-1/startseite/wettbewerb"
  url_club_passe_base = "https://www.transfermarkt.fr/m/startseite/verein/"
  
  championnats <- c("CN2A")#,"CN2B","CN2C","C3CM","C3NA","C3VL","C3PL","C3BR","C3NO","C3HF","C3IF","F19A","F19B","F19C","F19D")
  # Configuration du Headers
  
  
  resultats <- list()
  
  for (championnat in championnats) {
    url_championnat <- paste0(url_transfermarkt,"/",championnat)
    
    rep_championnat <- GET(url_championnat, add_headers(.headers = headers))
    
    
    if (status_code(rep_championnat) == 200) {
      #Récupère la page du championnat
      page_championnat <- read_html(rep_championnat)
      
      #On parcours la table des clubs et on récup le lien de chaque club de chaque championnat
      lien_clubs <- page_championnat |>
        html_elements("#yw1 .no-border-links a:nth-child(1)") |>
        html_attr("href")
      
      for (club in lien_clubs){
        
        url_club <- paste0(url_simple,club)
        
        rep_club <- GET(url_club, add_headers(.headers = headers))
        
        if (status_code(rep_club) == 200) {
          
          page_club <- read_html(rep_club)
          
          lien_joueurs <- page_club |> html_elements(".inline-table a")  |> html_attr("href")
          for (joueur in lien_joueurs) {
            
            #INITIALISATION DES VAR
            url_joueur <- "Non trouvée"
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
            
            Buts_En_Cours <- 0; Passes_En_Cours <- 0; Division_En_Cours <- ""; Pays_En_Cours <- ""; Titularisations_En_Cours <- 0; Groupes_En_Cours <- ""; Pourcentage_titu_En_Cours <- 0; Nombre_Matchs_Equipe_En_Cours = 0; Club_En_Cours = ""; Temps_de_jeu_En_Cours = 0
            Buts_N_1 <- 0; Passes_N_1 <- 0; Division_N_1 <- ""; Pays_N_1 <- ""; Titularisations_N_1 <- 0; Groupes_N_1 <- ""; Pourcentage_titu_N_1 <- 0; Nombre_Matchs_Equipe_N_1 = 0; Club_N_1 = ""; Temps_de_jeu_N_1 = 0
            Buts_N_2 <- 0; Passes_N_2 <- 0; Division_N_2 <- ""; Pays_N_2 <- ""; Titularisations_N_2 <- 0; Groupes_N_2 <- ""; Pourcentage_titu_N_2 <- 0; Nombre_Matchs_Equipe_N_2 = 0; Club_N_2 = ""; Temps_de_jeu_N_2 = 0
            Buts_N_3 <- 0; Passes_N_3 <- 0; Division_N_3 <- ""; Pays_N_3 <- ""; Titularisations_N_3 <- 0; Groupes_N_3 <- ""; Pourcentage_titu_N_3 <- 0; Nombre_Matchs_Equipe_N_3 = 0; Club_N_3 = ""; Temps_de_jeu_N_3 = 0
            
            url_joueur <- paste0(url_simple, joueur)
            
            rep_joueur <- GET(url_joueur,add_headers(.headers = headers))
            
            
            if (status_code(rep_joueur) == 200) {
              
              page_joueur <- read_html(rep_joueur)
              
              nom_prenom <- page_joueur |> html_element(".data-header__headline-wrapper") |> html_text(trim = TRUE)
              nom_prenom <- str_squish(nom_prenom)
              
              nom_prenom <- str_remove(nom_prenom, "^#\\d+\\s*")
              nom_prenom <- str_split(nom_prenom," ")[1][1]
              prenom <- nom_prenom[[1]][1]
              nom <- nom_prenom[[1]][2:3] |> paste(collapse = " ")
              if (grepl(" NA",nom)) nom <- str_remove(nom, " NA")
              
              print(nom)
              print(prenom)
              
              club_noeud <- page_joueur |> html_element(".data-header__club a") |> html_text(trim = TRUE)
              if (!is.na(club_noeud)) club <- club_noeud
              
              photo_profil_noeud <- page_joueur |> html_element(".data-header__profile-image") |> html_attr("src")
              if (!is.na(photo_profil_noeud)) photo_profil <- photo_profil_noeud
              
              photo_club_noeud <- page_joueur |> html_element(".data-header__box__club-link img") |> html_attr("srcset")
              if (!is.na(photo_club_noeud)) photo_club <- trimws(strsplit(photo_club_noeud, " 1x")[[1]][1])
              
              division_club_actuel <- page_joueur |> html_element(".data-header__league-link") |> html_text(trim = TRUE)
              if (!is.na(division_club_actuel)) {
                division_club_actuel <- str_split(division_club_actuel, "-")[[1]][1] |> str_trim()
                
                nationalite_noeud <- page_joueur |> html_element(".data-header__items .data-header__label~ .data-header__label+ .data-header__label .data-header__content") |> html_text(trim = TRUE)
                if (!is.na(nationalite_noeud)) nationalité <- nationalite_noeud 
                
                date_naissance_noeud <- page_joueur |> html_element(".data-header__items:nth-child(1) .data-header__label:nth-child(1) .data-header__content") |> html_text(trim = TRUE)
                if (!is.na(date_naissance_noeud)) date_naissance <- date_naissance_noeud
                
                #Taille
                taille_noeud <- page_joueur |> html_element(".data-header__items:nth-child(2) .data-header__label:nth-child(1) .data-header__content") |> html_text(trim = TRUE)
                #si taille fini pas par m alors non trouvée
                
                if (!is.na(taille_noeud) && grepl("m$", taille_noeud)) taille <- taille_noeud
                
                club_jeune_brut <- page_joueur |> html_element("h2+ .content") |> html_text(trim = TRUE)
                if(is.na(club_jeune_brut) || length(club_jeune_brut) == 0 || club_jeune_brut == "") {
                  club_jeune <- "" 
                } else {
                  
                  club_jeune_net <- str_replace_all(club_jeune_brut, "[\r\n\t]", " ")
                  
                  # coupe le texte à chaque virgule
                  liste_clubs <- str_split(club_jeune_net, ",")[[1]]
                  
                  # On sélectionne UNIQUEMENT le dernier élément de cette liste
                  dernier_club <- liste_clubs[length(liste_clubs)]
                  
                  # On enlève les dates entre parenthèses pour que ce soit propre
                  
                  dernier_club <- str_replace_all(dernier_club, "\\(.*?\\)", "")
                  
                  club_jeune <- str_trim(dernier_club)
                }
                
                agent <- page_joueur |> 
                  html_element(xpath = "//span[contains(@class, 'info-table__content--regular') and contains(text(), 'Agent')]/following-sibling::span[1]") |> 
                  html_text(trim = TRUE)
                
                
                position_principale_noeud <- page_joueur |> html_element('.data-header__items+ .data-header__items .data-header__content') |> html_text(trim = TRUE)
                if (!is.na(position_principale_noeud) && !grepl("m$" ,position_principale_noeud)) position_principale <- position_principale_noeud else position_principale <- page_joueur |> html_element('.data-header__items+ .data-header__items .data-header__label+ .data-header__label .data-header__content') |> html_text(trim = TRUE)
                
                #position secondaire
                position_secondaire_noeud <- page_joueur |> html_element(".detail-position__position .detail-position__position") |> html_text(trim = TRUE)
                position_secondaire <- if (!is.na(position_secondaire_noeud)) position_secondaire_noeud else ""
                
                position_complete <- paste0(position_principale, " / ", position_secondaire)
                
                
                # Construction de URL de L'API
                id_joueur <- str_extract(url_joueur,"[0-9]+")
                url_api <- paste0("https://tmapi-alpha.transfermarkt.technology/player/", id_joueur, "/performance-game")
                
                # Header simple pour dire à l'API qu'on veut du JSON
                headers_api <- c(
                  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
                  `Accept` = "application/json"
                )
                
                # --- SYSTÈME DE RETRY (NOUVEL ESSAI) ---
                tentatives <- 0
                max_tentatives <- 3
                code_statut <- 0
                
                while (tentatives < max_tentatives) {
                  rep_api <- GET(url_api, add_headers(.headers = headers_api))
                  code_statut <- status_code(rep_api)
                  
                  # test si le code est bon ou si erreur fatale on break
                  if (code_statut < 500) {
                    break
                  } # Si c'est un hoquet du coté du serveur (500, 502, 503), on attend et on réessaie
                  
                  tentatives <- tentatives + 1
                  message(sprintf("    -> ⚠️ Serveur indisponible (Code %s). Nouvel essai dans 5 secondes... (%d/%d)", code_statut, tentatives, max_tentatives))
                  Sys.sleep(5)
                }
                
                
                if (code_statut == 200) {
                  # 3. LECTURE DU JSON
                  json_text <- content(rep_api, as = "text", encoding = "UTF-8")
                  
                  # flatten = TRUE aplatit l'arborescence (ex: statistics.playingTimeStatistics.playedMinutes)
                  data_api <- fromJSON(json_text, flatten = TRUE) 
                  
                  # ici que tableau_stats est la liste des matchs
                  tableau_stats <- if ("data" %in% names(data_api)) data_api$data$performance else data_api
                  
                  if (is.data.frame(tableau_stats)) {
                    
                    # 4. BOUCLE SUR LES MATCHS
                    for (j in 1:nrow(tableau_stats)) {
                      
                      # Récupérer les cartons jaunes et rouge
                      # En gros ici on test si il y a la colonne présente dans le json qui stock les infos des cartons jaunes, rouges et jaune-rouge (deuxième jaune) et si elle existe on regarde si il y a une valeur pour le match en cours. Si oui, on incrémente le compteur correspondant.
                      
                      # Cartons jaunes uniques
                      col_jaune <- "statistics.cardStatistics.yellowCard.minute"
                      if (col_jaune %in% names(tableau_stats)) {
                        val_jaune <- tableau_stats[[col_jaune]][j]
                        # Si la valeur existe et n'est pas NA, il a pris un jaune : on fait +1
                        if (length(val_jaune) > 0 && !is.na(val_jaune)) {
                          cartons_jaunes <- cartons_jaunes + 1
                        }
                      }
                      
                      # Cartons Rouges directs
                      col_rouge <- "statistics.cardStatistics.redCard.minute"
                      if (col_rouge %in% names(tableau_stats)) {
                        val_rouge <- tableau_stats[[col_rouge]][j]
                        if (length(val_rouge) > 0 && !is.na(val_rouge)) {
                          cartons_rouges <- cartons_rouges + 1
                        }
                      }
                      
                      # Deuxièmes cartons jaunes (Jaune-Rouge)
                      col_jr <- "statistics.cardStatistics.yellowRedCard.minute"
                      if (col_jr %in% names(tableau_stats)) {
                        val_jr <- tableau_stats[[col_jr]][j]
                        if (length(val_jr) > 0 && !is.na(val_jr)) {
                          cartons_jaunes_rouges <- cartons_jaunes_rouges + 1
                        }
                      }
                      
                      # Récupérer l'ID de la compétition (ex: "FR3", "CN2B", "FRC")
                      competition_id <- tableau_stats$gameInformation.competitionId[j]
                      
                      # Récupérer les minutes (qui peuvent être NA si non joué)
                      minutes <- tableau_stats$statistics.playingTimeStatistics.playedMinutes[j]
                      
                      # Si l'ID est manquant ou que le joueur n'a pas joué (minutes = NA), on passe au match suivant
                      if (is.na(competition_id) || is.na(minutes)) {
                        next
                      }
                      
                      # On met en minuscules pour comparer plus facilement
                      comp_id_lower <- tolower(as.character(competition_id))
                      
                      # 5. CATÉGORISATION GLOBALE (Basée sur les ID officiels de Transfermarkt)
                      if (grepl("fr1", comp_id_lower)) {
                        Temps_L1 <- Temps_L1 + minutes
                        
                      } else if (grepl("fr2", comp_id_lower)) {
                        Temps_L2 <- Temps_L2 + minutes
                        
                      } else if (comp_id_lower == "fr3") {
                        Temps_N1 <- Temps_N1 + minutes
                        
                      } else if (grepl("^cn2", comp_id_lower)) {
                        # Tous les ID commençant par "cn2" (CN2A, CN2B, etc.)
                        Temps_N2 <- Temps_N2 + minutes
                        
                      } else if (grepl("^c3[a-z]+|^cn3", comp_id_lower)) {
                        # En France, N3 est souvent tagué CN3 ou C3 suivi de lettres (ex: C3AR)
                        Temps_N3 <- Temps_N3 + minutes
                        
                      } else if (comp_id_lower == "frc") {
                        # FRC = France Coupe
                        Temps_Coupe_France <- Temps_Coupe_France + minutes
                        
                      } else {
                        # Tout le reste (EL, C1, autres championnats,jeunes...)
                        Temps_Etranger_Autres <- Temps_Etranger_Autres + minutes
                      }
                    }
                  }
                  
                  ### PARTIE SUR L'HISTORIQUE DU JOUEUR sur les 3 dernières saisons
                  if (is.data.frame(tableau_stats)) {
                    
                    # les saisons qu'on veut obtenir 
                    annee_actuelle <- as.numeric(format(Sys.Date(), "%Y"))
                    mois_actuel <- as.numeric(format(Sys.Date(), "%m"))
                    debut_saison_actuelle <- ifelse(mois_actuel >= 7, annee_actuelle, annee_actuelle - 1) # Si on est en juillet ou après, la saison actuelle est celle de l'année en cours, sinon c'est celle de l'année précédente)
                    
                    saisons_cibles <- c(
                      debut_saison_actuelle,       
                      debut_saison_actuelle - 1,   
                      debut_saison_actuelle - 2,   
                      debut_saison_actuelle - 3    
                    )
                    noms_saisons <- c("En_Cours", "N_1", "N_2", "N_3")
                    
                    for (k in 1:length(saisons_cibles)) {
                      id_saison_actuel <- saisons_cibles[k]
                      suffixe <- noms_saisons[k]
                      
                      # On filtre le tableau pour la saison en cours de la boucle
                      if ("gameInformation.season.id" %in% names(tableau_stats)) {
                        stats_saison <- tableau_stats |> filter(gameInformation.season.id == id_saison_actuel & statistics.generalStatistics.participationState != "injured" & statistics.generalStatistics.participationState != "absent" & statistics.generalStatistics.participationState != "not in squad")
                      } else {
                        stats_saison <- data.frame() 
                        
                      }
                      
                      # On prépare nos compteurs à ZÉRO pour cette saison 
                      titularisations <- 0
                      saison_buts <- 0
                      saison_passes <- 0
                      division <- ""
                      pays_club_saison <- ""
                      groupes <- ""
                      nbr_matchs <- 0
                      pourcentage_titu <-0
                      nom_club = ""
                      club_id = ""
                      minutes_saison = 0
                      
                      #Si le joueur a joué cette saison-là
                      if (nrow(stats_saison) > 0) {
                        # Récupérer le niveau du championnat dans lequel il a joué
                        # Récupérer l'ID de la compétition (ex: "FR3", "CN2B")
                        competition_id <- stats_saison$gameInformation.competitionId[1]
                        
                        # On met en minuscules pour comparer plus facilement
                        comp_id_lower <- tolower(as.character(competition_id))
                        
                        # ==========================================
                        # PAYS du CHAMPIONNAT (en se basant sur l'ID de la compétition)
                        # ==========================================
                        # Si l'ID commence par fr, cn, c3 ou f19, c'est en France 
                        if (grepl("^fr|^cn|^c3|^f19", comp_id_lower)) {
                          pays_club_saison <- "France"
                        } else if (grepl("^gb", comp_id_lower)) {
                          pays_club_saison <- "Angleterre"
                        } else if (grepl("^es", comp_id_lower)) {
                          pays_club_saison <- "Espagne"
                        } else if (grepl("^it", comp_id_lower)) {
                          pays_club_saison <- "Italie"
                        } else if (grepl("^l[1-9]", comp_id_lower)) {
                          pays_club_saison <- "Allemagne"
                        } else if (grepl("^nl", comp_id_lower)) {
                          pays_club_saison <- "Pays-Bas"
                        } else if (grepl("^be", comp_id_lower)) {
                          pays_club_saison <- "Belgique"
                        } else if (grepl("^pt", comp_id_lower)) {
                          pays_club_saison <- "Portugal"
                        } else if (grepl("^lux", comp_id_lower)) {
                          pays_club_saison <- "Luxembourg"
                        } else if (grepl("c1|c2", comp_id_lower)) {
                          pays_club_saison <- "Suisse"
                        }
                        else if (grepl("fs", comp_id_lower)) {
                          pays_club_saison <- nationalité
                          division <- "Sélection Nationale"
                        } 
                        else {
                          pays_club_saison <- "Étranger"
                        }
                        
                        
                        # ==========================================
                        # 2. TEST DE LA DIVISION
                        # ==========================================
                        if (grepl("1$", comp_id_lower)) {
                          division <- "L1"
                        } else if (grepl("2$", comp_id_lower)) {
                          division <- "L2"
                        } else if (comp_id_lower == "fr3" ||grepl("3$", comp_id_lower)) {
                          division <- "N1/L3"
                        } else if (grepl("^cn2", comp_id_lower)) {
                          # Tous les ID commençant par "cn2" (CN2A, CN2B, etc.)
                          division <- "N2"
                          #Extrait que les lettres après CN2 pour différencier les groupes (ex: CN2A, CN2B)
                          groupes <- toupper(sub("^cn2","",comp_id_lower)) # Extrait A ou B pour le groupe
                        } else if (grepl("^c3[a-z]+|^cn3", comp_id_lower)) {
                          # En France, N3 est souvent tagué CN3 ou C3 suivi de lettres (ex: C3AR)
                          division <- "N3"
                          groupes <- toupper(sub("^c3|^cn3", "", comp_id_lower)) 
                        } else if (grepl("^f19", comp_id_lower)) {
                          # F19 = U19 NAT
                          division <- "U19 NAT"
                          groupes <- toupper(sub("^f19", "", comp_id_lower))
                        } else {
                          # Tout le reste
                          division <- "Division étrangère"
                        }
                        
                        # Addition des Buts et Passes
                        if ("statistics.goalStatistics.goalsScoredTotal" %in% names(stats_saison)) {
                          saison_buts <- sum(stats_saison$statistics.goalStatistics.goalsScoredTotal, na.rm = TRUE)
                        }
                        if ("statistics.goalStatistics.assists" %in% names(stats_saison)) {
                          saison_passes <- sum(stats_saison$statistics.goalStatistics.assists, na.rm = TRUE)
                        }
                        #Titularisation
                        if ("statistics.playingTimeStatistics.isStarting" %in% names(stats_saison)) {
                          titularisations <- sum(stats_saison$statistics.playingTimeStatistics.isStarting, na.rm = TRUE)
                        }
                        
                        if ("gameInformation.gameId" %in% names(stats_saison)) { #Le pourcentage de Titu est calculé sur les matchs de l'équipe du joueur meme quand il est pas dans l'effectif
                          nbr_matchs <- n_distinct(stats_saison$gameInformation.gameId)
                          pourcentage_titu <- round((titularisations/nbr_matchs),2)
                        }
                        
                        #Titularisation
                        if ("statistics.playingTimeStatistics.playedMinutes" %in% names(stats_saison)) {
                          minutes_saison <- sum(stats_saison$statistics.playingTimeStatistics.playedMinutes, na.rm = TRUE)
                        }
                        
                        # Récupérer le nom des clubs des saisons passés
                        if ("clubsInformation.club.clubId" %in% names(stats_saison)) {
                          club_id <- stats_saison$clubsInformation.club.clubId[1]
                          
                          if (length(club_id) > 0) {
                            url_club_passe <- paste0(url_club_passe_base, club_id)
                            
                            rep_club_passe <- GET(url_club_passe, add_headers(.headers = headers))
                            
                            if (status_code(rep_club_passe) == 200) {
                              nom_club = ""
                              page_club_passe <- read_html(rep_club_passe)
                              nom_club <- page_club_passe |> html_element(".data-header__headline-wrapper--oswald") |> html_text(trim = TRUE)
                              
                            }
                          }
                        }
                        
                        # ENREGISTREMENT
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
                }
              }
              
              resultats[[joueur]] <- data.frame(
                NOMS = nom,
                PRENOMS = prenom,
                #URL_Recherche_Utilisee = url_recherche, # Pratique pour vérifier ce qui a été cherché
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
                URL_Profil = url_joueur,
                Date_Naissance = date_naissance,
                Position_Principale = position_principale,
                Position_Secondaire = position_secondaire,
                Position_Complete = position_complete,
                Forme_A = club_jeune,
                Pied_Fort = pied_fort,
                Taille = taille,
                Club_actuel = club,
                Photo_Club = photo_club,
                Pays_Club_actuel = "France",
                
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
                
                
                #Position_Secondaire = position_secondaire,
                #Position_Schema = position_schema,
                
                
                stringsAsFactors = FALSE
              )
            }
            
          }
          
        }
      }
      
      
    }
    df_final <- bind_rows(resultats)
    return(df_final)
  }
}
  
#write.csv2(lancer_scraping(), "resultats_scraping.csv", row.names = FALSE)
  
  
