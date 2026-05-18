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

championnats <- c("CN2A") # "FR1","FR2","FR3","CN2B","CN2C","C3CM","C3NA","C3VL","C3PL","C3BR","C3NO","C3HF","C3IF")
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
          
          Buts_25_26 <- 0; Passes_25_26 <- 0; Division_25_26 <- ""; Pays_25_26 <- ""; Titularisations_25_26 <- 0; Groupes_25_26 <- ""; Pourcentage_titu_25_26 <- 0; Nombre_Matchs_Equipe_25_26 = 0
          Buts_24_25 <- 0; Passes_24_25 <- 0; Division_24_25 <- ""; Pays_24_25 <- ""; Titularisations_24_25 <- 0; Groupes_24_25 <- ""; Pourcentage_titu_24_25 <- 0; Nombre_Matchs_Equipe_24_25 = 0
          Buts_23_24 <- 0; Passes_23_24 <- 0; Division_23_24 <- ""; Pays_23_24 <- ""; Titularisations_23_24 <- 0; Groupes_23_24 <- ""; Pourcentage_titu_23_24 <- 0; Nombre_Matchs_Equipe_23_24 = 0
          Buts_22_23 <- 0; Passes_22_23 <- 0; Division_22_23 <- ""; Pays_22_23 <- ""; Titularisations_22_23 <- 0; Groupes_22_23 <- ""; Pourcentage_titu_22_23 <- 0; Nombre_Matchs_Equipe_22_23 = 0
          
          url_joueur <- paste0(url_simple, joueur)
          
          rep_joueur <- GET(url_joueur,add_headers(.headers = headers))
          
          
          if (status_code(rep_joueur) == 200) {
            
            page_joueur <- read_html(rep_joueur)
            
            nom_prenom <- page_joueur |> html_element(".data-header__headline-wrapper") |> html_text(trim = TRUE)
            nom_prenom <- str_squish(nom_prenom)
            
            nom_prenom <- str_remove(nom_prenom, "^#\\d+\\s*")
            nom_prenom <- str_split(nom_prenom," ")[1][1]
            prenom <- nom_prenom[[1]][1]
            nom <- nom_prenom[[1]][2]
            
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
              
              
              position_principale_noeud <- page_joueur |> html_element(".detail-position__inner-box .detail-position__position") |> html_text(trim = TRUE)
              if (!is.na(position_principale_noeud)) position_principale <- position_principale_noeud
              
              #position secondaire
              position_secondaire_noeud <- page_joueur |> html_element(".detail-position__position .detail-position__position") |> html_text(trim = TRUE)
              position_secondaire <- if (!is.na(position_secondaire_noeud)) position_secondaire_noeud else ""
              
              position_principale <- paste0(position_principale, " / ", position_secondaire)
              
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
                  
                  # les saisons qu'on veut obtenir ( à modifier à chaque début de saison)
                  saisons_cibles <- c(2025, 2024, 2023, 2022)
                  noms_saisons <- c("25_26", "24_25", "23_24", "22_23")
                  
                  for (k in 1:length(saisons_cibles)) {
                    id_saison_actuel <- saisons_cibles[k]
                    suffixe <- noms_saisons[k]
                    
                    # On filtre le tableau pour la saison en cours de la boucle
                    if ("gameInformation.season.id" %in% names(tableau_stats)) {
                      stats_saison <- tableau_stats |> filter(gameInformation.season.id == id_saison_actuel & statistics.generalStatistics.participationState != "injured" & statistics.generalStatistics.participationState != "absent")
                      
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
                    }
                    
                    # ENREGISTREMENT
                    if (suffixe == "25_26") {
                      Buts_25_26 <- saison_buts; Passes_25_26 <- saison_passes; Division_25_26 <- division; Pays_25_26 <- pays_club_saison; Titularisations_25_26 <- titularisations; Groupes_25_26 <- groupes; Pourcentage_titu_25_26 <- pourcentage_titu; Nombre_Matchs_Equipe_25_26 = nbr_matchs
                    } else if (suffixe == "24_25") {
                      Buts_24_25 <- saison_buts; Passes_24_25 <- saison_passes; Division_24_25 <- division; Pays_24_25 <- pays_club_saison; Titularisations_24_25 <- titularisations;Groupes_24_25 <- groupes; Pourcentage_titu_24_25 <- pourcentage_titu; Nombre_Matchs_Equipe_24_25 = nbr_matchs
                    } else if (suffixe == "23_24") {
                      Buts_23_24 <- saison_buts; Passes_23_24 <- saison_passes; Division_23_24 <- division; Pays_23_24 <- pays_club_saison; Titularisations_23_24 <- titularisations; Groupes_23_24 <- groupes; Pourcentage_titu_23_24 <- pourcentage_titu; Nombre_Matchs_Equipe_23_24 = nbr_matchs
                    } else if (suffixe == "22_23") {
                      Buts_22_23 <- saison_buts; Passes_22_23 <- saison_passes; Division_22_23 <- division; Pays_22_23 <- pays_club_saison; Titularisations_22_23 <- titularisations; Groupes_22_23 <- groupes; Pourcentage_titu_22_23 <- pourcentage_titu; Nombre_Matchs_Equipe_22_23 = nbr_matchs
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
            Forme_A = club_jeune,
            Pied_Fort = pied_fort,
            Taille = taille,
            Club_actuel = club,
            Photo_Club = photo_club,
            Pays_Club_actuel = "France",
            
            # Saison 25/26
            Titu_25_26 = Titularisations_25_26,
            Nombre_Matchs_Equipe_25_26 = Nombre_Matchs_Equipe_25_26,
            Pourcentage_titu_25_26 = Pourcentage_titu_25_26,
            Buts_25_26 = Buts_25_26,
            Passes_25_26 = Passes_25_26,
            Division_25_26 = Division_25_26,
            Groupes_25_26 = Groupes_25_26,
            Pays_25_26 = Pays_25_26,
            
            
            # Saison 24/25
            Titu_24_25 = Titularisations_24_25,
            Nombre_Matchs_Equipe_24_25 = Nombre_Matchs_Equipe_24_25,
            Pourcentage_titu_24_25 = Pourcentage_titu_24_25,
            Buts_24_25 = Buts_24_25,
            Passes_24_25 = Passes_24_25,
            Division_24_25 = Division_24_25,
            Groupes_24_25 = Groupes_24_25,
            Pays_24_25 = Pays_24_25,
            
            # Saison 23/24
            Titu_23_24 = Titularisations_23_24,
            Nombre_Matchs_Equipe_23_24 = Nombre_Matchs_Equipe_23_24,
            Pourcentage_titu_23_24 = Pourcentage_titu_23_24,
            Buts_23_24 = Buts_23_24,
            Passes_23_24 = Passes_23_24,
            Division_23_24 = Division_23_24,
            Groupes_23_24 = Groupes_23_24,
            Pays_23_24 = Pays_23_24,
            
            # Saison 22/23
            Titu_22_23 = Titularisations_22_23,
            Nombre_Matchs_Equipe_22_23 = Nombre_Matchs_Equipe_22_23,
            Pourcentage_titu_22_23 = Pourcentage_titu_22_23,
            Buts_22_23 = Buts_22_23,
            Passes_22_23 = Passes_22_23,
            Division_22_23 = Division_22_23,
            Groupes_22_23 = Groupes_22_23,
            Pays_22_23 = Pays_22_23,
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

#write.csv2(lancer_scraping(), "resultats_scraping.csv", row.names = FALSE)

