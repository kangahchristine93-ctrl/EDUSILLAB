# EDUSILLAB - v35 POSIT CLOUD CORRIGÉ - logo CCNB + icônes + répertoire analyses + manifest
library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(readxl)
library(jsonlite)

# ============================================================
# EDUSILLAB / CCNB CAMPUS DE DIEPPE
# VERSION WEB
# ============================================================
# Les utilisateurs accèdent au portail depuis un navigateur.
# R et RStudio ne sont nécessaires que sur le serveur d'hébergement.
#
# Variables d'environnement prises en charge :
#   EDUSILLAB_DATA_DIR          dossier persistant de la base
#   EDUSILLAB_DB_FILE           nom du fichier SQLite
#   EDUSILLAB_ADMIN_PASSWORD    mot de passe initial admin
#   EDUSILLAB_TZ                fuseau horaire
#   PORT                        port fourni par l'hébergeur
#
# En Docker, EDUSILLAB_DATA_DIR=/data est utilisé par défaut.

options(
  shiny.maxRequestSize = 100 * 1024^2,
  shiny.fullstacktrace = FALSE
)

edusillab_tz <- Sys.getenv("EDUSILLAB_TZ", unset = "America/Moncton")
Sys.setenv(TZ = edusillab_tz)

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

data_dir_env <- trimws(Sys.getenv("EDUSILLAB_DATA_DIR", unset = ""))
data_dir <- if (nzchar(data_dir_env)) {
  normalizePath(data_dir_env, winslash = "/", mustWork = FALSE)
} else {
  app_dir
}

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

db_file <- trimws(Sys.getenv("EDUSILLAB_DB_FILE", unset = "laboratoire.db"))
if (!nzchar(db_file)) db_file <- "laboratoire.db"

db_path <- file.path(data_dir, db_file)

message("EDUSILLAB WEB")
message("Application : ", app_dir)
message("Données persistantes : ", data_dir)
message("Base : ", db_path)
message("Fuseau horaire : ", edusillab_tz)
message("Version : EDUSILLAB v36.2 MENU BLEU COMPLET")
message("Logo CCNB intégré : OUI")
message("Icônes du tableau de bord intégrées : OUI")
message("Correctif import patients : OUI")
message("Correctif détection # of tubes : OUI")
message("Namespaces DBI/DT explicites : OUI")

ouvrir_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Paramètres adaptés à une application web avec plusieurs sessions.
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA synchronous = NORMAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 15000")

  con
}

norm_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

norm_upper <- function(x) toupper(norm_txt(x))

sql_colonnes <- function(con, table) {
  DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))$name
}

ajouter_colonne_si_absente <- function(con, table, colonne, definition) {
  if (!colonne %in% sql_colonnes(con, table)) {
    DBI::dbExecute(con, paste(
      "ALTER TABLE", table,
      "ADD COLUMN", colonne, definition
    ))
  }
}

email_valide <- function(x) {
  x <- norm_txt(x)
  nzchar(x) &&
    grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)
}

generer_numero_requisition <- function(con) {
  repeat {
    numero <- paste0(sample(0:9, 10, replace = TRUE), collapse = "")
    if (substr(numero, 1, 1) == "0") {
      substr(numero, 1, 1) <- as.character(sample(1:9, 1))
    }

    existe <- DBI::dbGetQuery(
      con,
      "SELECT id FROM requisitions WHERE numero_requisition = ? LIMIT 1",
      params = list(numero)
    )

    if (nrow(existe) == 0) return(numero)
  }
}

generer_code_barre <- function(con) {
  repeat {
    code <- paste0(sample(0:9, 6, replace = TRUE), collapse = "")
    if (substr(code, 1, 1) == "0") substr(code, 1, 1) <- as.character(sample(1:9, 1))
    existe <- DBI::dbGetQuery(con, "SELECT id FROM specimens WHERE code_barre = ? LIMIT 1", params = list(code))
    if (nrow(existe) == 0) return(code)
  }
}

code_priorite <- function(x) {
  x <- toupper(norm_txt(x))
  if (x == "STAT") return("S")
  if (x == "URGENCE") return("U")
  "R"
}


formater_nombre_tubes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]

  if (length(x) == 0) return("N/P")

  total <- sum(x)

  if (abs(total - round(total)) < 0.000001) {
    return(as.character(as.integer(round(total))))
  }

  format(total, trim = TRUE, scientific = FALSE)
}

generer_numero_specimen <- function(con, date_specimen, px, priorite) {
  d <- as.Date(date_specimen)
  if (is.na(d)) d <- Sys.Date()
  prefixe_date <- format(d, "%d%m")
  px <- toupper(gsub("[^A-Z0-9]", "", norm_txt(px)))
  if (!nzchar(px)) px <- "X"
  n <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM specimens WHERE date_specimen = ? AND UPPER(COALESCE(px,'')) = UPPER(?)",
    params = list(as.character(d), px)
  )$n[1] + 1L
  paste0(prefixe_date, ":", px, sprintf("%05d", n), code_priorite(priorite))
}

code39_patterns <- c(
  "0"="nnnwwnwnn","1"="wnnwnnnnw","2"="nnwwnnnnw","3"="wnwwnnnnn",
  "4"="nnnwwnnnw","5"="wnnwwnnnn","6"="nnwwwnnnn","7"="nnnwnnwnw",
  "8"="wnnwnnwnn","9"="nnwwnnwnn","*"="nwnnwnwnn"
)

dessiner_code39 <- function(code, x, y, largeur, hauteur) {
  chars <- strsplit(paste0("*", code, "*"), "")[[1]]
  poids_total <- 0
  for (ch in chars) {
    pat <- strsplit(code39_patterns[[ch]], "")[[1]]
    poids_total <- poids_total + sum(ifelse(pat == "w", 3, 1)) + 1
  }
  pos <- x
  for (ch in chars) {
    pat <- strsplit(code39_patterns[[ch]], "")[[1]]
    for (i in seq_along(pat)) {
      w <- largeur * (ifelse(pat[i] == "w", 3, 1) / poids_total)
      if (i %% 2 == 1) rect(pos, y, pos + w, y + hauteur, col = "black", border = NA)
      pos <- pos + w
    }
    pos <- pos + largeur * (1 / poids_total)
  }
}

preview_dir <- file.path(tempdir(), "edulab_label_previews")
dir.create(preview_dir, showWarnings = FALSE, recursive = TRUE)
addResourcePath("edulab_labels", preview_dir)

creer_pdf_etiquettes <- function(
  labels, fichier, largeur_mm = 100, hauteur_mm = 50,
  offset_x_mm = 0, offset_y_mm = 0, echelle_pct = 100
) {
  if (is.null(labels) || nrow(labels) == 0) stop("Aucune étiquette à générer.")

  # La taille physique du PDF est directement celle configurée pour
  # l'imprimante. La mise en page reste proportionnelle et la taille
  # du texte est ajustée légèrement selon le format de l'étiquette.
  facteur <- min(
    1.35,
    max(
      0.72,
      min(largeur_mm / 100, hauteur_mm / 50)
    )
  )

  grDevices::pdf(
    fichier,
    width = largeur_mm / 25.4,
    height = hauteur_mm / 25.4,
    paper = "special",
    onefile = TRUE,
    family = "Helvetica"
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  for (i in seq_len(nrow(labels))) {
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot.new()

    echelle <- suppressWarnings(as.numeric(echelle_pct))
    if (is.na(echelle) || echelle <= 0) echelle <- 100
    echelle <- echelle / 100

    dx <- suppressWarnings(as.numeric(offset_x_mm))
    dy <- suppressWarnings(as.numeric(offset_y_mm))
    if (is.na(dx)) dx <- 0
    if (is.na(dy)) dy <- 0

    dx_logique <- dx / largeur_mm * 100
    dy_logique <- dy / hauteur_mm * 50
    largeur_vue <- 100 / echelle
    hauteur_vue <- 50 / echelle

    plot.window(
      xlim = c(50 - largeur_vue / 2 - dx_logique, 50 + largeur_vue / 2 - dx_logique),
      ylim = c(25 - hauteur_vue / 2 - dy_logique, 25 + hauteur_vue / 2 - dy_logique)
    )

    # GRAND CODE À BARRES
    dessiner_code39(
      labels$code_barre[i],
      x = 3,
      y = 38.5,
      largeur = 94,
      hauteur = 10
    )

    # BC# affiché une seule fois.
    text(
      50, 36.3,
      paste0("BC# ", labels$code_barre[i]),
      adj = 0.5,
      cex = 0.72 * facteur,
      font = 2
    )

    # POINT 2 + POINT 7 : numéro d'échantillon et dossier.
    text(
      4, 32.7,
      labels$numero_specimen[i],
      adj = 0,
      cex = 0.76 * facteur,
      font = 2
    )
    text(
      96, 32.7,
      paste0("DOSSIER ", labels$numero_dossier[i]),
      adj = 1,
      cex = 0.70 * facteur,
      font = 2
    )

    # POINT 3 + POINT 12
    text(
      4, 28.6,
      paste0(labels$nom[i], ", ", labels$prenom[i]),
      adj = 0,
      cex = 0.76 * facteur,
      font = 2
    )
    text(
      96, 28.6,
      paste0("SEXE: ", labels$sexe[i]),
      adj = 1,
      cex = 0.66 * facteur
    )

    # POINT 8 + POINT 5
    text(
      4, 24.5,
      paste0("MED ", labels$medicare[i]),
      adj = 0,
      cex = 0.64 * facteur
    )
    text(
      96, 24.5,
      paste0("LOC ", labels$location[i]),
      adj = 1,
      cex = 0.64 * facteur
    )

    # POINT 9 + POINT 10
    text(
      4, 20.3,
      paste0("# TUBES: ", labels$quantite[i]),
      adj = 0,
      cex = 0.66 * facteur,
      font = 2
    )
    text(
      96, 20.3,
      labels$contenant[i],
      adj = 1,
      cex = 0.62 * facteur,
      font = 2
    )

    # POINT 6 : analyses en ligne, séparées par des virgules.
    analyses_aff <- labels$analyses[i]
    analyses_aff <- gsub(" | ", ", ", analyses_aff, fixed = TRUE)

    cex_tests <- if (nchar(analyses_aff) > 100) {
      0.46
    } else if (nchar(analyses_aff) > 75) {
      0.52
    } else if (nchar(analyses_aff) > 55) {
      0.58
    } else {
      0.64
    }

    text(
      4, 15.7,
      paste0("ANALYSES: ", analyses_aff),
      adj = 0,
      cex = cex_tests * facteur,
      font = 2
    )

    # Le PX et le département ne sont pas imprimés sur l'étiquette.
    # Le numéro de réquisition reste discret en bas.
    text(
      96, 10.8,
      paste0("REQ ", labels$numero_requisition[i]),
      adj = 1,
      cex = 0.52 * facteur
    )

    # TRAÇABILITÉ DE SAISIE : nom complet, date/heure et signature automatique.
    nom_saisie <- if ("saisi_nom_complet" %in% names(labels)) labels$saisi_nom_complet[i] else ""
    date_saisie <- if ("date_saisie" %in% names(labels)) labels$date_saisie[i] else ""
    signature_saisie <- if ("signature_initiales" %in% names(labels)) labels$signature_initiales[i] else ""
    nom_saisie <- ifelse(is.na(nom_saisie), "", as.character(nom_saisie))
    date_saisie <- ifelse(is.na(date_saisie), "", as.character(date_saisie))
    signature_saisie <- ifelse(is.na(signature_saisie), "", as.character(signature_saisie))

    text(4, 10.8, paste0("SAISI PAR: ", nom_saisie), adj=0, cex=0.46*facteur, font=2)
    text(4, 7.4, paste0("DATE/HEURE: ", date_saisie), adj=0, cex=0.43*facteur)
    text(96, 7.4, paste0("SIGNATURE: ", signature_saisie), adj=1, cex=0.46*facteur, font=3)
  }

  invisible(fichier)
}


creer_pdf_etiquettes_selection <- function(
  labels,
  indices,
  fichier,
  largeur_mm = 100,
  hauteur_mm = 50
) {
  if (is.null(labels) || nrow(labels) == 0) {
    stop("Aucune étiquette disponible.")
  }

  indices <- suppressWarnings(as.integer(indices))
  indices <- indices[
    !is.na(indices) &
      indices >= 1 &
      indices <= nrow(labels)
  ]

  indices <- unique(indices)

  if (length(indices) == 0) {
    stop("Sélectionnez au moins une étiquette.")
  }

  creer_pdf_etiquettes(
    labels = labels[indices, , drop = FALSE],
    fichier = fichier,
    largeur_mm = largeur_mm,
    hauteur_mm = hauteur_mm
  )
}

# ============================================================
# 1. INITIALISATION DE LA BASE
# ============================================================

con <- ouvrir_db()

# ------------------------- UTILISATEURS ----------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS utilisateurs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  identifiant TEXT UNIQUE NOT NULL,
  mot_de_passe TEXT NOT NULL,
  role TEXT NOT NULL,
  actif INTEGER NOT NULL DEFAULT 1
)
")

user_cols <- list(
  matricule = "TEXT",
  initiales = "TEXT",
  nom = "TEXT",
  prenom = "TEXT",
  email = "TEXT",
  tentatives_echouees = "INTEGER NOT NULL DEFAULT 0",
  verrouille = "INTEGER NOT NULL DEFAULT 0",
  changer_mot_de_passe = "INTEGER NOT NULL DEFAULT 0",
  perm_recherche_patient = "INTEGER NOT NULL DEFAULT 0",
  perm_patients = "INTEGER NOT NULL DEFAULT 0",
  perm_requisition = "INTEGER NOT NULL DEFAULT 0",
  perm_ajout_tests = "INTEGER NOT NULL DEFAULT 1",
  perm_reception = "INTEGER NOT NULL DEFAULT 0",
  perm_historique = "INTEGER NOT NULL DEFAULT 0",
  perm_utilisateurs = "INTEGER NOT NULL DEFAULT 0",
  perm_analyses = "INTEGER NOT NULL DEFAULT 0",
  perm_portail = "INTEGER NOT NULL DEFAULT 0"
)

for (nm in names(user_cols)) {
  ajouter_colonne_si_absente(con, "utilisateurs", nm, user_cols[[nm]])
}

admin_password_initial <- Sys.getenv(
  "EDUSILLAB_ADMIN_PASSWORD",
  unset = "Admin123!"
)

if (!nzchar(trimws(admin_password_initial))) {
  admin_password_initial <- "Admin123!"
}

DBI::dbExecute(
  con,
  "
  INSERT OR IGNORE INTO utilisateurs
  (identifiant, mot_de_passe, role, actif)
  VALUES ('admin', ?, 'ADMIN', 1)
  ",
  params = list(admin_password_initial)
)

if (identical(admin_password_initial, "Admin123!")) {
  message(
    "ATTENTION SECURITE : EDUSILLAB_ADMIN_PASSWORD n'est pas défini. ",
    "Le mot de passe initial admin est encore celui par défaut. ",
    "Changez-le avant une mise en ligne publique."
  )
}

DBI::dbExecute(con, "
UPDATE utilisateurs
SET
  matricule = CASE
    WHEN matricule IS NULL OR TRIM(matricule) = '' THEN 'ADM001'
    ELSE matricule END,
  initiales = CASE
    WHEN initiales IS NULL OR TRIM(initiales) = '' THEN 'EA'
    ELSE initiales END,
  nom = CASE
    WHEN nom IS NULL OR TRIM(nom) = '' THEN 'ADMINISTRATEUR'
    ELSE nom END,
  prenom = CASE
    WHEN prenom IS NULL OR TRIM(prenom) = '' THEN 'EduLab'
    ELSE prenom END,
  email = CASE
    WHEN email IS NULL OR TRIM(email) = '' THEN 'admin@edulab.local'
    ELSE email END,
  perm_recherche_patient = 1,
  perm_patients = 1,
  perm_requisition = 1,
  perm_ajout_tests = 1,
  perm_reception = 1,
  perm_historique = 1,
  perm_utilisateurs = 1,
  perm_analyses = 1,
  perm_portail = 1
WHERE identifiant = 'admin'
")

# Uniformisation des anciens niveaux vers les 4 profils du portail.
DBI::dbExecute(
  con,
  "UPDATE utilisateurs SET role = 'UTILISATEUR' WHERE UPPER(role) IN ('PROF','TESTEUR')"
)

DBI::dbExecute(
  con,
  "
  UPDATE utilisateurs
  SET perm_portail = CASE
    WHEN UPPER(TRIM(role)) IN ('ADMIN','ADMINISTRATEUR','SUPERUTILISATEUR','SUPERUSER') THEN 1
    ELSE COALESCE(perm_portail,0)
  END
  "
)

# -------------------- CONFIGURATION DU PORTAIL ----------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS portail_configuration (
  cle TEXT PRIMARY KEY,
  valeur TEXT,
  date_modification TEXT DEFAULT CURRENT_TIMESTAMP,
  modifie_par INTEGER
)
")

config_defaut <- list(
  portail_nom = "EDUSILLAB",
  institution = "CCNB",
  campus = "Campus de Dieppe",
  sous_titre = "Système d'information de laboratoire — environnement d'enseignement — version Web",
  tableau_bord_titre = "Tableau de bord",
  tableau_bord_sous_titre = "Vue d'ensemble de l'activité du laboratoire d'enseignement.",
  footer_texte = "© 2026 CCNB Campus de Dieppe. Tous droits réservés.",
  couleur_principale = "#00757c",
  couleur_secondaire = "#00585f",
  couleur_sidebar_bas = "#003f45",
  logo_data_uri = ""
)

for (k in names(config_defaut)) {
  DBI::dbExecute(
    con,
    "
    INSERT OR IGNORE INTO portail_configuration (cle,valeur)
    VALUES (?,?)
    ",
    params=list(k,config_defaut[[k]])
  )
}

# ------------------------- PATIENTS --------------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS patients (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  numero_dossier TEXT UNIQUE,
  medicare TEXT UNIQUE,
  nom TEXT NOT NULL,
  prenom TEXT NOT NULL,
  date_naissance TEXT,
  sexe TEXT,
  actif INTEGER NOT NULL DEFAULT 1,
  date_creation TEXT DEFAULT CURRENT_TIMESTAMP
)
")

patient_cols <- list(
  adresse = "TEXT",
  ville = "TEXT",
  province = "TEXT",
  code_postal = "TEXT",
  pays = "TEXT DEFAULT 'Canada'",
  latitude = "REAL",
  longitude = "REAL"
)

for (nm in names(patient_cols)) {
  ajouter_colonne_si_absente(con, "patients", nm, patient_cols[[nm]])
}

# ---------------------- DEPARTEMENTS / PX --------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS departements_px (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  departement TEXT UNIQUE NOT NULL,
  px TEXT
)
")

# Les 28 correspondances présentes dans le classeur fourni.
px_init <- data.frame(
  px = c(
    "EL","C","BS","EG","ES","M","S","HC","I","PR","BM","GR","H","CL",
    "PA","MY","MM","BSD","R","HL","U","SP","VC","HS","MS","TB","CG","MT"
  ),
  departement = c(
    "ENVOIS EXTERIEURS LAB",
    "CHIMIE",
    "BANQUE DE SANG",
    "ENVOI GENETIQUE MOLECULAIRE",
    "ENVOIS EXTERIEURS SERO",
    "MICROBIOLOGIE",
    "SEROLOGIE",
    "HEMATOLOGIE COAGULATION",
    "IMMUNOLOGIE (CHIMIE)",
    "PROJET RECHERCHE",
    "BIOLOGIE MOLECULAIRE",
    "GROUP",
    "HEMATOLOGIE GENERALE",
    "CHLAMYDIA",
    "PARASITOLOGIE",
    "MYCOLOGIE",
    "MICROBIOLOGIE MOLECULAIRE",
    "BANQUE DE SANG DONNEUR AUTOLOG",
    "CHIMIE SPECIALE",
    "HEMATO LIQ.BIOLOGIQUES",
    "URINE",
    "ANALYSE DE SPERME",
    "VIROLOGIE CULTURE",
    "HEMATOLOGIE SPECIALE",
    "MICROBIOLOGIE SEQUENCAGE",
    "MYCOBACTERIE",
    "CYTOLOGIE GENETIQUE",
    "MICROBIOLOGIE ID. TIQUE"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(px_init))) {
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO departements_px (departement, px) VALUES (?, ?)",
    params = list(px_init$departement[i], px_init$px[i])
  )
}

# -------------------------- ANALYSES -------------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS analyses (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  nom TEXT NOT NULL,
  mnemonique TEXT,
  actif INTEGER NOT NULL DEFAULT 1
)
")

analyse_cols <- list(
  departement = "TEXT",
  tube_prelevement = "TEXT",
  nb_tubes = "REAL",
  priorite_specimen = "TEXT",
  delai_h = "REAL",
  page_reference = "INTEGER",
  notes = "TEXT",
  date_creation = "TEXT",
  date_modification = "TEXT"
)

for (nm in names(analyse_cols)) {
  ajouter_colonne_si_absente(con, "analyses", nm, analyse_cols[[nm]])
}

# SQLite n'autorise pas CURRENT_TIMESTAMP comme DEFAULT
# lorsqu'une colonne est ajoutée avec ALTER TABLE.
# On ajoute donc la colonne sans DEFAULT, puis on initialise
# les anciennes lignes et on utilise un trigger pour les nouvelles.
DBI::dbExecute(
  con,
  "
  UPDATE analyses
  SET date_creation = CURRENT_TIMESTAMP
  WHERE date_creation IS NULL OR TRIM(date_creation) = ''
  "
)

DBI::dbExecute(
  con,
  "
  CREATE TRIGGER IF NOT EXISTS trg_analyses_date_creation
  AFTER INSERT ON analyses
  FOR EACH ROW
  WHEN NEW.date_creation IS NULL OR TRIM(NEW.date_creation) = ''
  BEGIN
    UPDATE analyses
    SET date_creation = CURRENT_TIMESTAMP
    WHERE id = NEW.id;
  END;
  "
)

# Le répertoire fourni contient certains mnémoniques identiques
# pour des analyses différentes (ex. selon le département).
# On ne crée donc PAS d'index UNIQUE sur mnemonique.
DBI::dbExecute(con, "DROP INDEX IF EXISTS idx_analyses_mnemonique_unique")

# -------------------------- SOURCES --------------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS sources_prelevement (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  mnemonique TEXT,
  nom TEXT NOT NULL,
  categorie TEXT,
  UNIQUE(mnemonique, nom, categorie)
)
")

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS analyse_sources (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  analyse_id INTEGER NOT NULL,
  source_mnemonique TEXT,
  source_nom TEXT NOT NULL,
  categorie TEXT,
  FOREIGN KEY(analyse_id) REFERENCES analyses(id),
  UNIQUE(analyse_id, source_mnemonique, source_nom, categorie)
)
")

# ------------------------- REQUISITIONS ----------------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS requisitions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  patient_id INTEGER NOT NULL,
  priorite TEXT NOT NULL,
  prescripteur TEXT,
  commentaire TEXT,
  date_prelevement TEXT,
  heure_prelevement TEXT,
  cree_par INTEGER,
  statut TEXT NOT NULL DEFAULT 'ACTIVE',
  date_creation TEXT DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(patient_id) REFERENCES patients(id),
  FOREIGN KEY(cree_par) REFERENCES utilisateurs(id)
)
")

ajouter_colonne_si_absente(
  con, "requisitions", "numero_requisition", "TEXT"
)
ajouter_colonne_si_absente(con, "requisitions", "saisi_initiales", "TEXT")
ajouter_colonne_si_absente(con, "requisitions", "saisi_matricule", "TEXT")

# Numéro unique aux anciennes réquisitions qui n'en ont pas.
anciennes <- DBI::dbGetQuery(con, "
SELECT id
FROM requisitions
WHERE numero_requisition IS NULL OR TRIM(numero_requisition) = ''
")

if (nrow(anciennes) > 0) {
  for (i in seq_len(nrow(anciennes))) {
    nr <- generer_numero_requisition(con)
    DBI::dbExecute(
      con,
      "UPDATE requisitions SET numero_requisition = ? WHERE id = ?",
      params = list(nr, anciennes$id[i])
    )
  }
}

DBI::dbExecute(con, "
CREATE UNIQUE INDEX IF NOT EXISTS idx_requisitions_numero_unique
ON requisitions(numero_requisition)
")

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS requisition_analyses (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  requisition_id INTEGER NOT NULL,
  analyse_id INTEGER NOT NULL,
  FOREIGN KEY(requisition_id) REFERENCES requisitions(id),
  FOREIGN KEY(analyse_id) REFERENCES analyses(id)
)
")

ajouter_colonne_si_absente(con, "requisition_analyses", "source_prelevement", "TEXT")
ajouter_colonne_si_absente(con, "requisition_analyses", "source_autre", "TEXT")

# ------------------------- ÉTIQUETTES / SPÉCIMENS ------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS specimens (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  requisition_id INTEGER NOT NULL,
  patient_id INTEGER NOT NULL,
  numero_specimen TEXT NOT NULL,
  code_barre TEXT NOT NULL UNIQUE,
  date_specimen TEXT NOT NULL,
  departement TEXT,
  px TEXT,
  priorite TEXT,
  location TEXT,
  analyses TEXT,
  quantite TEXT,
  contenant TEXT,
  groupe_etiquette INTEGER,
  cree_par INTEGER,
  date_creation TEXT DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(requisition_id) REFERENCES requisitions(id),
  FOREIGN KEY(patient_id) REFERENCES patients(id),
  FOREIGN KEY(cree_par) REFERENCES utilisateurs(id)
)
")

ajouter_colonne_si_absente(con, "specimens", "saisi_initiales", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "saisi_matricule", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "recu_par", "INTEGER")
ajouter_colonne_si_absente(con, "specimens", "recu_initiales", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "recu_matricule", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "date_reception", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "statut_reception", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "date_specimen_originale", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "numero_specimen_original", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "patient_id_original", "INTEGER")
ajouter_colonne_si_absente(con, "specimens", "annule_par", "INTEGER")
ajouter_colonne_si_absente(con, "specimens", "annule_initiales", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "annule_matricule", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "date_annulation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "motif_annulation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "contexte_annulation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "heure_collecte_recue", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "initiales_preleveur", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "source_prelevement", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "statut_avant_annulation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "type_annulation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "annulation_apres_reception", "INTEGER DEFAULT 0")
ajouter_colonne_si_absente(con, "specimens", "reactive_par", "INTEGER")
ajouter_colonne_si_absente(con, "specimens", "reactive_initiales", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "reactive_matricule", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "date_reactivation", "TEXT")
ajouter_colonne_si_absente(con, "specimens", "commentaire_reactivation", "TEXT")


# V25 — Le BC# appartient au spécimen logique.
# Plusieurs étiquettes physiques d'un même spécimen utilisent le même BC#.
# Les nouvelles saisies créent un seul enregistrement specimen et plusieurs copies
# de l'étiquette. L'ancienne contrainte UNIQUE est donc inutile pour les migrations
# futures, mais on ne reconstruit pas automatiquement la table existante afin de ne
# pas risquer les données historiques.
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_specimens_patient ON specimens(patient_id)")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_specimens_requisition ON specimens(requisition_id)")

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS locations (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  code TEXT UNIQUE,
  nom TEXT NOT NULL,
  site TEXT,
  actif INTEGER NOT NULL DEFAULT 1
)
")

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS materiel_impression (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  nom TEXT NOT NULL,
  type TEXT NOT NULL DEFAULT 'IMPRIMANTE',
  fabricant_modele TEXT,
  numero_serie TEXT,
  largeur_mm REAL NOT NULL DEFAULT 100,
  hauteur_mm REAL NOT NULL DEFAULT 50,
  actif INTEGER NOT NULL DEFAULT 1,
  par_defaut INTEGER NOT NULL DEFAULT 0,
  date_creation TEXT DEFAULT CURRENT_TIMESTAMP
)
")

ajouter_colonne_si_absente(
  con,
  "materiel_impression",
  "mode_connexion",
  "TEXT"
)
ajouter_colonne_si_absente(con, "materiel_impression", "mode_ajustement", "TEXT")
ajouter_colonne_si_absente(con, "materiel_impression", "offset_x_mm", "REAL DEFAULT 0")
ajouter_colonne_si_absente(con, "materiel_impression", "offset_y_mm", "REAL DEFAULT 0")
ajouter_colonne_si_absente(con, "materiel_impression", "echelle_pct", "REAL DEFAULT 100")

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS contextes_annulation (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  libelle TEXT NOT NULL UNIQUE,
  actif INTEGER NOT NULL DEFAULT 1,
  date_creation TEXT DEFAULT CURRENT_TIMESTAMP
)
")

for (lib in c(
  "Spécimen non reçu",
  "Prélèvement non effectué",
  "Patient absent",
  "Erreur de demande",
  "Spécimen perdu / introuvable",
  "Annulation après réception",
  "Erreur d'identification après réception",
  "Prélèvement à reprendre",
  "Autre"
)) {
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO contextes_annulation(libelle, actif) VALUES (?,1)",
    params = list(lib)
  )
}

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS audit_trail (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  patient_id INTEGER,
  requisition_id INTEGER,
  specimen_id INTEGER,
  utilisateur_id INTEGER,
  initiales TEXT,
  matricule TEXT,
  action TEXT NOT NULL,
  details TEXT,
  date_action TEXT DEFAULT CURRENT_TIMESTAMP
)
")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_patient ON audit_trail(patient_id)")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_req ON audit_trail(requisition_id)")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_specimen ON audit_trail(specimen_id)")
DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_audit_date ON audit_trail(date_action)")

# ---------------------- IMPORTS DE DOCUMENTS -----------------

DBI::dbExecute(con, "
CREATE TABLE IF NOT EXISTS imports_documents (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  nom_fichier TEXT,
  extension TEXT,
  contenu_extrait TEXT,
  statut TEXT DEFAULT 'A_REVISER',
  importe_par INTEGER,
  date_import TEXT DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(importe_par) REFERENCES utilisateurs(id)
)
")

# Seuls les administrateurs et superutilisateurs peuvent gérer
# le répertoire d'analyses (créer, modifier, importer).
DBI::dbExecute(
  con,
  "
  UPDATE utilisateurs
  SET perm_analyses =
    CASE
      WHEN UPPER(TRIM(COALESCE(role,''))) IN (
        'ADMIN','ADMINISTRATEUR','SUPERUTILISATEUR','SUPERUSER'
      ) THEN 1
      ELSE 0
    END
  "
)

# Patient d'essai déjà utilisé dans le projet.
DBI::dbExecute(
  con,
  "
  INSERT OR IGNORE INTO patients
  (numero_dossier, medicare, nom, prenom, date_naissance, sexe, actif)
  VALUES (?, ?, ?, ?, ?, ?, 1)
  ",
  params = list(
    "623846", "907716405", "Leblanc", "Charlie",
    "2023-01-13", "M"
  )
)

DBI::dbDisconnect(con)

# ============================================================
# 2. IMPORT DU REPERTOIRE FOURNI
# ============================================================

trouver_colonne <- function(df, candidats) {
  nms <- names(df)
  # IMPORTANT : convertir en majuscules AVANT de retirer les caractères
  # non alphanumériques. L'ancienne version supprimait les minuscules
  # (ex. "# of tubes" devenait incorrect), ce qui faussait l'import Excel.
  nms_ascii <- toupper(iconv(nms, to = "ASCII//TRANSLIT", sub = ""))
  cand_ascii <- toupper(iconv(candidats, to = "ASCII//TRANSLIT", sub = ""))
  nms_norm <- gsub("[^A-Z0-9]", "", nms_ascii)
  cand_norm <- gsub("[^A-Z0-9]", "", cand_ascii)

  idx <- match(cand_norm, nms_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NULL)
  nms[idx[1]]
}

standardiser_table_analyse <- function(df) {
  c_nom <- trouver_colonne(df, c("NOM", "Nom", "Analyse", "Test"))
  c_mnemo <- trouver_colonne(df, c("Mnemonic", "Mnemonique", "Mnémonique", "Order"))
  c_dep <- trouver_colonne(df, c("Départements", "Departements", "Département", "Departement"))
  c_tube <- trouver_colonne(df, c("Tube de prélèvement", "Tube de prelevement", "Contenant"))
  c_nb <- trouver_colonne(df, c("# of tubes", "Nb tubes", "Nombre de tubes"))
  c_prio <- trouver_colonne(df, c("Priorité du spécimen", "Priorite du specimen", "Priorité", "Priorite"))
  c_delai <- trouver_colonne(df, c("Délai (h)", "Delai (h)", "Délai", "Delai"))
  c_page <- trouver_colonne(df, c("Page"))
  c_notes <- trouver_colonne(df, c("Notes / instructions", "Notes", "Instructions"))

  if (is.null(c_nom) || is.null(c_mnemo)) {
    stop("Le document doit contenir au minimum une colonne Nom et une colonne Mnemonic/Mnémonique.")
  }

  out <- data.frame(
    nom = norm_txt(df[[c_nom]]),
    mnemonique = norm_upper(df[[c_mnemo]]),
    departement = if (!is.null(c_dep)) norm_upper(df[[c_dep]]) else "",
    tube_prelevement = if (!is.null(c_tube)) norm_txt(df[[c_tube]]) else "",
    # Point 9 des étiquettes : valeur provenant de la colonne Excel "# of tubes".
    nb_tubes = if (!is.null(c_nb)) suppressWarnings(as.numeric(df[[c_nb]])) else NA_real_,
    priorite_specimen = if (!is.null(c_prio)) norm_txt(df[[c_prio]]) else "",
    delai_h = if (!is.null(c_delai)) suppressWarnings(as.numeric(df[[c_delai]])) else NA_real_,
    page_reference = if (!is.null(c_page)) suppressWarnings(as.integer(df[[c_page]])) else NA_integer_,
    notes = if (!is.null(c_notes)) norm_txt(df[[c_notes]]) else "",
    stringsAsFactors = FALSE
  )

  out <- out[nzchar(out$nom) & nzchar(out$mnemonique), , drop = FALSE]
  out
}

upsert_analyses <- function(con, tab) {
  if (nrow(tab) == 0) return(0L)

  # IMPORTANT :
  # L'import du répertoire est NON DESTRUCTIF.
  # - Une analyse déjà présente est mise à jour.
  # - Une nouvelle analyse est ajoutée.
  # - Une valeur vide dans le document importé N'EFFACE PAS
  #   une ancienne valeur déjà présente dans la base.
  # - Les analyses absentes du nouveau document restent dans la base.
  # - Aucun DELETE n'est effectué ici.

  n <- 0L

  valeur_texte_ou_ancienne <- function(nouvelle, ancienne) {
    nouvelle <- norm_txt(nouvelle)
    if (nzchar(nouvelle)) nouvelle else ancienne
  }

  valeur_num_ou_ancienne <- function(nouvelle, ancienne) {
    nouvelle <- suppressWarnings(as.numeric(nouvelle))
    if (!is.na(nouvelle)) nouvelle else ancienne
  }

  for (i in seq_len(nrow(tab))) {
    nom <- norm_txt(tab$nom[i])
    mnemo <- norm_upper(tab$mnemonique[i])
    dep <- norm_upper(tab$departement[i])

    if (!nzchar(nom) || !nzchar(mnemo)) next

    # Recherche prioritaire par Nom + Mnemonic + Département.
    existant <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM analyses
      WHERE UPPER(nom) = UPPER(?)
        AND UPPER(mnemonique) = UPPER(?)
        AND UPPER(COALESCE(departement,'')) = UPPER(?)
      ",
      params = list(nom, mnemo, dep)
    )

    # Compatibilité avec les anciennes bases :
    # si aucun département n'était encore enregistré, accepter
    # Nom + Mnemonic seulement si le résultat est unique.
    if (nrow(existant) == 0) {
      candidat <- DBI::dbGetQuery(
        con,
        "
        SELECT *
        FROM analyses
        WHERE UPPER(nom) = UPPER(?)
          AND UPPER(mnemonique) = UPPER(?)
        ",
        params = list(nom, mnemo)
      )

      if (nrow(candidat) == 1) {
        existant <- candidat
      }
    }

    if (nrow(existant) >= 1) {
      # Mise à jour NON DESTRUCTIVE :
      # seules les valeurs réellement présentes dans le document
      # remplacent les valeurs existantes.
      old <- existant[1, , drop = FALSE]

      dep_final <- valeur_texte_ou_ancienne(tab$departement[i], old$departement[1])
      tube_final <- valeur_texte_ou_ancienne(tab$tube_prelevement[i], old$tube_prelevement[1])
      prio_final <- valeur_texte_ou_ancienne(tab$priorite_specimen[i], old$priorite_specimen[1])
      notes_final <- valeur_texte_ou_ancienne(tab$notes[i], old$notes[1])

      nb_final <- valeur_num_ou_ancienne(tab$nb_tubes[i], old$nb_tubes[1])
      delai_final <- valeur_num_ou_ancienne(tab$delai_h[i], old$delai_h[1])

      page_nouvelle <- suppressWarnings(as.integer(tab$page_reference[i]))
      page_finale <- if (!is.na(page_nouvelle)) page_nouvelle else old$page_reference[1]

      DBI::dbExecute(
        con,
        "
        UPDATE analyses
        SET
          nom = ?,
          mnemonique = ?,
          departement = ?,
          tube_prelevement = ?,
          nb_tubes = ?,
          priorite_specimen = ?,
          delai_h = ?,
          page_reference = ?,
          notes = ?,
          date_modification = CURRENT_TIMESTAMP
        WHERE id = ?
        ",
        params = list(
          nom,
          mnemo,
          dep_final,
          tube_final,
          nb_final,
          prio_final,
          delai_final,
          page_finale,
          notes_final,
          old$id[1]
        )
      )
    } else {
      # Nouvelle analyse : ajout simple, sans toucher aux anciennes.
      DBI::dbExecute(
        con,
        "
        INSERT INTO analyses
        (
          nom,
          mnemonique,
          actif,
          departement,
          tube_prelevement,
          nb_tubes,
          priorite_specimen,
          delai_h,
          page_reference,
          notes,
          date_creation,
          date_modification
        )
        VALUES (?, ?, 1, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
        ",
        params = list(
          nom,
          mnemo,
          ifelse(nzchar(dep), dep, NA_character_),
          ifelse(nzchar(norm_txt(tab$tube_prelevement[i])), norm_txt(tab$tube_prelevement[i]), NA_character_),
          suppressWarnings(as.numeric(tab$nb_tubes[i])),
          ifelse(nzchar(norm_txt(tab$priorite_specimen[i])), norm_txt(tab$priorite_specimen[i]), NA_character_),
          suppressWarnings(as.numeric(tab$delai_h[i])),
          suppressWarnings(as.integer(tab$page_reference[i])),
          ifelse(nzchar(norm_txt(tab$notes[i])), tab$notes[i], NA_character_)
        )
      )
    }

    n <- n + 1L
  }

  n
}

importer_sources_structurees <- function(con, df_sources) {
  if (nrow(df_sources) == 0) return(0L)

  c1 <- trouver_colonne(df_sources, c("Mnemonic", "Mnemonique", "Mnémonique"))
  c2 <- trouver_colonne(df_sources, c("Nom", "Source"))
  c3 <- trouver_colonne(df_sources, c("Catégorie", "Categorie", "Category"))

  if (is.null(c1) || is.null(c2)) return(0L)

  current_analyse_name <- NULL
  ajoutees <- 0L

  for (i in seq_len(nrow(df_sources))) {
    code <- norm_txt(df_sources[[c1]][i])
    nom <- norm_txt(df_sources[[c2]][i])
    cat <- if (!is.null(c3)) norm_txt(df_sources[[c3]][i]) else ""

    # Une ligne "titre" a la première colonne remplie,
    # mais Nom/Catégorie vides. Dans le classeur fourni,
    # cette ligne indique l'analyse à laquelle appartiennent
    # les sources suivantes.
    if (nzchar(code) && !nzchar(nom) && !nzchar(cat)) {
      current_analyse_name <- code
      next
    }

    # ligne vide = fin d'un groupe
    if (!nzchar(code) && !nzchar(nom) && !nzchar(cat)) {
      current_analyse_name <- NULL
      next
    }

    if (is.null(current_analyse_name) || !nzchar(nom)) next

    analyse <- DBI::dbGetQuery(
      con,
      "SELECT id FROM analyses WHERE UPPER(nom) = UPPER(?) LIMIT 1",
      params = list(current_analyse_name)
    )

    if (nrow(analyse) != 1) next

    DBI::dbExecute(
      con,
      "
      INSERT OR IGNORE INTO sources_prelevement
      (mnemonique, nom, categorie)
      VALUES (?, ?, ?)
      ",
      params = list(
        ifelse(nzchar(code), code, NA_character_),
        nom,
        ifelse(nzchar(cat), cat, NA_character_)
      )
    )

    DBI::dbExecute(
      con,
      "
      INSERT OR IGNORE INTO analyse_sources
      (analyse_id, source_mnemonique, source_nom, categorie)
      VALUES (?, ?, ?, ?)
      ",
      params = list(
        analyse$id[1],
        ifelse(nzchar(code), code, NA_character_),
        nom,
        ifelse(nzchar(cat), cat, NA_character_)
      )
    )

    ajoutees <- ajoutees + 1L
  }

  ajoutees
}

importer_classeur_edulab <- function(path, con, remplacer_repertoire = FALSE) {
  sheets <- excel_sheets(path)

  if (length(sheets) == 0) {
    stop("Le classeur Excel ne contient aucune feuille.")
  }

  # ----------------------------------------------------------
  # A. LIRE TOUTES LES FEUILLES DU CLASSEUR
  # ----------------------------------------------------------
  # Les feuilles connues ont un rôle précis :
  # - Répertoire analyses : Nom, Mnemonic, tube, délai, page, notes
  # - repetoire analyse GDH : Département, tube, # of tubes, sources, priorité
  # - TEST DEPARTEMENT : correspondance Département -> PX
  # - SOURCES PRELEVEMENT : dictionnaire / associations de sources
  #
  # Les autres feuilles tabulaires sont également examinées. Si elles
  # contiennent au minimum Nom + Mnemonic, elles servent comme source
  # complémentaire. Les valeurs non vides peuvent compléter les champs.

  feuilles_lues <- list()

  for (sh in sheets) {
    feuilles_lues[[sh]] <- tryCatch(
      as.data.frame(
        read_excel(
          path,
          sheet = sh,
          .name_repair = "unique"
        )
      ),
      error = function(e) {
        message(
          "Feuille ignorée pendant la lecture : ",
          sh,
          " — ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  # ----------------------------------------------------------
  # B. DÉPARTEMENTS / PX
  # ----------------------------------------------------------
  if ("TEST DEPARTEMENT" %in% names(feuilles_lues)) {
    dep <- feuilles_lues[["TEST DEPARTEMENT"]]

    if (!is.null(dep)) {
      c_px <- trouver_colonne(dep, c("PX"))
      c_dep <- trouver_colonne(dep, c("DEPARTEMENT", "Département"))

      if (!is.null(c_px) && !is.null(c_dep)) {
        for (i in seq_len(nrow(dep))) {
          px <- norm_upper(dep[[c_px]][i])
          dd <- norm_upper(dep[[c_dep]][i])

          if (!nzchar(dd)) next

          DBI::dbExecute(
            con,
            "
            INSERT INTO departements_px (departement, px)
            VALUES (?, ?)
            ON CONFLICT(departement)
            DO UPDATE SET px = excluded.px
            ",
            params = list(
              dd,
              ifelse(nzchar(px), px, NA_character_)
            )
          )
        }
      }
    }
  }

  # ----------------------------------------------------------
  # C. TABLES D'ANALYSES ISSUES DE TOUTES LES FEUILLES
  # ----------------------------------------------------------
  tables_analyses <- list()

  for (sh in names(feuilles_lues)) {
    df <- feuilles_lues[[sh]]
    if (is.null(df) || nrow(df) == 0) next

    # Ne pas interpréter les dictionnaires comme des analyses.
    if (sh %in% c("TEST DEPARTEMENT", "SOURCES PRELEVEMENT")) next

    c_nom <- trouver_colonne(df, c("NOM", "Nom", "Analyse", "Test"))
    c_mnemo <- trouver_colonne(
      df,
      c("Mnemonic", "Mnemonique", "Mnémonique", "Order")
    )

    if (is.null(c_nom) || is.null(c_mnemo)) next

    tab_sh <- tryCatch(
      standardiser_table_analyse(df),
      error = function(e) NULL
    )

    if (!is.null(tab_sh) && nrow(tab_sh) > 0) {
      tab_sh$feuille_source <- sh
      tables_analyses[[sh]] <- tab_sh
    }
  }

  if (length(tables_analyses) == 0) {
    stop(
      paste0(
        "Aucune feuille du classeur ne contient les colonnes minimales ",
        "Nom + Mnemonic."
      )
    )
  }

  # ----------------------------------------------------------
  # D. FUSION DES ANALYSES ENTRE LES FEUILLES
  # ----------------------------------------------------------
  # La clé est Nom + Mnemonic. Le département provenant de GDH est
  # ensuite utilisé pour distinguer les analyses lorsque nécessaire.
  #
  # Priorités de provenance :
  #   1) Répertoire analyses : notes / délai / page
  #   2) repetoire analyse GDH : département / tube / # of tubes / priorité
  #   3) autres feuilles : valeurs complémentaires non vides

  choisir_txt <- function(nouveau, ancien) {
    nouveau <- norm_txt(nouveau)
    ancien <- norm_txt(ancien)
    ifelse(nzchar(nouveau), nouveau, ancien)
  }

  choisir_num <- function(nouveau, ancien) {
    nouveau <- suppressWarnings(as.numeric(nouveau))
    ancien <- suppressWarnings(as.numeric(ancien))
    ifelse(!is.na(nouveau), nouveau, ancien)
  }

  # Construire une table maître en partant de l'union de toutes les feuilles.
  toutes <- do.call(
    rbind,
    lapply(
      names(tables_analyses),
      function(sh) {
        z <- tables_analyses[[sh]]
        z$feuille_source <- sh
        z
      }
    )
  )

  toutes$cle <- paste(
    norm_upper(toutes$mnemonique),
    toupper(norm_txt(toutes$nom)),
    sep = "|||"
  )

  cles <- unique(toutes$cle)
  resultat <- vector("list", length(cles))

  for (k in seq_along(cles)) {
    bloc <- toutes[toutes$cle == cles[k], , drop = FALSE]

    # Valeurs de départ.
    ligne <- bloc[1, , drop = FALSE]

    # Appliquer les feuilles complémentaires en premier.
    for (j in seq_len(nrow(bloc))) {
      ligne$departement <- choisir_txt(
        bloc$departement[j],
        ligne$departement
      )
      ligne$tube_prelevement <- choisir_txt(
        bloc$tube_prelevement[j],
        ligne$tube_prelevement
      )
      ligne$nb_tubes <- choisir_num(
        bloc$nb_tubes[j],
        ligne$nb_tubes
      )
      ligne$priorite_specimen <- choisir_txt(
        bloc$priorite_specimen[j],
        ligne$priorite_specimen
      )
      ligne$delai_h <- choisir_num(
        bloc$delai_h[j],
        ligne$delai_h
      )
      ligne$page_reference <- choisir_num(
        bloc$page_reference[j],
        ligne$page_reference
      )
      ligne$notes <- choisir_txt(
        bloc$notes[j],
        ligne$notes
      )
    }

    # Priorité explicite à la feuille détaillée pour notes/délai/page.
    if ("Répertoire analyses" %in% bloc$feuille_source) {
      r <- bloc[
        bloc$feuille_source == "Répertoire analyses",
        ,
        drop = FALSE
      ][1, , drop = FALSE]

      ligne$tube_prelevement <- choisir_txt(
        r$tube_prelevement,
        ligne$tube_prelevement
      )
      ligne$delai_h <- choisir_num(
        r$delai_h,
        ligne$delai_h
      )
      ligne$page_reference <- choisir_num(
        r$page_reference,
        ligne$page_reference
      )
      ligne$notes <- choisir_txt(
        r$notes,
        ligne$notes
      )
    }

    # Priorité explicite à GDH pour Département, tube, # of tubes, priorité.
    if ("repetoire analyse GDH" %in% bloc$feuille_source) {
      g <- bloc[
        bloc$feuille_source == "repetoire analyse GDH",
        ,
        drop = FALSE
      ][1, , drop = FALSE]

      ligne$departement <- choisir_txt(
        g$departement,
        ligne$departement
      )
      ligne$tube_prelevement <- choisir_txt(
        g$tube_prelevement,
        ligne$tube_prelevement
      )

      # "# of tubes" : la colonne explicite de GDH est prioritaire.
      # Si elle est vide, une valeur numérique trouvée dans une autre
      # feuille est conservée. Aucune valeur n'est inventée.
      ligne$nb_tubes <- choisir_num(
        g$nb_tubes,
        ligne$nb_tubes
      )

      ligne$priorite_specimen <- choisir_txt(
        g$priorite_specimen,
        ligne$priorite_specimen
      )
    }

    resultat[[k]] <- data.frame(
      nom = norm_txt(ligne$nom),
      mnemonique = norm_upper(ligne$mnemonique),
      departement = norm_upper(ligne$departement),
      tube_prelevement = norm_txt(ligne$tube_prelevement),
      nb_tubes = suppressWarnings(as.numeric(ligne$nb_tubes)),
      priorite_specimen = norm_txt(ligne$priorite_specimen),
      delai_h = suppressWarnings(as.numeric(ligne$delai_h)),
      page_reference = suppressWarnings(as.integer(ligne$page_reference)),
      notes = as.character(ligne$notes),
      stringsAsFactors = FALSE
    )
  }

  tab <- do.call(rbind, resultat)
  tab <- tab[
    nzchar(tab$nom) & nzchar(tab$mnemonique),
    ,
    drop = FALSE
  ]

  # ----------------------------------------------------------
  # E. MODE REMPLACEMENT DU RÉPERTOIRE
  # ----------------------------------------------------------
  # Pour préserver l'historique des anciennes réquisitions, les anciennes
  # analyses ne sont PAS supprimées physiquement. Elles sont désactivées.
  # Le répertoire visible/actif est ensuite reconstruit depuis le classeur.
  if (isTRUE(remplacer_repertoire)) {
    DBI::dbExecute(
      con,
      "
      UPDATE analyses
      SET actif = 0,
          date_modification = CURRENT_TIMESTAMP
      "
    )
  }

  n_analyses <- upsert_analyses(con, tab)

  # Toute analyse présente dans le classeur téléversé redevient active.
  for (i in seq_len(nrow(tab))) {
    ids <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM analyses
      WHERE UPPER(nom) = UPPER(?)
        AND UPPER(mnemonique) = UPPER(?)
        AND UPPER(COALESCE(departement,'')) = UPPER(?)
      ",
      params = list(
        tab$nom[i],
        tab$mnemonique[i],
        tab$departement[i]
      )
    )

    # Anciennes fiches sans département : accepter uniquement si unique.
    if (nrow(ids) == 0) {
      ids2 <- DBI::dbGetQuery(
        con,
        "
        SELECT id
        FROM analyses
        WHERE UPPER(nom) = UPPER(?)
          AND UPPER(mnemonique) = UPPER(?)
        ",
        params = list(
          tab$nom[i],
          tab$mnemonique[i]
        )
      )

      if (nrow(ids2) == 1) ids <- ids2
    }

    if (nrow(ids) >= 1) {
      for (id_a in ids$id) {
        DBI::dbExecute(
          con,
          "
          UPDATE analyses
          SET actif = 1,
              date_modification = CURRENT_TIMESTAMP
          WHERE id = ?
          ",
          params = list(id_a)
        )
      }
    }
  }

  # ----------------------------------------------------------
  # F. SOURCES DE PRÉLÈVEMENT
  # ----------------------------------------------------------
  n_sources <- 0L

  if ("SOURCES PRELEVEMENT" %in% names(feuilles_lues)) {
    src <- feuilles_lues[["SOURCES PRELEVEMENT"]]

    if (!is.null(src) && nrow(src) > 0) {
      # En mode remplacement, reconstruire les associations du répertoire
      # actif. Le dictionnaire des sources est conservé.
      if (isTRUE(remplacer_repertoire)) {
        DBI::dbExecute(con, "DELETE FROM analyse_sources")
      }

      n_sources <- importer_sources_structurees(con, src)
    }
  }

  # ----------------------------------------------------------
  # G. RÉSUMÉ DE L'IMPORT
  # ----------------------------------------------------------
  list(
    analyses = n_analyses,
    sources = n_sources,
    feuilles = length(sheets),
    noms_feuilles = paste(sheets, collapse = ", "),
    nb_tubes_renseignes = sum(!is.na(tab$nb_tubes)),
    nb_tubes_vides = sum(is.na(tab$nb_tubes)),
    remplacement = isTRUE(remplacer_repertoire)
  )
}

# ============================================================
# IMPORT AUTOMATIQUE DU RÉPERTOIRE EXCEL LOCAL
# ============================================================
# Pour bénéficier automatiquement des notes/instructions détaillées,
# placer le classeur Excel dans le même dossier que app.R.
#
# Noms acceptés, par exemple :
#   Repertoire_analyses_EduLab.xlsx
#   Repertoire_analyses_EduLab (1).xlsx
#   Repertoire_analyses_EduLab (1)(1).xlsx
#
# À chaque démarrage, le classeur est relu et les fiches existantes
# sont mises à jour. Les notes gardent leurs retours à la ligne.
importer_repertoire_local_au_demarrage <- function() {
  fichiers <- list.files(
    path = app_dir,
    pattern = "^Repertoire_analyses_EduLab.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(fichiers) == 0) {
    message(
      "Répertoire Excel non trouvé dans le dossier de l'application. ",
      "Sur Connect Cloud, vérifiez que Repertoire_analyses_EduLab.xlsx est bien inclus dans le dépôt GitHub. ",
      "Vous pourrez aussi l'importer depuis l'administration du répertoire."
    )
    return(invisible(NULL))
  }

  # Si plusieurs copies existent, utiliser la plus récemment modifiée.
  infos <- file.info(fichiers)
  fichier <- rownames(infos)[which.max(infos$mtime)]

  con_imp <- ouvrir_db()
  on.exit(DBI::dbDisconnect(con_imp), add = TRUE)

  resultat <- tryCatch(
    importer_classeur_edulab(fichier, con_imp, remplacer_repertoire = FALSE),
    error = function(e) {
      message("Import automatique du répertoire impossible : ", conditionMessage(e))
      return(NULL)
    }
  )

  if (!is.null(resultat)) {
    message(
      "Répertoire Excel chargé : ", basename(fichier),
      " | analyses traitées : ", resultat$analyses,
      " | sources associées : ", resultat$sources
    )
  }

  invisible(resultat)
}

importer_repertoire_local_au_demarrage()

# ============================================================
# SYNCHRONISATION COMPLÈTE DU CHAMP "# of tubes"
# ============================================================
# La feuille "repetoire analyse GDH" est la source de référence pour
# le nombre de tubes. On met à jour toutes les analyses correspondantes.
# Une cellule vide dans Excel reste vide/NA : aucune valeur n'est inventée.
synchroniser_nb_tubes_excel <- function() {
  fichiers <- list.files(
    path = app_dir,
    pattern = "^Repertoire_analyses_EduLab.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(fichiers) == 0) {
    message("Synchronisation # of tubes : aucun classeur Excel trouvé.")
    return(invisible(NULL))
  }

  infos <- file.info(fichiers)
  fichier <- rownames(infos)[which.max(infos$mtime)]

  resultat <- tryCatch({
    feuilles <- readxl::excel_sheets(fichier)

    if (!"repetoire analyse GDH" %in% feuilles) {
      stop("La feuille 'repetoire analyse GDH' est absente du classeur.")
    }

    brut <- as.data.frame(
      readxl::read_excel(
        fichier,
        sheet = "repetoire analyse GDH"
      ),
      stringsAsFactors = FALSE
    )

    c_nom <- trouver_colonne(brut, c("NOM", "Nom", "Analyse", "Test"))
    c_mnemo <- trouver_colonne(brut, c("Mnemonic", "Mnemonique", "Mnémonique", "Order"))
    c_dep <- trouver_colonne(brut, c("Départements", "Departements", "Département", "Departement"))
    c_nb <- trouver_colonne(brut, c("# of tubes", "Nb tubes", "Nombre de tubes"))

    if (is.null(c_nom) || is.null(c_mnemo) || is.null(c_nb)) {
      stop("Colonnes Nom, Mnemonic ou # of tubes introuvables.")
    }

    tab <- data.frame(
      nom = norm_txt(brut[[c_nom]]),
      mnemonique = norm_upper(brut[[c_mnemo]]),
      departement = if (!is.null(c_dep)) norm_upper(brut[[c_dep]]) else "",
      nb_tubes = suppressWarnings(as.numeric(brut[[c_nb]])),
      stringsAsFactors = FALSE
    )

    tab <- tab[
      nzchar(tab$nom) & nzchar(tab$mnemonique),
      ,
      drop = FALSE
    ]

    con_sync <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con_sync), add = TRUE)

    maj <- 0L

    for (i in seq_len(nrow(tab))) {
      # Recherche d'abord la combinaison exacte Nom + Mnemonic + Département.
      ids <- DBI::dbGetQuery(
        con_sync,
        "
        SELECT id
        FROM analyses
        WHERE UPPER(nom) = UPPER(?)
          AND UPPER(mnemonique) = UPPER(?)
          AND UPPER(COALESCE(departement,'')) = UPPER(?)
        ",
        params = list(
          tab$nom[i],
          tab$mnemonique[i],
          tab$departement[i]
        )
      )

      # Pour les anciennes bases où le département n'était pas encore rempli,
      # accepter le couple Nom + Mnemonic seulement s'il est unique.
      if (nrow(ids) == 0) {
        ids2 <- DBI::dbGetQuery(
          con_sync,
          "
          SELECT id
          FROM analyses
          WHERE UPPER(nom) = UPPER(?)
            AND UPPER(mnemonique) = UPPER(?)
          ",
          params = list(tab$nom[i], tab$mnemonique[i])
        )
        if (nrow(ids2) == 1) ids <- ids2
      }

      if (nrow(ids) >= 1) {
        for (id_analyse in ids$id) {
          DBI::dbExecute(
            con_sync,
            "
            UPDATE analyses
            SET nb_tubes = ?,
                date_modification = CURRENT_TIMESTAMP
            WHERE id = ?
            ",
            params = list(tab$nb_tubes[i], id_analyse)
          )
          maj <- maj + 1L
        }
      }
    }

    list(
      fichier = basename(fichier),
      lignes_excel = nrow(tab),
      avec_nombre = sum(!is.na(tab$nb_tubes)),
      sans_nombre = sum(is.na(tab$nb_tubes)),
      fiches_mises_a_jour = maj
    )
  }, error = function(e) {
    message("Synchronisation # of tubes impossible : ", conditionMessage(e))
    NULL
  })

  if (!is.null(resultat)) {
    message(
      "Synchronisation # of tubes terminée : ",
      resultat$fichier,
      " | lignes Excel : ", resultat$lignes_excel,
      " | valeurs présentes : ", resultat$avec_nombre,
      " | cellules vides : ", resultat$sans_nombre,
      " | fiches mises à jour : ", resultat$fiches_mises_a_jour
    )
  }

  invisible(resultat)
}

synchroniser_nb_tubes_excel()

# Contrôle pédagogique : vérifier que la fiche SAT% possède bien
# les notes détaillées provenant du classeur lorsqu'elle existe.
try({
  con_check <- ouvrir_db()
  sat_check <- DBI::dbGetQuery(
    con_check,
    "
    SELECT nom, mnemonique, notes
    FROM analyses
    WHERE UPPER(mnemonique) = 'SAT%'
      AND UPPER(nom) = UPPER('% SATURATION DU FER')
    LIMIT 1
    "
  )
  DBI::dbDisconnect(con_check)

  if (nrow(sat_check) == 1 && nzchar(norm_txt(sat_check$notes[1]))) {
    message("Contrôle SAT% : notes/instructions chargées.")
    con_sat_nb <- ouvrir_db()
    sat_nb <- DBI::dbGetQuery(
      con_sat_nb,
      "
      SELECT nb_tubes
      FROM analyses
      WHERE UPPER(mnemonique) = 'SAT%'
        AND UPPER(nom) = UPPER('% SATURATION DU FER')
      LIMIT 1
      "
    )
    DBI::dbDisconnect(con_sat_nb)
    if (nrow(sat_nb) == 1) {
      message(
        "Contrôle SAT% : # of tubes = ",
        ifelse(is.na(sat_nb$nb_tubes[1]), "vide", sat_nb$nb_tubes[1])
      )
    }
  }
}, silent = TRUE)

extraire_document_non_tabulaire <- function(path) {
  ext <- tolower(tools::file_ext(path))

  # Formats texte simples.
  if (ext %in% c("txt", "md", "log")) {
    return(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }

  # JSON.
  if (ext == "json") {
    obj <- fromJSON(path, simplifyVector = FALSE)
    return(toJSON(obj, pretty = TRUE, auto_unbox = TRUE))
  }

  # PDF/DOC/DOCX/RTF/ODT/HTML etc. via readtext si installé.
  if (requireNamespace("readtext", quietly = TRUE)) {
    res <- try(readtext::readtext(path), silent = TRUE)
    if (!inherits(res, "try-error") && nrow(res) > 0) {
      return(paste(res$text, collapse = "\n\n"))
    }
  }

  paste0(
    "Le fichier a été téléversé, mais son contenu structuré n'a pas pu être ",
    "converti automatiquement. Installez le paquet R 'readtext' pour élargir ",
    "la prise en charge des PDF/DOC/DOCX/RTF/ODT/HTML. Le document reste ",
    "enregistré comme import à réviser."
  )
}

importer_fichier_analyse <- function(path, nom_original, con, user_id) {
  ext <- tolower(tools::file_ext(nom_original))

  if (ext %in% c("xlsx", "xls")) {
    res <- importer_classeur_edulab(
      path,
      con,
      remplacer_repertoire = TRUE
    )

    return(list(
      type = "auto",
      message = paste0(
        "Répertoire actif remplacé à partir du classeur téléversé. ",
        res$analyses, " analyse(s) traitée(s); ",
        res$sources, " association(s) de source(s); ",
        res$feuilles, " feuille(s) examinée(s). ",
        "# of tubes renseigné pour ",
        res$nb_tubes_renseignes,
        " analyse(s); ",
        res$nb_tubes_vides,
        " valeur(s) laissée(s) vide(s)."
      )
    ))
  }

  if (ext %in% c("csv", "tsv")) {
    sep <- ifelse(ext == "tsv", "\t", ",")
    df <- read.csv(
      path,
      sep = sep,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
    tab <- standardiser_table_analyse(df)
    n <- upsert_analyses(con, tab)
    return(list(
      type = "auto",
      message = paste0(n, " analyse(s) importée(s)/mise(s) à jour.")
    ))
  }

  # Tout autre format est accepté comme document à réviser.
  # Le texte est extrait lorsque possible, mais n'est jamais transformé
  # silencieusement en analyses, car la structure peut être ambiguë.
  texte <- extraire_document_non_tabulaire(path)

  DBI::dbExecute(
    con,
    "
    INSERT INTO imports_documents
    (nom_fichier, extension, contenu_extrait, statut, importe_par)
    VALUES (?, ?, ?, 'A_REVISER', ?)
    ",
    params = list(nom_original, ext, texte, user_id)
  )

  list(
    type = "review",
    message = paste0(
      "Document '", nom_original,
      "' enregistré dans la file d'importation à réviser."
    )
  )
}

# ============================================================
# 2C. IDENTITÉ VISUELLE CCNB - CAMPUS DE DIEPPE
# ============================================================

CCNB_LOGO_DATA_URI <- "data:image/png;base64,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"


ccnb_svg_data_uri <- function(svg) {
  paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(svg, reserved=TRUE)
  )
}

# Logo institutionnel fourni par l’utilisateur.
# Le fichier www/ccnb_laboratoire_dieppe.png est prioritaire;
# l’ancien logo intégré reste disponible comme solution de secours.
CCNB_LOGO_FICHIER <- "ccnb_laboratoire_dieppe.png"
ccnb_logo_src_default <- function() {
  chemin <- file.path(app_dir, "www", CCNB_LOGO_FICHIER)
  if (file.exists(chemin)) CCNB_LOGO_FICHIER else CCNB_LOGO_DATA_URI
}


CCNB_ICON_DOCUMENT <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#00757c'/><path d='M20 12h17l10 10v30H20z' fill='none' stroke='white' stroke-width='4' stroke-linejoin='round'/><path d='M37 12v11h10M26 31h15M26 39h15M26 47h10' stroke='white' stroke-width='4' stroke-linecap='round'/></svg>"
)

CCNB_ICON_TUBE <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#00585f'/><path d='M24 13h16M28 13v31a8 8 0 0 0 16 0V13' fill='none' stroke='white' stroke-width='4' stroke-linecap='round'/><path d='M29 37h14' stroke='white' stroke-width='4'/></svg>"
)

CCNB_ICON_CLOCK <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#ff851b'/><circle cx='32' cy='32' r='18' fill='none' stroke='white' stroke-width='4'/><path d='M32 21v12l8 5' fill='none' stroke='white' stroke-width='4' stroke-linecap='round'/></svg>"
)

CCNB_ICON_CANCEL <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#d92d20'/><circle cx='32' cy='32' r='18' fill='none' stroke='white' stroke-width='4'/><path d='M25 25l14 14M39 25L25 39' stroke='white' stroke-width='4' stroke-linecap='round'/></svg>"
)

CCNB_ICON_RECEIVE <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#00757c'/><path d='M18 18h28v28H18z' fill='none' stroke='white' stroke-width='4'/><path d='M32 11v25M24 28l8 8 8-8' fill='none' stroke='white' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/></svg>"
)

CCNB_ICON_ADD <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#14844c'/><path d='M19 13h22l7 7v31H19z' fill='none' stroke='white' stroke-width='4'/><path d='M34 30v14M27 37h14' stroke='white' stroke-width='4' stroke-linecap='round'/></svg>"
)

CCNB_ICON_BOOK <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#ff851b'/><path d='M12 18c8-3 14-2 20 3v29c-6-5-12-6-20-3zM52 18c-8-3-14-2-20 3v29c6-5 12-6 20-3z' fill='none' stroke='white' stroke-width='4' stroke-linejoin='round'/></svg>"
)

CCNB_ICON_HISTORY <- ccnb_svg_data_uri(
  "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'><rect width='64' height='64' rx='14' fill='#00585f'/><path d='M18 21v-9l-7 7 7 7v-5a19 19 0 1 1-2 23' fill='none' stroke='white' stroke-width='4' stroke-linecap='round' stroke-linejoin='round'/><path d='M32 22v12l9 5' fill='none' stroke='white' stroke-width='4' stroke-linecap='round'/></svg>"
)

ccnb_icon_img <- function(src, alt, size=58) {
  tags$img(
    src=src,
    alt=alt,
    style=paste0(
      "width:",size,"px;height:",size,
      "px;object-fit:contain;display:block;"
    )
  )
}

# ============================================================
# 3. INTERFACE
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root {
        --navy:#00585f;
        --navy-dark:#003f45;
        --blue:#00757c;
        --teal:#008b8f;
        --green:#16a34a;
        --orange:#ff851b;
        --red:#dc2626;
        --bg:#f6f7f7;
        --text:#123038;
        --muted:#6b7280;
        --line:#e5eaf0;
      }

      body {
        background:var(--bg);
        color:var(--text);
        font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
      }

      .container-fluid {
        max-width:none;
        padding-left:0;
        padding-right:0;
      }

      .portal-shell {
        display:grid;
        grid-template-columns:270px minmax(0,1fr);
        min-height:100vh;
      }

      .portal-sidebar {
        position:sticky;
        top:0;
        height:100vh;
        overflow-y:auto;
        background:linear-gradient(180deg,#00585f 0%,#00474d 55%,#003f45 100%);
        color:white;
        padding:18px 14px;
        box-shadow:8px 0 28px rgba(15,47,77,.08);
        z-index:10;
      }

      .portal-brand {
        display:flex;
        align-items:center;
        gap:11px;
        padding:5px 10px 20px 10px;
      }

      .portal-logo-img {
        width:54px;
        height:54px;
        object-fit:contain;
        border-radius:10px;
        flex:0 0 54px;
      }

      .portal-brand-title {
        font-size:26px;
        font-weight:800;
        letter-spacing:.4px;
        color:white;
        line-height:1;
      }

      .portal-brand-subtitle {
        color:#e4f2f2;
        font-size:12px;
        font-weight:700;
        letter-spacing:.3px;
        margin-top:7px;
      }

      .portal-menu-title {
        color:#9fb3c6;
        font-size:11px;
        text-transform:uppercase;
        letter-spacing:.9px;
        padding:18px 12px 7px 12px;
      }

      .portal-sidebar .btn {
        width:100%;
        text-align:left;
        color:#eef6fc;
        background:transparent;
        border:0;
        box-shadow:none;
        border-radius:9px;
        margin:3px 0;
        padding:11px 12px;
      }

      .portal-sidebar .btn:hover,
      .portal-sidebar .btn:focus {
        color:white;
        background:rgba(255,255,255,.10);
      }

      .portal-user {
        margin-top:22px;
        border-radius:14px;
        padding:15px;
        background:rgba(255,255,255,.08);
        border:1px solid rgba(255,255,255,.08);
      }

      .portal-user-code {
        display:flex;
        align-items:center;
        gap:10px;
        font-size:18px;
        font-weight:800;
      }

      .portal-avatar {
        width:42px;
        height:42px;
        border-radius:50%;
        display:flex;
        align-items:center;
        justify-content:center;
        background:linear-gradient(145deg,#0fb7b5,#1bb77d);
        color:white;
        font-weight:800;
      }

      .portal-role {
        margin:6px 0 5px 52px;
        color:#dce7ef;
        font-size:12px;
      }

      .portal-online {
        margin-left:52px;
        color:#43d17d;
        font-size:12px;
      }

      .portal-timeout {
        color:#9fb3c6;
        font-size:10px;
        text-align:center;
        margin-top:8px;
      }

      .portal-main { min-width:0; }

      .portal-topbar {
        height:72px;
        background:white;
        border-bottom:1px solid var(--line);
        display:flex;
        align-items:center;
        justify-content:space-between;
        padding:0 26px;
        position:sticky;
        top:0;
        z-index:8;
      }

      .portal-topbar-title {
        font-size:24px;
        font-weight:800;
        color:var(--navy);
      }

      .portal-topbar-meta {
        display:flex;
        gap:22px;
        color:#334155;
        font-size:13px;
      }

      .portal-content {
        padding:22px 24px 34px 24px;
      }

      .well {
        background:white;
        border:1px solid var(--line);
        border-radius:14px;
        box-shadow:0 5px 18px rgba(15,47,77,.05);
      }

      .form-control {
        border:1px solid #cfd8e3;
        border-radius:8px;
        box-shadow:none;
      }

      .form-control:focus {
        border-color:#5a8dee;
        box-shadow:0 0 0 3px rgba(37,99,235,.10);
      }

      .btn {
        border-radius:8px;
        min-height:38px;
        font-weight:600;
        margin-right:6px;
        margin-bottom:5px;
      }

      .btn-primary { background:var(--blue); border-color:var(--blue); }
      .btn-success { background:var(--green); border-color:var(--green); }
      .btn-warning { background:var(--orange); border-color:var(--orange); color:white; }
      .btn-danger { background:var(--red); border-color:var(--red); }
      .btn-info { background:var(--teal); border-color:var(--teal); }

      .login-modern {
        max-width:440px;
        margin:8vh auto 0 auto;
        padding:28px;
        background:white;
        border-radius:18px;
        border:1px solid var(--line);
        box-shadow:0 18px 50px rgba(15,47,77,.12);
      }

      /* ===== EDUSILLAB v42 - Connexion visuelle fidele a la maquette ===== */
      .login-modern { display:none !important; }
      .edu-login-screen {
        position:relative; width:100vw; height:100vh; min-height:720px; overflow:hidden;
        background:#eef6fd url('login_v42_reference.png') center center / 100% 100% no-repeat;
        font-family:Arial, Helvetica, sans-serif;
      }
      /* Les controles Shiny reels sont superposes aux champs de la maquette. */
      .edu-login-live {
        position:absolute; z-index:20;
        left:57.45%; top:33.35%; width:33.0%; height:36.5%;
      }
      .edu-login-live .form-group { margin:0; position:absolute; left:0; width:100%; }
      .edu-login-live .form-group:nth-of-type(1) { top:0; }
      .edu-login-live .form-group:nth-of-type(2) { top:25.8%; }
      .edu-login-live label { display:none !important; }
      .edu-login-live .form-control {
        width:100%; height:64px; box-sizing:border-box;
        border:0 !important; outline:0 !important; box-shadow:none !important;
        border-radius:10px; background:rgba(255,255,255,.06) !important;
        color:#123b70; font-size:20px; padding:0 58px 0 58px;
      }
      .edu-login-live .form-control::placeholder { color:transparent; }
      .edu-login-live .login-user-icon,
      .edu-login-live .login-lock-icon {
        position:absolute; left:18px; z-index:25; color:#123b70; font-size:24px; pointer-events:none;
      }
      .edu-login-live .login-user-icon { top:17px; }
      .edu-login-live .login-lock-icon { top:calc(25.8% + 17px); }
      .edu-login-live .btn-primary {
        position:absolute; top:53.7%; left:0; width:100%; height:66px;
        margin:0; border:0 !important; border-radius:10px;
        background:rgba(0,0,0,0) !important; color:transparent !important;
        box-shadow:none !important; cursor:pointer;
      }
      .edu-login-live .btn-primary:hover { background:rgba(0,82,180,.08) !important; }
      .edu-login-live .edu-login-forgot {
        position:absolute; top:75.0%; left:0; width:100%; height:34px;
        display:block; color:transparent !important; text-decoration:none;
      }
      .edu-login-live #message_connexion {
        position:absolute; top:86%; left:0; width:100%; text-align:center;
        font-size:14px; color:#b42318;
      }
      @media (max-width:1100px), (max-height:760px) {
        .edu-login-screen { min-height:620px; overflow:auto; }
      }

      .user-box {
        background:#eef3f8;
        padding:12px;
        border-radius:8px;
        margin-bottom:10px;
      }

      .order-zone {
        background:white;
        border:1px solid #cbd5e1;
        padding:18px;
        min-height:280px;
        border-radius:12px;
      }

      .success-box {
        padding:12px;
        background:#e7f7ed;
        border:1px solid #b7e4c7;
        border-radius:9px;
      }

      .warning-box {
        padding:12px;
        background:#fff5df;
        border:1px solid #f8d79a;
        border-radius:9px;
      }

      .danger-box {
        padding:12px;
        background:#fdebec;
        border:1px solid #efb7bc;
        border-radius:9px;
      }

      .req-number { font-size:22px; font-weight:bold; }

      .analysis-toolbar {
        background:#eef4fb;
        padding:12px;
        border-radius:9px;
        margin-bottom:12px;
      }

      .inactive-row { opacity:.6; }
      .modal-dialog { width:900px; max-width:95%; }

      .notes-box {
        white-space:pre-wrap;
        font-family:inherit;
        background:#f8f9fa;
        padding:14px;
        border-radius:8px;
      }

      .ccnb-dashboard-title {
        margin:0 0 4px 0;
        color:#123038;
        font-weight:800;
        font-size:32px;
      }

      .ccnb-dashboard-subtitle {
        color:#65757b;
        margin-bottom:22px;
      }

      .dash-kpi-grid {
        display:grid;
        grid-template-columns:repeat(4,minmax(0,1fr));
        gap:16px;
        margin-bottom:18px;
      }

      .dash-kpi {
        background:white;
        border:1px solid var(--line);
        border-radius:12px;
        min-height:145px;
        box-shadow:0 4px 14px rgba(0,63,69,.05);
        overflow:hidden;
      }

      .dash-kpi-body {
        padding:22px 22px 15px 22px;
        display:flex;
        gap:16px;
        align-items:flex-start;
      }

      .dash-icon {
        width:58px;
        height:58px;
        border-radius:11px;
        display:flex;
        align-items:center;
        justify-content:center;
        font-size:28px;
        color:white;
        flex:0 0 58px;
      }

      .dash-icon.teal { background:#00757c; }
      .dash-icon.green { background:#14844c; }
      .dash-icon.orange { background:#ff851b; }
      .dash-icon.red { background:#d92d20; }

      .dash-icon-img {
        width:58px;
        height:58px;
        flex:0 0 58px;
        border-radius:12px;
        overflow:hidden;
      }

      .dash-quick > img {
        margin-bottom:12px;
      }

      .dash-kpi-label {
        font-size:15px;
        font-weight:700;
        margin:1px 0 5px;
      }

      .dash-kpi-value {
        font-size:36px;
        line-height:1;
        font-weight:800;
        color:#102a31;
        margin-bottom:8px;
      }

      .dash-kpi-help {
        font-size:13px;
        color:#6b777c;
      }

      .dash-kpi-footer {
        border-top:1px solid #edf0f1;
        padding:10px 18px;
        text-align:center;
        font-size:13px;
        font-weight:700;
        color:#00757c;
      }

      .dash-grid-main {
        display:grid;
        grid-template-columns:minmax(0,2.2fr) minmax(290px,.8fr);
        gap:16px;
        align-items:start;
      }

      .dash-panel {
        background:white;
        border:1px solid var(--line);
        border-radius:12px;
        box-shadow:0 4px 14px rgba(0,63,69,.05);
        overflow:hidden;
        margin-bottom:16px;
      }

      .dash-panel-header {
        padding:16px 18px;
        border-bottom:1px solid #edf0f1;
        font-size:18px;
        font-weight:800;
        color:#00585f;
      }

      .dash-panel-body { padding:16px 18px; }

      .dash-alert {
        padding:12px 0;
        border-bottom:1px solid #edf0f1;
        display:flex;
        gap:10px;
        align-items:flex-start;
      }
      .dash-alert:last-child { border-bottom:0; }
      .dash-alert .a-title { font-weight:700; }
      .dash-alert .a-sub { font-size:12px; color:#6b777c; margin-top:3px; }

      .dash-quick-grid {
        display:grid;
        grid-template-columns:repeat(4,minmax(0,1fr));
        gap:14px;
        margin-top:16px;
      }

      .dash-quick {
        background:white;
        border:1px solid var(--line);
        border-radius:12px;
        padding:16px;
        min-height:118px;
        box-shadow:0 4px 14px rgba(0,63,69,.04);
      }

      .dash-quick h4 {
        margin:0 0 6px 0;
        color:#123038;
        font-size:16px;
        font-weight:800;
      }

      .dash-quick p {
        color:#6b777c;
        font-size:12px;
        min-height:34px;
      }

      .dash-quick .btn {
        background:transparent;
        color:#00757c;
        border:0;
        padding:0;
        min-height:auto;
        font-weight:800;
      }

      .portal-footer {
        background:white;
        border-top:1px solid var(--line);
        padding:16px 24px;
        display:flex;
        align-items:center;
        justify-content:space-between;
        gap:18px;
        color:#54666c;
        font-size:13px;
      }

      .portal-footer-brand {
        display:flex;
        align-items:center;
        gap:12px;
        font-weight:800;
        color:#00585f;
      }

      .portal-footer-brand img {
        width:38px;
        height:38px;
        border-radius:7px;
      }


      /* ======================================================
         EDUSILLAB v36 - Accueil inspiré de la maquette CCNB
         ====================================================== */
      .portal-sidebar { background:linear-gradient(180deg,#103b63 0%,#0b2f52 100%) !important; }
      .portal-sidebar .btn { border-radius:0 !important; padding:14px 18px !important; font-size:15px !important; }
      .portal-sidebar .btn:hover,.portal-sidebar .btn:focus { background:#1f74cf !important; transform:none !important; }
      .portal-sidebar .btn.active { background:#247bd8 !important; border-left:4px solid #f59a2a !important; }
      .portal-topbar { background:linear-gradient(90deg,#123d66,#0d3155) !important; color:#fff !important; min-height:92px !important; }
      .portal-topbar-title { color:#fff !important; font-size:31px !important; font-weight:800 !important; }
      .portal-topbar-title small { color:#e8f0f8 !important; font-size:16px !important; }
      .portal-topbar-meta { color:#fff !important; }
      .portal-content { padding:0 !important; background:#f4f7fb !important; }
      .edu-home-hero { min-height:470px; display:grid; grid-template-columns:1.05fr 1fr; align-items:center; gap:42px; padding:42px 6%; position:relative; overflow:hidden; background:linear-gradient(100deg,rgba(247,251,255,.97),rgba(247,251,255,.90)),url('hero_ccnb_dieppe.png') center/cover no-repeat; border-bottom:1px solid #dce5ef; }
      .edu-home-hero:after { content:''; position:absolute; right:-70px; bottom:-120px; width:280px; height:280px; background:#f58220; transform:rotate(45deg); opacity:.85; }
      .edu-hero-logo { position:relative; z-index:1; display:flex; justify-content:center; }
      .edu-hero-logo img { width:min(100%,650px); max-height:290px; object-fit:contain; filter:drop-shadow(0 8px 18px rgba(10,42,72,.08)); }
      .edu-hero-copy { position:relative; z-index:1; color:#0c3159; }
      .edu-welcome { font-size:34px; font-weight:750; line-height:1.05; }
      .edu-title { font-size:66px; font-weight:900; letter-spacing:1px; line-height:1.05; margin:8px 0 12px; }
      .edu-orange { color:#ee5b16; }
      .edu-subtitle { font-size:22px; color:#243b55; margin-top:8px; }
      .edu-campus { font-size:21px; color:#243b55; margin-top:10px; }
      .edu-rule { width:105px; height:4px; background:#ef641d; margin:24px 0; }
      .edu-tagline { font-family:Georgia,serif; font-style:italic; font-size:25px; color:#173a62; }
      .edu-home-actions { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:18px; padding:0 20px 28px; margin-top:-62px; position:relative; z-index:3; }
      .edu-action-card { min-height:215px; background:#fff; border:1px solid #dce4ee; border-radius:17px; box-shadow:0 5px 18px rgba(22,49,79,.12); text-align:center; padding:22px 25px 24px; position:relative; }
      .edu-action-icon { width:76px; height:76px; margin:0 auto 12px; border-radius:50%; display:flex; align-items:center; justify-content:center; }
      .edu-action-icon img { width:54px !important; height:54px !important; }
      .edu-blue .edu-action-icon { background:#2879d5; }
      .edu-green .edu-action-icon { background:#38a66e; }
      .edu-orange-card .edu-action-icon { background:#ed6a21; }
      .edu-purple .edu-action-icon { background:#6750bd; }
      .edu-action-card h3 { color:#0c3159; font-size:21px; font-weight:800; margin:5px 0 9px; }
      .edu-action-card p { color:#344b63; font-size:15px; line-height:1.4; margin:0 auto; max-width:220px; }
      .edu-arrow { position:absolute !important; right:15px; bottom:10px; width:38px !important; min-width:38px !important; padding:0 !important; border:0 !important; background:transparent !important; color:#0c3159 !important; font-size:34px !important; box-shadow:none !important; }
      .portal-footer { background:#fff !important; border-top:1px solid #dce5ef !important; color:#3b526b !important; }
      @media(max-width:1100px){ .edu-home-hero{grid-template-columns:1fr; text-align:center; min-height:620px}.edu-rule{margin:22px auto}.edu-home-actions{grid-template-columns:repeat(2,1fr);margin-top:-35px}.edu-title{font-size:54px}.edu-hero-logo img{max-height:230px} }
      @media(max-width:680px){ .edu-home-actions{grid-template-columns:1fr;margin-top:0;padding:16px}.edu-home-hero{padding:30px 20px;min-height:auto}.edu-title{font-size:42px}.edu-welcome{font-size:26px}.edu-subtitle,.edu-campus{font-size:17px}.edu-tagline{font-size:20px} }
      /* v36.2 - menu bleu unique, sans second menu vert */
      .portal-sidebar {
        background: linear-gradient(180deg,#123f6d 0%,#0b2f52 100%) !important;
        width: 272px !important;
      }
      .portal-main { margin-left: 272px !important; }
      .portal-brand { padding: 14px 14px 12px !important; }
      .portal-logo-img { width: 58px !important; max-height: 58px !important; object-fit: contain !important; }
      .portal-brand-title { color:#fff !important; font-size:22px !important; font-weight:800 !important; }
      .portal-brand-subtitle { color:#fff !important; font-size:11px !important; letter-spacing:.4px !important; }
      .portal-sidebar .btn {
        color:#fff !important; background:transparent !important; border:0 !important;
        border-radius:0 !important; text-align:left !important; width:100% !important;
        padding:13px 20px !important; font-size:15px !important; font-weight:600 !important;
      }
      .portal-sidebar .btn:hover,.portal-sidebar .btn:focus { background:#1d67ad !important; }
      .portal-sidebar .btn.active { background:#247bd8 !important; border-left:4px solid #f5a13a !important; }
      .portal-menu-title { color:#9fc1df !important; padding:15px 20px 7px !important; font-size:11px !important; letter-spacing:.8px !important; }
      .portal-user { margin:18px 14px 16px !important; background:rgba(255,255,255,.06) !important; border:1px solid rgba(255,255,255,.15) !important; }
      @media(max-width:900px){ .portal-main{margin-left:0 !important}.portal-sidebar{width:100% !important} }

      /* ======================================================
         EDUSILLAB v39 - reproduction fidèle de la maquette
         ====================================================== */
      html,body { margin:0 !important; min-height:100%; background:#f7fafd !important; }
      body { font-family:Arial,Helvetica,sans-serif !important; }
      .portal-shell { min-height:100vh !important; background:#f7fafd !important; }
      .portal-sidebar {
        width:300px !important;
        background:linear-gradient(180deg,#123f70 0%,#0c4a7d 48%,#063c6d 100%) !important;
        box-shadow:none !important;
      }
      .portal-main { margin-left:300px !important; min-height:100vh !important; }
      .portal-brand { display:none !important; }
      .portal-sidebar .btn { min-height:62px !important; padding:0 24px !important; display:flex !important; align-items:center !important; color:#fff !important; font-size:16px !important; font-weight:500 !important; border-bottom:0 !important; }
      .portal-sidebar .btn:hover,.portal-sidebar .btn:focus { background:rgba(40,126,217,.72) !important; }
      .portal-sidebar #accueil { background:linear-gradient(90deg,#1976d2,#2f8af0) !important; border-left:4px solid #ff9c37 !important; font-size:18px !important; }
      .portal-menu-title { margin:4px 24px 0 !important; padding:16px 0 8px !important; border-top:1px solid rgba(255,255,255,.20); color:#a9c5df !important; font-size:12px !important; letter-spacing:.8px !important; }
      .portal-user { margin:18px 14px 18px !important; padding:18px !important; border-radius:14px !important; background:rgba(255,255,255,.075) !important; border:1px solid rgba(255,255,255,.18) !important; color:#fff !important; }
      .portal-user-code { font-size:18px !important; font-weight:800 !important; }
      .portal-avatar { width:48px !important; height:48px !important; background:#1fc9a1 !important; color:#fff !important; }
      .portal-role { margin-left:60px !important; color:#fff !important; }
      .portal-online { margin-left:60px !important; color:#20e6a5 !important; }
      .portal-timeout { color:#c8d8e8 !important; text-align:center !important; font-size:11px !important; }
      .portal-user .btn-danger { margin-top:16px !important; padding:12px 0 !important; min-height:auto !important; border-top:1px solid rgba(255,255,255,.2) !important; background:transparent !important; }
      .portal-topbar { height:105px !important; min-height:105px !important; padding:0 24px 0 14px !important; background:linear-gradient(90deg,#123f70,#073768) !important; display:flex !important; align-items:center !important; justify-content:space-between !important; }
      .portal-topbar-left { display:flex; align-items:center; gap:20px; min-width:0; }
      .portal-topbar-logo { width:275px; height:80px; object-fit:contain; object-position:left center; background:#fff; border-radius:7px; padding:2px 8px; }
      .portal-topbar-title { color:#fff !important; font-size:36px !important; font-weight:800 !important; line-height:1.05 !important; white-space:nowrap; }
      .portal-topbar-title small { display:block !important; color:#fff !important; font-size:18px !important; font-weight:400 !important; margin-top:7px !important; }
      .portal-topbar-meta { display:flex !important; align-items:center !important; gap:36px !important; color:#fff !important; font-size:18px !important; }
      .topbar-user { white-space:nowrap; }
      .topbar-logout { border:0; background:transparent; color:#fff; font-size:18px; padding:10px 0; cursor:pointer; white-space:nowrap; }
      .portal-content { padding:0 !important; min-height:calc(100vh - 180px) !important; background:linear-gradient(180deg,#f8fbfe,#fff) !important; }
      .edu-home-hero { min-height:560px !important; height:560px !important; grid-template-columns:1.08fr .92fr !important; gap:28px !important; padding:36px 5% 125px !important; background:linear-gradient(100deg,rgba(248,252,255,.78),rgba(248,252,255,.67)),url('hero_ccnb_dieppe.png') center/cover no-repeat !important; border-bottom:0 !important; }
      .edu-home-hero:before { content:''; position:absolute; left:0; bottom:0; width:170px; height:210px; background:rgba(49,128,196,.18); clip-path:polygon(0 0,100% 100%,0 100%); }
      .edu-home-hero:after { right:-82px !important; bottom:-112px !important; width:250px !important; height:250px !important; background:#f58220 !important; opacity:.78 !important; }
      .edu-hero-logo img { width:min(100%,590px) !important; max-height:330px !important; filter:none !important; }
      .edu-hero-copy { padding-right:10px; }
      .edu-welcome { font-size:39px !important; font-weight:800 !important; color:#073b73 !important; }
      .edu-title { font-size:66px !important; font-weight:900 !important; margin:6px 0 14px !important; color:#073b73 !important; letter-spacing:-1px !important; }
      .edu-orange { color:#ff5a00 !important; }
      .edu-subtitle,.edu-campus { font-size:18px !important; color:#173a62 !important; }
      .edu-rule { width:80px !important; height:4px !important; margin:18px 0 !important; background:#ff681e !important; }
      .edu-tagline { font-family:Georgia,'Times New Roman',serif !important; font-style:italic !important; font-size:22px !important; color:#0b3b70 !important; white-space:nowrap; }
      .edu-home-actions { grid-template-columns:repeat(4,minmax(0,1fr)) !important; gap:14px !important; padding:0 18px 24px !important; margin-top:-165px !important; }
      .edu-action-card { min-height:255px !important; border-radius:18px !important; padding:22px 16px 26px !important; box-shadow:0 6px 20px rgba(0,0,0,.10) !important; border:1px solid #e6edf5 !important; }
      .edu-action-icon { width:90px !important; height:90px !important; margin-bottom:18px !important; }
      .edu-action-icon img { width:58px !important; height:58px !important; }
      .edu-blue .edu-action-icon { background:#147bea !important; }
      .edu-green .edu-action-icon { background:#18b978 !important; }
      .edu-orange-card .edu-action-icon { background:#ff620b !important; }
      .edu-purple .edu-action-icon { background:#7138cf !important; }
      .edu-action-card h3 { color:#073b73 !important; font-size:20px !important; font-weight:800 !important; margin:3px 0 10px !important; }
      .edu-action-card p { color:#5a7190 !important; font-size:16px !important; line-height:1.4 !important; }
      .edu-arrow { color:#073b73 !important; font-size:38px !important; right:12px !important; bottom:8px !important; }
      .portal-footer { min-height:75px !important; height:75px !important; padding:0 25px !important; background:#fff !important; border-top:1px solid #e4eaf1 !important; color:#547095 !important; display:flex !important; align-items:center !important; justify-content:space-between !important; }
      .portal-footer-brand { display:none !important; }
      @media(max-width:1200px){ .portal-topbar-logo{width:220px}.portal-topbar-title{font-size:30px !important}.portal-topbar-meta{gap:18px !important}.edu-title{font-size:54px !important}.edu-tagline{white-space:normal}.edu-home-actions{grid-template-columns:repeat(2,1fr) !important;margin-top:-90px !important}.edu-home-hero{height:auto !important;min-height:600px !important} }
      @media(max-width:900px){ .portal-main{margin-left:0 !important}.portal-sidebar{width:100% !important;position:relative !important;height:auto !important}.portal-topbar{height:auto !important;min-height:120px !important;flex-wrap:wrap !important}.portal-topbar-logo{width:190px}.edu-home-hero{grid-template-columns:1fr !important;text-align:center !important;padding-bottom:60px !important}.edu-rule{margin:18px auto !important}.edu-home-actions{margin-top:0 !important}.portal-footer{height:auto !important;min-height:75px !important;flex-wrap:wrap !important;gap:10px !important;padding:16px 20px !important} }
      @media(max-width:650px){ .edu-home-actions{grid-template-columns:1fr !important}.portal-topbar-meta{width:100%;justify-content:space-between}.portal-topbar-title small{font-size:14px !important}.edu-title{font-size:44px !important}.edu-welcome{font-size:30px !important} }

      /* ======================================================
         EDUSILLAB v40 - accueil visuel exact de la reference
         ====================================================== */
      .edu-reference-screen {
        position:fixed; inset:0; z-index:9998; overflow:auto; background:#eef3f8;
      }
      .edu-reference-canvas {
        position:relative; width:100vw; min-width:980px; aspect-ratio:1226 / 1283; margin:0;
      }
      .edu-reference-image {
        position:absolute; inset:0; width:100%; height:100%; object-fit:fill; display:block; user-select:none; -webkit-user-drag:none;
      }
      .edu-hotspot {
        position:absolute; z-index:2; border:0; background:transparent; cursor:pointer; padding:0; margin:0;
        color:transparent; font-size:0; outline:none; box-shadow:none;
      }
      .edu-hotspot:hover { background:rgba(255,255,255,.07); }
      .edu-hotspot:focus { outline:2px solid rgba(255,156,55,.55); outline-offset:-2px; }
      .edu-hotspot.logout:hover { background:rgba(255,255,255,.05); }

      .portal-topbar-title small {
        display:block;
        font-size:11px;
        color:#6b777c;
        font-weight:600;
        margin-top:3px;
      }

      .dt-container { overflow-x:auto; }

      @media (max-width:1150px) {
        .dash-kpi-grid { grid-template-columns:repeat(2,minmax(0,1fr)); }
        .dash-quick-grid { grid-template-columns:repeat(2,minmax(0,1fr)); }
      }

      @media (max-width:900px) {
        .portal-shell { grid-template-columns:220px minmax(0,1fr); }
      }

      @media (max-width:700px) {
        .portal-shell { display:block; }
        .portal-sidebar { position:relative; height:auto; }
        .portal-topbar { position:relative; }
        .portal-content { padding:14px; }
      }
    ")),
    tags$script(HTML("
      $(document).on('keydown', '#req_order_mnemo', function(e) {
        if (e.key === 'Enter' || e.keyCode === 13) {
          e.preventDefault();
          Shiny.setInputValue(
            'order_enter',
            $(this).val(),
            {priority:'event'}
          );
        }
      });

      Shiny.addCustomMessageHandler('focusOrder', function(message) {
        setTimeout(function() {
          $('#req_order_mnemo').focus();
        }, 100);
      });

      // Déconnexion automatique après 10 minutes d'inactivité.
      (function() {
        var inactivityTimer = null;
        var TIMEOUT_MS = 10 * 60 * 1000;

        function signalerInactivite() {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              'inactive_logout',
              Date.now(),
              {priority:'event'}
            );
          }
        }

        function reinitialiserTimer() {
          if (inactivityTimer) clearTimeout(inactivityTimer);
          inactivityTimer = setTimeout(signalerInactivite, TIMEOUT_MS);
        }

        ['mousemove','mousedown','keydown','touchstart','scroll','click'].forEach(function(evt) {
          document.addEventListener(evt, reinitialiserTimer, {passive:true});
        });

        $(document).on('shiny:connected', reinitialiserTimer);

        Shiny.addCustomMessageHandler('resetInactivityTimer', function(message) {
          reinitialiserTimer();
        });
      })();
    "))
  ),

  uiOutput("page")
)

# ============================================================
# 3B. IMPORT / EXPORT DES CLIENTS (PATIENTS)
# ============================================================

normaliser_nom_colonne_client <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

standardiser_fichier_clients <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  noms_orig <- names(df)
  noms_norm <- normaliser_nom_colonne_client(noms_orig)

  trouver_col <- function(candidats) {
    idx <- which(noms_norm %in% candidats)
    if (length(idx) == 0) return(NULL)
    noms_orig[idx[1]]
  }

  c_dossier <- trouver_col(c(
    "numero_dossier","numero_de_dossier","dossier","no_dossier","num_dossier",
    "patient_id","numero_patient"
  ))
  c_medicare <- trouver_col(c(
    "medicare","numero_medicare","numero_assurance_maladie","assurance_maladie",
    "health_card","health_card_number"
  ))
  c_nom <- trouver_col(c("nom","last_name","lastname","surname"))
  c_prenom <- trouver_col(c("prenom","first_name","firstname","given_name"))
  c_ddn <- trouver_col(c(
    "date_naissance","date_de_naissance","dob","birth_date","date_birth"
  ))
  c_sexe <- trouver_col(c("sexe","sex","gender"))
  c_actif <- trouver_col(c("actif","active","statut"))
  c_adresse <- trouver_col(c("adresse","address"))
  c_ville <- trouver_col(c("ville","city"))
  c_province <- trouver_col(c("province","state"))
  c_cp <- trouver_col(c("code_postal","postal_code","zip","zipcode"))
  c_pays <- trouver_col(c("pays","country"))
  c_lat <- trouver_col(c("latitude","lat"))
  c_lon <- trouver_col(c("longitude","lon","lng"))

  get_chr <- function(col) {
    if (is.null(col)) rep("", nrow(df)) else trimws(as.character(df[[col]]))
  }

  parse_date_client <- function(x) {
    x <- trimws(as.character(x))
    out <- rep(NA_character_, length(x))
    for (i in seq_along(x)) {
      if (!nzchar(x[i]) || is.na(x[i])) next
      val <- x[i]
      essais <- c(
        "%Y-%m-%d","%m/%d/%Y","%m/%d/%y","%d/%m/%Y","%d/%m/%y",
        "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"
      )
      ok <- NA
      for (fmt in essais) {
        d <- suppressWarnings(as.Date(val, format = fmt))
        if (!is.na(d)) { ok <- d; break }
      }
      if (!is.na(ok)) out[i] <- format(ok, "%Y-%m-%d")
    }
    out
  }

  sexe <- toupper(get_chr(c_sexe))
  sexe[sexe %in% c("FEMME","FEMALE")] <- "F"
  sexe[sexe %in% c("HOMME","MALE")] <- "M"
  sexe[sexe %in% c("NOUVEAU-NE","NOUVEAU_NÉ","NOUVEAU_NE","BABY")] <- "BB"
  sexe[!sexe %in% c("F","M","BB","U") & nzchar(sexe)] <- "U"

  actif_chr <- toupper(get_chr(c_actif))
  actif <- rep(1L, nrow(df))
  actif[actif_chr %in% c("0","NON","NO","INACTIF","INACTIVE","FALSE")] <- 0L

  data.frame(
    numero_dossier = get_chr(c_dossier),
    medicare = get_chr(c_medicare),
    nom = toupper(get_chr(c_nom)),
    prenom = get_chr(c_prenom),
    date_naissance = parse_date_client(get_chr(c_ddn)),
    sexe = sexe,
    actif = actif,
    adresse = get_chr(c_adresse),
    ville = get_chr(c_ville),
    province = toupper(get_chr(c_province)),
    code_postal = toupper(get_chr(c_cp)),
    pays = get_chr(c_pays),
    latitude = suppressWarnings(as.numeric(get_chr(c_lat))),
    longitude = suppressWarnings(as.numeric(get_chr(c_lon))),
    stringsAsFactors = FALSE
  )
}

lire_fichier_clients <- function(path, nom_fichier = "") {
  ext <- tolower(tools::file_ext(nom_fichier))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx","xls")) {
    feuilles <- readxl::excel_sheets(path)
    if (length(feuilles) == 0) stop("Le classeur Excel ne contient aucune feuille.")
    blocs <- lapply(feuilles, function(sh) {
      tryCatch(
        as.data.frame(readxl::read_excel(path, sheet = sh), stringsAsFactors = FALSE),
        error = function(e) NULL
      )
    })
    blocs <- Filter(function(x) !is.null(x) && nrow(x) > 0, blocs)
    if (length(blocs) == 0) stop("Aucune donnée client exploitable dans le classeur.")
    return(do.call(rbind, lapply(blocs, function(x) {
      names(x) <- make.unique(names(x))
      x
    })))
  }

  if (ext == "csv") {
    # Essayer séparateur virgule puis point-virgule.
    d1 <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(d1) && ncol(d1) > 1) return(d1)
    return(read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (ext %in% c("tsv","tab")) {
    return(read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (ext == "json") {
    obj <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.data.frame(obj)) return(obj)
    if (is.list(obj)) return(as.data.frame(obj, stringsAsFactors = FALSE))
    stop("Structure JSON non reconnue.")
  }

  if (ext %in% c("txt","text")) {
    # Détection simple tabulation / point-virgule / virgule.
    lignes <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (length(lignes) == 0) return(data.frame())
    sep <- if (grepl("\t", lignes[1])) "\t" else if (grepl(";", lignes[1])) ";" else ","
    return(read.table(
      path, header = TRUE, sep = sep, stringsAsFactors = FALSE,
      check.names = FALSE, quote = "\"", comment.char = ""
    ))
  }

  stop(
    paste0(
      "Format non pris en charge pour une mise à jour structurée : .", ext,
      ". Formats acceptés : XLSX, XLS, CSV, TSV, TXT et JSON."
    )
  )
}

mettre_a_jour_clients_importes <- function(con, tab) {
  if (is.null(tab) || nrow(tab) == 0) {
    return(list(ajoutes = 0L, maj = 0L, ignores = 0L, erreurs = character()))
  }

  ajoutes <- 0L
  maj <- 0L
  ignores <- 0L
  erreurs <- character()

  for (i in seq_len(nrow(tab))) {
    r <- tab[i, , drop = FALSE]

    dossier <- trimws(ifelse(is.na(r$numero_dossier), "", r$numero_dossier))
    medicare <- trimws(ifelse(is.na(r$medicare), "", r$medicare))

    # Il faut au moins un identifiant stable pour éviter de créer des doublons.
    if (!nzchar(dossier) && !nzchar(medicare)) {
      ignores <- ignores + 1L
      erreurs <- c(erreurs, paste0("Ligne ", i, " ignorée : dossier et Medicare absents."))
      next
    }

    existants <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM patients
      WHERE (? <> '' AND numero_dossier = ?)
         OR (? <> '' AND medicare = ?)
      ORDER BY id
      ",
      params = list(dossier, dossier, medicare, medicare)
    )

    # Détecter un conflit si dossier et Medicare pointent vers 2 patients différents.
    if (nrow(existants) > 1) {
      ids_dossier <- existants$id[existants$numero_dossier == dossier & nzchar(dossier)]
      ids_med <- existants$id[existants$medicare == medicare & nzchar(medicare)]
      if (length(ids_dossier) > 0 && length(ids_med) > 0 &&
          length(intersect(ids_dossier, ids_med)) == 0) {
        ignores <- ignores + 1L
        erreurs <- c(
          erreurs,
          paste0("Ligne ", i, " ignorée : conflit dossier/Medicare entre deux patients.")
        )
        next
      }
    }

    if (nrow(existants) >= 1) {
      id_patient <- existants$id[1]

      ancien <- DBI::dbGetQuery(con, "SELECT * FROM patients WHERE id = ?", params = list(id_patient))[1,]

      garder <- function(nouveau, ancien_val) {
        nv <- ifelse(length(nouveau)==0 || is.na(nouveau), "", as.character(nouveau))
        if (nzchar(trimws(nv))) nv else ancien_val
      }

      DBI::dbExecute(
        con,
        "
        UPDATE patients
        SET
          numero_dossier = ?,
          medicare = ?,
          nom = ?,
          prenom = ?,
          date_naissance = ?,
          sexe = ?,
          actif = ?,
          adresse = ?,
          ville = ?,
          province = ?,
          code_postal = ?,
          pays = ?,
          latitude = ?,
          longitude = ?
        WHERE id = ?
        ",
        params = list(
          garder(r$numero_dossier, ancien$numero_dossier),
          garder(r$medicare, ancien$medicare),
          garder(r$nom, ancien$nom),
          garder(r$prenom, ancien$prenom),
          garder(r$date_naissance, ancien$date_naissance),
          garder(r$sexe, ancien$sexe),
          ifelse(is.na(r$actif), ancien$actif, as.integer(r$actif)),
          garder(r$adresse, ancien$adresse),
          garder(r$ville, ancien$ville),
          garder(r$province, ancien$province),
          garder(r$code_postal, ancien$code_postal),
          garder(r$pays, ancien$pays),
          ifelse(is.na(r$latitude), ancien$latitude, r$latitude),
          ifelse(is.na(r$longitude), ancien$longitude, r$longitude),
          id_patient
        )
      )
      maj <- maj + 1L
    } else {
      if (!nzchar(r$nom) || !nzchar(r$prenom)) {
        ignores <- ignores + 1L
        erreurs <- c(
          erreurs,
          paste0("Ligne ", i, " ignorée : nouveau client sans nom/prénom.")
        )
        next
      }

      DBI::dbExecute(
        con,
        "
        INSERT INTO patients (
          numero_dossier, medicare, nom, prenom, date_naissance, sexe,
          actif, date_creation, adresse, ville, province, code_postal,
          pays, latitude, longitude
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, ?, ?, ?, ?, ?, ?)
        ",
        params = list(
          if (nzchar(r$numero_dossier)) as.character(r$numero_dossier) else NA_character_,
          if (nzchar(r$medicare)) as.character(r$medicare) else NA_character_,
          as.character(r$nom)[1],
          as.character(r$prenom)[1],
          if (is.na(r$date_naissance) || !nzchar(r$date_naissance)) NA_character_ else as.character(r$date_naissance),
          if (nzchar(as.character(r$sexe)[1])) as.character(r$sexe)[1] else "U",
          as.integer(ifelse(is.na(r$actif), 1L, r$actif)),
          as.character(r$adresse)[1], as.character(r$ville)[1], as.character(r$province)[1], as.character(r$code_postal)[1],
          if (nzchar(as.character(r$pays)[1])) as.character(r$pays)[1] else "Canada",
          if (is.na(as.numeric(r$latitude)[1])) NA_real_ else as.numeric(r$latitude)[1],
          if (is.na(as.numeric(r$longitude)[1])) NA_real_ else as.numeric(r$longitude)[1]
        )
      )
      ajoutes <- ajoutes + 1L
    }
  }

  list(ajoutes = ajoutes, maj = maj, ignores = ignores, erreurs = erreurs)
}


# ============================================================
# 4. SERVEUR
# ============================================================

server <- function(input, output, session) {

  # Logo par défaut disponible dans la session Shiny.
  # get0() évite l'erreur « object 'CCNB_LOGO_DATA_URI' not found »
  # si l'environnement de déploiement ne voit pas la constante globale.
  ccnb_logo_default <- ccnb_logo_src_default()
  # v35.3 : le logo institutionnel CCNB Laboratoire Dieppe est imposé dans l’interface.
  # Une ancienne image enregistrée dans portail_configuration ne peut plus le remplacer.

  refresh_portail <- reactiveVal(0L)

  lire_config_portail <- function() {
    refresh_portail()
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    d <- DBI::dbGetQuery(
      con,
      "SELECT cle,valeur FROM portail_configuration"
    )

    valeurs <- setNames(as.list(d$valeur),d$cle)

    getv <- function(k,default="") {
      x <- valeurs[[k]]
      if (is.null(x) || is.na(x) || !nzchar(as.character(x))) default else as.character(x)
    }

    list(
      portail_nom=getv("portail_nom","EDUSILLAB"),
      institution=getv("institution","CCNB"),
      campus=getv("campus","Campus de Dieppe"),
      sous_titre=getv(
        "sous_titre",
        "Système d'information de laboratoire — environnement d'enseignement — version Web"
      ),
      tableau_bord_titre=getv("tableau_bord_titre","Tableau de bord"),
      tableau_bord_sous_titre=getv(
        "tableau_bord_sous_titre",
        "Vue d'ensemble de l'activité du laboratoire d'enseignement."
      ),
      footer_texte=getv(
        "footer_texte",
        "© 2026 CCNB Campus de Dieppe. Tous droits réservés."
      ),
      couleur_principale=getv("couleur_principale","#00757c"),
      couleur_secondaire=getv("couleur_secondaire","#00585f"),
      couleur_sidebar_bas=getv("couleur_sidebar_bas","#003f45"),
      logo_data_uri=getv("logo_data_uri","")
    )
  }

  peut_modifier_portail <- function() {
    u <- utilisateur_connecte()
    if (is.null(u)) return(FALSE)
    isTRUE(est_administrateur()) ||
      isTRUE(est_superutilisateur()) ||
      (!is.null(u$perm_portail) && as.integer(u$perm_portail)==1L)
  }


  utilisateur_connecte <- reactiveVal(NULL)
  page_active <- reactiveVal("accueil")
  patient_requisition <- reactiveVal(NULL)
  historique <- reactiveVal(NULL)
  refresh_analyses <- reactiveVal(0L)
  refresh_users <- reactiveVal(0L)
  refresh_imports <- reactiveVal(0L)
  ouvrir_import_depuis_menu <- reactiveVal(0L)
  refresh_materiel <- reactiveVal(0L)
  dernieres_etiquettes <- reactiveVal(NULL)
  dernier_pdf_etiquettes <- reactiveVal(NULL)
  etiquettes_selectionnees <- reactiveVal(integer(0))
  dernier_pdf_reimpression <- reactiveVal(NULL)
  specimen_reception <- reactiveVal(NULL)
  specimens_reception_multiple <- reactiveVal(data.frame())
  reception_unitaire_confirmee <- reactiveVal(0L)

  dashboard_snapshot <- reactive({
    req(utilisateur_connecte())
    invalidateLater(60000, session)

    con_dash <- ouvrir_db()
    on.exit({
      if (!is.null(con_dash) && DBI::dbIsValid(con_dash)) {
        DBI::dbDisconnect(con_dash)
      }
    }, add=TRUE)

    today <- format(Sys.Date(), "%Y-%m-%d")

    scalar_n <- function(sql, params=list()) {
      x <- tryCatch(
        DBI::dbGetQuery(con_dash, sql, params=params),
        error=function(e) data.frame(n=0)
      )
      if (nrow(x)==0 || is.na(x$n[1])) 0L else as.integer(x$n[1])
    }

    demandes <- scalar_n(
      "SELECT COUNT(*) AS n FROM requisitions WHERE substr(date_creation,1,10)=?",
      list(today)
    )

    specimens_jour <- scalar_n(
      "SELECT COUNT(*) AS n FROM specimens WHERE date_specimen=?",
      list(today)
    )

    attente <- scalar_n("
      SELECT COUNT(*) AS n
      FROM specimens
      WHERE UPPER(COALESCE(statut_reception,'')) NOT IN ('RECU','ANNULE')
    ")

    annules <- scalar_n(
      "SELECT COUNT(*) AS n FROM specimens WHERE substr(COALESCE(date_annulation,''),1,10)=?",
      list(today)
    )

    retard <- scalar_n("
      SELECT COUNT(*) AS n
      FROM specimens
      WHERE UPPER(COALESCE(statut_reception,'')) NOT IN ('RECU','ANNULE')
        AND date(date_specimen) < date('now','localtime')
    ")

    stat_attente <- scalar_n("
      SELECT COUNT(*) AS n
      FROM specimens
      WHERE UPPER(COALESCE(statut_reception,'')) NOT IN ('RECU','ANNULE')
        AND UPPER(COALESCE(priorite,''))='STAT'
    ")

    liste_attente <- tryCatch(
      DBI::dbGetQuery(
        con_dash,
        "
        SELECT
          s.code_barre AS `BC#`,
          s.numero_specimen AS `N° Spécimen`,
          trim(COALESCE(p.nom,'') || ' ' || COALESCE(p.prenom,'')) AS Patient,
          COALESCE(s.departement,'') AS Département,
          COALESCE(s.priorite,'Routine') AS Priorité,
          CASE
            WHEN UPPER(COALESCE(s.statut_reception,''))='RECU' THEN 'Reçu'
            WHEN UPPER(COALESCE(s.statut_reception,''))='ANNULE' THEN 'Annulé'
            ELSE 'En attente'
          END AS Statut,
          substr(COALESCE(r.date_creation,''),1,16) AS `Date demande`
        FROM specimens s
        LEFT JOIN patients p ON p.id=s.patient_id
        LEFT JOIN requisitions r ON r.id=s.requisition_id
        WHERE UPPER(COALESCE(s.statut_reception,'')) NOT IN ('RECU','ANNULE')
        ORDER BY
          CASE UPPER(COALESCE(s.priorite,''))
            WHEN 'STAT' THEN 1
            WHEN 'URGENCE' THEN 2
            ELSE 3
          END,
          s.id DESC
        LIMIT 5
        "
      ),
      error=function(e) data.frame()
    )

    activite <- tryCatch(
      DBI::dbGetQuery(
        con_dash,
        "
        SELECT
          substr(COALESCE(date_action,''),12,5) AS Heure,
          COALESCE(action,'') AS Action,
          COALESCE(details,'') AS Details,
          trim(COALESCE(initiales,'') || COALESCE(matricule,'')) AS Utilisateur
        FROM audit_trail
        ORDER BY id DESC
        LIMIT 5
        "
      ),
      error=function(e) data.frame()
    )

    list(
      demandes=demandes,
      specimens_jour=specimens_jour,
      attente=attente,
      annules=annules,
      retard=retard,
      stat_attente=stat_attente,
      liste_attente=liste_attente,
      activite=activite
    )
  })

  order_vide <- function() {
    data.frame(
      id = integer(),
      mnemonique = character(),
      nom = character(),
      departement = character(),
      px = character(),
      source_prelevement = character(),
      source_autre = character(),
      stringsAsFactors = FALSE
    )
  }

  order_actuel <- reactiveVal(order_vide())

  permission <- function(x) {
    if (length(x) == 0 || is.na(x)) 0L else as.integer(x)
  }

  code_utilisateur <- function() {
    u <- utilisateur_connecte()
    if (is.null(u)) return("")
    ini <- toupper(gsub("[^A-Za-z]", "", ifelse(is.null(u$initiales), "", u$initiales)))
    mat <- gsub("[^0-9A-Za-z]", "", ifelse(is.null(u$matricule), "", u$matricule))
    paste0(ini, mat)
  }

  est_administrateur <- function() {
    u <- utilisateur_connecte()
    if (is.null(u)) return(FALSE)
    role <- toupper(trimws(ifelse(is.null(u$role), "", u$role)))
    role_compact <- gsub("[^A-Z]", "", iconv(role, to="ASCII//TRANSLIT", sub=""))
    role_compact %in% c("ADMIN","ADMINISTRATEUR")
  }

  est_superutilisateur <- function() {
    u <- utilisateur_connecte()
    if (is.null(u)) return(FALSE)
    role <- toupper(trimws(ifelse(is.null(u$role), "", u$role)))
    role_compact <- gsub("[^A-Z]", "", iconv(role, to="ASCII//TRANSLIT", sub=""))
    role_compact %in% c("SUPERUTILISATEUR","SUPERUSER")
  }

  peut_gerer_analyses <- function() {
    # Création, modification et import du répertoire :
    # uniquement ADMIN et SUPERUTILISATEUR.
    u <- utilisateur_connecte()
    if (is.null(u)) return(FALSE)

    est_administrateur() || est_superutilisateur()
  }

  journaliser <- function(action, details="", patient_id=NULL, requisition_id=NULL, specimen_id=NULL) {
    u <- utilisateur_connecte()

    tryCatch({
      con_audit <- ouvrir_db()
      on.exit({
        if (!is.null(con_audit) && DBI::dbIsValid(con_audit)) {
          DBI::dbDisconnect(con_audit)
        }
      }, add=TRUE)

      DBI::dbExecute(
        con_audit,
        "
        INSERT INTO audit_trail(
          patient_id,requisition_id,specimen_id,utilisateur_id,
          initiales,matricule,action,details
        ) VALUES (?,?,?,?,?,?,?,?)
        ",
        params=list(
          patient_id,requisition_id,specimen_id,
          if (is.null(u)) NULL else u$id,
          if (is.null(u)) "" else u$initiales,
          if (is.null(u)) "" else u$matricule,
          action,details
        )
      )

      TRUE
    }, error=function(e) {
      message("Audit non enregistré : ", conditionMessage(e))
      FALSE
    })
  }

  parametres_impression <- function(con=NULL) {
    fermer <- FALSE
    if (is.null(con)) {
      con <- ouvrir_db()
      fermer <- TRUE
    }
    if (fermer) on.exit(DBI::dbDisconnect(con), add=TRUE)

    imp <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM materiel_impression
      WHERE actif=1
        AND UPPER(COALESCE(type,'')) IN ('IMPRIMANTE','IMPRIMANTE ETIQUETTES','PDF')
      ORDER BY par_defaut DESC,id
      LIMIT 1
      "
    )

    if (nrow(imp)!=1) {
      return(list(largeur=100,hauteur=50,offset_x=0,offset_y=0,echelle=100,mode="AUTO"))
    }

    mode <- toupper(ifelse(
      is.na(imp$mode_ajustement[1]) || !nzchar(imp$mode_ajustement[1]),
      "AUTO",imp$mode_ajustement[1]
    ))

    list(
      largeur=ifelse(is.na(imp$largeur_mm[1]),100,imp$largeur_mm[1]),
      hauteur=ifelse(is.na(imp$hauteur_mm[1]),50,imp$hauteur_mm[1]),
      offset_x=ifelse(mode=="MANUEL",ifelse(is.na(imp$offset_x_mm[1]),0,imp$offset_x_mm[1]),0),
      offset_y=ifelse(mode=="MANUEL",ifelse(is.na(imp$offset_y_mm[1]),0,imp$offset_y_mm[1]),0),
      echelle=ifelse(mode=="MANUEL",ifelse(is.na(imp$echelle_pct[1]),100,imp$echelle_pct[1]),100),
      mode=mode
    )
  }

  creer_pdf_selon_imprimante <- function(labels,fichier,con=NULL) {
    p <- parametres_impression(con)
    creer_pdf_etiquettes(
      labels,fichier,
      largeur_mm=p$largeur,
      hauteur_mm=p$hauteur,
      offset_x_mm=p$offset_x,
      offset_y_mm=p$offset_y,
      echelle_pct=p$echelle
    )
  }

  charger_historique <- function() {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    tab <- DBI::dbGetQuery(con, "
    SELECT
      r.id AS requisition_id,
      r.numero_requisition AS Req,
      r.date_creation AS Date_creation,
      r.date_prelevement AS Date_prelevement,
      r.heure_prelevement AS Heure,
      p.numero_dossier AS Dossier,
      p.medicare AS Medicare,
      p.nom AS Nom_patient,
      p.prenom AS Prenom_patient,
      r.priorite AS Priorite,
      r.prescripteur AS Prescripteur,
      GROUP_CONCAT(a.mnemonique, ', ') AS Analyses,
      GROUP_CONCAT(COALESCE(dp.px, ''), ', ') AS Px,
      r.statut AS Statut,
      COALESCE(NULLIF(r.saisi_initiales,''), u.initiales, '') AS Initiales_saisie,
      COALESCE(NULLIF(r.saisi_matricule,''), u.matricule, '') AS Matricule_saisie,
      u.nom AS Nom_utilisateur,
      u.prenom AS Prenom_utilisateur,
      u.email AS Courriel,
      u.role AS Niveau,
      (
        SELECT GROUP_CONCAT(DISTINCT TRIM(COALESCE(ur.prenom,'') || ' ' || COALESCE(ur.nom,'')))
        FROM specimens sr
        LEFT JOIN utilisateurs ur ON ur.id = sr.recu_par
        WHERE sr.requisition_id = r.id
          AND sr.recu_par IS NOT NULL
      ) AS Receptionne_par,
      (
        SELECT GROUP_CONCAT(DISTINCT COALESCE(NULLIF(sr.recu_initiales,''), ur.initiales, ''))
        FROM specimens sr
        LEFT JOIN utilisateurs ur ON ur.id = sr.recu_par
        WHERE sr.requisition_id = r.id
          AND sr.recu_par IS NOT NULL
      ) AS Initiales_reception,
      (
        SELECT MAX(sr.date_reception)
        FROM specimens sr
        WHERE sr.requisition_id = r.id
      ) AS Date_reception
    FROM requisitions r
    INNER JOIN patients p ON p.id = r.patient_id
    LEFT JOIN utilisateurs u ON u.id = r.cree_par
    LEFT JOIN requisition_analyses ra ON ra.requisition_id = r.id
    LEFT JOIN analyses a ON a.id = ra.analyse_id
    LEFT JOIN departements_px dp ON UPPER(dp.departement) = UPPER(a.departement)
    GROUP BY r.id
    ORDER BY r.id DESC
    ")

    historique(tab)
  }

  # ----------------------------------------------------------
  # PAGE PRINCIPALE / CONNEXION
  # ----------------------------------------------------------

  output$page <- renderUI({
    cfg <- lire_config_portail()
    logo_portail <- ccnb_logo_default

    if (is.null(utilisateur_connecte())) {
      return(
        div(
          class = "edu-login-screen",
          div(
            class = "edu-login-live",
            tags$span(class="login-user-icon", "●"),
            tags$span(class="login-lock-icon", "▣"),
            textInput("identifiant", NULL, placeholder="Identifiant"),
            passwordInput("mot_de_passe", NULL, placeholder="Mot de passe"),
            actionButton("connexion", "Se connecter", class="btn-primary"),
            tags$a(class="edu-login-forgot", href="#", onclick="return false;", "Mot de passe oublié ?"),
            uiOutput("message_connexion")
          )
        )
      )
    }

    if (page_active() == "changer_mdp") {
      return(
        div(
          class = "login-modern",
          h2("Changer le mot de passe temporaire"),
          div(
            class = "warning-box",
            "Vous devez choisir votre propre mot de passe avant d'accéder au portail."
          ),
          br(),
          p(strong("Code utilisateur : "), code_utilisateur()),
          p(
            strong("Utilisateur : "),
            paste(utilisateur_connecte()$prenom, utilisateur_connecte()$nom)
          ),
          p(strong("Courriel : "), utilisateur_connecte()$email),
          passwordInput("nouveau_mdp_obligatoire", "Nouveau mot de passe"),
          passwordInput("confirmation_mdp_obligatoire", "Confirmer le nouveau mot de passe"),
          actionButton("enregistrer_nouveau_mdp", "Enregistrer", class = "btn-success"),
          br(), br(),
          uiOutput("message_changement_mdp")
        )
      )
    }

    u <- utilisateur_connecte()
    ini <- toupper(substr(ifelse(is.null(u$initiales), "", u$initiales), 1, 2))
    cfg <- lire_config_portail()

    logo_portail <- ccnb_logo_default

    div(
      tags$style(HTML(sprintf(
        "
        :root { --blue:%s; --teal:%s; }
        .portal-sidebar {
          background:linear-gradient(180deg,#123f70 0%%,#0c4a7d 45%%,#063c6d 100%%) !important;
        }
        ",
        cfg$couleur_principale,
        cfg$couleur_principale
      ))),
      class = "portal-shell",

      div(
        class = "portal-sidebar",

        div(
          class = "portal-brand",
          tags$img(
            class = "portal-logo-img",
            src = logo_portail,
            alt = "Logo du portail"
          ),
          div(
            div(class = "portal-brand-title", "CCNB"),
            div(class = "portal-brand-subtitle", "CAMPUS DE DIEPPE")
          )
        ),

        actionButton("accueil", "⌂  Accueil"),

        if (u$perm_recherche_patient == 1 || u$perm_patients == 1)
          actionButton("menu_patients", "♟  Patients"),

        if (u$perm_requisition == 1)
          actionButton("menu_requisition", "▣  Nouvelle demande"),

        if (u$perm_reception == 1)
          actionButton("menu_reception", "⇩  Réception des spécimens"),

        actionButton("menu_repertoire", "⚗  Répertoire d'analyses"),

        if (u$perm_historique == 1)
          actionButton("menu_historique", "◷  Historique"),

        if (u$perm_utilisateurs == 1)
          actionButton("menu_utilisateurs", "♣  Utilisateurs"),

        if (u$perm_utilisateurs == 1)
          actionButton("menu_materiel", "▧  Matériel / Imprimantes"),

        div(class = "portal-menu-title", "ADMINISTRATION"),

        actionButton(
          "menu_mon_compte",
          if (u$perm_utilisateurs == 1) "⚙  Profils et permissions" else "⚙  Mon compte"
        ),

        if (peut_modifier_portail())
          actionButton("menu_parametres", "⚙  Paramètres"),

        actionButton("menu_aide", "?  Aide"),

        div(
          class = "portal-user",
          div(
            class = "portal-user-code",
            div(
              class = "portal-avatar",
              if (nzchar(ini)) ini else "US"
            ),
            span(code_utilisateur())
          ),
          div(
            class = "portal-role",
            switch(
              toupper(u$role),
              "SUPERUTILISATEUR" = "Super utilisateur",
              "UTILISATEUR" = "Utilisateur",
              "ETUDIANT" = "Étudiant",
              "ADMIN" = "Administrateur",
              u$role
            )
          ),
          div(class = "portal-online", "●  En ligne"),
          actionButton(
            "deconnexion",
            "↪  Déconnexion",
            class = "btn-danger"
          ),
          div(
            class = "portal-timeout",
            "Déconnexion automatique après 10 min d'inactivité"
          )
        )
      ),

      div(
        class = "portal-main",
        div(
          class = "portal-topbar",
          div(
            class = "portal-topbar-left",
            tags$img(class="portal-topbar-logo", src="ccnb_laboratoire_dieppe.png", alt="CCNB Laboratoire Dieppe"),
            div(
              class = "portal-topbar-title",
              "EDUSILLAB",
              tags$small("Système d’information de laboratoire pédagogique")
            )
          ),
          div(
            class = "portal-topbar-meta",
            span(class="topbar-user", "◉  ", switch(toupper(u$role), "ADMIN"="Administrateur", "SUPERUTILISATEUR"="Super utilisateur", "UTILISATEUR"="Utilisateur", "ETUDIANT"="Étudiant", u$role), "  ▾"),
            tags$button(type="button", class="topbar-logout", onclick="$('#deconnexion').click();", "↪  Déconnexion")
          )
        ),
        div(
          class = "portal-content",
          uiOutput("contenu_page")
        ),
        div(
          class = "portal-footer",
          div(
            class = "portal-footer-brand",
            tags$img(src=logo_portail, alt="Logo du portail"),
            span(paste(cfg$institution,"—",cfg$campus))
          ),
          div("EDUSILLAB v40 | CCNB – Campus de Dieppe"),
          div("Qualité   •   Traçabilité   •   Apprentissage   •   Excellence")
        )
      )
    )
  })

  # ----------------------------------------------------------
  # CONTENU DES PAGES
  # ----------------------------------------------------------

  output$contenu_page <- renderUI({
    req(utilisateur_connecte())
    cfg <- lire_config_portail()

    if (page_active() == "accueil") {
      return(
        div(
          class = "edu-reference-screen",
          div(
            class = "edu-reference-canvas",
            tags$img(src="edusillab_reference_exacte.png", class="edu-reference-image", alt="EDUSILLAB - accueil"),

            # Menu gauche
            tags$button(class="edu-hotspot", style="left:0%;top:9.2%;width:24.5%;height:6.1%;", onclick="$('#accueil').click();", `aria-label`="Accueil"),
            tags$button(class="edu-hotspot", style="left:0%;top:15.3%;width:24.5%;height:5.8%;", onclick="$('#menu_patients').click();", `aria-label`="Patients"),
            tags$button(class="edu-hotspot", style="left:0%;top:21.1%;width:24.5%;height:5.8%;", onclick="$('#menu_requisition').click();", `aria-label`="Nouvelle demande"),
            tags$button(class="edu-hotspot", style="left:0%;top:26.9%;width:24.5%;height:5.8%;", onclick="$('#menu_reception').click();", `aria-label`="Reception des specimens"),
            tags$button(class="edu-hotspot", style="left:0%;top:32.7%;width:24.5%;height:5.8%;", onclick="$('#menu_repertoire').click();", `aria-label`="Repertoire d'analyses"),
            tags$button(class="edu-hotspot", style="left:0%;top:38.5%;width:24.5%;height:5.8%;", onclick="$('#menu_historique').click();", `aria-label`="Historique"),
            tags$button(class="edu-hotspot", style="left:0%;top:44.3%;width:24.5%;height:5.8%;", onclick="$('#menu_utilisateurs').click();", `aria-label`="Utilisateurs"),
            tags$button(class="edu-hotspot", style="left:0%;top:50.1%;width:24.5%;height:5.8%;", onclick="$('#menu_materiel').click();", `aria-label`="Materiel et imprimantes"),
            tags$button(class="edu-hotspot", style="left:0%;top:59.1%;width:24.5%;height:5.2%;", onclick="$('#menu_mon_compte').click();", `aria-label`="Profils et permissions"),
            tags$button(class="edu-hotspot", style="left:0%;top:64.3%;width:24.5%;height:4.8%;", onclick="$('#menu_parametres').click();", `aria-label`="Parametres"),
            tags$button(class="edu-hotspot", style="left:0%;top:69.1%;width:24.5%;height:4.8%;", onclick="$('#menu_aide').click();", `aria-label`="Aide"),
            tags$button(class="edu-hotspot logout", style="left:1.5%;top:85.1%;width:21.5%;height:5.4%;", onclick="$('#deconnexion').click();", `aria-label`="Deconnexion"),

            # Barre superieure
            tags$button(class="edu-hotspot logout", style="left:82.8%;top:0%;width:17.2%;height:8.3%;", onclick="$('#deconnexion').click();", `aria-label`="Deconnexion"),

            # Quatre cartes principales
            tags$button(class="edu-hotspot", style="left:25.8%;top:52.1%;width:17.4%;height:21.4%;", onclick="$('#menu_requisition').click();", `aria-label`="Nouvelle demande"),
            tags$button(class="edu-hotspot", style="left:44.0%;top:52.1%;width:17.4%;height:21.4%;", onclick="$('#menu_reception').click();", `aria-label`="Reception des specimens"),
            tags$button(class="edu-hotspot", style="left:62.2%;top:52.1%;width:17.4%;height:21.4%;", onclick="$('#menu_repertoire').click();", `aria-label`="Repertoire d'analyses"),
            tags$button(class="edu-hotspot", style="left:80.4%;top:52.1%;width:17.0%;height:21.4%;", onclick="$('#menu_historique').click();", `aria-label`="Historique")
          )
        )
      )
    }

    if (page_active() == "patients") {
      return(
        tagList(
          div(
            class = "well",
            h2("Clients / Patients"),

            fluidRow(
              column(
                6,
                h4("Rechercher un client"),
                textInput(
                  "recherche_patient",
                  "Medicare ou numéro de dossier"
                ),
                actionButton(
                  "rechercher_patient",
                  "Rechercher",
                  class = "btn-primary"
                ),
                br(), br(),
                uiOutput("resultat_patient")
              ),

              column(
                6,
                h4("Import / mise à jour de la liste clients"),
                p(
                  "Formats structurés acceptés : Excel (.xlsx/.xls), CSV, TSV, TXT et JSON."
                ),
                fileInput(
                  "import_clients_fichier",
                  "Choisir un fichier clients",
                  accept = c(
                    ".xlsx", ".xls", ".csv", ".tsv", ".tab", ".txt", ".json"
                  )
                ),
                if (utilisateur_connecte()$perm_patients == 1)
                  actionButton(
                    "import_clients_lancer",
                    "Importer et mettre à jour",
                    class = "btn-success"
                  )
                else
                  div(
                    class = "warning-box",
                    "Votre profil peut consulter les clients, mais ne peut pas importer ou mettre à jour la liste."
                  ),
                br(), br(),
                uiOutput("message_import_clients")
              )
            ),

            hr(),

            h3("Télécharger la liste clients"),
            p(
              "Choisissez le format désiré. Les exports contiennent les données actuelles de la base EDUSILLAB."
            ),
            downloadButton(
              "telecharger_clients_xlsx",
              "Excel (.xlsx)",
              class = "btn-primary"
            ),
            downloadButton(
              "telecharger_clients_csv",
              "CSV (.csv)",
              class = "btn-default"
            ),
            downloadButton(
              "telecharger_clients_tsv",
              "TSV (.tsv)",
              class = "btn-default"
            ),
            downloadButton(
              "telecharger_clients_json",
              "JSON (.json)",
              class = "btn-default"
            ),

            br(), br(),
            h3("Liste actuelle"),
            DT::DTOutput("table_clients")
          )
        )
      )
    }

    if (page_active() == "repertoire") {
      return(tagList(
        wellPanel(
          h2("Répertoire d'analyses"),
          uiOutput("message_repertoire"),
          p(
            class = "text-muted",
            "Le répertoire est accessible en consultation à tout utilisateur connecté. Les fonctions de modification restent réservées aux profils autorisés."
          ),
          p(
            class = "text-muted",
            "Cliquez sur « Voir détails / notes » dans la colonne Notes pour afficher les instructions complètes de l'analyse."
          ),
          if (est_administrateur())
            div(
              class = "info-box",
              strong("Administrateur : "),
              "utilisez le bouton « Modifier » dans la colonne Action pour éditer directement une analyse, son tube, # of tubes, sa priorité, ses notes, ses sources et son statut."
            ),
          p("Le Px est associé automatiquement au département selon la table de correspondance Département → Px."),
          div(
            class = "analysis-toolbar",
            fluidRow(
              column(
                4,
                textInput(
                  "filtre_repertoire",
                  "Rechercher",
                  placeholder = "Nom, mnémonique, département, Px ou tube"
                )
              ),
              column(
                2,
                selectInput(
                  "filtre_actif",
                  "Statut",
                  choices = c(
                    "Analyses actives" = "1",
                    "Analyses retirées" = "0",
                    "Toutes" = "TOUS"
                  ),
                  selected = "1"
                )
              ),
              column(
                2,
                br(),
                actionButton("actualiser_repertoire", "Actualiser")
              )
            )
          ),
          DT::DTOutput("table_repertoire")
        ),

        if (peut_gerer_analyses())
          tagList(
            wellPanel(
              h3("Gestion du répertoire"),
              p(
                class="text-muted",
                "Fonctions réservées aux administrateurs et superutilisateurs."
              ),
              actionButton("ouvrir_ajout_analyse", "+ Ajouter une analyse", class = "btn-success"),
              actionButton("ouvrir_import_analyse", "Téléverser un document", class = "btn-primary"),
              actionButton("ouvrir_gestion_px", "Départements / Px", class = "btn-default"),
              downloadButton(
                "telecharger_sauvegarde_db",
                "Sauvegarder la base",
                class = "btn-default"
              ),
              hr(),
              h4("Import direct du répertoire Excel"),
              p(
                class="text-muted",
                "Utilisez cette zone si le bouton de téléversement ne s'ouvre pas. Le fichier XLSX/XLS est importé directement dans le répertoire."
              ),
              fileInput(
                "fichier_import_analyse_direct",
                "Choisir le fichier du répertoire",
                accept=c(".xlsx",".xls")
              ),
              actionButton(
                "lancer_import_analyse_direct",
                "Importer / mettre à jour le répertoire",
                class="btn-primary"
              ),
              br(), br(),
              uiOutput("message_import_analyse_direct")
            ),
            wellPanel(
              h3("Documents importés à réviser"),
              DT::DTOutput("table_imports_documents")
            )
          )
      ))
    }

    if (
      page_active() == "requisition" &&
      utilisateur_connecte()$perm_requisition == 1
    ) {
      return(
        wellPanel(
          fluidRow(
            column(7, h2("Enter / Edit Requisition")),
            column(
              5,
              br(),
              if (utilisateur_connecte()$perm_ajout_tests == 1)
                actionButton("req_ouvrir_repertoire", "⚡ Répertoire d'analyses", class = "btn-info")
              else
                span(class = "text-muted", "Sélection de tests bloquée pour ce profil")
            )
          ),

          div(
            class = "warning-box",
            p(
              strong("Req # : "),
              span(class = "req-number", "généré à l'enregistrement")
            ),
            p(
              "Tous les tests saisis dans cet Order sont enregistrés sous un seul Req #. ",
              "Une nouvelle réquisition créée plus tard reçoit un nouveau Req #, même pour le même patient."
            )
          ),

          br(),
          div(
            class = "user-box",
            strong("Réquisition entrée par : "),
            code_utilisateur()
          ),

          h3("Patient"),
          fluidRow(
            column(3, textInput("req_numero_dossier", "Acct # / Numéro de dossier")),
            column(3, textInput("req_medicare", "Medicare")),
            column(3, br(), actionButton(
              "req_rechercher_patient", "Rechercher patient", class = "btn-primary"
            ))
          ),
          fluidRow(
            column(3, textInput("req_nom", "Nom")),
            column(3, textInput("req_prenom", "Prénom")),
            column(3, dateInput(
              "req_date_naissance", "DOB / Date de naissance",
              value = NA,
              min = as.Date("1900-01-01"),
              max = Sys.Date() + 3650,
              format = "dd-mm-yyyy"
            )),
            column(3, selectInput(
              "req_sexe", "Age/Sx — Sexe",
              choices = c(
                "F — Femme" = "F",
                "M — Homme" = "M",
                "BB — Nouveau-né" = "BB",
                "U — Inconnu" = "U"
              )
            ))
          ),
          fluidRow(
            column(4, textInput("req_adresse", "Adresse")),
            column(2, textInput("req_ville", "Ville")),
            column(2, textInput("req_province", "Province")),
            column(2, textInput("req_code_postal", "Code postal")),
            column(2, textInput("req_pays", "Pays", value = "Canada"))
          ),
          uiOutput("req_patient_message"),

          hr(),
          h3("Informations de la demande"),
          fluidRow(
            column(3, textInput("req_prescripteur", "Reg Dr / Prescripteur")),
            column(3, dateInput(
              "req_coll_date", "Coll Date",
              value = Sys.Date(), format = "dd-mm-yyyy"
            )),
            column(3, textInput("req_coll_time", "Coll Time", placeholder = "HH:MM")),
            column(3, selectInput(
              "req_priorite", "Priority",
              choices = c(
                "Routine" = "ROUTINE",
                "Urgence" = "URGENCE",
                "STAT" = "STAT"
              )
            ))
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                "req_location",
                "Location du patient",
                choices = character(0),
                options = list(create = TRUE, placeholder = "Choisir ou écrire une location")
              )
            ),
            column(6, p(class = "text-muted", "Le dictionnaire de locations pourra être ajouté et complété plus tard."))
          ),
          textAreaInput("req_commentaire", "Commentaire", rows = 2),

          hr(),
          div(
            class = "order-zone",
            h3("Order"),
            if (utilisateur_connecte()$perm_ajout_tests == 1)
              tagList(
                p("Entrer le mnémonique puis appuyer sur Entrée."),
                fluidRow(
                  column(
                    4,
                    textInput(
                      "req_order_mnemo",
                      "Order / Mnémonique",
                      placeholder = "Exemple : FSC"
                    )
                  ),
                  column(
                    8,
                    br(),
                    p("Le nom de l'analyse, le département et le Px s'affichent automatiquement.")
                  )
                )
              )
            else
              div(
                class = "warning-box",
                strong("Ajout de tests bloqué : "),
                "ce profil n'est pas autorisé à sélectionner ou ajouter des tests dans une réquisition."
              ),
            uiOutput("message_order"),
            tableOutput("table_order"),
            if (utilisateur_connecte()$perm_ajout_tests == 1)
              tagList(
                actionButton(
                  "supprimer_dernier_order",
                  "Retirer le dernier test",
                  class = "btn-warning"
                ),
                actionButton("vider_order", "Vider Order", class = "btn-danger")
              )
          ),

          hr(),
          actionButton(
            "req_enregistrer",
            "Enregistrer la réquisition et générer les étiquettes",
            class = "btn-success"
          ),
          br(), br(),
          uiOutput("req_message"),
          uiOutput("zone_apercu_etiquettes")
        )
      )
    }

    if (
      page_active() == "historique" &&
      utilisateur_connecte()$perm_historique == 1
    ) {
      return(
        wellPanel(
          h2("Historique et suivi des réquisitions"),
          p(
            class = "text-muted",
            "Utilisez « Réimprimer » pour reproduire une étiquette existante sans créer un nouveau BC#."
          ),
          textInput(
            "filtre_utilisateur",
            "Rechercher par matricule, nom, prénom ou courriel"
          ),
          actionButton("actualiser_historique", "Actualiser"),
          br(), br(),
          DT::DTOutput("table_historique")
        )
      )
    }

    if (
      page_active() == "reception" &&
      utilisateur_connecte()$perm_reception == 1
    ) {
      return(
        tagList(
          wellPanel(
            h2("Réception des spécimens"),
            div(
              class = "user-box",
              strong("Réception effectuée par : "),
              code_utilisateur()
            ),
            br(),
            fluidRow(
              column(
                6,
                textInput(
                  "reception_code",
                  "BC#, numéro de spécimen ou dossier",
                  placeholder = "Ex. 983421, 2808 H5, 2808:H00005R ou dossier patient"
                ),
                p(
                  class = "text-muted",
                  "Avec un scanner configuré comme clavier, scannez directement. Sans scanner, saisissez le BC#, le numéro de spécimen ou le dossier."
                )
              ),
              column(
                3,
                br(),
                actionButton(
                  "rechercher_specimen_reception",
                  "Rechercher",
                  class = "btn-primary"
                )
              )
            ),
            uiOutput("resultat_reception"),
            uiOutput("reception_pdf_corrige"),
            hr(),
            h3("Réception multiple"),
            p(
              class="text-muted",
              "Saisissez ou scannez plusieurs BC# / numéros de spécimen, un par ligne. Les formats 2808 H10 et ;H10 sont acceptés."
            ),
            fluidRow(
              column(
                7,
                textAreaInput(
                  "reception_multiple_refs",
                  "Références des tubes",
                  rows=5,
                  placeholder="983421\n2808 H10\n;H11"
                )
              ),
              column(
                5,
                textInput(
                  "reception_multiple_heure",
                  "Heure de collecte commune",
                  placeholder="Ex. 08:35"
                ),
                textInput(
                  "reception_multiple_initiales",
                  "Initiales du préleveur",
                  placeholder="Ex. CK"
                )
              )
            ),
            fluidRow(
              column(4,actionButton("preparer_reception_multiple","Préparer les tubes",class="btn-primary")),
              column(4,actionButton("confirmer_reception_multiple","Recevoir tous",class="btn-success")),
              column(4,actionButton("cancel_reception_multiple","Cancel",class="btn-default"))
            ),
            br(),
            uiOutput("message_reception_multiple"),
            DT::DTOutput("table_reception_multiple")
          ),
          wellPanel(
            h3("Annulation de spécimens"),
            p(
              class="text-muted",
              "Vous pouvez annuler un spécimen reçu, non reçu ou indiquer qu'un prélèvement n'a pas été effectué. La réception antérieure reste conservée dans la traçabilité."
            ),
            fluidRow(
              column(
                4,
                textInput(
                  "annulation_reference",
                  "BC#, spécimen, dossier patient ou Req #",
                  placeholder="Ex. 983421, 2808 H10, ;H10, dossier ou Req #"
                )
              ),
              column(
                4,
                selectInput(
                  "annulation_portee",
                  "Portée",
                  choices=c(
                    "Spécimen correspondant à la référence"="SPECIMEN",
                    "Toute la réquisition / dossier"="DOSSIER",
                    "Un département seulement"="DEPARTEMENT"
                  )
                )
              ),
              column(
                4,
                selectInput(
                  "annulation_departement",
                  "Département détecté",
                  choices=setNames("","Saisir une référence d'abord"),
                  selected=""
                )
              )
            ),
            uiOutput("annulation_reference_info"),
            fluidRow(
              column(
                4,
                selectInput(
                  "annulation_situation",
                  "Situation du spécimen",
                  choices=c(
                    "Automatique selon le statut actuel"="AUTO",
                    "Spécimen reçu"="RECU",
                    "Spécimen non reçu"="NON_RECU",
                    "Prélèvement non effectué / pas prélevé"="PAS_PRELEVE"
                  )
                )
              ),
              column(4,uiOutput("choix_contexte_annulation")),
              column(
                4,
                textInput(
                  "annulation_commentaire",
                  "Commentaire / précision",
                  placeholder="Précision facultative"
                )
              )
            ),
            fluidRow(
              column(
                4,
                actionButton(
                  "previsualiser_annulation",
                  "Vérifier les spécimens",
                  class="btn-warning"
                )
              ),
              column(
                4,
                actionButton(
                  "annuler_specimens",
                  "Annuler le(s) spécimen(s)",
                  class="btn-danger"
                )
              )
            ),
            br(),
            uiOutput("apercu_annulation"),
            hr(),
            h4("Gestion des contextes d'annulation"),
            p(
              class="text-muted",
              "L'ajout et la suppression des contextes sont réservés aux administrateurs et superutilisateurs."
            ),
            if (est_administrateur() || est_superutilisateur()) {
              tagList(
                fluidRow(
                  column(
                    8,
                    textInput(
                      "nouveau_contexte_annulation",
                      NULL,
                      placeholder="Nouveau contexte"
                    )
                  ),
                  column(
                    4,
                    actionButton(
                      "ajouter_contexte_annulation",
                      "Ajouter",
                      class="btn-default"
                    )
                  )
                ),
                br(),
                fluidRow(
                  column(
                    8,
                    uiOutput("choix_contexte_suppression")
                  ),
                  column(
                    4,
                    br(),
                    actionButton(
                      "supprimer_contexte_annulation",
                      "Supprimer le contexte",
                      class="btn-danger"
                    )
                  )
                )
              )
            },
            uiOutput("message_annulation")
          ),
          wellPanel(
            h3("Réactiver / Uncancel un spécimen annulé"),
            p(
              class="text-muted",
              "Réactive un spécimen annulé sans effacer l'historique de l'annulation. Un commentaire peut être ajouté à la traçabilité."
            ),
            fluidRow(
              column(
                4,
                textInput(
                  "uncancel_reference",
                  "BC#, spécimen, dossier patient ou Req #",
                  placeholder="Ex. 983421, 2808 H10, ;H10, dossier ou Req #"
                )
              ),
              column(
                4,
                selectInput(
                  "uncancel_portee",
                  "Portée",
                  choices=c(
                    "Spécimen correspondant à la référence"="SPECIMEN",
                    "Toute la réquisition / dossier"="DOSSIER",
                    "Un département seulement"="DEPARTEMENT"
                  )
                )
              ),
              column(
                4,
                selectInput(
                  "uncancel_departement",
                  "Département détecté",
                  choices=setNames("","Saisir une référence d'abord"),
                  selected=""
                )
              )
            ),
            uiOutput("uncancel_reference_info"),
            textInput(
              "uncancel_commentaire",
              "Commentaire de réactivation",
              placeholder="Ex. Annulation faite par erreur"
            ),
            actionButton(
              "previsualiser_uncancel",
              "Vérifier",
              class="btn-warning"
            ),
            actionButton(
              "uncancel_specimens",
              "Réactiver",
              class="btn-success"
            ),
            br(),br(),
            uiOutput("apercu_uncancel"),
            uiOutput("message_uncancel")
          ),
          wellPanel(
            h3("Traçabilité d'un patient"),
            fluidRow(
              column(5,textInput("audit_patient_ref","Dossier ou Medicare")),
              column(3,br(),actionButton("audit_patient_rechercher","Afficher la trace",class="btn-info")),
              column(4,br(),
                downloadButton("audit_patient_excel","Exporter Excel"),
                downloadButton("audit_patient_pdf","Exporter PDF")
              )
            ),
            DT::DTOutput("table_audit_patient")
          )
        )
      )
    }

    if (
      page_active() == "materiel" &&
      utilisateur_connecte()$perm_utilisateurs == 1
    ) {
      return(
        wellPanel(
          h2("🖨 Matériel / imprimantes"),
          p("Enregistrez ici l'imprimante, le modèle, le numéro de série et la taille des étiquettes."),
          fluidRow(
            column(3, textInput("mat_nom", "Nom de l'imprimante")),
            column(3, textInput("mat_modele", "Fabricant / modèle")),
            column(3, textInput("mat_serie", "Numéro de série")),
            column(
              3,
              selectInput(
                "mat_connexion",
                "Connexion",
                choices = c(
                  "USB / connectée à l'ordinateur" = "USB",
                  "Bluetooth" = "BLUETOOTH",
                  "Réseau / Wi-Fi" = "RESEAU",
                  "PDF seulement" = "PDF"
                )
              )
            )
          ),
          fluidRow(
            column(
              4,
              selectInput(
                "mat_type",
                "Type",
                choices = c(
                  "IMPRIMANTE",
                  "IMPRIMANTE ETIQUETTES",
                  "SCANNER CODE-BARRES",
                  "PDF"
                )
              )
            ),
            column(
              8,
              div(
                class = "info-box",
                strong("Connexion Bluetooth : "),
                "l'imprimante doit d'abord être jumelée dans macOS ou Windows. ",
                "EduLab l'enregistre ensuite comme imprimante disponible et le PDF peut être envoyé à cette imprimante depuis la boîte d'impression du système."
              )
            )
          ),
          fluidRow(
            column(3,numericInput("mat_largeur","Largeur étiquette (mm)",100,min=20,max=300)),
            column(3,numericInput("mat_hauteur","Hauteur étiquette (mm)",50,min=15,max=300)),
            column(3,selectInput("mat_ajustement","Ajustement étiquette",
              choices=c("Automatique"="AUTO","Manuel"="MANUEL"))),
            column(3,checkboxInput("mat_defaut","Matériel par défaut",TRUE))
          ),
          fluidRow(
            column(3,numericInput("mat_offset_x","Décalage horizontal (mm)",0,min=-30,max=30,step=.5)),
            column(3,numericInput("mat_offset_y","Décalage vertical (mm)",0,min=-30,max=30,step=.5)),
            column(3,numericInput("mat_echelle","Échelle (%)",100,min=60,max=140,step=1)),
            column(3,br(),actionButton("mat_enregistrer","Enregistrer le matériel",class="btn-success"))
          ),
          p(class="text-muted",
            "Automatique : taille de l'imprimante, sans décalage. Manuel : ajustez les offsets et l'échelle pour aligner l'étiquette."),
          uiOutput("mat_message"),
          hr(),
          DT::DTOutput("table_materiel")
        )
      )
    }

    if (page_active() == "editeur_portail" && peut_modifier_portail()) {
      cfg_edit <- lire_config_portail()

      return(tagList(
        wellPanel(
          h2("Éditeur du portail"),
          div(
            class="info-box",
            strong("Modification sans RStudio : "),
            "les utilisateurs autorisés peuvent modifier ici l'identité visuelle et les textes du portail. ",
            "Les modifications sont enregistrées dans la base et s'appliquent à tous les utilisateurs."
          ),
          fluidRow(
            column(4,textInput("cfg_portail_nom","Nom du portail",value=cfg_edit$portail_nom)),
            column(4,textInput("cfg_institution","Institution",value=cfg_edit$institution)),
            column(4,textInput("cfg_campus","Campus / site",value=cfg_edit$campus))
          ),
          textInput("cfg_sous_titre","Sous-titre / description",value=cfg_edit$sous_titre),
          fluidRow(
            column(6,textInput(
              "cfg_dashboard_titre",
              "Titre du tableau de bord",
              value=cfg_edit$tableau_bord_titre
            )),
            column(6,textInput(
              "cfg_dashboard_sous_titre",
              "Sous-titre du tableau de bord",
              value=cfg_edit$tableau_bord_sous_titre
            ))
          ),
          textInput("cfg_footer","Texte du pied de page",value=cfg_edit$footer_texte),
          hr(),
          h4("Couleurs"),
          p(class="text-muted","Entrer les couleurs au format hexadécimal, par exemple #00757c."),
          fluidRow(
            column(4,textInput(
              "cfg_couleur_principale",
              "Couleur principale",
              value=cfg_edit$couleur_principale
            )),
            column(4,textInput(
              "cfg_couleur_secondaire",
              "Haut de la barre latérale",
              value=cfg_edit$couleur_secondaire
            )),
            column(4,textInput(
              "cfg_couleur_sidebar_bas",
              "Bas de la barre latérale",
              value=cfg_edit$couleur_sidebar_bas
            ))
          ),
          hr(),
          h4("Logo"),
          fileInput(
            "cfg_logo",
            "Téléverser un nouveau logo",
            accept=c("image/png","image/jpeg",".png",".jpg",".jpeg")
          ),
          checkboxInput(
            "cfg_retirer_logo",
            "Revenir au logo CCNB par défaut",
            FALSE
          ),
          hr(),
          actionButton(
            "enregistrer_configuration_portail",
            "Enregistrer les modifications",
            class="btn-success"
          ),
          actionButton(
            "restaurer_configuration_portail",
            "Restaurer les textes et couleurs par défaut",
            class="btn-warning"
          ),
          br(),br(),
          uiOutput("message_configuration_portail")
        ),
        wellPanel(
          h3("Ce qui peut être modifié sans RStudio"),
          tags$ul(
            tags$li("Nom du portail, institution et campus."),
            tags$li("Titre et sous-titres."),
            tags$li("Texte du pied de page."),
            tags$li("Couleurs principales."),
            tags$li("Logo."),
            tags$li("Les autres données fonctionnelles déjà prévues : utilisateurs, analyses, départements, contextes, imprimantes, etc.")
          ),
          div(
            class="warning-box",
            strong("Limite : "),
            "l'ajout d'une nouvelle fonction informatique ou la modification du code métier nécessite encore une nouvelle version de l'application. ",
            "L'éditeur intégré évite RStudio pour les modifications courantes du portail."
          )
        )
      ))
    }

    if (page_active() == "aide") {
      return(div(class="portal-page",
        h2("Aide"),
        p("EDUSILLAB — système d'information de laboratoire pédagogique."),
        div(class="portal-card",
          h4("Accès rapide"),
          p("Utilisez le menu bleu pour accéder aux patients, créer une nouvelle demande, recevoir les spécimens, consulter le répertoire d'analyses et l'historique."),
          p("Pour un problème technique, communiquez avec l'administrateur EDUSILLAB de votre établissement.")
        )
      ))
    }

    if (page_active() == "mon_compte") {
      return(tagList(
        wellPanel(
          h2("Mon compte"),
          p(strong("Initiales : "), utilisateur_connecte()$initiales),
          p(strong("Matricule : "), utilisateur_connecte()$matricule),
          p(strong("Nom : "),
            paste(utilisateur_connecte()$prenom, utilisateur_connecte()$nom)),
          p(strong("Courriel : "), utilisateur_connecte()$email),
          p(strong("Niveau : "), utilisateur_connecte()$role)
        ),
        wellPanel(
          h3("Modifier mon mot de passe"),
          passwordInput("mdp_actuel_compte", "Mot de passe actuel"),
          passwordInput("nouveau_mdp_compte", "Nouveau mot de passe"),
          passwordInput("confirmation_mdp_compte", "Confirmer le nouveau mot de passe"),
          actionButton("modifier_mon_mdp", "Modifier mon mot de passe", class = "btn-success"),
          br(), br(),
          uiOutput("message_mon_mdp")
        )
      ))
    }

    if (
      page_active() == "utilisateurs" &&
      utilisateur_connecte()$perm_utilisateurs == 1
    ) {
      return(tagList(
        wellPanel(
          h2("Administration des utilisateurs"),
          h3("Créer un utilisateur"),
          fluidRow(
            column(3, textInput("user_matricule", "Matricule")),
            column(3, textInput("user_initiales", "Initiales", placeholder = "Ex. CK")),
            column(3, textInput("user_nom", "Nom")),
            column(3, textInput("user_prenom", "Prénom"))
          ),
          fluidRow(
            column(4, textInput("user_email", "Adresse courriel")),
            column(4, textInput("user_identifiant", "Identifiant de connexion")),
            column(4, passwordInput("user_mot_de_passe", "Mot de passe temporaire"))
          ),
          selectInput(
            "user_role", "Niveau",
            choices = c(
              "Étudiant" = "ETUDIANT",
              "Utilisateur" = "UTILISATEUR",
              "Super utilisateur" = "SUPERUTILISATEUR",
              "Administrateur" = "ADMIN"
            )
          ),
          h4("Permissions"),
          checkboxInput("p_recherche", "Recherche patient", TRUE),
          checkboxInput("p_patients", "Ajouter / modifier patients", FALSE),
          checkboxInput("p_requisition", "Créer des réquisitions", TRUE),
          checkboxInput(
            "p_ajout_tests",
            "Autoriser la sélection / l'ajout de tests dans la réquisition",
            TRUE
          ),
          checkboxInput("p_reception", "Réception des spécimens", FALSE),
          checkboxInput("p_historique", "Historique / suivi", FALSE),
          checkboxInput("p_utilisateurs", "Administration des utilisateurs", FALSE),
          checkboxInput("p_analyses", "Gestion du répertoire d'analyses", FALSE),
          checkboxInput("p_portail", "Modifier la présentation du portail", FALSE),
          checkboxInput("user_actif", "Compte actif", TRUE),
          actionButton(
            "enregistrer_utilisateur",
            "Créer l'utilisateur",
            class = "btn-success"
          ),
          br(), br(),
          uiOutput("message_utilisateur")
        ),

        wellPanel(
          h3("Utilisateurs existants"),
          tableOutput("table_utilisateurs")
        ),

        wellPanel(
          h3("Modifier un utilisateur"),
          uiOutput("choix_utilisateur_modifier"),
          actionButton("charger_utilisateur_modifier", "Charger l'utilisateur"),
          br(), br(),
          uiOutput("formulaire_modifier_utilisateur")
        ),

        wellPanel(
          h3("Réinitialiser un mot de passe"),
          div(
            class = "warning-box",
            p("Après 3 erreurs de mot de passe, le compte est verrouillé."),
            p("La recherche peut être faite par matricule ou par courriel."),
            p("Le mot de passe attribué par l'administrateur est temporaire.")
          ),
          br(),
          textInput("reset_identification", "Matricule ou adresse courriel"),
          passwordInput("reset_password", "Nouveau mot de passe temporaire"),
          passwordInput("reset_password_confirmation", "Confirmer le mot de passe"),
          actionButton(
            "reset_password_button",
            "Réinitialiser et déverrouiller",
            class = "btn-warning"
          ),
          br(), br(),
          uiOutput("message_reset")
        )
      ))
    }
  })

  # ----------------------------------------------------------
  # CONNEXION
  # ----------------------------------------------------------

  observeEvent(input$connexion, {
    identifiant <- norm_txt(input$identifiant)
    mdp <- input$mot_de_passe

    if (!nzchar(identifiant) || !nzchar(mdp)) {
      output$message_connexion <- renderUI(
        div(style = "color:red;", "Identifiant et mot de passe obligatoires.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit({
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    }, add = TRUE)

    u <- DBI::dbGetQuery(
      con,
      "SELECT * FROM utilisateurs WHERE identifiant = ? AND actif = 1",
      params = list(identifiant)
    )

    if (nrow(u) != 1) {
      output$message_connexion <- renderUI(
        div(style = "color:red;", "Identifiant ou mot de passe incorrect.")
      )
      return()
    }

    if (u$verrouille[1] == 1) {
      output$message_connexion <- renderUI(
        div(
          class = "danger-box",
          strong("Compte verrouillé."),
          br(),
          "Contactez un administrateur pour réinitialiser le mot de passe."
        )
      )
      return()
    }

    if (!identical(as.character(u$mot_de_passe[1]), as.character(mdp))) {
      tentatives <- ifelse(is.na(u$tentatives_echouees[1]), 0, u$tentatives_echouees[1]) + 1
      verrou <- as.integer(tentatives >= 3)

      DBI::dbExecute(
        con,
        "
        UPDATE utilisateurs
        SET tentatives_echouees = ?, verrouille = ?
        WHERE id = ?
        ",
        params = list(tentatives, verrou, u$id[1])
      )

      if (verrou == 1) {
        output$message_connexion <- renderUI(
          div(class = "danger-box", "3 tentatives incorrectes. Compte verrouillé.")
        )
      } else {
        output$message_connexion <- renderUI(
          div(
            style = "color:red;",
            paste0(
              "Mot de passe incorrect. ",
              3 - tentatives,
              " tentative(s) restante(s)."
            )
          )
        )
      }
      return()
    }

    DBI::dbExecute(
      con,
      "UPDATE utilisateurs SET tentatives_echouees = 0 WHERE id = ?",
      params = list(u$id[1])
    )

    utilisateur_connecte(list(
      id = u$id[1],
      identifiant = u$identifiant[1],
      matricule = ifelse(is.na(u$matricule[1]), "", u$matricule[1]),
      initiales = ifelse(is.na(u$initiales[1]), "", u$initiales[1]),
      nom = ifelse(is.na(u$nom[1]), "", u$nom[1]),
      prenom = ifelse(is.na(u$prenom[1]), "", u$prenom[1]),
      email = ifelse(is.na(u$email[1]), "", u$email[1]),
      role = u$role[1],
      perm_recherche_patient = permission(u$perm_recherche_patient[1]),
      perm_patients = permission(u$perm_patients[1]),
      perm_requisition = permission(u$perm_requisition[1]),
      perm_ajout_tests = permission(u$perm_ajout_tests[1]),
      perm_reception = permission(u$perm_reception[1]),
      perm_historique = permission(u$perm_historique[1]),
      perm_utilisateurs = permission(u$perm_utilisateurs[1]),
      perm_analyses = if (
        toupper(trimws(as.character(u$role[1]))) %in%
          c("ADMIN","ADMINISTRATEUR","SUPERUTILISATEUR","SUPERUSER")
      ) 1L else 0L
    ))

    # La connexion d'authentification n'est plus nécessaire.
    # On la ferme avant l'écriture du journal afin d'éviter un verrou SQLite.
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
      con <- NULL
    }

    session$sendCustomMessage("resetInactivityTimer", list())

    # L'audit ne doit jamais empêcher l'ouverture du portail.
    try(
      journaliser(
        "CONNEXION",
        paste0("Connexion au portail : ", code_utilisateur())
      ),
      silent = TRUE
    )

    if (u$changer_mot_de_passe[1] == 1) {
      page_active("changer_mdp")
    } else {
      page_active("accueil")
    }
  })

  output$dashboard_table_attente <- DT::renderDT({
    req(utilisateur_connecte())
    d <- dashboard_snapshot()

    if (is.null(d$liste_attente) || nrow(d$liste_attente)==0) {
      return(
        DT::datatable(
          data.frame(Message="Aucun spécimen en attente."),
          rownames=FALSE,
          options=list(dom="t")
        )
      )
    }

    DT::datatable(
      d$liste_attente,
      rownames=FALSE,
      selection="none",
      options=list(
        dom="t",
        paging=FALSE,
        searching=FALSE,
        ordering=FALSE,
        autoWidth=TRUE
      )
    )
  })

  output$dashboard_activite <- renderUI({
    req(utilisateur_connecte())
    a <- dashboard_snapshot()$activite

    if (is.null(a) || nrow(a)==0) {
      return(div(class="text-muted","Aucune activité récente enregistrée."))
    }

    tagList(
      lapply(seq_len(nrow(a)), function(i) {
        div(
          class="dash-alert",
          span(style="color:#00757c;","●"),
          div(
            div(
              class="a-title",
              paste0(
                ifelse(is.na(a$Action[i]),"",a$Action[i]),
                ifelse(
                  is.na(a$Utilisateur[i]) || !nzchar(a$Utilisateur[i]),
                  "",
                  paste0(" — ",a$Utilisateur[i])
                )
              )
            ),
            div(
              class="a-sub",
              paste0(
                ifelse(is.na(a$Details[i]),"",a$Details[i]),
                ifelse(is.na(a$Heure[i]) || !nzchar(a$Heure[i]),"",paste0(" · ",a$Heure[i]))
              )
            )
          )
        )
      })
    )
  })

  # ----------------------------------------------------------
  # NAVIGATION
  # ----------------------------------------------------------

  observeEvent(input$accueil, page_active("accueil"))
  observeEvent(input$menu_aide, page_active("aide"))
  observeEvent(input$menu_parametres, {
    if (peut_modifier_portail()) page_active("editeur_portail")
  })

  observeEvent(input$dashboard_reception, {
    req(utilisateur_connecte()$perm_reception==1)
    page_active("reception")
  })

  observeEvent(input$dashboard_ouvrir_reception, {
    req(utilisateur_connecte()$perm_reception==1)
    page_active("reception")
  })

  observeEvent(input$dashboard_demande, {
    req(utilisateur_connecte()$perm_requisition==1)
    patient_requisition(NULL)
    order_actuel(order_vide())
    page_active("requisition")
  })

  observeEvent(input$dashboard_repertoire, {
    req(utilisateur_connecte())
    page_active("repertoire")
    refresh_analyses(refresh_analyses()+1L)
  })

  observeEvent(input$dashboard_historique, {
    req(utilisateur_connecte()$perm_historique==1)
    charger_historique()
    page_active("historique")
  })

  observeEvent(input$menu_patients, {
    req(utilisateur_connecte())
    req(
      utilisateur_connecte()$perm_recherche_patient == 1 ||
      utilisateur_connecte()$perm_patients == 1
    )
    page_active("patients")
  })

  observeEvent(input$menu_repertoire, {
    req(utilisateur_connecte())
    page_active("repertoire")
    refresh_analyses(refresh_analyses()+1L)
    try(
      journaliser(
        "CONSULTATION_REPERTOIRE_ANALYSES",
        "Ouverture du répertoire d'analyses."
      ),
      silent=TRUE
    )
  })

  observeEvent(input$menu_requisition, {
    req(utilisateur_connecte()$perm_requisition == 1)
    patient_requisition(NULL)
    order_actuel(order_vide())
    page_active("requisition")
  })

  observeEvent(input$menu_historique, {
    req(utilisateur_connecte()$perm_historique == 1)
    charger_historique()
    page_active("historique")
  })

  observeEvent(input$menu_reception, {
    req(utilisateur_connecte()$perm_reception == 1)
    page_active("reception")
  })

  observeEvent(input$menu_import_analyses, {
    req(est_administrateur() || est_superutilisateur())
    page_active("repertoire")
    ouvrir_import_depuis_menu(ouvrir_import_depuis_menu()+1L)
  })

  observeEvent(input$menu_editeur_portail, {
    req(peut_modifier_portail())
    page_active("editeur_portail")
  })

  observeEvent(input$menu_mon_compte, page_active("mon_compte"))

  observeEvent(input$menu_utilisateurs, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    page_active("utilisateurs")
  })

  observeEvent(input$menu_materiel, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    page_active("materiel")
  })

  observeEvent(input$deconnexion, {
    utilisateur_connecte(NULL)
    patient_requisition(NULL)
    order_actuel(order_vide())
    page_active("accueil")
  })

  observeEvent(input$inactive_logout, {
    if (!is.null(utilisateur_connecte())) {
      utilisateur_connecte(NULL)
      patient_requisition(NULL)
      order_actuel(order_vide())
      page_active("accueil")
      showNotification(
        "Session fermée automatiquement après 10 minutes d'inactivité.",
        type = "warning",
        duration = 8
      )
    }
  })

  # ----------------------------------------------------------
  # ACCÈS RAPIDE AU RÉPERTOIRE DEPUIS LA RÉQUISITION
  # ----------------------------------------------------------

  observeEvent(input$req_ouvrir_repertoire, {
    req(utilisateur_connecte()$perm_requisition == 1)
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    showModal(modalDialog(
      title = "Répertoire d'analyses - accès rapide",
      p("La réquisition reste ouverte. Recherchez puis cliquez sur Ajouter."),
      DT::DTOutput("req_repertoire_rapide"),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Fermer et retourner à la réquisition")
    ))
  })

  output$req_repertoire_rapide <- DT::renderDT({
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    d <- DBI::dbGetQuery(con, "
      SELECT a.id, a.mnemonique AS Mnemonic, a.nom AS Analyse,
             COALESCE(a.departement,'') AS Departement,
             COALESCE(dp.px,'') AS PX,
             COALESCE(a.tube_prelevement,'') AS Tube,
             CASE
               WHEN a.nb_tubes IS NULL THEN ''
               WHEN ABS(a.nb_tubes - ROUND(a.nb_tubes)) < 0.000001
                 THEN CAST(CAST(ROUND(a.nb_tubes) AS INTEGER) AS TEXT)
               ELSE CAST(a.nb_tubes AS TEXT)
             END AS `# of tubes`
      FROM analyses a
      LEFT JOIN departements_px dp ON UPPER(dp.departement)=UPPER(a.departement)
      WHERE a.actif=1
      ORDER BY a.nom
    ")
    d$Ajouter <- sprintf(
      "<button class='btn btn-success btn-xs' onclick=\"Shiny.setInputValue('req_ajout_repertoire_id', %s, {priority:'event'})\">Ajouter</button>",
      d$id
    )
    d$id <- NULL
    DT::datatable(d, escape=FALSE, rownames=FALSE, filter="top",
              options=list(pageLength=15, scrollX=TRUE))
  })

  observeEvent(input$req_ajout_repertoire_id, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    id <- as.integer(input$req_ajout_repertoire_id)
    con <- ouvrir_db()
    a <- DBI::dbGetQuery(con, "
      SELECT a.id, a.mnemonique, a.nom,
             COALESCE(a.departement,'') AS departement,
             COALESCE(dp.px,'') AS px
      FROM analyses a
      LEFT JOIN departements_px dp ON UPPER(dp.departement)=UPPER(a.departement)
      WHERE a.id=? AND a.actif=1
    ", params=list(id))
    DBI::dbDisconnect(con)
    if (nrow(a)==1) {
      ajouter_analyse_order(a)
    }
  })

  observe({
    req(utilisateur_connecte())
    con <- ouvrir_db()
    loc <- DBI::dbGetQuery(con, "SELECT code, nom FROM locations WHERE actif=1 ORDER BY nom")
    DBI::dbDisconnect(con)
    choix <- if (nrow(loc)==0) character(0) else setNames(
      ifelse(nzchar(norm_txt(loc$code)), loc$code, loc$nom),
      ifelse(nzchar(norm_txt(loc$code)), paste0(loc$code," - ",loc$nom), loc$nom)
    )
    updateSelectizeInput(session, "req_location", choices=choix, server=TRUE)
  })

  normaliser_reference_specimen <- function(x, date_defaut=Sys.Date()) {
    x <- toupper(norm_txt(x))
    if (!nzchar(x)) return("")

    # ;H10 ou :H10 = date du jour + H10
    if (grepl("^[;:]\\s*[A-Z]+\\s*0*[0-9]+$", x)) {
      x <- paste0(format(as.Date(date_defaut), "%d%m"), sub("^[;:]\\s*", "", x))
    }

    # 2808 H10 / 2808:H10 / 2808-H10
    x <- gsub("[^A-Z0-9]", "", x)

    m <- regexec("^([0-9]{4})([A-Z]+)0*([0-9]+)([RUS])?$", x)
    z <- regmatches(x, m)[[1]]

    if (length(z) >= 4) {
      return(paste0(z[2], z[3], as.integer(z[4])))
    }

    x
  }

  trouver_specimen_par_reference <- function(con, code) {
    code <- norm_txt(code)
    if (!nzchar(code)) return(data.frame())

    code_normalise <- toupper(gsub("[^A-Z0-9]","",code))

    base_sql <- "
      SELECT
        s.id,s.requisition_id,s.patient_id,s.numero_specimen,s.code_barre,
        s.date_specimen,s.departement,s.px,s.analyses,s.contenant,s.quantite,
        s.location,s.priorite,s.statut_reception,s.date_reception,
        COALESCE(s.heure_collecte_recue,'') AS heure_collecte_recue,
        COALESCE(s.initiales_preleveur,'') AS initiales_preleveur,
        COALESCE(s.source_prelevement,'') AS source_prelevement,
        p.numero_dossier,p.medicare,p.nom,p.prenom,p.sexe,
        r.numero_requisition,
        COALESCE(NULLIF(s.saisi_initiales,''),uc.initiales,'') AS saisi_initiales,
        COALESCE(NULLIF(s.saisi_matricule,''),uc.matricule,'') AS saisi_matricule,
        COALESCE(NULLIF(s.recu_initiales,''),ur.initiales,'') AS recu_initiales,
        COALESCE(NULLIF(s.recu_matricule,''),ur.matricule,'') AS recu_matricule
      FROM specimens s
      JOIN patients p ON p.id=s.patient_id
      JOIN requisitions r ON r.id=s.requisition_id
      LEFT JOIN utilisateurs uc ON uc.id=s.cree_par
      LEFT JOIN utilisateurs ur ON ur.id=s.recu_par
    "

    direct <- DBI::dbGetQuery(
      con,
      paste0(
        base_sql,
        "
        WHERE
          s.code_barre=?
          OR UPPER(REPLACE(REPLACE(REPLACE(s.numero_specimen,':',''),' ',''),'-',''))=?
          OR p.numero_dossier=?
          OR r.numero_requisition=?
        ORDER BY CASE WHEN s.code_barre=? THEN 0 ELSE 1 END,s.id DESC
        LIMIT 1
        "
      ),
      params=list(code,code_normalise,code,code,code)
    )

    if (nrow(direct)==1) return(direct)

    # Recherche abrégée : 2808 H10 ou ;H10 pour la journée courante.
    cle_recherche <- normaliser_reference_specimen(code)
    if (!nzchar(cle_recherche)) return(data.frame())

    candidats <- DBI::dbGetQuery(
      con,
      paste0(
        base_sql,
        "
        WHERE substr(COALESCE(s.date_specimen,''),1,10) >= date('now','localtime','-7 day')
        ORDER BY s.id DESC
        LIMIT 500
        "
      )
    )

    if (nrow(candidats)==0) return(data.frame())

    cles <- vapply(
      candidats$numero_specimen,
      normaliser_reference_specimen,
      character(1)
    )

    candidats <- candidats[cles == cle_recherche, , drop=FALSE]
    if (nrow(candidats)==0) return(data.frame())

    candidats[1, , drop=FALSE]
  }

  # ----------------------------------------------------------
  # RÉCEPTION DES SPÉCIMENS
  # ----------------------------------------------------------

  observeEvent(input$rechercher_specimen_reception, {
    req(utilisateur_connecte()$perm_reception == 1)

    code <- norm_txt(input$reception_code)
    if (!nzchar(code)) {
      output$resultat_reception <- renderUI(
        div(class="danger-box","Saisissez ou scannez un BC#, un numéro de spécimen ou un dossier (ex. 2808 H10 ou ;H10 pour aujourd’hui).")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    s <- trouver_specimen_par_reference(con, code)

    if (nrow(s)!=1) {
      specimen_reception(NULL)
      output$resultat_reception <- renderUI(div(class="danger-box","Spécimen non trouvé."))
      return()
    }

    specimen_reception(s)
    journaliser(
      "CONSULTATION_SPECIMEN",
      paste0("Consultation réception ",s$numero_specimen[1]," / BC# ",s$code_barre[1]),
      patient_id=s$patient_id[1],requisition_id=s$requisition_id[1],specimen_id=s$id[1]
    )

    deja_recu <- !is.na(s$statut_reception[1]) &&
      toupper(norm_txt(s$statut_reception[1]))=="RECU"
    annule <- !is.na(s$statut_reception[1]) &&
      toupper(norm_txt(s$statut_reception[1]))=="ANNULE"

    date_s <- suppressWarnings(as.Date(s$date_specimen[1]))
    retard <- !is.na(date_s) && date_s < Sys.Date()

    output$resultat_reception <- renderUI(
      div(
        class="admin-zone",
        h4(paste0(s$prenom[1]," ",s$nom[1])),
        p(strong("Dossier : "),s$numero_dossier[1]),
        p(strong("Req # : "),s$numero_requisition[1]),
        p(strong("Date du spécimen : "),s$date_specimen[1]),
        p(strong("Spécimen : "),s$numero_specimen[1]),
        p(strong("BC# : "),s$code_barre[1]),
        p(strong("Département : "),s$departement[1]),
        p(strong("Analyses : "),s$analyses[1]),
        p(strong("Contenant : "),s$contenant[1]),
        p(strong("Tests saisis par : "),paste0(s$saisi_initiales[1],s$saisi_matricule[1])),
        if (annule) {
          div(class="danger-box",strong("Ce spécimen est annulé."))
        } else if (deja_recu) {
          div(
            class="warning-box",
            p(strong("Déjà reçu.")),
            p(paste0("Reçu par ",s$recu_initiales[1],s$recu_matricule[1]," le ",s$date_reception[1]))
          )
        } else {
          tagList(
            if (retard) {
              div(
                class="warning-box",
                strong("Spécimen saisi à une date antérieure. "),
                "EDUSILLAB proposera de conserver ou corriger la date au moment de la réception."
              )
            },
            hr(),
            h4("Informations de collecte"),
            fluidRow(
              column(
                6,
                textInput(
                  "reception_heure_collecte",
                  "Heure de collecte",
                  value=ifelse(
                    nzchar(norm_txt(s$heure_collecte_recue[1])),
                    s$heure_collecte_recue[1],
                    ""
                  ),
                  placeholder="Ex. 08:35"
                )
              ),
              column(
                6,
                textInput(
                  "reception_initiales_preleveur",
                  "Initiales de la personne qui a prélevé",
                  value=s$initiales_preleveur[1],
                  placeholder="Ex. CK"
                )
              )
            ),
            p(
              class="text-muted",
              "Ces informations peuvent être saisies manuellement par la personne qui reçoit le spécimen."
            ),
            div(
              style="display:flex;gap:10px;flex-wrap:wrap;",
              actionButton("confirmer_reception_specimen","Confirmer la réception",class="btn-success"),
              actionButton("cancel_reception_specimen","Cancel",class="btn-default")
            )
          )
        }
      )
    )
  })

  observeEvent(input$cancel_reception_specimen, {
    specimen_reception(NULL)
    updateTextInput(session,"reception_code",value="")
    output$resultat_reception <- renderUI(
      div(class="info-box","Réception annulée. Aucun changement n'a été enregistré.")
    )
  })

  observeEvent(input$confirmer_reception_specimen, {
    req(utilisateur_connecte()$perm_reception == 1)
    s <- specimen_reception()
    req(!is.null(s),nrow(s)==1)

    showModal(
      modalDialog(
        title="Confirmer la réception",
        div(
          class="warning-box",
          p(strong("Êtes-vous sûr de vouloir recevoir ce spécimen ?")),
          p(strong("Patient : "),paste(s$prenom[1],s$nom[1])),
          p(strong("Spécimen : "),s$numero_specimen[1]),
          p(strong("BC# : "),s$code_barre[1])
        ),
        footer=tagList(
          modalButton("Cancel"),
          actionButton("ok_reception_specimen","OK",class="btn-success")
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$ok_reception_specimen, {
    removeModal()
    reception_unitaire_confirmee(reception_unitaire_confirmee()+1L)
  })

  observeEvent(reception_unitaire_confirmee(), {
    req(utilisateur_connecte()$perm_reception == 1)
    s <- specimen_reception()
    req(!is.null(s),nrow(s)==1)

    date_s <- suppressWarnings(as.Date(s$date_specimen[1]))
    retard <- !is.na(date_s) && date_s < Sys.Date()

    if (retard) {
      showModal(
        modalDialog(
          title="Spécimen reçu après sa date de saisie",
          div(
            class="warning-box",
            p(strong("BC# conservé : "),s$code_barre[1]),
            p("La correction ne modifie jamais le code-barres.")
          ),
          radioButtons(
            "reception_option_date",
            "Date du spécimen",
            choices=setNames(
              c("CONSERVER","AUJOURDHUI","AUTRE"),
              c(
                paste0("Conserver ",s$date_specimen[1]),
                paste0("Changer pour aujourd'hui (",Sys.Date(),")"),
                "Choisir une autre date"
              )
            ),
            selected="CONSERVER"
          ),
          dateInput("reception_date_personnalisee","Autre date",value=Sys.Date()),
          textInput(
            "reception_nouveau_dossier",
            "Numéro de dossier patient",
            value=s$numero_dossier[1]
          ),
          textInput(
            "reception_heure_collecte_retard",
            "Heure de collecte",
            value=norm_txt(input$reception_heure_collecte),
            placeholder="Ex. 08:35"
          ),
          textInput(
            "reception_initiales_preleveur_retard",
            "Initiales de la personne qui a prélevé",
            value=norm_upper(input$reception_initiales_preleveur),
            placeholder="Ex. CK"
          ),
          p(class="text-muted",
            "Si la date change, le numéro de spécimen est régénéré pour la nouvelle date. Le BC# reste identique et une nouvelle étiquette est produite."),
          footer=tagList(
            modalButton("Cancel"),
            actionButton("appliquer_reception_retard","Appliquer et recevoir",class="btn-success")
          )
        )
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    DBI::dbExecute(
      con,
      "
      UPDATE specimens
      SET recu_par=?,recu_initiales=?,recu_matricule=?,
          heure_collecte_recue=?,initiales_preleveur=?,
          date_reception=CURRENT_TIMESTAMP,statut_reception='RECU'
      WHERE id=?
      ",
      params=list(
        utilisateur_connecte()$id,utilisateur_connecte()$initiales,
        utilisateur_connecte()$matricule,
        norm_txt(input$reception_heure_collecte),
        norm_upper(input$reception_initiales_preleveur),
        s$id[1]
      )
    )

    journaliser(
      "RECEPTION_SPECIMEN",
      paste0(
        "Réception BC# ",s$code_barre[1]," / ",s$numero_specimen[1],
        "; heure collecte ",norm_txt(input$reception_heure_collecte),
        "; préleveur ",norm_upper(input$reception_initiales_preleveur)
      ),
      patient_id=s$patient_id[1],requisition_id=s$requisition_id[1],specimen_id=s$id[1]
    )

    output$resultat_reception <- renderUI(
      div(class="success-box",
        h4("Spécimen reçu"),
        p(strong("Reçu par : "),code_utilisateur()),
        p(strong("BC# : "),s$code_barre[1]),
        p(strong("Spécimen : "),s$numero_specimen[1])
      )
    )
  })
  normaliser_liste_reception <- function(x) {
    if (is.null(x) || !nzchar(norm_txt(x))) return(character(0))
    x <- gsub(",", "\n", x, fixed=TRUE)
    refs <- trimws(unlist(strsplit(x, "\n", fixed=TRUE)))
    refs <- refs[nzchar(refs)]
    unique(refs)
  }

  observeEvent(input$preparer_reception_multiple, {
    req(utilisateur_connecte()$perm_reception == 1)

    refs <- normaliser_liste_reception(input$reception_multiple_refs)
    if (length(refs)==0) {
      specimens_reception_multiple(data.frame())
      output$message_reception_multiple <- renderUI(
        div(class="danger-box","Ajoutez au moins une référence de tube.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    trouves <- list()
    erreurs <- character(0)
    deja_vus <- integer(0)

    for (ref in refs) {
      s <- tryCatch(
        trouver_specimen_par_reference(con,ref),
        error=function(e) data.frame()
      )

      if (nrow(s)!=1) {
        erreurs <- c(erreurs,paste0(ref," : non trouvé"))
        next
      }

      if (s$id[1] %in% deja_vus) next
      deja_vus <- c(deja_vus,s$id[1])

      statut <- toupper(norm_txt(s$statut_reception[1]))
      if (statut=="ANNULE") {
        erreurs <- c(erreurs,paste0(ref," : annulé"))
        next
      }
      if (statut=="RECU") {
        erreurs <- c(erreurs,paste0(ref," : déjà reçu"))
        next
      }

      trouves[[length(trouves)+1L]] <- s
    }

    if (length(trouves)==0) {
      specimens_reception_multiple(data.frame())
      output$message_reception_multiple <- renderUI(
        div(
          class="warning-box",
          paste(
            "Aucun tube disponible pour la réception.",
            if (length(erreurs)) paste(erreurs,collapse=" | ") else ""
          )
        )
      )
      return()
    }

    d <- do.call(rbind,trouves)
    specimens_reception_multiple(d)

    msg <- paste0(nrow(d)," tube(s) prêt(s) à recevoir.")
    if (length(erreurs)>0) {
      msg <- paste0(msg," Ignorés : ",paste(erreurs,collapse=" | "))
    }

    output$message_reception_multiple <- renderUI(
      div(class="info-box",msg)
    )
  })

  output$table_reception_multiple <- DT::renderDT({
    d <- specimens_reception_multiple()

    if (is.null(d) || nrow(d)==0) {
      return(
        DT::datatable(
          data.frame(Message="Aucun tube préparé."),
          rownames=FALSE,
          options=list(dom="t")
        )
      )
    }

    affichage <- data.frame(
      BC = d$code_barre,
      Numero_specimen = d$numero_specimen,
      Patient = paste(d$prenom, d$nom),
      Dossier = d$numero_dossier,
      Departement = d$departement,
      Analyses = d$analyses,
      Date = d$date_specimen,
      check.names = FALSE
    )
    names(affichage) <- c(
      "BC#", "N\u00b0 sp\u00e9cimen", "Patient", "Dossier",
      "D\u00e9partement", "Analyses", "Date"
    )

    DT::datatable(
      affichage,
      rownames=FALSE,
      selection="none",
      options=list(dom="t",paging=FALSE,searching=FALSE,ordering=FALSE,scrollX=TRUE)
    )
  })

  observeEvent(input$cancel_reception_multiple, {
    specimens_reception_multiple(data.frame())
    updateTextAreaInput(session,"reception_multiple_refs",value="")
    updateTextInput(session,"reception_multiple_heure",value="")
    updateTextInput(session,"reception_multiple_initiales",value="")
    output$message_reception_multiple <- renderUI(
      div(class="info-box","Réception multiple annulée. Aucun tube n'a été reçu.")
    )
  })

  observeEvent(input$confirmer_reception_multiple, {
    req(utilisateur_connecte()$perm_reception == 1)

    d <- specimens_reception_multiple()
    if (is.null(d) || nrow(d)==0) {
      output$message_reception_multiple <- renderUI(
        div(class="danger-box","Préparez d'abord les tubes à recevoir.")
      )
      return()
    }

    dates <- suppressWarnings(as.Date(d$date_specimen))
    n_retard <- sum(!is.na(dates) & dates < Sys.Date())

    showModal(
      modalDialog(
        title="Confirmer la réception multiple",
        div(
          class="warning-box",
          p(strong("Êtes-vous sûr de vouloir recevoir tous les tubes sélectionnés ?")),
          p(strong("Nombre de tubes : "),nrow(d)),
          if (n_retard>0)
            p(
              strong("Attention : "),
              paste0(
                n_retard,
                " tube(s) ont une date antérieure. Leur date sera conservée en réception multiple. Pour modifier une date, recevez le tube individuellement."
              )
            )
        ),
        p(
          strong("Heure de collecte : "),
          ifelse(
            nzchar(norm_txt(input$reception_multiple_heure)),
            norm_txt(input$reception_multiple_heure),
            "Non précisée"
          )
        ),
        p(
          strong("Initiales du préleveur : "),
          ifelse(
            nzchar(norm_upper(input$reception_multiple_initiales)),
            norm_upper(input$reception_multiple_initiales),
            "Non précisées"
          )
        ),
        footer=tagList(
          modalButton("Cancel"),
          actionButton("ok_reception_multiple","OK",class="btn-success")
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$ok_reception_multiple, {
    req(utilisateur_connecte()$perm_reception == 1)

    d <- specimens_reception_multiple()
    req(!is.null(d),nrow(d)>0)

    heure <- norm_txt(input$reception_multiple_heure)
    initiales <- norm_upper(input$reception_multiple_initiales)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    DBI::dbBegin(con)
    ok <- FALSE
    on.exit({
      if (!ok && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con),silent=TRUE)
      }
    },add=TRUE)

    recus <- list()

    for (i in seq_len(nrow(d))) {
      courant <- DBI::dbGetQuery(
        con,
        "
        SELECT id,patient_id,requisition_id,numero_specimen,code_barre,
               COALESCE(statut_reception,'') AS statut_reception
        FROM specimens
        WHERE id=?
        ",
        params=list(d$id[i])
      )

      if (nrow(courant)!=1) next
      statut <- toupper(norm_txt(courant$statut_reception[1]))
      if (statut %in% c("RECU","ANNULE")) next

      DBI::dbExecute(
        con,
        "
        UPDATE specimens
        SET recu_par=?,recu_initiales=?,recu_matricule=?,
            heure_collecte_recue=?,initiales_preleveur=?,
            date_reception=CURRENT_TIMESTAMP,statut_reception='RECU'
        WHERE id=?
        ",
        params=list(
          utilisateur_connecte()$id,
          utilisateur_connecte()$initiales,
          utilisateur_connecte()$matricule,
          heure,
          initiales,
          courant$id[1]
        )
      )

      recus[[length(recus)+1L]] <- courant
    }

    DBI::dbCommit(con)
    ok <- TRUE
    removeModal()

    for (r in recus) {
      journaliser(
        "RECEPTION_MULTIPLE_SPECIMEN",
        paste0(
          "Réception multiple BC# ",r$code_barre[1],
          " / ",r$numero_specimen[1],
          "; heure collecte ",heure,
          "; préleveur ",initiales
        ),
        patient_id=r$patient_id[1],
        requisition_id=r$requisition_id[1],
        specimen_id=r$id[1]
      )
    }

    n_recus <- length(recus)
    specimens_reception_multiple(data.frame())
    updateTextAreaInput(session,"reception_multiple_refs",value="")

    output$message_reception_multiple <- renderUI(
      div(
        class="success-box",
        paste0(n_recus," tube(s) reçu(s) avec succès par ",code_utilisateur(),".")
      )
    )
  })

  pdf_reception_corrige <- reactiveVal(NULL)

  observeEvent(input$appliquer_reception_retard, {
    req(utilisateur_connecte()$perm_reception == 1)
    s <- specimen_reception()
    req(!is.null(s),nrow(s)==1)

    option <- input$reception_option_date
    nouvelle_date <- if (option=="CONSERVER") {
      as.Date(s$date_specimen[1])
    } else if (option=="AUJOURDHUI") {
      Sys.Date()
    } else {
      as.Date(input$reception_date_personnalisee)
    }

    if (is.na(nouvelle_date)) {
      showNotification("Date invalide.",type="error")
      return()
    }

    nouveau_dossier <- norm_txt(input$reception_nouveau_dossier)
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    nouveau_patient_id <- s$patient_id[1]
    if (nzchar(nouveau_dossier) && nouveau_dossier != s$numero_dossier[1]) {
      p2 <- DBI::dbGetQuery(
        con,
        "SELECT * FROM patients WHERE numero_dossier=? AND actif=1 LIMIT 1",
        params=list(nouveau_dossier)
      )
      if (nrow(p2)!=1) {
        showNotification("Le nouveau dossier patient n'existe pas.",type="error",duration=8)
        return()
      }
      nouveau_patient_id <- p2$id[1]
    }

    nouveau_numero <- s$numero_specimen[1]
    if (as.character(nouvelle_date) != s$date_specimen[1]) {
      nouveau_numero <- generer_numero_specimen(con,nouvelle_date,s$px[1],s$priorite[1])
    }

    DBI::dbBegin(con)
    ok <- FALSE
    on.exit({if (!ok) try(DBI::dbRollback(con),silent=TRUE)},add=TRUE)

    DBI::dbExecute(
      con,
      "
      UPDATE specimens
      SET
        date_specimen_originale=COALESCE(date_specimen_originale,date_specimen),
        numero_specimen_original=COALESCE(numero_specimen_original,numero_specimen),
        patient_id_original=COALESCE(patient_id_original,patient_id),
        date_specimen=?,numero_specimen=?,patient_id=?,
        recu_par=?,recu_initiales=?,recu_matricule=?,
        heure_collecte_recue=?,initiales_preleveur=?,
        date_reception=CURRENT_TIMESTAMP,statut_reception='RECU'
      WHERE id=?
      ",
      params=list(
        as.character(nouvelle_date),nouveau_numero,nouveau_patient_id,
        utilisateur_connecte()$id,utilisateur_connecte()$initiales,
        utilisateur_connecte()$matricule,
        norm_txt(input$reception_heure_collecte_retard),
        norm_upper(input$reception_initiales_preleveur_retard),
        s$id[1]
      )
    )

    if (nouveau_patient_id != s$patient_id[1]) {
      DBI::dbExecute(
        con,
        "UPDATE requisitions SET patient_id=? WHERE id=?",
        params=list(nouveau_patient_id,s$requisition_id[1])
      )
    }

    DBI::dbCommit(con)
    ok <- TRUE

    journaliser(
      "CORRECTION_RECEPTION_RETARD",
      paste0(
        "BC# conservé ",s$code_barre[1],
        "; ancien spécimen ",s$numero_specimen[1],
        "; nouveau spécimen ",nouveau_numero,
        "; ancienne date ",s$date_specimen[1],
        "; nouvelle date ",as.character(nouvelle_date),
        "; ancien dossier ",s$numero_dossier[1],
        "; nouveau dossier ",ifelse(nzchar(nouveau_dossier),nouveau_dossier,s$numero_dossier[1]),
        "; heure collecte ",norm_txt(input$reception_heure_collecte_retard),
        "; préleveur ",norm_upper(input$reception_initiales_preleveur_retard)
      ),
      patient_id=nouveau_patient_id,requisition_id=s$requisition_id[1],specimen_id=s$id[1]
    )

    label <- DBI::dbGetQuery(
      con,
      "
      SELECT
        r.numero_requisition,s.numero_specimen,s.code_barre,
        p.nom,p.prenom,p.numero_dossier,p.medicare,p.sexe,
        COALESCE(s.location,'') AS location,
        COALESCE(s.analyses,'') AS analyses,
        COALESCE(s.quantite,'N/P') AS quantite,
        COALESCE(s.contenant,'NON PRECISE') AS contenant,
        COALESCE(s.departement,'') AS departement,
        COALESCE(s.px,'') AS px,
        TRIM(COALESCE(uc.prenom,'') || ' ' || COALESCE(uc.nom,'')) AS saisi_nom_complet,
        COALESCE(s.date_creation,'') AS date_saisie,
        COALESCE(NULLIF(s.saisi_initiales,''),uc.initiales,'') AS signature_initiales
      FROM specimens s
      JOIN requisitions r ON r.id=s.requisition_id
      JOIN patients p ON p.id=s.patient_id
      LEFT JOIN utilisateurs uc ON uc.id=s.cree_par
      WHERE s.id=?
      ",
      params=list(s$id[1])
    )

    nom_pdf <- paste0("etiquette_corrigee_BC_",s$code_barre[1],"_",
      format(Sys.time(),"%Y%m%d_%H%M%S"),".pdf")
    chemin <- file.path(preview_dir,nom_pdf)
    creer_pdf_selon_imprimante(label,chemin,con)
    pdf_reception_corrige(chemin)

    removeModal()

    output$resultat_reception <- renderUI(
      div(class="success-box",
        h4("Spécimen reçu et étiquette corrigée"),
        p(strong("BC# conservé : "),s$code_barre[1]),
        p(strong("Nouveau numéro de spécimen : "),nouveau_numero),
        p(strong("Date : "),as.character(nouvelle_date)),
        p(strong("Reçu par : "),code_utilisateur())
      )
    )
  })

  output$reception_pdf_corrige <- renderUI({
    p <- pdf_reception_corrige()
    if (is.null(p) || !file.exists(p)) return(NULL)
    div(
      class="well",
      h4("Étiquette corrigée"),
      tags$iframe(
        src=paste0("edulab_labels/",basename(p),"?v=",as.integer(Sys.time())),
        style="width:100%;height:300px;border:1px solid #bbb;"
      ),
      downloadButton("telecharger_etiquette_reception_corrigee","Télécharger l'étiquette corrigée")
    )
  })

  output$telecharger_etiquette_reception_corrigee <- downloadHandler(
    filename=function() {
      p <- pdf_reception_corrige()
      if (is.null(p)) "etiquette_corrigee.pdf" else basename(p)
    },
    content=function(file) {
      p <- pdf_reception_corrige()
      req(!is.null(p),file.exists(p))
      file.copy(p,file,overwrite=TRUE)
    },
    contentType="application/pdf"
  )

  # ----------------------------------------------------------
  # ÉDITEUR DU PORTAIL
  # ----------------------------------------------------------

  couleur_hex_valide <- function(x) {
    grepl("^#[0-9A-Fa-f]{6}$",norm_txt(x))
  }

  enregistrer_config_cle <- function(con,cle,valeur) {
    DBI::dbExecute(
      con,
      "
      INSERT INTO portail_configuration(cle,valeur,date_modification,modifie_par)
      VALUES (?,?,CURRENT_TIMESTAMP,?)
      ON CONFLICT(cle)
      DO UPDATE SET
        valeur=excluded.valeur,
        date_modification=CURRENT_TIMESTAMP,
        modifie_par=excluded.modifie_par
      ",
      params=list(cle,valeur,utilisateur_connecte()$id)
    )
  }

  observeEvent(input$enregistrer_configuration_portail, {
    req(peut_modifier_portail())

    couleurs <- c(
      norm_txt(input$cfg_couleur_principale),
      norm_txt(input$cfg_couleur_secondaire),
      norm_txt(input$cfg_couleur_sidebar_bas)
    )

    if (!all(vapply(couleurs,couleur_hex_valide,logical(1)))) {
      output$message_configuration_portail <- renderUI(
        div(
          class="danger-box",
          "Les couleurs doivent être au format #RRGGBB, par exemple #00757c."
        )
      )
      return()
    }

    valeurs <- list(
      portail_nom=norm_txt(input$cfg_portail_nom),
      institution=norm_txt(input$cfg_institution),
      campus=norm_txt(input$cfg_campus),
      sous_titre=norm_txt(input$cfg_sous_titre),
      tableau_bord_titre=norm_txt(input$cfg_dashboard_titre),
      tableau_bord_sous_titre=norm_txt(input$cfg_dashboard_sous_titre),
      footer_texte=norm_txt(input$cfg_footer),
      couleur_principale=couleurs[1],
      couleur_secondaire=couleurs[2],
      couleur_sidebar_bas=couleurs[3]
    )

    if (!nzchar(valeurs$portail_nom)) valeurs$portail_nom <- "EDUSILLAB"
    if (!nzchar(valeurs$institution)) valeurs$institution <- "CCNB"
    if (!nzchar(valeurs$campus)) valeurs$campus <- "Campus de Dieppe"

    logo_data <- NULL

    if (isTRUE(input$cfg_retirer_logo)) {
      logo_data <- ""
    } else if (!is.null(input$cfg_logo)) {
      f <- input$cfg_logo
      ext <- tolower(tools::file_ext(f$name))
      mime <- if (ext=="png") "image/png" else if (ext %in% c("jpg","jpeg")) "image/jpeg" else ""

      if (!nzchar(mime)) {
        output$message_configuration_portail <- renderUI(
          div(class="danger-box","Le logo doit être PNG, JPG ou JPEG.")
        )
        return()
      }

      taille <- file.info(f$datapath)$size
      if (is.na(taille) || taille > 2*1024*1024) {
        output$message_configuration_portail <- renderUI(
          div(class="danger-box","Le logo doit faire 2 Mo maximum.")
        )
        return()
      }

      brut <- readBin(f$datapath,"raw",n=taille)
      logo_data <- paste0(
        "data:",mime,";base64,",
        jsonlite::base64_enc(brut)
      )
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    for (k in names(valeurs)) {
      enregistrer_config_cle(con,k,valeurs[[k]])
    }

    if (!is.null(logo_data)) {
      enregistrer_config_cle(con,"logo_data_uri",logo_data)
    }

    journaliser(
      "MODIFICATION_PORTAIL",
      paste0(
        "Configuration du portail modifiée par ",
        code_utilisateur()
      )
    )

    refresh_portail(refresh_portail()+1L)

    output$message_configuration_portail <- renderUI(
      div(
        class="success-box",
        "Configuration enregistrée. Les nouvelles valeurs s'appliquent au portail."
      )
    )
  })

  observeEvent(input$restaurer_configuration_portail, {
    req(peut_modifier_portail())

    showModal(
      modalDialog(
        title="Restaurer la configuration",
        p("Êtes-vous sûr de vouloir restaurer les textes et couleurs par défaut ?"),
        footer=tagList(
          modalButton("Cancel"),
          actionButton("ok_restaurer_configuration_portail","OK",class="btn-warning")
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$ok_restaurer_configuration_portail, {
    req(peut_modifier_portail())

    defaut <- list(
      portail_nom="EDUSILLAB",
      institution="CCNB",
      campus="Campus de Dieppe",
      sous_titre="Système d'information de laboratoire — environnement d'enseignement — version Web",
      tableau_bord_titre="Tableau de bord",
      tableau_bord_sous_titre="Vue d'ensemble de l'activité du laboratoire d'enseignement.",
      footer_texte="© 2026 CCNB Campus de Dieppe. Tous droits réservés.",
      couleur_principale="#00757c",
      couleur_secondaire="#00585f",
      couleur_sidebar_bas="#003f45",
      logo_data_uri=""
    )

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    for (k in names(defaut)) {
      enregistrer_config_cle(con,k,defaut[[k]])
    }

    removeModal()

    journaliser(
      "RESTAURATION_PORTAIL",
      paste0("Configuration du portail restaurée par ",code_utilisateur())
    )

    refresh_portail(refresh_portail()+1L)

    output$message_configuration_portail <- renderUI(
      div(class="success-box","Configuration par défaut restaurée.")
    )
  })

  # ----------------------------------------------------------
  # MATÉRIEL / IMPRIMANTES
  # ----------------------------------------------------------

  output$table_materiel <- DT::renderDT({
    refresh_materiel()
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add=TRUE)
    d <- DBI::dbGetQuery(con, "
      SELECT id AS ID, nom AS Nom, type AS Type,
             COALESCE(mode_connexion,'') AS Connexion,
             fabricant_modele AS `Fabricant / modèle`,
             numero_serie AS `Numéro de série`,
             largeur_mm AS `Largeur mm`, hauteur_mm AS `Hauteur mm`,
             CASE WHEN par_defaut=1 THEN 'Oui' ELSE 'Non' END AS `Par défaut`,
             CASE WHEN actif=1 THEN 'Actif' ELSE 'Inactif' END AS Statut
      FROM materiel_impression
      ORDER BY par_defaut DESC, nom
    ")
    DT::datatable(d, rownames=FALSE, options=list(pageLength=10, scrollX=TRUE))
  })

  observeEvent(input$mat_enregistrer, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    nom <- norm_txt(input$mat_nom)
    if (!nzchar(nom)) {
      output$mat_message <- renderUI(div(class="danger-box","Le nom de l'imprimante est obligatoire."))
      return()
    }
    con <- ouvrir_db()
    if (isTRUE(input$mat_defaut)) DBI::dbExecute(con, "UPDATE materiel_impression SET par_defaut=0")
    DBI::dbExecute(con, "
      INSERT INTO materiel_impression
      (
        nom, type, mode_connexion, fabricant_modele, numero_serie,
        largeur_mm, hauteur_mm, actif, par_defaut,
        mode_ajustement, offset_x_mm, offset_y_mm, echelle_pct
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, 1, ?, ?, ?, ?, ?)
    ", params=list(
      nom,
      norm_txt(input$mat_type),
      norm_txt(input$mat_connexion),
      norm_txt(input$mat_modele),
      norm_txt(input$mat_serie),
      as.numeric(input$mat_largeur),
      as.numeric(input$mat_hauteur),
      ifelse(isTRUE(input$mat_defaut),1,0),
      norm_txt(input$mat_ajustement),
      as.numeric(input$mat_offset_x),
      as.numeric(input$mat_offset_y),
      as.numeric(input$mat_echelle)
    ))
    DBI::dbDisconnect(con)
    refresh_materiel(refresh_materiel()+1L)
    output$mat_message <- renderUI(div(class="success-box","Matériel enregistré."))
  })

  # ----------------------------------------------------------
  # REPERTOIRE D'ANALYSES
  # ----------------------------------------------------------

  donnees_repertoire <- reactive({
    refresh_analyses()
    req(utilisateur_connecte())

    con <- ouvrir_db()
    on.exit({
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    }, add = TRUE)

    where <- "1 = 1"
    params <- list()

    if (!is.null(input$filtre_actif) && input$filtre_actif != "TOUS") {
      where <- paste(where, "AND a.actif = ?")
      params <- c(params, list(as.integer(input$filtre_actif)))
    }

    filtre <- if (is.null(input$filtre_repertoire)) "" else norm_txt(input$filtre_repertoire)

    if (nzchar(filtre)) {
      where <- paste0(
        where,
        " AND (",
        "UPPER(a.nom) LIKE UPPER(?) OR ",
        "UPPER(a.mnemonique) LIKE UPPER(?) OR ",
        "UPPER(COALESCE(a.departement,'')) LIKE UPPER(?) OR ",
        "UPPER(COALESCE(dp.px,'')) LIKE UPPER(?) OR ",
        "UPPER(COALESCE(a.tube_prelevement,'')) LIKE UPPER(?)",
        ")"
      )
      patt <- paste0("%", filtre, "%")
      params <- c(params, rep(list(patt), 5))
    }

    sql_principal <- paste0("
      SELECT
        a.id,
        a.actif,
        a.nom,
        a.mnemonique,
        COALESCE(a.departement, '') AS departement,
        COALESCE(dp.px, '') AS px,
        COALESCE(a.tube_prelevement, '') AS tube_prelevement,
        a.nb_tubes,
        COALESCE(a.priorite_specimen, '') AS priorite_specimen,
        a.delai_h,
        a.page_reference,
        COALESCE(a.notes, '') AS notes,
        (
          SELECT COUNT(*)
          FROM analyse_sources s
          WHERE s.analyse_id = a.id
        ) AS nb_sources
      FROM analyses a
      LEFT JOIN departements_px dp
        ON UPPER(dp.departement) = UPPER(a.departement)
      WHERE ", where, "
      ORDER BY a.nom
    ")

    resultat <- tryCatch({
      if (length(params) == 0) {
        DBI::dbGetQuery(con, sql_principal)
      } else {
        DBI::dbGetQuery(con, sql_principal, params = params)
      }
    }, error = function(e) {
      message("Répertoire - requête principale : ", conditionMessage(e))
      NULL
    })

    if (!is.null(resultat)) {
      output$message_repertoire <- renderUI(NULL)
      return(resultat)
    }

    # Requête de secours : elle ne dépend ni de la table Px ni des sources.
    where_secours <- "1 = 1"
    params_secours <- list()

    if (!is.null(input$filtre_actif) && input$filtre_actif != "TOUS") {
      where_secours <- paste(where_secours, "AND a.actif = ?")
      params_secours <- c(params_secours, list(as.integer(input$filtre_actif)))
    }

    if (nzchar(filtre)) {
      where_secours <- paste0(
        where_secours,
        " AND (",
        "UPPER(a.nom) LIKE UPPER(?) OR ",
        "UPPER(a.mnemonique) LIKE UPPER(?) OR ",
        "UPPER(COALESCE(a.departement,'')) LIKE UPPER(?) OR ",
        "UPPER(COALESCE(a.tube_prelevement,'')) LIKE UPPER(?)",
        ")"
      )
      patt <- paste0("%", filtre, "%")
      params_secours <- c(params_secours, rep(list(patt), 4))
    }

    sql_secours <- paste0("
      SELECT
        a.id,
        a.actif,
        a.nom,
        a.mnemonique,
        COALESCE(a.departement, '') AS departement,
        '' AS px,
        COALESCE(a.tube_prelevement, '') AS tube_prelevement,
        a.nb_tubes,
        COALESCE(a.priorite_specimen, '') AS priorite_specimen,
        a.delai_h,
        a.page_reference,
        COALESCE(a.notes, '') AS notes,
        0 AS nb_sources
      FROM analyses a
      WHERE ", where_secours, "
      ORDER BY a.nom
    ")

    resultat_secours <- tryCatch({
      if (length(params_secours) == 0) {
        DBI::dbGetQuery(con, sql_secours)
      } else {
        DBI::dbGetQuery(con, sql_secours, params = params_secours)
      }
    }, error = function(e) {
      output$message_repertoire <- renderUI(
        div(
          class = "danger-box",
          paste0(
            "Impossible de lire le répertoire d'analyses : ",
            conditionMessage(e)
          )
        )
      )
      data.frame()
    })

    if (nrow(resultat_secours) > 0) {
      output$message_repertoire <- renderUI(
        div(
          class = "warning-box",
          "Le répertoire est affiché en mode de secours. Les analyses sont accessibles, mais certaines informations auxiliaires (Px ou sources) peuvent être temporairement indisponibles."
        )
      )
    }

    resultat_secours
  })

  output$table_repertoire <- DT::renderDT({
    d <- donnees_repertoire()

    if (nrow(d) == 0) {
      return(DT::datatable(
        data.frame(
          Message = if (peut_gerer_analyses()) {
            "Aucune analyse trouvée. Choisissez Statut = Toutes. Si le répertoire est vide, utilisez « Import direct du répertoire Excel » plus bas sur cette page."
          } else {
            "Aucune analyse trouvée avec les filtres actuels. Essayez le statut « Toutes » ou cliquez sur « Actualiser »."
          }
        ),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    # La colonne Notes devient la zone cliquable qui ouvre la fiche
    # détaillée de l'analyse. Il n'y a donc plus de colonne Détails séparée.
    notes_btn <- ifelse(
      nzchar(trimws(d$notes)),
      sprintf(
        "<button class='btn btn-info btn-sm' onclick=\"Shiny.setInputValue('detail_analyse_id', %s, {priority:'event'})\">Voir détails / notes</button>",
        d$id
      ),
      sprintf(
        "<button class='btn btn-default btn-sm' onclick=\"Shiny.setInputValue('detail_analyse_id', %s, {priority:'event'})\">Voir détails</button>",
        d$id
      )
    )

    source_btn <- ifelse(
      d$nb_sources > 0,
      sprintf(
        "<button class='btn btn-link btn-sm' onclick=\"Shiny.setInputValue('sources_analyse_id', %s, {priority:'event'})\">%s source(s)</button>",
        d$id, d$nb_sources
      ),
      ""
    )

    action_btn <- if (utilisateur_connecte()$perm_analyses == 1) {
      statut_btn <- ifelse(
        d$actif == 1,
        sprintf(
          "<button class='btn btn-danger btn-xs' onclick=\"Shiny.setInputValue('retirer_analyse_id', %s, {priority:'event'})\">Retirer</button>",
          d$id
        ),
        sprintf(
          "<button class='btn btn-success btn-xs' onclick=\"Shiny.setInputValue('reactiver_analyse_id', %s, {priority:'event'})\">Réactiver</button>",
          d$id
        )
      )

      # Modification directe réservée à l'administrateur.
      if (toupper(utilisateur_connecte()$role) == "ADMIN") {
        modifier_btn <- sprintf(
          "<button class='btn btn-warning btn-xs' style='margin-right:6px' onclick=\"Shiny.setInputValue('modifier_analyse_id', %s, {priority:'event'})\">Modifier</button>",
          d$id
        )
        paste0(modifier_btn, statut_btn)
      } else {
        statut_btn
      }
    } else {
      rep("", nrow(d))
    }

    # Affichage propre du nombre de tubes :
    # 1 au lieu de 1.0, 2 au lieu de 2.0, etc.
    nb_tubes_aff <- ifelse(
      is.na(d$nb_tubes),
      "",
      ifelse(
        abs(d$nb_tubes - round(d$nb_tubes)) < 0.000001,
        as.character(as.integer(round(d$nb_tubes))),
        as.character(d$nb_tubes)
      )
    )

    delai_aff <- ifelse(
      is.na(d$delai_h),
      "",
      ifelse(
        abs(d$delai_h - round(d$delai_h)) < 0.000001,
        as.character(as.integer(round(d$delai_h))),
        as.character(d$delai_h)
      )
    )

    table <- data.frame(
      Nom = d$nom,
      Mnemonic = d$mnemonique,
      Departements = d$departement,
      Px = d$px,
      Tube_prelevement = d$tube_prelevement,
      Nombre_tubes = nb_tubes_aff,
      Delai_h = delai_aff,
      Notes = notes_btn,
      Sources = source_btn,
      Priorite_specimen = d$priorite_specimen,
      Statut = ifelse(d$actif == 1, "Active", "Retir\u00e9e"),
      Action = action_btn,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(table) <- c(
      "Nom", "Mnemonic", "D\u00e9partements", "Px",
      "Tube de pr\u00e9l\u00e8vement", "# of tubes", "D\u00e9lai (h)",
      "Notes", "Sources", "Priorit\u00e9 du sp\u00e9cimen", "Statut", "Action"
    )

    DT::datatable(
      table,
      escape = FALSE,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 3, 5, 6, 7, 8, 10, 11))
        )
      )
    )
  })

  observeEvent(input$actualiser_repertoire, {
    refresh_analyses(refresh_analyses() + 1L)
  })

  # Détails / notes
  observeEvent(input$detail_analyse_id, {
    id <- as.integer(input$detail_analyse_id)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    a <- DBI::dbGetQuery(
      con,
      "
      SELECT
        a.*,
        COALESCE(dp.px, '') AS px
      FROM analyses a
      LEFT JOIN departements_px dp
        ON UPPER(dp.departement) = UPPER(a.departement)
      WHERE a.id = ?
      ",
      params = list(id)
    )

    if (nrow(a) != 1) return()

    showModal(modalDialog(
      title = paste0(a$nom[1], " — ", a$mnemonique[1]),
      fluidRow(
        column(6, p(strong("Département : "), a$departement[1])),
        column(6, p(strong("Px : "), a$px[1]))
      ),
      fluidRow(
        column(6, p(strong("Tube de prélèvement : "), a$tube_prelevement[1])),
        column(
          6,
          p(
            strong("# of tubes : "),
            ifelse(
              is.na(a$nb_tubes[1]),
              "Non précisé",
              ifelse(
                abs(a$nb_tubes[1] - round(a$nb_tubes[1])) < 0.000001,
                as.character(as.integer(round(a$nb_tubes[1]))),
                as.character(a$nb_tubes[1])
              )
            )
          )
        )
      ),
      fluidRow(
        column(6, p(strong("Priorité : "), a$priorite_specimen[1])),
        column(6, p(strong("Délai (h) : "), a$delai_h[1]))
      ),
      hr(),
      h4("Notes / instructions"),
      div(
        class = "notes-box",
        ifelse(
          is.na(a$notes[1]) || !nzchar(a$notes[1]),
          "Aucune note / instruction.",
          a$notes[1]
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })

  # Sources
  observeEvent(input$sources_analyse_id, {
    id <- as.integer(input$sources_analyse_id)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    a <- DBI::dbGetQuery(
      con,
      "SELECT nom, mnemonique FROM analyses WHERE id = ?",
      params = list(id)
    )

    src <- DBI::dbGetQuery(
      con,
      "
      SELECT
        COALESCE(source_mnemonique, '') AS Mnemonic,
        source_nom AS Nom,
        COALESCE(categorie, '') AS Catégorie
      FROM analyse_sources
      WHERE analyse_id = ?
      ORDER BY source_nom
      ",
      params = list(id)
    )

    if (nrow(a) != 1) return()

    showModal(modalDialog(
      title = paste0("Sources — ", a$nom[1], " (", a$mnemonique[1], ")"),
      if (nrow(src) == 0)
        p("Aucune source associée.")
      else
        DT::DTOutput("modal_sources_table"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))

    output$modal_sources_table <- DT::renderDT({
      DT::datatable(
        src,
        rownames = FALSE,
        options = list(pageLength = 15, dom = "tip")
      )
    })
  })

  # ----------------------------------------------------------
  # MODIFICATION DIRECTE DU RÉPERTOIRE — ADMIN UNIQUEMENT
  # ----------------------------------------------------------

  observeEvent(input$modifier_analyse_id, {
    req(
      utilisateur_connecte()$perm_analyses == 1,
      toupper(utilisateur_connecte()$role) == "ADMIN"
    )

    id <- as.integer(input$modifier_analyse_id)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    a <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM analyses
      WHERE id = ?
      ",
      params = list(id)
    )

    if (nrow(a) != 1) {
      showNotification("Analyse introuvable.", type = "error")
      return()
    }

    dep <- DBI::dbGetQuery(
      con,
      "
      SELECT departement, px
      FROM departements_px
      ORDER BY departement
      "
    )

    choices_dep <- setNames("", "")
    if (nrow(dep) > 0) {
      vals <- dep$departement
      names(vals) <- paste0(
        dep$departement,
        ifelse(
          is.na(dep$px) | dep$px == "",
          "",
          paste0(" — Px ", dep$px)
        )
      )
      choices_dep <- c(choices_dep, vals)
    }

    # Si le département actuel n'existe pas encore dans le dictionnaire,
    # il est quand même proposé afin de ne pas perdre la valeur existante.
    dep_actuel <- norm_txt(a$departement[1])
    if (nzchar(dep_actuel) && !dep_actuel %in% unname(choices_dep)) {
      choices_dep <- c(
        choices_dep,
        setNames(dep_actuel, paste0(dep_actuel, " — non mappé"))
      )
    }

    src <- DBI::dbGetQuery(
      con,
      "
      SELECT
        COALESCE(source_mnemonique, '') AS code,
        COALESCE(source_nom, '') AS nom,
        COALESCE(categorie, '') AS categorie
      FROM analyse_sources
      WHERE analyse_id = ?
      ORDER BY id
      ",
      params = list(id)
    )

    sources_txt <- ""
    if (nrow(src) > 0) {
      sources_txt <- paste(
        apply(
          src,
          1,
          function(x) {
            paste(
              norm_txt(x[["code"]]),
              norm_txt(x[["nom"]]),
              norm_txt(x[["categorie"]]),
              sep = " | "
            )
          }
        ),
        collapse = "\n"
      )
    }

    session$userData$analyse_modification_id <- id

    showModal(
      modalDialog(
        title = paste0(
          "Modifier l'analyse — ",
          a$nom[1],
          " (",
          a$mnemonique[1],
          ")"
        ),

        div(
          class = "warning-box",
          strong("Modification administrateur : "),
          "les changements sont enregistrés directement dans le répertoire EduLab."
        ),

        fluidRow(
          column(
            7,
            textInput(
              "edit_an_nom",
              "Nom de l'analyse",
              value = ifelse(is.na(a$nom[1]), "", a$nom[1])
            )
          ),
          column(
            5,
            textInput(
              "edit_an_mnemo",
              "Mnemonic",
              value = ifelse(is.na(a$mnemonique[1]), "", a$mnemonique[1])
            )
          )
        ),

        fluidRow(
          column(
            6,
            selectInput(
              "edit_an_dep",
              "Département",
              choices = choices_dep,
              selected = dep_actuel
            )
          ),
          column(
            6,
            textInput(
              "edit_an_tube",
              "Tube de prélèvement",
              value = ifelse(
                is.na(a$tube_prelevement[1]),
                "",
                a$tube_prelevement[1]
              )
            )
          )
        ),

        fluidRow(
          column(
            4,
            numericInput(
              "edit_an_nb_tubes",
              "# of tubes",
              value = ifelse(
                is.na(a$nb_tubes[1]),
                NA,
                a$nb_tubes[1]
              ),
              min = 0,
              step = 1
            )
          ),
          column(
            4,
            textInput(
              "edit_an_prio",
              "Priorité du spécimen",
              value = ifelse(
                is.na(a$priorite_specimen[1]),
                "",
                a$priorite_specimen[1]
              )
            )
          ),
          column(
            4,
            textInput(
              "edit_an_delai",
              "Délai (h)",
              value = ifelse(
                is.na(a$delai_h[1]),
                "",
                as.character(a$delai_h[1])
              )
            )
          )
        ),

        textAreaInput(
          "edit_an_notes",
          "Notes / instructions",
          value = ifelse(is.na(a$notes[1]), "", a$notes[1]),
          rows = 10,
          width = "100%"
        ),

        textAreaInput(
          "edit_an_sources",
          "Sources — une ligne par source : CODE | NOM | CATEGORIE",
          value = sources_txt,
          rows = 7,
          width = "100%",
          placeholder = "B | BOUCHE | RESPIRATOIRE\nGE | GENCIVES | RESPIRATOIRE"
        ),

        checkboxInput(
          "edit_an_actif",
          "Analyse active dans le laboratoire",
          value = isTRUE(a$actif[1] == 1)
        ),

        footer = tagList(
          modalButton("Annuler"),
          actionButton(
            "enregistrer_modification_analyse",
            "Enregistrer les modifications",
            class = "btn-success"
          )
        ),
        easyClose = FALSE,
        size = "l"
      )
    )
  })

  observeEvent(input$enregistrer_modification_analyse, {
    req(
      utilisateur_connecte()$perm_analyses == 1,
      toupper(utilisateur_connecte()$role) == "ADMIN"
    )

    id <- session$userData$analyse_modification_id
    if (is.null(id)) {
      showNotification("Aucune analyse sélectionnée.", type = "error")
      return()
    }

    nom <- norm_txt(input$edit_an_nom)
    mnemo <- norm_upper(input$edit_an_mnemo)
    dep <- norm_upper(input$edit_an_dep)
    tube <- norm_txt(input$edit_an_tube)
    priorite <- norm_txt(input$edit_an_prio)
    notes <- input$edit_an_notes

    if (!nzchar(nom) || !nzchar(mnemo)) {
      showNotification(
        "Le nom et le Mnemonic sont obligatoires.",
        type = "error"
      )
      return()
    }

    nb_tubes <- suppressWarnings(as.numeric(input$edit_an_nb_tubes))
    if (length(nb_tubes) == 0 || is.nan(nb_tubes)) nb_tubes <- NA_real_

    delai_h <- suppressWarnings(as.numeric(input$edit_an_delai))
    if (length(delai_h) == 0 || is.nan(delai_h)) delai_h <- NA_real_

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Empêcher de transformer la fiche en doublon d'une autre analyse.
    doublon <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM analyses
      WHERE id <> ?
        AND UPPER(nom) = UPPER(?)
        AND UPPER(mnemonique) = UPPER(?)
        AND UPPER(COALESCE(departement, '')) = UPPER(?)
      LIMIT 1
      ",
      params = list(
        id,
        nom,
        mnemo,
        dep
      )
    )

    if (nrow(doublon) > 0) {
      showNotification(
        "Une autre analyse possède déjà ce Nom + Mnemonic + Département.",
        type = "error"
      )
      return()
    }

    DBI::dbBegin(con)

    ok <- tryCatch({
      DBI::dbExecute(
        con,
        "
        UPDATE analyses
        SET
          nom = ?,
          mnemonique = ?,
          departement = ?,
          tube_prelevement = ?,
          nb_tubes = ?,
          priorite_specimen = ?,
          delai_h = ?,
          notes = ?,
          actif = ?,
          date_modification = CURRENT_TIMESTAMP
        WHERE id = ?
        ",
        params = list(
          nom,
          mnemo,
          ifelse(nzchar(dep), dep, NA_character_),
          ifelse(nzchar(tube), tube, NA_character_),
          nb_tubes,
          ifelse(nzchar(priorite), priorite, NA_character_),
          delai_h,
          ifelse(nzchar(norm_txt(notes)), notes, NA_character_),
          ifelse(isTRUE(input$edit_an_actif), 1L, 0L),
          id
        )
      )

      # Les sources sont éditables dans la même fenêtre.
      DBI::dbExecute(
        con,
        "DELETE FROM analyse_sources WHERE analyse_id = ?",
        params = list(id)
      )

      src_txt <- input$edit_an_sources
      if (!is.null(src_txt) && nzchar(norm_txt(src_txt))) {
        lignes <- strsplit(src_txt, "\n", fixed = TRUE)[[1]]

        for (ligne in lignes) {
          if (!nzchar(norm_txt(ligne))) next

          parts <- trimws(strsplit(ligne, "\\|")[[1]])
          if (length(parts) < 2) next

          code <- ifelse(length(parts) >= 1, norm_upper(parts[1]), "")
          src_nom <- ifelse(length(parts) >= 2, norm_txt(parts[2]), "")
          categorie <- ifelse(length(parts) >= 3, norm_txt(parts[3]), "")

          if (!nzchar(src_nom)) next

          DBI::dbExecute(
            con,
            "
            INSERT OR IGNORE INTO sources_prelevement
            (mnemonique, nom, categorie)
            VALUES (?, ?, ?)
            ",
            params = list(
              ifelse(nzchar(code), code, NA_character_),
              src_nom,
              ifelse(nzchar(categorie), categorie, NA_character_)
            )
          )

          DBI::dbExecute(
            con,
            "
            INSERT OR IGNORE INTO analyse_sources
            (analyse_id, source_mnemonique, source_nom, categorie)
            VALUES (?, ?, ?, ?)
            ",
            params = list(
              id,
              ifelse(nzchar(code), code, NA_character_),
              src_nom,
              ifelse(nzchar(categorie), categorie, NA_character_)
            )
          )
        }
      }

      DBI::dbCommit(con)
      TRUE
    }, error = function(e) {
      try(DBI::dbRollback(con), silent = TRUE)
      showNotification(
        paste("Erreur pendant la modification :", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
      FALSE
    })

    if (!ok) return()

    session$userData$analyse_modification_id <- NULL
    removeModal()
    refresh_analyses(refresh_analyses() + 1L)

    showNotification(
      paste0("Analyse mise à jour : ", mnemo, " — ", nom),
      type = "message"
    )
  })

  # Retirer = désactivation logique
  observeEvent(input$retirer_analyse_id, {
    req(peut_gerer_analyses())
    id <- as.integer(input$retirer_analyse_id)

    showModal(modalDialog(
      title = "Retirer l'analyse du laboratoire",
      p(
        "L'analyse sera désactivée. Elle ne sera plus disponible dans Order, ",
        "mais son historique sera conservé."
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(
          "confirmer_retrait_analyse",
          "Retirer l'analyse",
          class = "btn-danger"
        )
      )
    ))

    session$userData$analyse_retrait_id <- id
  })

  observeEvent(input$confirmer_retrait_analyse, {
    req(peut_gerer_analyses())

    id <- session$userData$analyse_retrait_id
    if (is.null(id)) return()

    con <- ouvrir_db()
    DBI::dbExecute(
      con,
      "
      UPDATE analyses
      SET actif = 0, date_modification = CURRENT_TIMESTAMP
      WHERE id = ?
      ",
      params = list(id)
    )
    DBI::dbDisconnect(con)

    removeModal()
    refresh_analyses(refresh_analyses() + 1L)
  })

  observeEvent(input$reactiver_analyse_id, {
    req(peut_gerer_analyses())
    id <- as.integer(input$reactiver_analyse_id)

    con <- ouvrir_db()
    DBI::dbExecute(
      con,
      "
      UPDATE analyses
      SET actif = 1, date_modification = CURRENT_TIMESTAMP
      WHERE id = ?
      ",
      params = list(id)
    )
    DBI::dbDisconnect(con)

    refresh_analyses(refresh_analyses() + 1L)
  })

  # Ajouter une analyse manuellement
  observeEvent(input$ouvrir_ajout_analyse, {
    req(peut_gerer_analyses())

    con <- ouvrir_db()
    dep <- DBI::dbGetQuery(
      con,
      "SELECT departement, px FROM departements_px ORDER BY departement"
    )
    DBI::dbDisconnect(con)

    choices_dep <- setNames("", "")
    if (nrow(dep) > 0) {
      vals <- dep$departement
      names(vals) <- paste0(dep$departement, ifelse(
        is.na(dep$px) | dep$px == "", "", paste0(" — Px ", dep$px)
      ))
      choices_dep <- c(choices_dep, vals)
    }

    showModal(modalDialog(
      title = "Ajouter une analyse",
      textInput("man_nom", "Nom de l'analyse"),
      textInput("man_mnemo", "Mnémonique"),
      selectInput(
        "man_dep",
        "Département",
        choices = choices_dep
      ),
      textInput("man_tube", "Tube de prélèvement"),
      numericInput("man_nb_tubes", "# of tubes", value = 1, min = 0),
      textInput("man_prio", "Priorité du spécimen", value = "Routine"),
      textInput("man_delai", "Délai (h)", value = ""),
      textAreaInput(
        "man_notes",
        "Notes / instructions",
        rows = 8
      ),
      textAreaInput(
        "man_sources",
        "Sources (une ligne par source : CODE | NOM | CATEGORIE)",
        rows = 6,
        placeholder = "B | BOUCHE | RESPIRATOIRE\nGE | GENCIVES | RESPIRATOIRE"
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("enregistrer_analyse_man", "Enregistrer", class = "btn-success")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$enregistrer_analyse_man, {
    req(peut_gerer_analyses())

    nom <- norm_txt(input$man_nom)
    mn <- norm_upper(input$man_mnemo)

    if (!nzchar(nom) || !nzchar(mn)) {
      showNotification("Nom et mnémonique obligatoires.", type = "error")
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    existe <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM analyses
      WHERE UPPER(mnemonique) = UPPER(?)
        AND UPPER(nom) = UPPER(?)
        AND UPPER(COALESCE(departement, '')) = UPPER(?)
      ",
      params = list(
        mn,
        nom,
        norm_upper(input$man_dep)
      )
    )

    if (nrow(existe) > 0) {
      showNotification(
        "Cette combinaison Nom + Mnémonique + Département existe déjà.",
        type = "error"
      )
      return()
    }

    DBI::dbExecute(
      con,
      "
      INSERT INTO analyses
      (
        nom, mnemonique, departement, tube_prelevement,
        nb_tubes, priorite_specimen, delai_h, notes, actif
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)
      ",
      params = list(
        nom,
        mn,
        ifelse(nzchar(norm_txt(input$man_dep)), norm_upper(input$man_dep), NA_character_),
        ifelse(nzchar(norm_txt(input$man_tube)), norm_txt(input$man_tube), NA_character_),
        as.numeric(input$man_nb_tubes),
        ifelse(nzchar(norm_txt(input$man_prio)), norm_txt(input$man_prio), NA_character_),
        suppressWarnings(as.numeric(input$man_delai)),
        ifelse(nzchar(norm_txt(input$man_notes)), input$man_notes, NA_character_)
      )
    )

    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

    src_txt <- norm_txt(input$man_sources)
    if (nzchar(src_txt)) {
      lignes <- strsplit(src_txt, "\n", fixed = TRUE)[[1]]

      for (ligne in lignes) {
        parts <- trimws(strsplit(ligne, "\\|")[[1]])
        if (length(parts) < 2) next

        code <- parts[1]
        src_nom <- parts[2]
        cat <- if (length(parts) >= 3) parts[3] else ""

        if (!nzchar(src_nom)) next

        DBI::dbExecute(
          con,
          "
          INSERT OR IGNORE INTO sources_prelevement
          (mnemonique, nom, categorie)
          VALUES (?, ?, ?)
          ",
          params = list(
            ifelse(nzchar(code), code, NA_character_),
            src_nom,
            ifelse(nzchar(cat), cat, NA_character_)
          )
        )

        DBI::dbExecute(
          con,
          "
          INSERT OR IGNORE INTO analyse_sources
          (analyse_id, source_mnemonique, source_nom, categorie)
          VALUES (?, ?, ?, ?)
          ",
          params = list(
            id,
            ifelse(nzchar(code), code, NA_character_),
            src_nom,
            ifelse(nzchar(cat), cat, NA_character_)
          )
        )
      }
    }

    removeModal()
    refresh_analyses(refresh_analyses() + 1L)
    showNotification("Analyse ajoutée.", type = "message")
  })

  output$telecharger_sauvegarde_db <- downloadHandler(
    filename=function() {
      paste0(
        "EDUSILLAB_sauvegarde_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".db"
      )
    },
    content=function(file) {
      req(est_administrateur() || est_superutilisateur())

      # Sauvegarde cohérente via l'API SQLite.
      con_src <- ouvrir_db()
      on.exit(DBI::dbDisconnect(con_src), add=TRUE)

      con_dst <- DBI::dbConnect(RSQLite::SQLite(), file)
      on.exit(DBI::dbDisconnect(con_dst), add=TRUE)

      RSQLite::sqliteCopyDatabase(con_src, con_dst)
    },
    contentType="application/octet-stream"
  )

  # Import document
  observeEvent(input$lancer_import_analyse_direct, {
    req(peut_gerer_analyses())
    req(input$fichier_import_analyse_direct)

    f <- input$fichier_import_analyse_direct
    ext <- tolower(tools::file_ext(f$name))

    if (!ext %in% c("xlsx","xls")) {
      output$message_import_analyse_direct <- renderUI(
        div(class="danger-box","Le fichier doit être au format XLSX ou XLS.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit({
      if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    }, add=TRUE)

    resultat <- tryCatch(
      importer_classeur_edulab(
        f$datapath,
        con,
        remplacer_repertoire=TRUE
      ),
      error=function(e) e
    )

    if (inherits(resultat,"error")) {
      output$message_import_analyse_direct <- renderUI(
        div(
          class="danger-box",
          strong("Import impossible : "),
          conditionMessage(resultat)
        )
      )
      return()
    }

    refresh_analyses(refresh_analyses()+1L)
    refresh_imports(refresh_imports()+1L)

    try(
      journaliser(
        "IMPORT_REPERTOIRE_ANALYSES",
        paste0(
          "Fichier : ",f$name,
          "; analyses traitées : ",resultat$analyses,
          "; feuilles : ",resultat$feuilles
        )
      ),
      silent=TRUE
    )

    output$message_import_analyse_direct <- renderUI(
      div(
        class="success-box",
        strong("Répertoire importé avec succès. "),
        paste0(
          resultat$analyses,
          " analyse(s) traitée(s) à partir de ",
          resultat$feuilles,
          " feuille(s)."
        )
      )
    )
  })

  ouvrir_modal_import_analyses <- function() {
    req(est_administrateur() || est_superutilisateur())
    showModal(modalDialog(
      title = "Téléverser un répertoire ou un document",
      fileInput(
        "fichier_import_analyse",
        "Document",
        accept = c(
          ".xlsx", ".xls", ".csv", ".tsv",
          ".json", ".txt", ".md",
          ".pdf", ".doc", ".docx", ".rtf", ".odt", ".html", ".htm"
        )
      ),
      div(
        class = "warning-box",
        p(strong("XLSX/XLS : remplacement du Répertoire d'analyses actif.")),
        p(
          "Toutes les feuilles du classeur Excel sont examinées. ",
          "Les informations complémentaires sont fusionnées entre les feuilles. ",
          "Pour « # of tubes », la valeur explicite de la feuille « repetoire analyse GDH » est prioritaire."
        ),
        p(
          "CSV/TSV : import tabulaire. Les autres formats sont enregistrés dans ",
          "« Documents importés à réviser » afin d'éviter de créer de mauvaises analyses à partir d'un document non structuré."
        )
      ),
      br(),
      actionButton("lancer_import_analyse", "Importer", class = "btn-primary"),
      br(), br(),
      uiOutput("message_import_analyse"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  }

  observeEvent(input$ouvrir_import_analyse, {
    ouvrir_modal_import_analyses()
  })

  observeEvent(ouvrir_import_depuis_menu(), {
    if (ouvrir_import_depuis_menu() > 0) {
      ouvrir_modal_import_analyses()
    }
  })

  observeEvent(input$lancer_import_analyse, {
    req(peut_gerer_analyses())
    req(input$fichier_import_analyse)

    f <- input$fichier_import_analyse

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    res <- try(
      importer_fichier_analyse(
        f$datapath,
        f$name,
        con,
        utilisateur_connecte()$id
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      output$message_import_analyse <- renderUI(
        div(
          class = "danger-box",
          paste("Erreur d'importation :", as.character(res))
        )
      )
      return()
    }

    refresh_analyses(refresh_analyses() + 1L)
    refresh_imports(refresh_imports() + 1L)

    output$message_import_analyse <- renderUI(
      div(class = "success-box", res$message)
    )
  })

  output$table_imports_documents <- DT::renderDT({
    req(peut_gerer_analyses())
    refresh_imports()

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    d <- DBI::dbGetQuery(con, "
    SELECT
      id,
      nom_fichier AS Fichier,
      extension AS Format,
      statut AS Statut,
      date_import AS Date
    FROM imports_documents
    ORDER BY id DESC
    ")

    if (nrow(d) == 0) {
      return(DT::datatable(
        data.frame(Message = "Aucun document à réviser."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    d$Ouvrir <- sprintf(
      "<button class='btn btn-default btn-xs' onclick=\"Shiny.setInputValue('ouvrir_import_document_id', %s, {priority:'event'})\">Voir le texte</button>",
      d$id
    )

    DT::datatable(
      d[, c("Fichier","Format","Statut","Date","Ouvrir")],
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })

  observeEvent(input$ouvrir_import_document_id, {
    req(peut_gerer_analyses())
    id <- as.integer(input$ouvrir_import_document_id)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    d <- DBI::dbGetQuery(
      con,
      "SELECT * FROM imports_documents WHERE id = ?",
      params = list(id)
    )

    if (nrow(d) != 1) return()

    showModal(modalDialog(
      title = paste0("Document à réviser — ", d$nom_fichier[1]),
      div(class = "notes-box", d$contenu_extrait[1]),
      footer = modalButton("Fermer"),
      easyClose = TRUE
    ))
  })

  # Gestion Département / Px
  observeEvent(input$ouvrir_gestion_px, {
    req(peut_gerer_analyses())

    showModal(modalDialog(
      title = "Correspondance Département → Px",
      fluidRow(
        column(6, textInput("px_departement", "Département")),
        column(3, textInput("px_code", "Px"))
      ),
      actionButton("enregistrer_px", "Ajouter / modifier", class = "btn-success"),
      hr(),
      DT::DTOutput("table_px_modal"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))

    output$table_px_modal <- DT::renderDT({
      refresh_analyses()

      con <- ouvrir_db()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      dep <- DBI::dbGetQuery(con, "
      SELECT
        d.departement AS Département,
        COALESCE(d.px, '') AS Px
      FROM departements_px d
      ORDER BY d.departement
      ")

      DT::datatable(
        dep,
        rownames = FALSE,
        options = list(pageLength = 20, dom = "tip")
      )
    })
  })

  observeEvent(input$enregistrer_px, {
    req(peut_gerer_analyses())

    dd <- norm_upper(input$px_departement)
    px <- norm_upper(input$px_code)

    if (!nzchar(dd)) {
      showNotification("Département obligatoire.", type = "error")
      return()
    }

    con <- ouvrir_db()
    DBI::dbExecute(
      con,
      "
      INSERT INTO departements_px (departement, px)
      VALUES (?, ?)
      ON CONFLICT(departement)
      DO UPDATE SET px = excluded.px
      ",
      params = list(dd, ifelse(nzchar(px), px, NA_character_))
    )
    DBI::dbDisconnect(con)

    refresh_analyses(refresh_analyses() + 1L)
    showNotification("Correspondance Px enregistrée.", type = "message")
  })

  # ----------------------------------------------------------
  # RECHERCHE / AJOUT PATIENT
  # ----------------------------------------------------------

  observeEvent(input$rechercher_patient, {
    req(utilisateur_connecte()$perm_recherche_patient == 1)

    recherche <- norm_txt(input$recherche_patient)
    if (!nzchar(recherche)) return()

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    p <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM patients
      WHERE actif = 1
        AND (numero_dossier = ? OR medicare = ?)
      ",
      params = list(recherche, recherche)
    )

    if (nrow(p) == 1) {
      output$resultat_patient <- renderUI(
        wellPanel(
          h3(paste(p$nom[1], p$prenom[1])),
          p(strong("Dossier : "), p$numero_dossier[1]),
          p(strong("Medicare : "), p$medicare[1]),
          p(strong("Date de naissance : "), p$date_naissance[1]),
          p(strong("Sexe : "), p$sexe[1]),
          p(strong("Adresse : "), p$adresse[1])
        )
      )
    } else {
      output$resultat_patient <- renderUI(
        div(style = "color:red;", "Patient non trouvé.")
      )
    }
  })

  observeEvent(input$ajouter_patient, {
    req(utilisateur_connecte()$perm_patients == 1)

    output$formulaire_patient <- renderUI(
      wellPanel(
        h3("Ajouter un patient"),
        fluidRow(
          column(4, textInput("nouveau_nom", "Nom")),
          column(4, textInput("nouveau_prenom", "Prénom")),
          column(4, dateInput(
            "nouvelle_naissance",
            "Date de naissance",
            value = NA,
            min = as.Date("1900-01-01"),
            max = Sys.Date() + 3650,
            format = "dd-mm-yyyy"
          ))
        ),
        fluidRow(
          column(4, selectInput(
            "nouveau_sexe", "Sexe",
            choices = c(
              "F — Femme" = "F",
              "M — Homme" = "M",
              "BB — Nouveau-né" = "BB",
              "U — Inconnu" = "U"
            )
          )),
          column(4, textInput("nouveau_medicare", "Medicare")),
          column(4, textInput("nouveau_dossier", "Numéro de dossier"))
        ),
        textInput("nouvelle_adresse", "Adresse"),
        textInput("nouvelle_ville", "Ville"),
        textInput("nouvelle_province", "Province"),
        textInput("nouveau_code_postal", "Code postal"),
        textInput("nouveau_pays", "Pays", value = "Canada"),
        actionButton("enregistrer_patient", "Enregistrer", class = "btn-success"),
        br(), br(),
        uiOutput("message_patient")
      )
    )
  })

  observeEvent(input$enregistrer_patient, {
    req(utilisateur_connecte()$perm_patients == 1)

    nom <- norm_upper(input$nouveau_nom)
    prenom <- norm_txt(input$nouveau_prenom)
    dossier <- norm_txt(input$nouveau_dossier)
    medicare <- norm_txt(input$nouveau_medicare)

    if (!nzchar(nom) || !nzchar(prenom)) {
      output$message_patient <- renderUI(
        div(style = "color:red;", "Nom et prénom obligatoires.")
      )
      return()
    }

    if (!nzchar(dossier) && !nzchar(medicare)) {
      output$message_patient <- renderUI(
        div(style = "color:red;", "Dossier ou Medicare obligatoire.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    existe <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM patients
      WHERE
        (numero_dossier = ? AND ? <> '')
        OR
        (medicare = ? AND ? <> '')
      ",
      params = list(dossier, dossier, medicare, medicare)
    )

    if (nrow(existe) > 0) {
      output$message_patient <- renderUI(
        div(style = "color:red;", "Dossier ou Medicare déjà existant.")
      )
      return()
    }

    date_naissance <- if (
      is.null(input$nouvelle_naissance) ||
      is.na(input$nouvelle_naissance)
    ) NA_character_ else as.character(input$nouvelle_naissance)

    DBI::dbExecute(
      con,
      "
      INSERT INTO patients
      (
        numero_dossier, medicare, nom, prenom,
        date_naissance, sexe, adresse, ville,
        province, code_postal, pays, actif
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
      ",
      params = list(
        ifelse(nzchar(dossier), dossier, NA_character_),
        ifelse(nzchar(medicare), medicare, NA_character_),
        nom,
        prenom,
        date_naissance,
        input$nouveau_sexe,
        norm_txt(input$nouvelle_adresse),
        norm_txt(input$nouvelle_ville),
        norm_txt(input$nouvelle_province),
        norm_upper(input$nouveau_code_postal),
        norm_txt(input$nouveau_pays)
      )
    )

    output$message_patient <- renderUI(
      div(class = "success-box", "Patient ajouté.")
    )
  })

  # ----------------------------------------------------------
  # PATIENT DANS REQUISITION
  # ----------------------------------------------------------

  observeEvent(input$req_rechercher_patient, {
    dossier <- norm_txt(input$req_numero_dossier)
    medicare <- norm_txt(input$req_medicare)

    if (!nzchar(dossier) && !nzchar(medicare)) {
      output$req_patient_message <- renderUI(
        div(style = "color:red;", "Entrez le dossier ou le Medicare.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    p <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM patients
      WHERE actif = 1
        AND (numero_dossier = ? OR medicare = ?)
      ",
      params = list(dossier, medicare)
    )

    if (nrow(p) == 1) {
      patient_requisition(p)

      updateTextInput(
        session, "req_numero_dossier",
        value = ifelse(is.na(p$numero_dossier[1]), "", p$numero_dossier[1])
      )
      updateTextInput(
        session, "req_medicare",
        value = ifelse(is.na(p$medicare[1]), "", p$medicare[1])
      )
      updateTextInput(session, "req_nom", value = p$nom[1])
      updateTextInput(session, "req_prenom", value = p$prenom[1])

      if (!is.na(p$date_naissance[1]) && nzchar(p$date_naissance[1])) {
        updateDateInput(
          session, "req_date_naissance",
          value = as.Date(p$date_naissance[1])
        )
      }

      updateSelectInput(session, "req_sexe", selected = p$sexe[1])
      updateTextInput(
        session, "req_adresse",
        value = ifelse(is.na(p$adresse[1]), "", p$adresse[1])
      )
      updateTextInput(
        session, "req_ville",
        value = ifelse(is.na(p$ville[1]), "", p$ville[1])
      )
      updateTextInput(
        session, "req_province",
        value = ifelse(is.na(p$province[1]), "", p$province[1])
      )
      updateTextInput(
        session, "req_code_postal",
        value = ifelse(is.na(p$code_postal[1]), "", p$code_postal[1])
      )
      updateTextInput(
        session, "req_pays",
        value = ifelse(is.na(p$pays[1]), "Canada", p$pays[1])
      )

      output$req_patient_message <- renderUI(
        div(style = "color:green;font-weight:bold;", "Patient trouvé.")
      )

      session$sendCustomMessage("focusOrder", list())
    } else if (nrow(p) > 1) {
      patient_requisition(NULL)
      output$req_patient_message <- renderUI(
        div(
          style = "color:red;font-weight:bold;",
          "Conflit : plusieurs patients correspondent aux identifiants."
        )
      )
    } else {
      patient_requisition(NULL)
      output$req_patient_message <- renderUI(
        div(
          style = "color:orange;font-weight:bold;",
          "Patient non trouvé."
        )
      )
    }
  })

  # ----------------------------------------------------------
  # ORDER PAR MNEMONIQUE
  # ----------------------------------------------------------

  ajouter_analyse_order_final <- function(a, source_prelevement="", source_autre="") {
    ordre <- order_actuel()

    if (a$id[1] %in% ordre$id) {
      output$message_order <- renderUI(
        div(
          style = "color:orange;font-weight:bold;",
          paste(a$mnemonique[1], "—", a$nom[1], "est déjà présent.")
        )
      )
      updateTextInput(session, "req_order_mnemo", value = "")
      session$sendCustomMessage("focusOrder", list())
      return()
    }

    ordre <- rbind(
      ordre,
      data.frame(
        id = a$id[1],
        mnemonique = a$mnemonique[1],
        nom = a$nom[1],
        departement = a$departement[1],
        px = a$px[1],
        source_prelevement = norm_txt(source_prelevement),
        source_autre = norm_txt(source_autre),
        stringsAsFactors = FALSE
      )
    )

    order_actuel(ordre)

    source_aff <- if (nzchar(norm_txt(source_prelevement))) {
      if (toupper(norm_txt(source_prelevement)) == "AUTRE" && nzchar(norm_txt(source_autre))) {
        paste0(" — Source : ", norm_txt(source_autre))
      } else {
        paste0(" — Source : ", norm_txt(source_prelevement))
      }
    } else ""

    output$message_order <- renderUI(
      div(
        style = "color:green;font-weight:bold;",
        paste0(
          a$mnemonique[1], " — ", a$nom[1],
          ifelse(nzchar(a$px[1]), paste0(" — Px: ", a$px[1]), ""),
          source_aff
        )
      )
    )

    updateTextInput(session, "req_order_mnemo", value = "")
    session$sendCustomMessage("focusOrder", list())
  }

  ajouter_analyse_order <- function(a) {
    con_src <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con_src), add=TRUE)

    src <- DBI::dbGetQuery(
      con_src,
      "
      SELECT DISTINCT
        COALESCE(source_nom,'') AS source_nom,
        COALESCE(categorie,'') AS categorie
      FROM analyse_sources
      WHERE analyse_id=?
        AND TRIM(COALESCE(source_nom,'')) <> ''
      ORDER BY categorie, source_nom
      ",
      params=list(a$id[1])
    )

    if (nrow(src)==0) {
      ajouter_analyse_order_final(a)
      return()
    }

    session$userData$analyse_source_en_attente <- a

    valeurs <- src$source_nom
    libelles <- ifelse(
      nzchar(src$categorie),
      paste0(src$source_nom, " — ", src$categorie),
      src$source_nom
    )
    choix <- setNames(c(valeurs, "AUTRE"), c(libelles, "Autre — saisir manuellement"))

    showModal(
      modalDialog(
        title=paste0("Source du prélèvement — ",a$mnemonique[1]),
        p(strong(a$nom[1])),
        p("Cette analyse possède plusieurs sources possibles. Choisissez la source du spécimen."),
        selectInput(
          "order_source_selection",
          "Source",
          choices=choix,
          selected=valeurs[1]
        ),
        conditionalPanel(
          condition="input.order_source_selection == 'AUTRE'",
          textInput(
            "order_source_autre",
            "Autre source",
            placeholder="Écrire la source manuellement"
          )
        ),
        footer=tagList(
          modalButton("Annuler"),
          actionButton(
            "confirmer_source_order",
            "Ajouter l'analyse",
            class="btn-primary"
          )
        ),
        easyClose=FALSE
      )
    )
  }

  observeEvent(input$confirmer_source_order, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    a <- session$userData$analyse_source_en_attente
    req(!is.null(a), nrow(a)==1)

    source <- norm_txt(input$order_source_selection)
    autre <- norm_txt(input$order_source_autre)

    if (!nzchar(source)) {
      showNotification("Choisissez une source.", type="error")
      return()
    }

    if (toupper(source)=="AUTRE" && !nzchar(autre)) {
      showNotification("Écrivez la source dans le champ « Autre source ».", type="error")
      return()
    }

    removeModal()
    session$userData$analyse_source_en_attente <- NULL
    ajouter_analyse_order_final(a, source, autre)
  })

  observeEvent(input$order_enter, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    mn <- norm_upper(input$order_enter)

    if (!nzchar(mn)) {
      session$sendCustomMessage("focusOrder", list())
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    a <- DBI::dbGetQuery(
      con,
      "
      SELECT
        a.id,
        UPPER(a.mnemonique) AS mnemonique,
        a.nom,
        COALESCE(a.departement, '') AS departement,
        COALESCE(dp.px, '') AS px
      FROM analyses a
      LEFT JOIN departements_px dp
        ON UPPER(dp.departement) = UPPER(a.departement)
      WHERE UPPER(a.mnemonique) = ?
        AND a.actif = 1
      ORDER BY a.departement, a.nom
      ",
      params = list(mn)
    )

    if (nrow(a) == 0) {
      output$message_order <- renderUI(
        div(
          style = "color:red;font-weight:bold;",
          paste("Mnémonique non reconnu :", mn)
        )
      )
      session$sendCustomMessage("focusOrder", list())
      return()
    }

    if (nrow(a) == 1) {
      ajouter_analyse_order(a)
      return()
    }

    # Certains mnémoniques du répertoire sont utilisés par plusieurs
    # analyses. On demande alors explicitement laquelle est voulue.
    choix <- a$id
    names(choix) <- paste0(
      a$nom, " — ", a$departement,
      ifelse(nzchar(a$px), paste0(" — Px ", a$px), "")
    )

    session$userData$order_choix_table <- a

    showModal(modalDialog(
      title = paste0("Plusieurs analyses utilisent le mnémonique ", mn),
      p("Choisissez l'analyse désirée :"),
      radioButtons(
        "order_choix_analyse_id",
        NULL,
        choices = choix
      ),
      footer = tagList(
        modalButton("Annuler"),
        actionButton(
          "confirmer_order_choix",
          "Ajouter",
          class = "btn-primary"
        )
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirmer_order_choix, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    req(input$order_choix_analyse_id)

    a <- session$userData$order_choix_table
    if (is.null(a)) return()

    sel <- a[a$id == as.integer(input$order_choix_analyse_id), , drop = FALSE]
    if (nrow(sel) != 1) return()

    removeModal()
    ajouter_analyse_order(sel)
  })

  output$table_order <- renderTable({
    o <- order_actuel()

    if (nrow(o) == 0) return(NULL)

    source_aff <- ifelse(
      toupper(norm_txt(o$source_prelevement))=="AUTRE",
      o$source_autre,
      o$source_prelevement
    )

    ordre_affichage <- data.frame(
      Order = o$mnemonique,
      Name = o$nom,
      Source = source_aff,
      Px = o$px,
      Departement = o$departement,
      check.names = FALSE
    )
    names(ordre_affichage)[5] <- "D\u00e9partement"
    ordre_affichage
  }, striped = TRUE, bordered = TRUE)

  observeEvent(input$supprimer_dernier_order, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    o <- order_actuel()
    if (nrow(o) == 0) return()
    order_actuel(o[-nrow(o), , drop = FALSE])
    session$sendCustomMessage("focusOrder", list())
  })

  observeEvent(input$vider_order, {
    req(utilisateur_connecte()$perm_ajout_tests == 1)
    order_actuel(order_vide())
    updateTextInput(session, "req_order_mnemo", value = "")
    session$sendCustomMessage("focusOrder", list())
  })

  # ----------------------------------------------------------
  # ENREGISTRER REQUISITION
  # ----------------------------------------------------------

  observeEvent(input$req_enregistrer, {
    req(utilisateur_connecte()$perm_requisition == 1)

    dossier <- norm_txt(input$req_numero_dossier)
    medicare <- norm_txt(input$req_medicare)
    nom <- norm_upper(input$req_nom)
    prenom <- norm_txt(input$req_prenom)
    ordre <- order_actuel()

    if (!nzchar(nom) || !nzchar(prenom)) {
      output$req_message <- renderUI(
        div(style = "color:red;", "Nom et prénom obligatoires.")
      )
      return()
    }

    if (!nzchar(dossier) && !nzchar(medicare)) {
      output$req_message <- renderUI(
        div(style = "color:red;", "Dossier ou Medicare obligatoire.")
      )
      return()
    }

    if (nrow(ordre) == 0) {
      output$req_message <- renderUI(
        div(style = "color:red;", "Au moins une analyse est nécessaire.")
      )
      return()
    }

    con <- ouvrir_db()
    DBI::dbBegin(con)

    erreur <- FALSE
    numero_requisition <- NA_character_

    tryCatch({

      patient_dossier <- data.frame()
      patient_medicare <- data.frame()

      if (nzchar(dossier)) {
        patient_dossier <- DBI::dbGetQuery(
          con,
          "SELECT * FROM patients WHERE numero_dossier = ?",
          params = list(dossier)
        )
      }

      if (nzchar(medicare)) {
        patient_medicare <- DBI::dbGetQuery(
          con,
          "SELECT * FROM patients WHERE medicare = ?",
          params = list(medicare)
        )
      }

      if (
        nrow(patient_dossier) == 1 &&
        nrow(patient_medicare) == 1 &&
        patient_dossier$id[1] != patient_medicare$id[1]
      ) {
        stop(
          "Le numéro de dossier et le Medicare correspondent à deux patients différents."
        )
      }

      if (nrow(patient_dossier) == 1) {
        patient <- patient_dossier
      } else if (nrow(patient_medicare) == 1) {
        patient <- patient_medicare
      } else {
        patient <- data.frame()
      }

      if (nrow(patient) == 0) {
        if (utilisateur_connecte()$perm_patients != 1) {
          stop(
            "Patient inexistant. Votre niveau d'accès ne permet pas de créer un patient."
          )
        }

        date_naissance <- if (
          is.null(input$req_date_naissance) ||
          is.na(input$req_date_naissance)
        ) NA_character_ else as.character(input$req_date_naissance)

        DBI::dbExecute(
          con,
          "
          INSERT INTO patients
          (
            numero_dossier, medicare, nom, prenom,
            date_naissance, sexe, adresse, ville,
            province, code_postal, pays, actif
          )
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
          ",
          params = list(
            ifelse(nzchar(dossier), dossier, NA_character_),
            ifelse(nzchar(medicare), medicare, NA_character_),
            nom,
            prenom,
            date_naissance,
            input$req_sexe,
            norm_txt(input$req_adresse),
            norm_txt(input$req_ville),
            norm_txt(input$req_province),
            norm_upper(input$req_code_postal),
            norm_txt(input$req_pays)
          )
        )

        patient_id <- DBI::dbGetQuery(
          con, "SELECT last_insert_rowid() AS id"
        )$id[1]
      } else {
        patient_id <- patient$id[1]
      }

      # UN SEUL Req # pour tous les tests saisis maintenant.
      numero_requisition <- generer_numero_requisition(con)

      DBI::dbExecute(
        con,
        "
        INSERT INTO requisitions
        (
          numero_requisition, patient_id, priorite,
          prescripteur, commentaire, date_prelevement,
          heure_prelevement, cree_par, saisi_initiales, saisi_matricule, statut
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'ACTIVE')
        ",
        params = list(
          numero_requisition,
          patient_id,
          input$req_priorite,
          norm_txt(input$req_prescripteur),
          norm_txt(input$req_commentaire),
          as.character(input$req_coll_date),
          norm_txt(input$req_coll_time),
          utilisateur_connecte()$id,
          utilisateur_connecte()$initiales,
          utilisateur_connecte()$matricule
        )
      )

      requisition_id <- DBI::dbGetQuery(
        con, "SELECT last_insert_rowid() AS id"
      )$id[1]

      # Tous les tests sont liés à la même requisition_id.
      for (i in seq_len(nrow(ordre))) {
        DBI::dbExecute(
          con,
          "
          INSERT INTO requisition_analyses
          (requisition_id, analyse_id, source_prelevement, source_autre)
          VALUES (?, ?, ?, ?)
          ",
          params = list(
            requisition_id,
            ordre$id[i],
            ordre$source_prelevement[i],
            ordre$source_autre[i]
          )
        )
      }

      infos_tests <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT a.id,a.mnemonique,a.nom,COALESCE(a.departement,'') AS departement,",
          "COALESCE(dp.px,'') AS px,COALESCE(a.tube_prelevement,'') AS tube_prelevement,a.nb_tubes,",
          "COALESCE(ra.source_prelevement,'') AS source_prelevement,",
          "COALESCE(ra.source_autre,'') AS source_autre ",
          "FROM requisition_analyses ra ",
          "JOIN analyses a ON a.id=ra.analyse_id ",
          "LEFT JOIN departements_px dp ON UPPER(dp.departement)=UPPER(a.departement) ",
          "WHERE ra.requisition_id=? ",
          "ORDER BY a.departement,a.mnemonique"
        ),
        params=list(requisition_id)
      )

      patient_label <- DBI::dbGetQuery(con, "SELECT * FROM patients WHERE id=?", params=list(patient_id))
      source_effective <- ifelse(
        toupper(norm_txt(infos_tests$source_prelevement))=="AUTRE",
        norm_txt(infos_tests$source_autre),
        norm_txt(infos_tests$source_prelevement)
      )
      cle_groupes <- paste(
        ifelse(nzchar(infos_tests$departement), infos_tests$departement, "SANS DEPARTEMENT"),
        ifelse(nzchar(source_effective), source_effective, "SANS SOURCE"),
        sep="|||"
      )
      groupes <- split(infos_tests, cle_groupes)
      labels_crees <- list()
      compteur_label <- 0L

      for (cle_groupe in names(groupes)) {
        gd <- groupes[[cle_groupe]]
        dep <- if (nzchar(gd$departement[1])) gd$departement[1] else "SANS DEPARTEMENT"

        nb <- suppressWarnings(as.numeric(gd$nb_tubes))
        nb_valides <- nb[!is.na(nb) & nb > 0]

        nombre_etiquettes <- if (length(nb_valides) == 0) {
          1L
        } else {
          as.integer(ceiling(max(nb_valides)))
        }

        quantite <- as.character(nombre_etiquettes)

        px_nonvide <- gd$px[nzchar(gd$px)]
        px_label <- if (length(px_nonvide)) px_nonvide[1] else "X"

        tubes <- unique(norm_txt(gd$tube_prelevement))
        tubes <- tubes[nzchar(tubes)]
        contenant <- if (length(tubes)) paste(tubes, collapse=" / ") else "NON PRECISE"

        source_effective_gd <- ifelse(
          toupper(norm_txt(gd$source_prelevement))=="AUTRE",
          norm_txt(gd$source_autre),
          norm_txt(gd$source_prelevement)
        )
        source_effective_gd <- unique(source_effective_gd[nzchar(source_effective_gd)])
        source_label <- if (length(source_effective_gd)) source_effective_gd[1] else ""

        analyses_label <- paste(
          unique(
            ifelse(
              nzchar(source_label),
              paste0(gd$mnemonique, " [", source_label, "]"),
              gd$mnemonique
            )
          ),
          collapse=", "
        )

        location_label <- norm_txt(input$req_location)
        if (!nzchar(location_label)) location_label <- "NON PRECISEE"

        # V25 :
        # Un seul spécimen logique est créé pour ce groupe.
        # Si 2 ou 3 étiquettes sont nécessaires, elles sont des copies du même
        # spécimen et utilisent donc le même BC# et le même numéro de spécimen.
        code_barre <- generer_code_barre(con)
        numero_specimen <- generer_numero_specimen(
          con,
          as.character(input$req_coll_date),
          px_label,
          input$req_priorite
        )

        compteur_label <- compteur_label + 1L

        DBI::dbExecute(con, "
          INSERT INTO specimens
          (requisition_id,patient_id,numero_specimen,code_barre,date_specimen,
           departement,px,priorite,location,analyses,quantite,contenant,
           groupe_etiquette,cree_par,saisi_initiales,saisi_matricule,source_prelevement)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ", params=list(
          requisition_id,patient_id,numero_specimen,code_barre,as.character(input$req_coll_date),
          dep,px_label,input$req_priorite,location_label,analyses_label,quantite,contenant,
          compteur_label,utilisateur_connecte()$id,
          utilisateur_connecte()$initiales,utilisateur_connecte()$matricule,
          source_label
        ))

        # Plusieurs copies physiques, toutes avec le MÊME BC#.
        for (copie_etiquette in seq_len(nombre_etiquettes)) {
          labels_crees[[length(labels_crees)+1L]] <- data.frame(
            numero_requisition=numero_requisition,
            numero_specimen=numero_specimen,
            code_barre=code_barre,
            nom=norm_upper(patient_label$nom[1]),
            prenom=norm_upper(patient_label$prenom[1]),
            numero_dossier=norm_txt(patient_label$numero_dossier[1]),
            medicare=norm_txt(patient_label$medicare[1]),
            sexe=norm_txt(patient_label$sexe[1]),
            location=location_label,
            analyses=analyses_label,
            quantite=quantite,
            contenant=contenant,
            departement=dep,
            px=px_label,
            saisi_nom_complet=trimws(paste(norm_txt(utilisateur_connecte()$prenom), norm_txt(utilisateur_connecte()$nom))),
            date_saisie=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            signature_initiales=norm_txt(utilisateur_connecte()$initiales),
            stringsAsFactors=FALSE
          )
        }
      }

      dernieres_etiquettes(do.call(rbind,labels_crees))
      DBI::dbCommit(con)

    }, error = function(e) {
      erreur <<- TRUE
      try(DBI::dbRollback(con), silent = TRUE)

      output$req_message <- renderUI(
        div(class = "danger-box", paste("Erreur :", e$message))
      )
    })

    DBI::dbDisconnect(con)

    if (!erreur) {
      output$req_message <- renderUI(
        div(
          class = "success-box",
          h4("Réquisition enregistrée"),
          p(
            strong("Req # : "),
            span(class = "req-number", numero_requisition)
          ),
          p(
            strong("Analyses : "),
            paste(ordre$mnemonique, collapse = ", ")
          ),
          p(
            strong("Entrée par : "),
            code_utilisateur()
          )
        )
      )

      labels_pdf <- dernieres_etiquettes()
      if (!is.null(labels_pdf) && nrow(labels_pdf)>0) {
        conp <- ouvrir_db()
        imp <- DBI::dbGetQuery(conp, "
          SELECT * FROM materiel_impression
          WHERE actif=1 ORDER BY par_defaut DESC,id LIMIT 1
        ")
        nom_pdf <- paste0("apercu_etiquettes_REQ_",numero_requisition,".pdf")
        chemin_pdf <- file.path(preview_dir,nom_pdf)
        creer_pdf_selon_imprimante(labels_pdf,chemin_pdf,conp)
        DBI::dbDisconnect(conp)
        dernier_pdf_etiquettes(chemin_pdf)
      }

      # Le prochain enregistrement = nouvelle réquisition = nouveau Req #.
      order_actuel(order_vide())
      updateTextInput(session, "req_order_mnemo", value = "")
    }
  })

  output$zone_apercu_etiquettes <- renderUI({
    labels <- dernieres_etiquettes()
    pdf <- dernier_pdf_etiquettes()

    if (is.null(labels) || nrow(labels) == 0 || is.null(pdf)) {
      return(NULL)
    }

    tagList(
      div(
        class = "info-box",
        strong("Impression initiale : "),
        "toutes les étiquettes générées pour cette saisie seront imprimées ensemble."
      ),

      tags$iframe(
        src = paste0(
          "edulab_labels/",
          basename(pdf),
          "?v=",
          as.integer(Sys.time())
        ),
        style = "width:100%; height:520px; border:1px solid #bbb;"
      ),

      br(),

      downloadButton(
        "telecharger_pdf_etiquettes",
        "Télécharger le PDF complet"
      ),

      actionButton(
        "imprimer_pdf_etiquettes",
        "Imprimer toutes les étiquettes",
        class = "btn-success"
      )
    )
  })

  output$telecharger_pdf_etiquettes <- downloadHandler(
    filename=function() {
      p <- dernier_pdf_etiquettes()
      if (is.null(p)) "apercu_etiquettes.pdf" else basename(p)
    },
    content=function(file) {
      p <- dernier_pdf_etiquettes()
      req(!is.null(p),file.exists(p))
      file.copy(p,file,overwrite=TRUE)
    },
    contentType="application/pdf"
  )

  observeEvent(input$imprimer_pdf_etiquettes, {
    p <- dernier_pdf_etiquettes()
    req(!is.null(p),file.exists(p))
    showModal(modalDialog(
      title="Impression des étiquettes",
      p("Le PDF a été généré. Après validation, ouvrez-le puis utilisez la commande Imprimer de macOS ou Windows."),
      p(
        strong("Imprimante USB/Bluetooth/réseau : "),
        "EduLab mémorise l'imprimante, son mode de connexion et son numéro de série. ",
        "L'imprimante doit être installée ou jumelée dans macOS/Windows. ",
        "La sélection et l'impression physiques sont ensuite effectuées par la boîte d'impression du système."
      ),
      downloadButton("telecharger_pdf_etiquettes_modal","Ouvrir / télécharger le PDF"),
      easyClose=TRUE,
      footer=modalButton("Fermer")
    ))
  })

  output$telecharger_pdf_etiquettes_modal <- downloadHandler(
    filename=function() {
      p <- dernier_pdf_etiquettes()
      if (is.null(p)) "apercu_etiquettes.pdf" else basename(p)
    },
    content=function(file) {
      p <- dernier_pdf_etiquettes()
      req(!is.null(p),file.exists(p))
      file.copy(p,file,overwrite=TRUE)
    },
    contentType="application/pdf"
  )

  # ----------------------------------------------------------
  # CLIENTS / PATIENTS : IMPORT, MISE À JOUR ET EXPORT
  # ----------------------------------------------------------

  liste_clients <- reactiveVal(data.frame())

  charger_clients <- function() {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    d <- DBI::dbGetQuery(
      con,
      "
      SELECT
        numero_dossier AS `Numéro de dossier`,
        medicare AS Medicare,
        nom AS Nom,
        prenom AS Prénom,
        date_naissance AS `Date de naissance`,
        sexe AS Sexe,
        CASE WHEN actif = 1 THEN 'Oui' ELSE 'Non' END AS Actif,
        adresse AS Adresse,
        ville AS Ville,
        province AS Province,
        code_postal AS `Code postal`,
        pays AS Pays,
        latitude AS Latitude,
        longitude AS Longitude,
        date_creation AS `Date création`
      FROM patients
      ORDER BY nom, prenom
      "
    )

    liste_clients(d)
    d
  }

  observeEvent(input$menu_patients, {
    charger_clients()
  })

  output$table_clients <- DT::renderDT({
    d <- liste_clients()
    if (is.null(d) || nrow(d) == 0) d <- charger_clients()

    DT::datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "_START_ à _END_ sur _TOTAL_ clients",
          paginate = list(previous = "Précédent", `next` = "Suivant")
        )
      )
    )
  })

  observeEvent(input$import_clients_lancer, {
    req(utilisateur_connecte())
    req(utilisateur_connecte()$perm_patients == 1)

    f <- input$import_clients_fichier

    if (is.null(f) || !file.exists(f$datapath)) {
      output$message_import_clients <- renderUI(
        div(class = "danger-box", "Choisissez d'abord un fichier.")
      )
      return()
    }

    resultat <- tryCatch({
      brut <- lire_fichier_clients(f$datapath, f$name)
      tab <- standardiser_fichier_clients(brut)

      if (nrow(tab) == 0) stop("Aucune ligne client exploitable.")

      con <- ouvrir_db()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbBegin(con)
      ok <- FALSE
      on.exit({
        if (!ok) try(DBI::dbRollback(con), silent = TRUE)
      }, add = TRUE)

      res <- mettre_a_jour_clients_importes(con, tab)
      DBI::dbCommit(con)
      ok <- TRUE
      res
    }, error = function(e) e)

    if (inherits(resultat, "error")) {
      output$message_import_clients <- renderUI(
        div(
          class = "danger-box",
          strong("Import impossible : "),
          resultat$message
        )
      )
      return()
    }

    charger_clients()

    output$message_import_clients <- renderUI(
      div(
        class = "success-box",
        strong("Import terminé. "),
        paste0(
          resultat$ajoutes, " nouveau(x) client(s), ",
          resultat$maj, " client(s) mis à jour, ",
          resultat$ignores, " ligne(s) ignorée(s)."
        ),
        if (length(resultat$erreurs) > 0)
          tags$details(
            tags$summary("Voir les lignes ignorées"),
            tags$ul(lapply(resultat$erreurs, tags$li))
          )
      )
    )
  })

  obtenir_clients_export <- function() {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    DBI::dbGetQuery(
      con,
      "
      SELECT
        numero_dossier AS numero_dossier,
        medicare AS medicare,
        nom AS nom,
        prenom AS prenom,
        date_naissance AS date_naissance,
        sexe AS sexe,
        actif AS actif,
        adresse AS adresse,
        ville AS ville,
        province AS province,
        code_postal AS code_postal,
        pays AS pays,
        latitude AS latitude,
        longitude AS longitude,
        date_creation AS date_creation
      FROM patients
      ORDER BY nom, prenom
      "
    )
  }

  output$telecharger_clients_xlsx <- downloadHandler(
    filename = function() {
      paste0("EDUSILLAB_clients_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(utilisateur_connecte())
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop(
          "Pour télécharger en Excel, installez d'abord writexl avec : install.packages('writexl')"
        )
      }
      d <- obtenir_clients_export()
      writexl::write_xlsx(list(Clients = d), path = file)
    }
  )

  output$telecharger_clients_csv <- downloadHandler(
    filename = function() {
      paste0("EDUSILLAB_clients_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(utilisateur_connecte())
      d <- obtenir_clients_export()
      write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$telecharger_clients_tsv <- downloadHandler(
    filename = function() {
      paste0("EDUSILLAB_clients_", format(Sys.Date(), "%Y%m%d"), ".tsv")
    },
    content = function(file) {
      req(utilisateur_connecte())
      d <- obtenir_clients_export()
      write.table(
        d, file, sep = "\t", row.names = FALSE, na = "",
        quote = TRUE, fileEncoding = "UTF-8"
      )
    }
  )

  output$telecharger_clients_json <- downloadHandler(
    filename = function() {
      paste0("EDUSILLAB_clients_", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      req(utilisateur_connecte())
      d <- obtenir_clients_export()
      jsonlite::write_json(
        d, path = file, dataframe = "rows",
        pretty = TRUE, auto_unbox = TRUE, na = "null"
      )
    }
  )

  # ----------------------------------------------------------
  # ANNULATIONS ET TRAÇABILITÉ PATIENT
  # ----------------------------------------------------------

  refresh_contextes_annulation <- reactiveVal(0L)

  output$choix_contexte_annulation <- renderUI({
    refresh_contextes_annulation()
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    cts <- DBI::dbGetQuery(con,"SELECT libelle FROM contextes_annulation WHERE actif=1 ORDER BY libelle")
    selectInput("annulation_contexte","Contexte d'annulation",choices=cts$libelle)
  })

  output$choix_contexte_suppression <- renderUI({
    req(est_administrateur() || est_superutilisateur())

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    d <- DBI::dbGetQuery(
      con,
      "
      SELECT id,libelle
      FROM contextes_annulation
      WHERE actif=1
      ORDER BY libelle
      "
    )

    if (nrow(d)==0) {
      return(
        p(class="text-muted","Aucun contexte actif à supprimer.")
      )
    }

    selectInput(
      "contexte_annulation_a_supprimer",
      "Contexte à supprimer",
      choices=setNames(as.character(d$id),d$libelle)
    )
  })


  observeEvent(input$ajouter_contexte_annulation, {
    req(est_administrateur() || est_superutilisateur())
    lib <- norm_txt(input$nouveau_contexte_annulation)
    if (!nzchar(lib)) return()
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)
    DBI::dbExecute(con,"INSERT OR IGNORE INTO contextes_annulation(libelle,actif) VALUES (?,1)",params=list(lib))
    refresh_contextes_annulation(refresh_contextes_annulation()+1L)
    updateTextInput(session,"nouveau_contexte_annulation",value="")
    journaliser("AJOUT_CONTEXTE_ANNULATION",paste0("Contexte ajouté : ",lib))
  })

  observeEvent(input$annulation_reference, {
    ref <- norm_txt(input$annulation_reference)

    if (!nzchar(ref)) {
      updateSelectInput(
        session,
        "annulation_departement",
        choices=setNames("","Saisir une référence d'abord"),
        selected=""
      )
      output$annulation_reference_info <- renderUI(NULL)
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    spec <- tryCatch(
      trouver_specimen_par_reference(con,ref),
      error=function(e) data.frame()
    )

    if (nrow(spec)==1) {
      req_num <- spec$numero_requisition[1]
      dossier <- spec$numero_dossier[1]
    } else {
      req_num <- ref
      dossier <- ref
    }

    deps <- DBI::dbGetQuery(
      con,
      "
      SELECT DISTINCT COALESCE(s.departement,'') AS departement
      FROM specimens s
      JOIN requisitions r ON r.id=s.requisition_id
      JOIN patients p ON p.id=s.patient_id
      WHERE (p.numero_dossier=? OR r.numero_requisition=? OR s.code_barre=?)
        AND TRIM(COALESCE(s.departement,'')) <> ''
      ORDER BY departement
      ",
      params=list(dossier,req_num,ref)
    )

    if (nrow(deps)==0 && nrow(spec)==1 && nzchar(norm_txt(spec$departement[1]))) {
      deps <- data.frame(departement=spec$departement[1],stringsAsFactors=FALSE)
    }

    if (nrow(deps)>0) {
      valeurs <- unique(norm_upper(deps$departement))
      valeurs <- valeurs[nzchar(valeurs)]

      updateSelectInput(
        session,
        "annulation_departement",
        choices=setNames(valeurs,valeurs),
        selected=valeurs[1]
      )

      output$annulation_reference_info <- renderUI(
        div(
          class="info-box",
          strong("Département détecté : "),
          paste(valeurs,collapse=" / ")
        )
      )
    } else {
      updateSelectInput(
        session,
        "annulation_departement",
        choices=setNames("","Aucun département détecté"),
        selected=""
      )
      output$annulation_reference_info <- renderUI(
        div(class="warning-box","Aucun département trouvé pour cette référence.")
      )
    }
  }, ignoreInit=TRUE)

  annulation_cibles <- reactiveVal(data.frame())

  rechercher_cibles_annulation <- function(ref, portee, dep="", situation="AUTO") {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add=TRUE)

    ref <- norm_txt(ref)
    dep <- norm_upper(dep)
    situation <- toupper(norm_txt(situation))
    portee <- toupper(norm_txt(portee))

    if (!nzchar(ref)) return(data.frame())

    # Recherche directe d'un spécimen : BC#, numéro complet ou format abrégé.
    spec_ref <- tryCatch(
      trouver_specimen_par_reference(con, ref),
      error=function(e) data.frame()
    )

    if (portee=="SPECIMEN") {
      if (nrow(spec_ref)!=1) return(data.frame())

      cible <- DBI::dbGetQuery(
        con,
        "
        SELECT
          s.id,s.patient_id,s.requisition_id,s.numero_specimen,s.code_barre,
          s.departement,s.statut_reception,s.date_reception,
          s.recu_initiales,s.recu_matricule,
          r.numero_requisition,p.numero_dossier,p.nom,p.prenom
        FROM specimens s
        JOIN requisitions r ON r.id=s.requisition_id
        JOIN patients p ON p.id=s.patient_id
        WHERE s.id=?
          AND UPPER(COALESCE(s.statut_reception,'')) <> 'ANNULE'
        ",
        params=list(spec_ref$id[1])
      )
    } else {
      # Si la référence est un spécimen, on utilise son dossier/réquisition pour
      # retrouver la portée demandée. Sinon, on cherche directement dossier/Req.
      req_ref <- if (nrow(spec_ref)==1) spec_ref$numero_requisition[1] else ref
      dossier_ref <- if (nrow(spec_ref)==1) spec_ref$numero_dossier[1] else ref

      cible <- DBI::dbGetQuery(
        con,
        "
        SELECT
          s.id,s.patient_id,s.requisition_id,s.numero_specimen,s.code_barre,
          s.departement,s.statut_reception,s.date_reception,
          s.recu_initiales,s.recu_matricule,
          r.numero_requisition,p.numero_dossier,p.nom,p.prenom
        FROM specimens s
        JOIN requisitions r ON r.id=s.requisition_id
        JOIN patients p ON p.id=s.patient_id
        WHERE (p.numero_dossier=? OR r.numero_requisition=?)
          AND UPPER(COALESCE(s.statut_reception,'')) <> 'ANNULE'
        ORDER BY s.id
        ",
        params=list(dossier_ref, req_ref)
      )
    }

    if (nrow(cible)==0) return(cible)

    if (portee=="DEPARTEMENT") {
      if (!nzchar(dep)) return(data.frame())
      cible <- cible[
        toupper(trimws(ifelse(is.na(cible$departement),"",cible$departement)))==dep,
        ,drop=FALSE
      ]
    }

    if (nrow(cible)==0) return(cible)

    statut <- toupper(trimws(ifelse(is.na(cible$statut_reception),"",cible$statut_reception)))

    if (situation=="RECU") {
      cible <- cible[statut=="RECU",,drop=FALSE]
    } else if (situation %in% c("NON_RECU","PAS_PRELEVE")) {
      cible <- cible[statut!="RECU",,drop=FALSE]
    }

    cible
  }

  preparer_annulation <- function() {
    ref <- norm_txt(input$annulation_reference)
    portee <- norm_txt(input$annulation_portee)
    dep <- norm_upper(input$annulation_departement)
    situation <- norm_txt(input$annulation_situation)

    if (!nzchar(ref)) {
      output$message_annulation <- renderUI(
        div(class="danger-box","Saisissez un BC#, un numéro de spécimen, un dossier ou un numéro de réquisition.")
      )
      annulation_cibles(data.frame())
      return(data.frame())
    }

    if (portee=="DEPARTEMENT" && !nzchar(dep)) {
      output$message_annulation <- renderUI(
        div(class="danger-box","Indiquez le département à annuler.")
      )
      annulation_cibles(data.frame())
      return(data.frame())
    }

    cible <- rechercher_cibles_annulation(ref,portee,dep,situation)

    if (nrow(cible)==0) {
      output$message_annulation <- renderUI(
        div(
          class="warning-box",
          "Aucun spécimen non annulé ne correspond à la référence et à la situation sélectionnées."
        )
      )
      annulation_cibles(data.frame())
      return(data.frame())
    }

    # État réel avant annulation, conservé dans l'aperçu et la base.
    statut <- toupper(trimws(ifelse(is.na(cible$statut_reception),"",cible$statut_reception)))
    cible$Situation <- ifelse(
      statut=="RECU",
      "REÇU",
      ifelse(
        toupper(norm_txt(situation))=="PAS_PRELEVE",
        "PAS PRÉLEVÉ",
        "NON REÇU"
      )
    )

    annulation_cibles(cible)
    cible
  }

  observeEvent(input$previsualiser_annulation, {
    cible <- preparer_annulation()
    if (nrow(cible)>0) {
      output$message_annulation <- renderUI(
        div(
          class="info-box",
          paste0(
            nrow(cible),
            " spécimen(s) sélectionné(s). Vérifiez l'aperçu avant de confirmer l'annulation."
          )
        )
      )
    }
  })

  output$apercu_annulation <- renderUI({
    cible <- annulation_cibles()
    if (is.null(cible) || nrow(cible)==0) return(NULL)

    lignes <- lapply(seq_len(nrow(cible)), function(i) {
      tags$tr(
        tags$td(cible$numero_specimen[i]),
        tags$td(cible$code_barre[i]),
        tags$td(cible$numero_dossier[i]),
        tags$td(cible$departement[i]),
        tags$td(cible$Situation[i])
      )
    })

    tagList(
      h4("Spécimens ciblés"),
      tags$table(
        class="table table-bordered table-condensed",
        tags$thead(
          tags$tr(
            tags$th("N° spécimen"),
            tags$th("BC#"),
            tags$th("Dossier"),
            tags$th("Département"),
            tags$th("Situation")
          )
        ),
        tags$tbody(lignes)
      )
    )
  })

  observeEvent(input$annuler_specimens, {
    req(utilisateur_connecte()$perm_reception==1)

    contexte <- norm_txt(input$contexte_annulation)
    commentaire <- norm_txt(input$annulation_commentaire)
    situation_demandee <- toupper(norm_txt(input$annulation_situation))
    portee <- toupper(norm_txt(input$annulation_portee))

    if (!nzchar(contexte)) {
      output$message_annulation <- renderUI(
        div(class="danger-box","Choisissez un contexte d'annulation.")
      )
      return()
    }

    cible <- preparer_annulation()
    if (nrow(cible)==0) return()

    # Cohérence métier : un spécimen déjà reçu ne peut pas être déclaré
    # « pas prélevé ».
    statut_reel <- toupper(trimws(ifelse(is.na(cible$statut_reception),"",cible$statut_reception)))
    if (situation_demandee=="PAS_PRELEVE" && any(statut_reel=="RECU")) {
      output$message_annulation <- renderUI(
        div(
          class="danger-box",
          "Impossible : un spécimen déjà reçu ne peut pas être classé « prélèvement non effectué »."
        )
      )
      return()
    }

    # Préparer un résumé avant confirmation.
    n_recu <- sum(statut_reel=="RECU")
    n_non_recu <- sum(statut_reel!="RECU")

    showModal(
      modalDialog(
        title="Confirmer l'annulation",
        div(
          class="warning-box",
          strong(paste0(nrow(cible)," spécimen(s) seront annulé(s).")),
          br(),
          paste0("Reçus : ",n_recu," — Non reçus / pas prélevés : ",n_non_recu),
          br(),
          "Les informations de réception déjà enregistrées seront conservées dans la traçabilité."
        ),
        p(strong("Contexte : "),contexte),
        if (nzchar(commentaire)) p(strong("Commentaire : "),commentaire),
        footer=tagList(
          modalButton("Retour"),
          actionButton(
            "confirmer_annulation_finale",
            "Confirmer l'annulation",
            class="btn-danger"
          )
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$confirmer_annulation_finale, {
    req(utilisateur_connecte()$perm_reception==1)

    cible <- annulation_cibles()
    req(!is.null(cible), nrow(cible)>0)

    contexte <- norm_txt(input$contexte_annulation)
    commentaire <- norm_txt(input$annulation_commentaire)
    situation_demandee <- toupper(norm_txt(input$annulation_situation))
    portee <- toupper(norm_txt(input$annulation_portee))

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    DBI::dbBegin(con)
    ok <- FALSE
    on.exit({
      if (!ok && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con),silent=TRUE)
      }
    },add=TRUE)

    for (i in seq_len(nrow(cible))) {
      sid <- cible$id[i]

      courant <- DBI::dbGetQuery(
        con,
        "
        SELECT
          id,patient_id,requisition_id,numero_specimen,code_barre,
          departement,COALESCE(statut_reception,'') AS statut_reception,
          date_reception
        FROM specimens
        WHERE id=?
        ",
        params=list(sid)
      )

      if (nrow(courant)!=1) next
      if (toupper(norm_txt(courant$statut_reception[1]))=="ANNULE") next

      statut_avant <- toupper(norm_txt(courant$statut_reception[1]))
      apres_reception <- as.integer(statut_avant=="RECU")

      type_annulation <- if (situation_demandee=="PAS_PRELEVE") {
        "PAS_PRELEVE"
      } else if (statut_avant=="RECU") {
        "RECU"
      } else {
        "NON_RECU"
      }

      # Si l'utilisateur a choisi AUTO, le type est déduit du statut réel.
      if (situation_demandee=="AUTO") {
        type_annulation <- if (statut_avant=="RECU") "RECU" else "NON_RECU"
      }

      DBI::dbExecute(
        con,
        "
        UPDATE specimens
        SET
          statut_avant_annulation=?,
          type_annulation=?,
          annulation_apres_reception=?,
          statut_reception='ANNULE',
          annule_par=?,
          annule_initiales=?,
          annule_matricule=?,
          date_annulation=CURRENT_TIMESTAMP,
          contexte_annulation=?,
          motif_annulation=?
        WHERE id=?
        ",
        params=list(
          statut_avant,
          type_annulation,
          apres_reception,
          utilisateur_connecte()$id,
          utilisateur_connecte()$initiales,
          utilisateur_connecte()$matricule,
          contexte,
          commentaire,
          sid
        )
      )
    }

    # Statut des réquisitions : ANNULEE seulement si tous les spécimens
    # sont annulés. Sinon la réquisition reste active.
    req_ids <- unique(cible$requisition_id)
    for (rid in req_ids) {
      n_actif <- DBI::dbGetQuery(
        con,
        "
        SELECT COUNT(*) AS n
        FROM specimens
        WHERE requisition_id=?
          AND UPPER(COALESCE(statut_reception,''))<>'ANNULE'
        ",
        params=list(rid)
      )$n[1]

      if (n_actif==0) {
        DBI::dbExecute(
          con,
          "UPDATE requisitions SET statut='ANNULEE' WHERE id=?",
          params=list(rid)
        )
      }
    }

    DBI::dbCommit(con)
    ok <- TRUE

    removeModal()

    # Journaliser après COMMIT pour éviter les verrous SQLite.
    for (i in seq_len(nrow(cible))) {
      statut_avant <- toupper(norm_txt(cible$statut_reception[i]))
      type_annulation <- if (situation_demandee=="PAS_PRELEVE") {
        "PAS_PRELEVE"
      } else if (statut_avant=="RECU") {
        "RECU"
      } else {
        "NON_RECU"
      }

      journaliser(
        "ANNULATION_SPECIMEN",
        paste0(
          "Spécimen ",cible$numero_specimen[i],
          " / BC# ",cible$code_barre[i],
          "; situation avant annulation ",ifelse(nzchar(statut_avant),statut_avant,"NON_RECU"),
          "; type ",type_annulation,
          "; portée ",portee,
          "; contexte ",contexte,
          if (nzchar(commentaire)) paste0("; ",commentaire) else ""
        ),
        patient_id=cible$patient_id[i],
        requisition_id=cible$requisition_id[i],
        specimen_id=cible$id[i]
      )
    }

    output$message_annulation <- renderUI(
      div(
        class="success-box",
        paste0(
          nrow(cible),
          " spécimen(s) annulé(s) par ",
          code_utilisateur(),
          ". La réception antérieure, lorsqu'elle existe, a été conservée."
        )
      )
    )

    annulation_cibles(data.frame())
    output$apercu_annulation <- renderUI(NULL)
  })

  observeEvent(input$supprimer_contexte_annulation, {
    req(est_administrateur() || est_superutilisateur())

    id_contexte <- suppressWarnings(as.integer(input$contexte_annulation_a_supprimer))
    if (is.na(id_contexte)) {
      output$message_annulation <- renderUI(
        div(class="danger-box","Choisissez un contexte à supprimer.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    ctx <- DBI::dbGetQuery(
      con,
      "
      SELECT id,libelle,actif
      FROM contextes_annulation
      WHERE id=?
      LIMIT 1
      ",
      params=list(id_contexte)
    )

    if (nrow(ctx)!=1 || ctx$actif[1]!=1) {
      output$message_annulation <- renderUI(
        div(class="warning-box","Ce contexte n'est plus actif.")
      )
      return()
    }

    showModal(
      modalDialog(
        title="Supprimer le contexte",
        div(
          class="warning-box",
          p(strong("Êtes-vous sûr de vouloir supprimer ce contexte ?")),
          p(strong("Contexte : "),ctx$libelle[1]),
          p(
            class="text-muted",
            "Les anciennes annulations qui utilisent ce contexte resteront dans l'historique."
          )
        ),
        footer=tagList(
          modalButton("Cancel"),
          actionButton(
            "ok_supprimer_contexte_annulation",
            "OK",
            class="btn-danger"
          )
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$ok_supprimer_contexte_annulation, {
    req(est_administrateur() || est_superutilisateur())

    id_contexte <- suppressWarnings(as.integer(input$contexte_annulation_a_supprimer))
    req(!is.na(id_contexte))

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    ctx <- DBI::dbGetQuery(
      con,
      "
      SELECT id,libelle
      FROM contextes_annulation
      WHERE id=? AND actif=1
      LIMIT 1
      ",
      params=list(id_contexte)
    )

    if (nrow(ctx)!=1) {
      removeModal()
      output$message_annulation <- renderUI(
        div(class="warning-box","Le contexte a déjà été supprimé.")
      )
      return()
    }

    # Suppression logique : on conserve l'historique et les références existantes.
    DBI::dbExecute(
      con,
      "
      UPDATE contextes_annulation
      SET actif=0
      WHERE id=?
      ",
      params=list(id_contexte)
    )

    removeModal()

    journaliser(
      "SUPPRESSION_CONTEXTE_ANNULATION",
      paste0(
        "Contexte désactivé : ",
        ctx$libelle[1],
        " par ",
        code_utilisateur()
      )
    )

    output$message_annulation <- renderUI(
      div(
        class="success-box",
        paste0(
          "Contexte « ",
          ctx$libelle[1],
          " » supprimé. Les anciennes annulations restent conservées."
        )
      )
    )
  })

  uncancel_cibles <- reactiveVal(data.frame())

  observeEvent(input$uncancel_reference, {
    ref <- norm_txt(input$uncancel_reference)

    if (!nzchar(ref)) {
      updateSelectInput(
        session,"uncancel_departement",
        choices=setNames("","Saisir une référence d'abord"),selected=""
      )
      output$uncancel_reference_info <- renderUI(NULL)
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    spec <- tryCatch(
      trouver_specimen_par_reference(con,ref),
      error=function(e) data.frame()
    )

    if (nrow(spec)==1) {
      dossier <- spec$numero_dossier[1]
      req_num <- spec$numero_requisition[1]
    } else {
      dossier <- ref
      req_num <- ref
    }

    deps <- DBI::dbGetQuery(
      con,
      "
      SELECT DISTINCT COALESCE(s.departement,'') AS departement
      FROM specimens s
      JOIN requisitions r ON r.id=s.requisition_id
      JOIN patients p ON p.id=s.patient_id
      WHERE (p.numero_dossier=? OR r.numero_requisition=? OR s.code_barre=?)
        AND UPPER(COALESCE(s.statut_reception,''))='ANNULE'
        AND TRIM(COALESCE(s.departement,'')) <> ''
      ORDER BY departement
      ",
      params=list(dossier,req_num,ref)
    )

    if (nrow(deps)>0) {
      valeurs <- unique(norm_upper(deps$departement))
      updateSelectInput(
        session,"uncancel_departement",
        choices=setNames(valeurs,valeurs),selected=valeurs[1]
      )
      output$uncancel_reference_info <- renderUI(
        div(class="info-box",strong("Département(s) annulé(s) détecté(s) : "),paste(valeurs,collapse=" / "))
      )
    } else {
      updateSelectInput(
        session,"uncancel_departement",
        choices=setNames("","Aucun département annulé détecté"),selected=""
      )
      output$uncancel_reference_info <- renderUI(
        div(class="warning-box","Aucun spécimen annulé détecté pour cette référence.")
      )
    }
  },ignoreInit=TRUE)

  rechercher_cibles_uncancel <- function(ref,portee,dep="") {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    ref <- norm_txt(ref)
    portee <- toupper(norm_txt(portee))
    dep <- norm_upper(dep)
    if (!nzchar(ref)) return(data.frame())

    spec <- tryCatch(
      trouver_specimen_par_reference(con,ref),
      error=function(e) data.frame()
    )

    if (portee=="SPECIMEN" && nrow(spec)==1) {
      d <- DBI::dbGetQuery(
        con,
        "
        SELECT
          s.id,s.patient_id,s.requisition_id,s.numero_specimen,s.code_barre,
          s.departement,s.statut_reception,s.statut_avant_annulation,
          s.type_annulation,s.contexte_annulation,
          r.numero_requisition,p.numero_dossier,p.nom,p.prenom
        FROM specimens s
        JOIN requisitions r ON r.id=s.requisition_id
        JOIN patients p ON p.id=s.patient_id
        WHERE s.id=?
          AND UPPER(COALESCE(s.statut_reception,''))='ANNULE'
        ",
        params=list(spec$id[1])
      )
    } else {
      dossier <- if (nrow(spec)==1) spec$numero_dossier[1] else ref
      req_num <- if (nrow(spec)==1) spec$numero_requisition[1] else ref

      d <- DBI::dbGetQuery(
        con,
        "
        SELECT
          s.id,s.patient_id,s.requisition_id,s.numero_specimen,s.code_barre,
          s.departement,s.statut_reception,s.statut_avant_annulation,
          s.type_annulation,s.contexte_annulation,
          r.numero_requisition,p.numero_dossier,p.nom,p.prenom
        FROM specimens s
        JOIN requisitions r ON r.id=s.requisition_id
        JOIN patients p ON p.id=s.patient_id
        WHERE (p.numero_dossier=? OR r.numero_requisition=? OR s.code_barre=?)
          AND UPPER(COALESCE(s.statut_reception,''))='ANNULE'
        ORDER BY s.id
        ",
        params=list(dossier,req_num,ref)
      )
    }

    if (nrow(d)>0 && portee=="DEPARTEMENT") {
      if (!nzchar(dep)) return(data.frame())
      d <- d[norm_upper(d$departement)==dep,,drop=FALSE]
    }

    d
  }

  observeEvent(input$previsualiser_uncancel, {
    d <- rechercher_cibles_uncancel(
      input$uncancel_reference,
      input$uncancel_portee,
      input$uncancel_departement
    )
    uncancel_cibles(d)

    if (nrow(d)==0) {
      output$message_uncancel <- renderUI(
        div(class="warning-box","Aucun spécimen annulé correspondant.")
      )
      return()
    }

    output$message_uncancel <- renderUI(
      div(class="info-box",paste0(nrow(d)," spécimen(s) annulé(s) peuvent être réactivé(s)."))
    )
  })

  output$apercu_uncancel <- renderUI({
    d <- uncancel_cibles()
    if (is.null(d) || nrow(d)==0) return(NULL)

    tags$table(
      class="table table-bordered table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Spécimen"),tags$th("BC#"),tags$th("Dossier"),
          tags$th("Département"),tags$th("Statut avant annulation"),
          tags$th("Contexte")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(d)),function(i) {
          tags$tr(
            tags$td(d$numero_specimen[i]),
            tags$td(d$code_barre[i]),
            tags$td(d$numero_dossier[i]),
            tags$td(d$departement[i]),
            tags$td(d$statut_avant_annulation[i]),
            tags$td(d$contexte_annulation[i])
          )
        })
      )
    )
  })

  observeEvent(input$uncancel_specimens, {
    req(utilisateur_connecte()$perm_reception==1)

    d <- rechercher_cibles_uncancel(
      input$uncancel_reference,
      input$uncancel_portee,
      input$uncancel_departement
    )

    if (nrow(d)==0) {
      output$message_uncancel <- renderUI(
        div(class="warning-box","Aucun spécimen annulé correspondant.")
      )
      return()
    }

    uncancel_cibles(d)

    showModal(
      modalDialog(
        title="Confirmer la réactivation",
        div(
          class="warning-box",
          p(strong("Êtes-vous sûr de vouloir réactiver ce ou ces spécimens ?")),
          p(strong("Nombre : "),nrow(d)),
          p("L'historique de l'annulation sera conservé.")
        ),
        p(
          strong("Commentaire : "),
          ifelse(
            nzchar(norm_txt(input$uncancel_commentaire)),
            norm_txt(input$uncancel_commentaire),
            "Aucun commentaire"
          )
        ),
        footer=tagList(
          modalButton("Cancel"),
          actionButton("ok_uncancel_specimens","OK",class="btn-success")
        ),
        easyClose=FALSE
      )
    )
  })

  observeEvent(input$ok_uncancel_specimens, {
    req(utilisateur_connecte()$perm_reception==1)
    d <- uncancel_cibles()
    req(!is.null(d),nrow(d)>0)

    commentaire <- norm_txt(input$uncancel_commentaire)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    DBI::dbBegin(con)
    ok <- FALSE
    on.exit({
      if (!ok && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con),silent=TRUE)
      }
    },add=TRUE)

    for (i in seq_len(nrow(d))) {
      ancien <- norm_upper(d$statut_avant_annulation[i])

      statut_restaure <- if (ancien=="RECU") {
        "RECU"
      } else if (nzchar(ancien) && ancien!="ANNULE") {
        ancien
      } else {
        "EN_ATTENTE"
      }

      DBI::dbExecute(
        con,
        "
        UPDATE specimens
        SET
          statut_reception=?,
          reactive_par=?,
          reactive_initiales=?,
          reactive_matricule=?,
          date_reactivation=CURRENT_TIMESTAMP,
          commentaire_reactivation=?
        WHERE id=?
          AND UPPER(COALESCE(statut_reception,''))='ANNULE'
        ",
        params=list(
          statut_restaure,
          utilisateur_connecte()$id,
          utilisateur_connecte()$initiales,
          utilisateur_connecte()$matricule,
          commentaire,
          d$id[i]
        )
      )
    }

    # Toute réquisition contenant au moins un spécimen réactivé redevient ACTIVE.
    for (rid in unique(d$requisition_id)) {
      DBI::dbExecute(
        con,
        "UPDATE requisitions SET statut='ACTIVE' WHERE id=?",
        params=list(rid)
      )
    }

    DBI::dbCommit(con)
    ok <- TRUE
    removeModal()

    for (i in seq_len(nrow(d))) {
      journaliser(
        "REACTIVATION_SPECIMEN",
        paste0(
          "Uncancel spécimen ",d$numero_specimen[i],
          " / BC# ",d$code_barre[i],
          "; ancien contexte ",norm_txt(d$contexte_annulation[i]),
          if (nzchar(commentaire)) paste0("; commentaire ",commentaire) else ""
        ),
        patient_id=d$patient_id[i],
        requisition_id=d$requisition_id[i],
        specimen_id=d$id[i]
      )
    }

    output$message_uncancel <- renderUI(
      div(
        class="success-box",
        paste0(nrow(d)," spécimen(s) réactivé(s). L'historique de l'annulation est conservé.")
      )
    )
    uncancel_cibles(data.frame())
  })

  audit_patient_data <- reactiveVal(data.frame())

  charger_audit_patient <- function(ref) {
    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con),add=TRUE)

    p <- DBI::dbGetQuery(
      con,
      "SELECT id,numero_dossier,medicare,nom,prenom FROM patients WHERE numero_dossier=? OR medicare=? LIMIT 1",
      params=list(ref,ref)
    )
    if (nrow(p)!=1) {
      audit_patient_data(data.frame())
      return(data.frame())
    }

    d <- DBI::dbGetQuery(
      con,
      "
      SELECT
        a.date_action AS Date_heure,
        a.action AS Action,
        COALESCE(a.initiales,'') || COALESCE(a.matricule,'') AS Utilisateur,
        a.details AS Details,
        r.numero_requisition AS Req,
        s.numero_specimen AS Specimen,
        s.code_barre AS BC
      FROM audit_trail a
      LEFT JOIN requisitions r ON r.id=a.requisition_id
      LEFT JOIN specimens s ON s.id=a.specimen_id
      WHERE a.patient_id=?
         OR a.requisition_id IN (SELECT id FROM requisitions WHERE patient_id=?)
         OR a.specimen_id IN (SELECT id FROM specimens WHERE patient_id=?)
      ORDER BY a.id DESC
      ",
      params=list(p$id[1],p$id[1],p$id[1])
    )
    audit_patient_data(d)
    d
  }

  observeEvent(input$audit_patient_rechercher, {
    ref <- norm_txt(input$audit_patient_ref)
    if (!nzchar(ref)) return()
    d <- charger_audit_patient(ref)
    journaliser("CONSULTATION_AUDIT_PATIENT",paste0("Traçabilité consultée : ",ref))
    if (nrow(d)==0) showNotification("Aucune trace trouvée.",type="warning")
  })

  output$table_audit_patient <- DT::renderDT({
    d <- audit_patient_data()
    if (is.null(d) || nrow(d)==0) {
      return(DT::datatable(data.frame(Message="Aucune trace affichée."),rownames=FALSE,options=list(dom="t")))
    }
    DT::datatable(d,rownames=FALSE,filter="top",options=list(pageLength=20,scrollX=TRUE))
  })

  output$audit_patient_excel <- downloadHandler(
    filename=function() paste0("EDUSILLAB_trace_patient_",format(Sys.Date(),"%Y%m%d"),".xlsx"),
    content=function(file) {
      d <- audit_patient_data()
      req(nrow(d)>0)
      if (!requireNamespace("writexl",quietly=TRUE)) {
        stop("Installez writexl avec install.packages('writexl').")
      }
      writexl::write_xlsx(list(Trace_patient=d),file)
    }
  )

  output$audit_patient_pdf <- downloadHandler(
    filename=function() paste0("EDUSILLAB_trace_patient_",format(Sys.Date(),"%Y%m%d"),".pdf"),
    content=function(file) {
      d <- audit_patient_data()
      req(nrow(d)>0)
      grDevices::pdf(file,width=11.7,height=8.3,paper="special")
      on.exit(grDevices::dev.off(),add=TRUE)
      par(mar=c(1,1,2,1))
      plot.new()
      text(.02,.97,"EDUSILLAB — Traçabilité patient",adj=c(0,1),font=2,cex=1.2)
      y <- .92
      for (i in seq_len(nrow(d))) {
        ligne <- paste0(
          d$Date_heure[i]," | ",d$Utilisateur[i]," | ",d$Action[i]," | ",
          ifelse(is.na(d$Req[i]),"",paste0("Req ",d$Req[i]," | ")),
          ifelse(is.na(d$Specimen[i]),"",paste0("Spécimen ",d$Specimen[i]," | ")),
          ifelse(is.na(d$Details[i]),"",d$Details[i])
        )
        morceaux <- strwrap(ligne,width=145)
        for (m in morceaux) {
          if (y<.05) { plot.new(); y <- .96 }
          text(.02,y,m,adj=c(0,1),cex=.62)
          y <- y-.027
        }
        y <- y-.012
      }
    },
    contentType="application/pdf"
  )

  # ----------------------------------------------------------
  # HISTORIQUE
  # ----------------------------------------------------------

  observeEvent(input$actualiser_historique, charger_historique())

  output$table_historique <- DT::renderDT({
    d <- historique()

    if (is.null(d) || nrow(d) == 0) {
      return(
        DT::datatable(
          data.frame(Message = "Aucune réquisition."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }

    filtre <- if (is.null(input$filtre_utilisateur)) {
      ""
    } else {
      norm_txt(input$filtre_utilisateur)
    }

    if (nzchar(filtre)) {
      texte <- tolower(paste(
        d$Matricule,
        d$Nom_utilisateur,
        d$Prenom_utilisateur,
        d$Courriel
      ))

      d <- d[
        grepl(tolower(filtre), texte, fixed = TRUE),
        ,
        drop = FALSE
      ]
    }

    d[["\u00c9tiquette"]] <- sprintf(
      paste0(
        "<button class='btn btn-primary btn-xs' ",
        "onclick=\"Shiny.setInputValue('reimprimer_requisition_id', %s, {priority:'event'})\">",
        "Réimprimer</button>"
      ),
      d$requisition_id
    )

    d$requisition_id <- NULL

    DT::datatable(
      d,
      escape = FALSE,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  # ----------------------------------------------------------
  # RÉIMPRESSION D'ÉTIQUETTES EXISTANTES
  # ----------------------------------------------------------
  # La réimpression conserve le même numéro d'échantillon et le
  # même BC#. Aucun nouveau code-barres n'est créé.

  observeEvent(input$reimprimer_requisition_id, {
    req(utilisateur_connecte()$perm_historique == 1)

    requisition_id <- as.integer(input$reimprimer_requisition_id)
    dernier_pdf_reimpression(NULL)
    journaliser(
      "REIMPRESSION_ETIQUETTES",
      paste0("Réimpression pour requisition_id=",requisition_id),
      requisition_id=requisition_id
    )

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    etiquettes <- DBI::dbGetQuery(
      con,
      "
      SELECT
        r.numero_requisition,
        s.numero_specimen,
        s.code_barre,
        p.nom,
        p.prenom,
        p.numero_dossier,
        p.medicare,
        p.sexe,
        COALESCE(s.location, '') AS location,
        COALESCE(s.analyses, '') AS analyses,
        COALESCE(s.quantite, 'N/P') AS quantite,
        COALESCE(s.contenant, 'NON PRECISE') AS contenant,
        COALESCE(s.departement, '') AS departement,
        COALESCE(s.px, '') AS px,
        TRIM(COALESCE(uc.prenom,'') || ' ' || COALESCE(uc.nom,'')) AS saisi_nom_complet,
        COALESCE(s.date_creation,'') AS date_saisie,
        COALESCE(NULLIF(s.saisi_initiales,''),uc.initiales,'') AS signature_initiales
      FROM specimens s
      INNER JOIN requisitions r ON r.id = s.requisition_id
      INNER JOIN patients p ON p.id = s.patient_id
      LEFT JOIN utilisateurs uc ON uc.id = s.cree_par
      WHERE s.requisition_id = ?
      ORDER BY s.groupe_etiquette, s.id
      ",
      params = list(requisition_id)
    )

    if (nrow(etiquettes) == 0) {
      showModal(
        modalDialog(
          title = "Réimpression impossible",
          p(
            "Aucune étiquette enregistrée n'est associée à cette réquisition. ",
            "Les anciennes réquisitions créées avant l'ajout du module d'étiquettes peuvent ne pas avoir de spécimens enregistrés."
          ),
          easyClose = TRUE,
          footer = modalButton("Fermer")
        )
      )
      return()
    }

    # Configuration de l'imprimante / format d'étiquette.
    imp <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM materiel_impression
      WHERE actif = 1
      ORDER BY par_defaut DESC, id
      LIMIT 1
      "
    )

    largeur <- if (nrow(imp) == 1) imp$largeur_mm[1] else 100
    hauteur <- if (nrow(imp) == 1) imp$hauteur_mm[1] else 50

    numero_req <- etiquettes$numero_requisition[1]
    nom_pdf <- paste0(
      "reimpression_etiquettes_REQ_",
      numero_req,
      "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".pdf"
    )

    chemin_pdf <- file.path(preview_dir, nom_pdf)

    creer_pdf_selon_imprimante(etiquettes,chemin_pdf,con)

    dernieres_etiquettes(etiquettes)
    etiquettes_selectionnees(seq_len(nrow(etiquettes)))
    dernier_pdf_etiquettes(chemin_pdf)

    choix_reimpression <- lapply(seq_len(nrow(etiquettes)), function(i) {
      analyse_txt <- ifelse(
        is.na(etiquettes$analyses[i]) || !nzchar(etiquettes$analyses[i]),
        "Sans analyse",
        etiquettes$analyses[i]
      )

      specimen_txt <- ifelse(
        is.na(etiquettes$numero_specimen[i]),
        "",
        etiquettes$numero_specimen[i]
      )

      tube_txt <- ifelse(
        is.na(etiquettes$contenant[i]),
        "",
        etiquettes$contenant[i]
      )

      checkboxInput(
        inputId = paste0("reprint_label_", i),
        label = paste0(
          "Étiquette ",
          i,
          " — Spécimen : ",
          specimen_txt,
          " — Analyses : ",
          analyse_txt,
          if (nzchar(tube_txt)) paste0(" — Tube : ", tube_txt) else ""
        ),
        value = TRUE
      )
    })

    showModal(
      modalDialog(
        title = paste("Réimpression - Req", numero_req),

        div(
          class = "info-box",
          strong("Choisir les étiquettes à réimprimer : "),
          "vous pouvez tout sélectionner, tout désélectionner, puis choisir individuellement les étiquettes désirées. ",
          "Le BC# et le numéro d'échantillon existants sont conservés."
        ),

        div(
          style = "margin:10px 0 14px 0;",
          actionButton(
            "reprint_tout_selectionner",
            "Tout sélectionner",
            class = "btn-default"
          ),
          tags$span(style = "display:inline-block; width:8px;"),
          actionButton(
            "reprint_tout_deselectionner",
            "Tout désélectionner",
            class = "btn-default"
          )
        ),

        do.call(tagList, choix_reimpression),

        hr(),

        actionButton(
          "generer_pdf_reimpression_selection",
          "Générer le PDF des étiquettes sélectionnées",
          class = "btn-primary"
        ),

        br(), br(),

        uiOutput("apercu_reimpression_selection"),

        size = "l",
        easyClose = FALSE,
        footer = modalButton("Fermer")
      )
    )
  })

  observeEvent(input$reprint_tout_selectionner, {
    etiquettes <- dernieres_etiquettes()
    req(!is.null(etiquettes), nrow(etiquettes) > 0)

    for (i in seq_len(nrow(etiquettes))) {
      updateCheckboxInput(
        session,
        paste0("reprint_label_", i),
        value = TRUE
      )
    }
  })

  observeEvent(input$reprint_tout_deselectionner, {
    etiquettes <- dernieres_etiquettes()
    req(!is.null(etiquettes), nrow(etiquettes) > 0)

    for (i in seq_len(nrow(etiquettes))) {
      updateCheckboxInput(
        session,
        paste0("reprint_label_", i),
        value = FALSE
      )
    }
  })

  observeEvent(input$generer_pdf_reimpression_selection, {
    etiquettes <- dernieres_etiquettes()
    req(!is.null(etiquettes), nrow(etiquettes) > 0)

    indices <- integer(0)

    for (i in seq_len(nrow(etiquettes))) {
      val <- input[[paste0("reprint_label_", i)]]
      if (isTRUE(val)) indices <- c(indices, i)
    }

    if (length(indices) == 0) {
      showNotification(
        "Sélectionnez au moins une étiquette à réimprimer.",
        type = "warning"
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    imp <- DBI::dbGetQuery(
      con,
      "
      SELECT *
      FROM materiel_impression
      WHERE actif = 1
      ORDER BY par_defaut DESC, id
      LIMIT 1
      "
    )

    largeur <- if (nrow(imp) == 1) imp$largeur_mm[1] else 100
    hauteur <- if (nrow(imp) == 1) imp$hauteur_mm[1] else 50

    nom_pdf <- paste0(
      "reimpression_selection_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".pdf"
    )

    chemin_pdf <- file.path(preview_dir, nom_pdf)

    tryCatch({
      creer_pdf_etiquettes_selection(
        labels = etiquettes,
        indices = indices,
        fichier = chemin_pdf,
        largeur_mm = largeur,
        hauteur_mm = hauteur
      )

      dernier_pdf_reimpression(chemin_pdf)

      showNotification(
        paste0(
          length(indices),
          " étiquette(s) préparée(s) pour la réimpression."
        ),
        type = "message"
      )
    }, error = function(e) {
      showNotification(
        paste("Erreur de réimpression :", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  output$apercu_reimpression_selection <- renderUI({
    chemin_reimpression <- dernier_pdf_reimpression()

    if (is.null(chemin_reimpression) || !file.exists(chemin_reimpression)) {
      return(
        tags$p(
          class = "text-muted",
          "Sélectionnez les étiquettes puis cliquez sur « Générer le PDF des étiquettes sélectionnées »."
        )
      )
    }

    tagList(
      h4("Aperçu des étiquettes sélectionnées"),
      tags$iframe(
        src = paste0(
          "edulab_labels/",
          basename(chemin_reimpression),
          "?v=",
          as.integer(Sys.time())
        ),
        style = "width:100%; height:420px; border:1px solid #bbb;"
      ),
      br(),
      downloadButton(
        "telecharger_pdf_reimpression",
        "Télécharger / imprimer les étiquettes sélectionnées"
      )
    )
  })


  output$telecharger_pdf_reimpression <- downloadHandler(
    filename = function() {
      p <- dernier_pdf_reimpression()
      if (is.null(p)) {
        "reimpression_etiquettes_selectionnees.pdf"
      } else {
        basename(p)
      }
    },
    content = function(file) {
      p <- dernier_pdf_reimpression()
      req(!is.null(p), file.exists(p))
      file.copy(p, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )

  # ----------------------------------------------------------
  # UTILISATEURS
  # ----------------------------------------------------------

  observeEvent(input$enregistrer_utilisateur, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)

    output$message_utilisateur <- renderUI(NULL)

    tryCatch({
      matricule <- norm_txt(input$user_matricule)
      initiales <- norm_upper(input$user_initiales)
      nom <- norm_upper(input$user_nom)
      prenom <- norm_txt(input$user_prenom)
      email <- tolower(norm_txt(input$user_email))
      identifiant <- norm_txt(input$user_identifiant)
      mdp <- norm_txt(input$user_mot_de_passe)
      role <- norm_upper(input$user_role)

      champs <- c(matricule, initiales, nom, prenom, email, identifiant, mdp, role)
      if (length(champs) != 8L || any(!nzchar(champs))) {
        output$message_utilisateur <- renderUI(
          div(style = "color:#b91c1c;font-weight:700;",
              "Tous les champs d'identification sont obligatoires.")
        )
        return()
      }

      if (!email_valide(email)) {
        output$message_utilisateur <- renderUI(
          div(style = "color:#b91c1c;font-weight:700;", "Adresse courriel invalide.")
        )
        return()
      }

      if (nchar(mdp) < 8L) {
        output$message_utilisateur <- renderUI(
          div(style = "color:#b91c1c;font-weight:700;",
              "Le mot de passe temporaire doit avoir au moins 8 caractères.")
        )
        return()
      }

      # Permissions. ADMIN et SUPERUTILISATEUR obtiennent les droits complets.
      if (role %in% c("ADMIN", "SUPERUTILISATEUR")) {
        pr <- pp <- pq <- pat <- prec <- ph <- pu <- pa <- pportail <- 1L
      } else {
        pr <- as.integer(isTRUE(input$p_recherche))
        pp <- as.integer(isTRUE(input$p_patients))
        pq <- as.integer(isTRUE(input$p_requisition))
        pat <- as.integer(isTRUE(input$p_ajout_tests))
        prec <- as.integer(isTRUE(input$p_reception))
        ph <- as.integer(isTRUE(input$p_historique))
        pu <- as.integer(isTRUE(input$p_utilisateurs))
        pa <- as.integer(isTRUE(input$p_analyses))
        pportail <- as.integer(isTRUE(input$p_portail))
      }

      actif <- as.integer(isTRUE(input$user_actif))

      con <- ouvrir_db()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      existe <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT id, matricule, identifiant, email FROM utilisateurs ",
          "WHERE matricule = ? OR identifiant = ? OR LOWER(email) = LOWER(?)"
        ),
        params = list(matricule, identifiant, email)
      )

      if (nrow(existe) > 0L) {
        output$message_utilisateur <- renderUI(
          div(style = "color:#b91c1c;font-weight:700;",
              "Impossible de créer le compte : le matricule, l'identifiant ou le courriel existe déjà.")
        )
        return()
      }

      DBI::dbBegin(con)
      ok <- FALSE
      on.exit(if (!ok && DBI::dbIsValid(con)) try(DBI::dbRollback(con), silent = TRUE), add = TRUE)

      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO utilisateurs (",
          "identifiant, mot_de_passe, role, actif, matricule, initiales, nom, prenom, email, ",
          "tentatives_echouees, verrouille, changer_mot_de_passe, ",
          "perm_recherche_patient, perm_patients, perm_requisition, perm_ajout_tests, ",
          "perm_reception, perm_historique, perm_utilisateurs, perm_analyses, perm_portail",
          ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0, 0, 1, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          identifiant, mdp, role, actif, matricule, initiales, nom, prenom, email,
          pr, pp, pq, pat, prec, ph, pu, pa, pportail
        )
      )

      DBI::dbCommit(con)
      ok <- TRUE
      refresh_users(refresh_users() + 1L)

      updateTextInput(session, "user_matricule", value = "")
      updateTextInput(session, "user_initiales", value = "")
      updateTextInput(session, "user_nom", value = "")
      updateTextInput(session, "user_prenom", value = "")
      updateTextInput(session, "user_email", value = "")
      updateTextInput(session, "user_identifiant", value = "")
      updateTextInput(session, "user_mot_de_passe", value = "")

      output$message_utilisateur <- renderUI(
        div(class = "success-box",
            paste0("Utilisateur ", identifiant, " créé avec succès. Le mot de passe est temporaire."))
      )
    }, error = function(e) {
      output$message_utilisateur <- renderUI(
        div(style = "color:#b91c1c;font-weight:700;",
            paste0("Création impossible : ", conditionMessage(e)))
      )
    })
  })

  output$table_utilisateurs <- renderTable({
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    refresh_users()

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    DBI::dbGetQuery(con, "
    SELECT
      initiales AS Initiales,
      matricule AS Matricule,
      nom AS Nom,
      prenom AS Prenom,
      email AS Courriel,
      identifiant AS Identifiant,
      role AS Niveau,
      CASE WHEN perm_ajout_tests = 1 THEN 'Oui' ELSE 'Non' END AS Ajout_tests,
      CASE WHEN actif = 1 THEN 'Oui' ELSE 'Non' END AS Actif,
      CASE WHEN verrouille = 1 THEN 'OUI' ELSE 'Non' END AS Verrouille,
      tentatives_echouees AS Echecs,
      CASE
        WHEN changer_mot_de_passe = 1 THEN 'Oui'
        ELSE 'Non'
      END AS Mot_de_passe_temporaire
    FROM utilisateurs
    ORDER BY nom, prenom
    ")
  }, striped = TRUE, bordered = TRUE)

  output$choix_utilisateur_modifier <- renderUI({
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    refresh_users()

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    u <- DBI::dbGetQuery(
      con,
      "
      SELECT id, matricule, nom, prenom, email
      FROM utilisateurs
      ORDER BY nom, prenom
      "
    )

    if (nrow(u) == 0) return(NULL)

    choix <- u$id
    names(choix) <- paste0(
      u$matricule, " — ",
      u$prenom, " ", u$nom,
      " — ", u$email
    )

    selectInput("modifier_user_id", "Utilisateur", choices = choix)
  })

  observeEvent(input$charger_utilisateur_modifier, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    req(input$modifier_user_id)

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    u <- DBI::dbGetQuery(
      con,
      "SELECT * FROM utilisateurs WHERE id = ?",
      params = list(as.integer(input$modifier_user_id))
    )

    if (nrow(u) != 1) return()

    output$formulaire_modifier_utilisateur <- renderUI(
      div(
        class = "admin-zone",
        fluidRow(
          column(3, textInput("edit_matricule", "Matricule", value = u$matricule[1])),
          column(3, textInput("edit_initiales", "Initiales", value = ifelse(is.na(u$initiales[1]), "", u$initiales[1]))),
          column(3, textInput("edit_nom", "Nom", value = u$nom[1])),
          column(3, textInput("edit_prenom", "Prénom", value = u$prenom[1]))
        ),
        fluidRow(
          column(4, textInput("edit_email", "Courriel", value = u$email[1])),
          column(4, textInput("edit_identifiant", "Identifiant", value = u$identifiant[1])),
          column(4, selectInput(
            "edit_role", "Niveau",
            choices = c(
              "Étudiant" = "ETUDIANT",
              "Utilisateur" = "UTILISATEUR",
              "Super utilisateur" = "SUPERUTILISATEUR",
              "Administrateur" = "ADMIN"
            ),
            selected = u$role[1]
          ))
        ),
        h4("Permissions"),
        checkboxInput(
          "edit_p_recherche", "Recherche patient",
          value = u$perm_recherche_patient[1] == 1
        ),
        checkboxInput(
          "edit_p_patients", "Ajouter / modifier patients",
          value = u$perm_patients[1] == 1
        ),
        checkboxInput(
          "edit_p_requisition", "Créer réquisitions",
          value = u$perm_requisition[1] == 1
        ),
        checkboxInput(
          "edit_p_ajout_tests",
          "Autoriser la sélection / l'ajout de tests dans la réquisition",
          value = u$perm_ajout_tests[1] == 1
        ),
        checkboxInput(
          "edit_p_reception", "Réception",
          value = u$perm_reception[1] == 1
        ),
        checkboxInput(
          "edit_p_historique", "Historique / suivi",
          value = u$perm_historique[1] == 1
        ),
        checkboxInput(
          "edit_p_utilisateurs", "Administration utilisateurs",
          value = u$perm_utilisateurs[1] == 1
        ),
        checkboxInput(
          "edit_p_analyses", "Gestion analyses",
          value = u$perm_analyses[1] == 1
        ),
        checkboxInput("edit_actif", "Compte actif", value = u$actif[1] == 1),
        actionButton(
          "sauvegarder_modifications_user",
          "Enregistrer les modifications",
          class = "btn-success"
        ),
        br(), br(),
        uiOutput("message_modifier_user")
      )
    )
  })

  observeEvent(input$sauvegarder_modifications_user, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)
    req(input$modifier_user_id)

    user_id <- as.integer(input$modifier_user_id)
    email <- tolower(norm_txt(input$edit_email))

    if (!email_valide(email)) {
      output$message_modifier_user <- renderUI(
        div(style = "color:red;", "Adresse courriel invalide.")
      )
      return()
    }

    if (input$edit_role == "ADMIN") {
      pr <- pp <- pq <- pat <- prec <- ph <- pu <- pa <- 1L
    } else {
      pr <- as.integer(input$edit_p_recherche)
      pp <- as.integer(input$edit_p_patients)
      pq <- as.integer(input$edit_p_requisition)
      pat <- as.integer(input$edit_p_ajout_tests)
      prec <- as.integer(input$edit_p_reception)
      ph <- as.integer(input$edit_p_historique)
      pu <- as.integer(input$edit_p_utilisateurs)
      pa <- as.integer(input$edit_p_analyses)
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    doublon <- DBI::dbGetQuery(
      con,
      "
      SELECT id
      FROM utilisateurs
      WHERE
        (
          matricule = ?
          OR identifiant = ?
          OR LOWER(email) = LOWER(?)
        )
        AND id <> ?
      ",
      params = list(
        norm_txt(input$edit_matricule),
        norm_txt(input$edit_identifiant),
        email,
        user_id
      )
    )

    if (nrow(doublon) > 0) {
      output$message_modifier_user <- renderUI(
        div(style = "color:red;", "Matricule, identifiant ou courriel déjà utilisé.")
      )
      return()
    }

    DBI::dbExecute(
      con,
      "
      UPDATE utilisateurs
      SET
        matricule = ?,
        initiales = ?,
        nom = ?,
        prenom = ?,
        email = ?,
        identifiant = ?,
        role = ?,
        actif = ?,
        perm_recherche_patient = ?,
        perm_patients = ?,
        perm_requisition = ?,
        perm_ajout_tests = ?,
        perm_reception = ?,
        perm_historique = ?,
        perm_utilisateurs = ?,
        perm_analyses = ?
      WHERE id = ?
      ",
      params = list(
        norm_txt(input$edit_matricule),
        norm_upper(input$edit_initiales),
        norm_upper(input$edit_nom),
        norm_txt(input$edit_prenom),
        email,
        norm_txt(input$edit_identifiant),
        input$edit_role,
        as.integer(input$edit_actif),
        pr, pp, pq, pat, prec, ph, pu, pa,
        user_id
      )
    )

    refresh_users(refresh_users() + 1L)

    output$message_modifier_user <- renderUI(
      div(class = "success-box", "Utilisateur et permissions mis à jour.")
    )
  })

  # ----------------------------------------------------------
  # RESET / MODIFICATION MOT DE PASSE
  # ----------------------------------------------------------

  observeEvent(input$reset_password_button, {
    req(utilisateur_connecte()$perm_utilisateurs == 1)

    ident <- norm_txt(input$reset_identification)
    mdp <- input$reset_password
    conf <- input$reset_password_confirmation

    if (!nzchar(ident) || !nzchar(mdp) || !nzchar(conf)) {
      output$message_reset <- renderUI(
        div(style = "color:red;", "Tous les champs sont obligatoires.")
      )
      return()
    }

    if (mdp != conf) {
      output$message_reset <- renderUI(
        div(style = "color:red;", "Les mots de passe ne correspondent pas.")
      )
      return()
    }

    if (nchar(mdp) < 8) {
      output$message_reset <- renderUI(
        div(style = "color:red;", "Minimum 8 caractères.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    u <- DBI::dbGetQuery(
      con,
      "
      SELECT id, matricule, nom, prenom, email
      FROM utilisateurs
      WHERE matricule = ? OR LOWER(email) = LOWER(?)
      ",
      params = list(ident, ident)
    )

    if (nrow(u) != 1) {
      output$message_reset <- renderUI(
        div(style = "color:red;", "Utilisateur non trouvé.")
      )
      return()
    }

    DBI::dbExecute(
      con,
      "
      UPDATE utilisateurs
      SET
        mot_de_passe = ?,
        tentatives_echouees = 0,
        verrouille = 0,
        changer_mot_de_passe = 1
      WHERE id = ?
      ",
      params = list(mdp, u$id[1])
    )

    refresh_users(refresh_users() + 1L)

    output$message_reset <- renderUI(
      div(
        class = "success-box",
        strong("Mot de passe réinitialisé."),
        br(),
        paste0(
          u$matricule[1], " — ",
          u$prenom[1], " ", u$nom[1]
        ),
        br(),
        "Le compte est déverrouillé. Le mot de passe devra être changé à la prochaine connexion."
      )
    )
  })

  observeEvent(input$enregistrer_nouveau_mdp, {
    req(utilisateur_connecte())

    nouveau <- input$nouveau_mdp_obligatoire
    conf <- input$confirmation_mdp_obligatoire

    if (!nzchar(nouveau) || !nzchar(conf)) {
      output$message_changement_mdp <- renderUI(
        div(style = "color:red;", "Veuillez remplir les deux champs.")
      )
      return()
    }

    if (nouveau != conf) {
      output$message_changement_mdp <- renderUI(
        div(style = "color:red;", "Les mots de passe ne correspondent pas.")
      )
      return()
    }

    if (nchar(nouveau) < 8) {
      output$message_changement_mdp <- renderUI(
        div(style = "color:red;", "Minimum 8 caractères.")
      )
      return()
    }

    con <- ouvrir_db()
    DBI::dbExecute(
      con,
      "
      UPDATE utilisateurs
      SET
        mot_de_passe = ?,
        changer_mot_de_passe = 0,
        tentatives_echouees = 0,
        verrouille = 0
      WHERE id = ?
      ",
      params = list(nouveau, utilisateur_connecte()$id)
    )
    DBI::dbDisconnect(con)

    page_active("accueil")
  })

  observeEvent(input$modifier_mon_mdp, {
    req(utilisateur_connecte())

    actuel <- input$mdp_actuel_compte
    nouveau <- input$nouveau_mdp_compte
    conf <- input$confirmation_mdp_compte

    if (!nzchar(actuel) || !nzchar(nouveau) || !nzchar(conf)) {
      output$message_mon_mdp <- renderUI(
        div(style = "color:red;", "Tous les champs sont obligatoires.")
      )
      return()
    }

    if (nouveau != conf) {
      output$message_mon_mdp <- renderUI(
        div(style = "color:red;", "Les nouveaux mots de passe ne correspondent pas.")
      )
      return()
    }

    if (nchar(nouveau) < 8) {
      output$message_mon_mdp <- renderUI(
        div(style = "color:red;", "Minimum 8 caractères.")
      )
      return()
    }

    if (actuel == nouveau) {
      output$message_mon_mdp <- renderUI(
        div(style = "color:red;", "Le nouveau mot de passe doit être différent.")
      )
      return()
    }

    con <- ouvrir_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    u <- DBI::dbGetQuery(
      con,
      "SELECT mot_de_passe FROM utilisateurs WHERE id = ?",
      params = list(utilisateur_connecte()$id)
    )

    if (nrow(u) != 1 || u$mot_de_passe[1] != actuel) {
      output$message_mon_mdp <- renderUI(
        div(style = "color:red;", "Mot de passe actuel incorrect.")
      )
      return()
    }

    DBI::dbExecute(
      con,
      "
      UPDATE utilisateurs
      SET
        mot_de_passe = ?,
        changer_mot_de_passe = 0,
        tentatives_echouees = 0,
        verrouille = 0
      WHERE id = ?
      ",
      params = list(nouveau, utilisateur_connecte()$id)
    )

    output$message_mon_mdp <- renderUI(
      div(class = "success-box", "Mot de passe modifié.")
    )
  })
}

# ============================================================
# 5. LANCEMENT
# ============================================================

shinyApp(ui = ui, server = server)