# EDUSILLAB v34 - génération officielle du manifest Posit Connect Cloud
# Exécuter ce fichier UNE FOIS dans RStudio depuis le dossier EDUSILLAB_v34_CONNECT_CLOUD.

packages <- c(
  "rsconnect",
  "shiny",
  "DBI",
  "RSQLite",
  "DT",
  "readxl",
  "jsonlite",
  "readtext",
  "writexl"
)

a_installer <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(a_installer) > 0) {
  install.packages(a_installer, repos = "https://cloud.r-project.org")
}

rsconnect::writeManifest(
  appDir = getwd(),
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)

cat("\nManifest généré : ", file.path(getwd(), "manifest.json"), "\n", sep = "")
