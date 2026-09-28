# EDUSILLAB — génération du manifest Posit Connect Cloud
# À exécuter dans RStudio depuis le dossier EDUSILLAB_CONNECT_CLOUD.

packages <- c(
  "shiny", "DBI", "RSQLite", "DT", "readxl", "jsonlite", "rsconnect"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing)
}

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")

cat("\nmanifest.json créé dans : ", normalizePath("manifest.json"), "\n", sep="")
