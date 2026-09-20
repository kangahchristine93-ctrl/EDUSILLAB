# EDUSILLAB v34 — Posit Connect Cloud

Cette version restaure et conserve les éléments suivants :

- logo CCNB orange fourni;
- icônes du tableau de bord;
- Répertoire d'analyses;
- import du classeur de référence au démarrage;
- patients et recherche patient;
- demandes / dossiers;
- génération et réimpression des étiquettes;
- réception simple et multiple des spécimens;
- annulation et réactivation;
- historique et audit;
- utilisateurs, profils et permissions;
- éditeur du portail;
- configuration Web et variables d'environnement.

## Fichiers à déposer sur GitHub

Déposez le CONTENU de ce dossier à la racine du dépôt GitHub :
`app.R`, `manifest.json`, `DESCRIPTION`, `Repertoire_analyses_EduLab.xlsx`,
`www/`, etc.

Ne déposez jamais votre base réelle `laboratoire.db` dans GitHub.

## Manifest

Un `manifest.json` est déjà fourni.

Pour obtenir le manifest le plus exact possible selon les versions de R et des
packages installés sur votre propre Mac, ouvrez le dossier dans RStudio et
exécutez :

```r
source("GENERER_MANIFEST.R")
```

Cela remplace `manifest.json` par celui produit officiellement avec
`rsconnect::writeManifest()`.

## Posit Connect Cloud

Connect Cloud exige un `manifest.json` pour les contenus R. Le fichier doit se
trouver à la racine du dépôt ou dans le même sous-dossier que `app.R`.

## Variables recommandées

Dans les variables d'environnement du déploiement :

- `EDUSILLAB_ADMIN_PASSWORD` : mot de passe administrateur fort
- `EDUSILLAB_TZ` : `America/Moncton`
- `EDUSILLAB_DB_FILE` : `laboratoire.db`

Attention : SQLite stocké dans le système de fichiers d'un hébergement cloud ne
constitue pas nécessairement un stockage durable. Pour un usage institutionnel
multi-utilisateur, prévoir ensuite une base persistante gérée.
