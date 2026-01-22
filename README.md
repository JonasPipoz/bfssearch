# Shiny App Project - Moteur de recherche BFS

Application Shiny développée avec R et renv dans DevPod pour rechercher et télécharger des données du Bureau fédéral de la statistique (BFS) suisse.

## Fonctionnalités

- 🔍 Recherche de datasets dans le catalogue BFS en français
- 📊 Sélection et visualisation de datasets
- 🎛️ Génération dynamique de filtres basés sur les métadonnées
- 📥 Interrogation et téléchargement de données filtrées
- 💾 Export des résultats au format CSV

## Prérequis

- DevPod configuré
- Docker
- Packages R : shiny, BFS, dplyr, DT, shinycssloaders, tidyr

## Structure du projet

- `.devcontainer/devcontainer.json` : Configuration DevPod avec build personnalisé
- `Dockerfile` : Image Docker basée sur rocker/shiny-verse avec outils DevPod
- `app.R` : Application Shiny principale (moteur de recherche BFS)
- `install_packages.R` : Script d'installation des packages requis
- `USAGE.md` : Guide d'utilisation détaillé
- `API_swissstatexplorer.md` : Documentation de l'API BFS
- `renv/` : Environnement R isolé (généré automatiquement)
- `.Rprofile` : Active automatiquement renv
- `TROUBLESHOOTING.md` : Guide de dépannage pour les erreurs courantes

## Installation et démarrage

1. Ouvrir le projet dans DevPod
2. L'environnement renv sera restauré automatiquement
3. Installer les packages requis (si nécessaire) :
   ```r
   source("install_packages.R")
   ```
4. Lancer l'application Shiny :
   ```r
   shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
   ```
5. L'application sera accessible sur le port 3838

## Utilisation rapide

1. **Recherche** : Entrez un terme de recherche (ex: "étudiants", "population")
2. **Sélection** : Cliquez sur un dataset dans les résultats
3. **Filtres** : Configurez les filtres dynamiques générés automatiquement
4. **Téléchargement** : Interrogez et téléchargez les données filtrées

Pour plus de détails, consultez le [Guide d'utilisation](USAGE.md).

## Ajouter des packages

```r
# Installer un package
install.packages("nom_du_package")

# Sauvegarder dans renv
renv::snapshot()
```

## Image Docker

Le projet utilise un Dockerfile personnalisé basé sur `rocker/shiny-verse:latest` qui inclut :
- R
- Shiny
- Tidyverse
- Outils de développement
- Outils nécessaires pour DevPod (curl, procps, openssh-client, etc.)

L'image est construite automatiquement lors du premier démarrage de DevPod.

## Dépannage

Si vous rencontrez des erreurs, consultez le fichier `TROUBLESHOOTING.md` pour des solutions détaillées.
