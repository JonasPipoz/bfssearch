# Shiny App Project - Moteur de recherche BFS

Application Shiny pour rechercher et télécharger des données du Office fédérale de la statistique (BFS) et les données de Swiss Stat Explorer (SSE). Le package utilise directement les fonctions developpées par @lgnbhl dans la librairie BFS : (github:  https://github.com/lgnbhl/BFS) (CRAN:  https://cloud.r-project.org/web/packages/BFS/index.html)

## Fonctionnalités

- Recherche de datasets dans le catalogue OFS en français
- Accès aux datasets Swiss Stat Explorer
- Sélection et visualisation de datasets
- Génération dynamique de filtres basés sur les métadonnées
- Interrogation et téléchargement de données filtrées
- Génération du code R pour intégration du script
- Export des résultats au format CSV

## Installation

``` R
devtools::install_github('jonaspipoz/bfssearch')
```

## Usage

``` R
# Lancement de l'app shiny
bfssearch::run_app()
```

## Information

- Ce package n'est pas mis à disposition ni maintenu par l'Office Fédérale de la Statistique. 


