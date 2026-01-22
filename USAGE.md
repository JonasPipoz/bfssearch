# Guide d'utilisation - Moteur de recherche BFS

## Description

Cette application Shiny permet de rechercher et télécharger des données du Bureau fédéral de la statistique (BFS) suisse en français. L'application utilise l'API Swiss Stats Explorer pour accéder aux données publiques.

## Fonctionnalités

1. **Recherche de datasets** : Recherchez dans le catalogue BFS en utilisant des mots-clés
2. **Sélection de dataset** : Choisissez un dataset parmi les résultats de recherche
3. **Filtres dynamiques** : L'application génère automatiquement des filtres basés sur les métadonnées du dataset
4. **Interrogation des données** : Téléchargez les données filtrées selon vos critères
5. **Export** : Téléchargez les résultats au format CSV

## Installation

### 1. Installer les packages requis

Exécutez le script d'installation :

```r
source("install_packages.R")
```

Ou installez manuellement les packages :

```r
install.packages(c("shiny", "BFS", "dplyr", "DT", "shinycssloaders", "tidyr"))
```

### 2. Lancer l'application

```r
shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
```

L'application sera accessible sur `http://localhost:3838`

## Utilisation

### Étape 1 : Recherche

1. Entrez un terme de recherche dans le champ "Terme de recherche" (ex: "étudiants", "population", "logement")
2. Optionnellement, sélectionnez une division spatiale (Suisse, Cantons, Districts, etc.)
3. Cliquez sur "Rechercher"
4. Les résultats s'affichent dans le tableau

### Étape 2 : Sélection d'un dataset

1. Cliquez sur une ligne du tableau de résultats pour sélectionner un dataset
2. L'application bascule automatiquement vers l'onglet "Configuration des filtres"
3. Les métadonnées du dataset sont chargées automatiquement

### Étape 3 : Configuration des filtres

1. L'application génère automatiquement des filtres pour chaque dimension du dataset
2. Pour chaque filtre :
   - Sélectionnez les valeurs que vous souhaitez inclure
   - Laissez vide pour inclure toutes les valeurs
3. Cliquez sur "Interroger les données"

### Étape 4 : Visualisation et téléchargement

1. Les données filtrées s'affichent dans l'onglet "Données"
2. Utilisez le bouton "Télécharger CSV" pour exporter les données

## Exemple d'utilisation

### Recherche de données sur les étudiants

1. **Recherche** : Entrez "étudiants" dans le champ de recherche
2. **Sélection** : Choisissez "University students by ISCED field, level of study and sex"
3. **Filtres** :
   - Année : Sélectionnez "2020/21" et "2021/22"
   - Niveau d'étude : Sélectionnez "Master" et "Doctorat"
   - Champ ISCED : Sélectionnez "Sciences de l'éducation"
4. **Interrogation** : Cliquez sur "Interroger les données"
5. **Téléchargement** : Exportez les résultats en CSV

## Notes importantes

- **Limite de résultats** : L'API BFS limite les résultats à 350 datasets maximum
- **Délai entre requêtes** : L'application inclut un délai de 2 secondes entre les requêtes pour éviter les erreurs "Too Many Requests"
- **Langue** : L'application est configurée pour utiliser le français ("fr") par défaut
- **Métadonnées** : Certains datasets peuvent ne pas avoir de métadonnées disponibles, auquel cas les filtres ne seront pas générés

## Dépannage

### Erreur "Too Many Requests"

Si vous obtenez cette erreur :
- Attendez quelques secondes avant de relancer une requête
- Réduisez le nombre de filtres sélectionnés
- Utilisez l'option de téléchargement direct du fichier avec `bfs_download_asset()`

### Aucun résultat de recherche

- Vérifiez votre terme de recherche (essayez des synonymes)
- Vérifiez que vous utilisez la bonne langue (français)
- Augmentez la limite de résultats si nécessaire

### Métadonnées non disponibles

- Certains datasets peuvent ne pas avoir de métadonnées structurées
- Essayez un autre dataset ou contactez le support BFS

## Structure du code

- `app.R` : Application Shiny principale
- `install_packages.R` : Script d'installation des dépendances
- `API_swissstatexplorer.md` : Documentation de l'API BFS

## Références

- [Package BFS sur CRAN](https://CRAN.R-project.org/package=BFS)
- [Documentation BFS](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogue.html)
- [Swiss Stats Explorer](https://stats.swiss/)
