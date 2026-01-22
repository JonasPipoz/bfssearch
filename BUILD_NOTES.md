# Notes sur le build du package

## Problème avec renv lors du build

Si vous rencontrez des erreurs lors de `devtools::build()` concernant des fichiers manquants dans `renv/library/macos/`, c'est normal. Ces fichiers n'existent que sur macOS et ne sont pas nécessaires pour le build.

## Solution

Le fichier `.Rbuildignore` exclut déjà le dossier `renv/` du build. Si le problème persiste :

1. Vérifiez que `.Rbuildignore` contient bien `^renv/`
2. Essayez de build depuis un répertoire propre :
   ```r
   # Nettoyer d'abord
   devtools::clean_vignettes()
   
   # Puis build
   devtools::build()
   ```

## Alternative : Build sans renv

Si vous voulez éviter complètement les problèmes avec renv :

```r
# Build en excluant explicitement renv
devtools::build(clean = TRUE)
```

## Installation locale

Pour installer le package localement sans build :

```r
devtools::load_all()
devtools::install()
```
