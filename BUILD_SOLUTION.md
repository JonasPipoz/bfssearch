# Solution pour le problème de build avec renv

## Problème

Lors de l'exécution de `devtools::build()` dans un devcontainer, R CMD build essaie de copier des fichiers depuis `renv/library/macos/` qui n'existent pas sur Linux, ce qui cause une erreur.

## Solutions

### Solution 1 : Utiliser le script de build (Recommandé)

Utilisez le script `build_package.R` qui crée un répertoire temporaire sans renv :

```r
source("build_package.R")
```

Ce script :
1. Crée un répertoire temporaire
2. Copie tous les fichiers sauf `renv/`
3. Build le package depuis ce répertoire
4. Retourne au répertoire original

### Solution 2 : Build depuis un répertoire parent

Si vous êtes dans `/workspaces/shinyapp`, essayez de builder depuis le répertoire parent :

```r
# Depuis /workspaces/
setwd("..")
devtools::build("shinyapp")
```

### Solution 3 : Utiliser R CMD build directement

```r
# Depuis le répertoire du package
system("R CMD build . --no-build-vignettes")
```

### Solution 4 : Installer localement sans build

Pour le développement, vous pouvez simplement installer localement :

```r
devtools::load_all()
devtools::install()
```

## Note sur .Rbuildignore

Le fichier `.Rbuildignore` contient déjà les patterns pour exclure `renv/`, mais dans certains contextes (comme devcontainer), R CMD build peut ne pas les respecter correctement. C'est pourquoi le script de build est recommandé.
