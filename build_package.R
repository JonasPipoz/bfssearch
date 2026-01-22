#!/usr/bin/env Rscript
# Script pour builder le package en excluant renv
# Solution de contournement pour le problème de devcontainer

# Obtenir le répertoire de travail actuel
original_dir <- getwd()

# Créer un répertoire temporaire sans renv
temp_dir <- file.path(tempdir(), "bfssearch_build")
if (dir.exists(temp_dir)) {
  unlink(temp_dir, recursive = TRUE)
}
dir.create(temp_dir, recursive = TRUE)

# Copier tous les fichiers sauf renv et autres fichiers à exclure
files_to_exclude <- c("renv", ".git", ".Rhistory", ".Rproj.user", 
                      ".devcontainer", "pck-", "BUILD_NOTES.md")

files_to_copy <- list.files(".", all.files = TRUE, no.. = TRUE)

for (file in files_to_copy) {
  # Vérifier si le fichier doit être exclu
  should_exclude <- any(sapply(files_to_exclude, function(pattern) {
    grepl(pattern, file, fixed = TRUE)
  }))
  
  if (!should_exclude && file.exists(file)) {
    dest <- file.path(temp_dir, file)
    if (dir.exists(file)) {
      file.copy(file, dirname(dest), recursive = TRUE)
    } else {
      file.copy(file, dest)
    }
  }
}

# Changer vers le répertoire temporaire
setwd(temp_dir)

cat("Building package from temporary directory (without renv)...\n")

# Builder le package
tryCatch({
  devtools::build()
  cat("\n✓ Package built successfully!\n")
  cat("The package tarball is in:", temp_dir, "\n")
}, error = function(e) {
  cat("\n✗ Error during build:", e$message, "\n")
})

# Retourner au répertoire original
setwd(original_dir)
