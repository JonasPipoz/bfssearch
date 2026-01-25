# Load renv
source("renv/activate.R")

# Load .env file if it exists (for shell compatibility)
if (file.exists(".env")) {
  env_vars <- readLines(".env", warn = FALSE)
  for (line in env_vars) {
    if (grepl("^[A-Z_]+=.*", line) && !grepl("^#", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        var_name <- parts[1]
        var_value <- parts[2]
        # Utiliser do.call pour créer dynamiquement l'argument nommé
        env_list <- list(var_value)
        names(env_list) <- var_name
        do.call(Sys.setenv, env_list)
      }
    }
  }
}
