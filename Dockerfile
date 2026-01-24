# Dockerfile personnalisé basé sur rocker/shiny-verse
# avec les outils nécessaires pour DevPod
FROM --platform=linux/amd64 rocker/shiny-verse:4.4

# Installation des outils nécessaires pour DevPod
RUN apt-get update && apt-get install -y --no-install-recommends \
    procps \
    curl \
    wget \
    ca-certificates \
    openssh-client \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Créer les répertoires workspace avec les bonnes permissions
# DevPod peut utiliser différents chemins selon la configuration
RUN mkdir -p /workspace /workspaces/shinyapp /workspaces/.dockerless/.devpod-internal /home/rstudio/workspace && \
    chmod -R 777 /workspace /workspaces /home/rstudio

# Définir le répertoire de travail par défaut
WORKDIR /workspace

# Ne pas définir de CMD - laisser DevPod gérer le conteneur
