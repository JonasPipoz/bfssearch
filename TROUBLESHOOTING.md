# Guide de dépannage DevPod

## Erreur "Inject Error: EOF"

Cette erreur indique que l'agent DevPod ne peut pas s'injecter correctement dans le conteneur.

### Causes possibles

1. **Outils manquants dans l'image** : L'image de base peut manquer des outils nécessaires (curl, procps, etc.)
2. **Problèmes de permissions** : L'utilisateur du conteneur n'a pas les permissions nécessaires
3. **Conteneur qui se termine** : Le conteneur s'arrête avant que l'agent ne puisse s'injecter
4. **Ressources insuffisantes** : Mémoire ou CPU insuffisants

### Solutions appliquées dans ce projet

1. **Dockerfile personnalisé** : Un Dockerfile basé sur `rocker/shiny-verse` avec les outils nécessaires
2. **Utilisateur root** : Utilisation de `root` pour éviter les problèmes de permissions
3. **Arguments Docker** : Ajout de `--init` et `--shm-size=2gb` pour la stabilité
4. **Mount point** : Utilisation de `/workspace` au lieu de `/home/rstudio/workspace`

### Si l'erreur persiste

1. **Essayez la version simplifiée** : Renommez `.devcontainer/devcontainer.json.simple` en `.devcontainer/devcontainer.json` pour utiliser l'image directement sans build personnalisé.

2. Vérifiez les logs du pod :
   ```bash
   kubectl logs <nom-du-pod> -n devpod
   ```

3. Vérifiez les ressources disponibles :
   ```bash
   kubectl describe pod <nom-du-pod> -n devpod
   ```

4. Essayez de reconstruire l'image :
   ```bash
   devpod rebuild
   ```

5. **Supprimez et recréez le workspace** :
   ```bash
   devpod delete workspace shinyapp
   devpod up
   ```

6. Utilisez Docker au lieu de Kubernetes (voir section suivante)

## Erreur "no nodes available to schedule pods"

Cette erreur indique que votre cluster Kubernetes n'a pas de nœuds disponibles pour planifier le pod DevPod.

### Solutions

#### 1. Vérifier les nœuds du cluster

Connectez-vous à votre cluster Kubernetes et vérifiez les nœuds :

```bash
kubectl get nodes
```

Si aucun nœud n'est disponible ou tous sont en état `NotReady`, c'est le problème.

#### 2. Vérifier les ressources disponibles

Vérifiez les ressources de votre cluster :

```bash
kubectl top nodes
kubectl describe nodes
```

Si les nœuds sont saturés (CPU/mémoire), libérez des ressources ou ajoutez des nœuds.

#### 3. Utiliser le provider Docker au lieu de Kubernetes

Si vous utilisez DevPod avec Kubernetes et rencontrez des problèmes, essayez d'utiliser Docker directement :

1. Dans DevPod, changez le provider pour utiliser Docker
2. Ou créez un fichier `.devpod.yml` à la racine :

```yaml
provider: docker
```

#### 4. Vérifier les quotas et limites

Si vous utilisez un provider cloud (GCP, AWS, etc.), vérifiez :
- Les quotas de ressources
- Les limites de pods par nœud
- Les quotas de CPU/mémoire

#### 5. Redémarrer DevPod

Parfois, un simple redémarrage résout le problème :

```bash
devpod stop
devpod up
```

#### 6. Vérifier les taints et tolerations

Si vos nœuds ont des taints, vous devrez peut-être ajouter des tolerations dans la configuration DevPod. Contactez votre administrateur Kubernetes.

### Vérification rapide

Exécutez ces commandes pour diagnostiquer :

```bash
# Vérifier les nœuds
kubectl get nodes -o wide

# Vérifier les pods en attente
kubectl get pods --all-namespaces | grep Pending

# Décrire un pod en erreur
kubectl describe pod <nom-du-pod> -n <namespace>
```

### Solution alternative : Utiliser Docker directement

Si Kubernetes pose problème, vous pouvez utiliser Docker directement :

1. Assurez-vous que Docker est installé et fonctionne
2. Dans DevPod, sélectionnez le provider "Docker"
3. Relancez `devpod up`

L'image `rocker/shiny-verse:latest` fonctionnera aussi bien avec Docker qu'avec Kubernetes.
