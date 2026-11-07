# Projet PWEB : application R Shiny d'exploration et de classification

Projet de cours à deux (un poste macOS, un poste Windows). La feuille de route est `PLAN.md` : les étapes se font une par une avec `/step`, chacune sur sa branche.

## Contraintes

- R 4.5.2 (version acceptée par shinyapps.io, ne pas monter de version sans vérifier) et Shiny : `app.R` et dossier `R/` chargé automatiquement, sans golem ni rhino. Packages de l'annexe B de `PLAN.md` ; tout ajout suit les conditions de cette annexe.
- Interface et étude de cas en français ; identifiants, noms de fichiers, commentaires et messages de commit en anglais.
- Statistiques descriptives en 1/n (convention du cours). Ne jamais utiliser `var()`, `sd()` ou `cov()` directement : passer par `R/fct_stats_base.R`.
- Valeurs attendues des tests recalculées indépendamment, jamais recopiées des slides, qui contiennent parfois des erreurs.
- Aucune fuite de données : tout paramètre de prétraitement est appris sur l'entraînement (contrat « recette apprise » de `PLAN.md`).
- Contrat entre onglets fixé dans `PLAN.md` : ne pas changer les signatures des modules sans modifier le plan.
- Code multiplateforme : chemins relatifs construits avec `file.path()`, encodage toujours explicite à la lecture d'un fichier.
- Aucune donnée dans Git : `data/*`, `data-raw/raw/`, `data-raw/out/` et les CSV du cours (`tests/testthat/fixtures/course/`) sont ignorés.
- Une étape qui touche l'interface est aussi vérifiée dans un navigateur (Chrome sans fenêtre piloté par chromote, installé hors du projet), en plus de `testServer`, qui ne simule pas l'aller-retour avec le navigateur.

## Commandes

```bash
Rscript -e 'renv::restore()'                      # installer les packages du lockfile
Rscript -e 'shiny::runApp()'                      # lancer l'app
Rscript -e 'testthat::test_dir("tests/testthat")' # lancer les tests
Rscript -e 'renv::status()'                       # vérifier que renv.lock est à jour
Rscript -e 'renv::snapshot()'                     # enregistrer un package ajouté au code
Rscript deploy.R                                  # déployer, depuis main propre seulement
```

## Git

Une branche par étape, créée depuis `main` à jour, fusionnée par demande de fusion après relecture par un agent indépendant, puis supprimée. Jamais de commit direct sur `main`. Messages de commit courts, en anglais, à l'impératif.
