# Plan du projet PWEB : application R Shiny d'exploration et de classification

Chaque étape se fait sur sa propre branche et se termine par une demande de fusion vers `main` (voir « Workflow Git »). La demande est relue par un agent indépendant, lancé dans un contexte neuf et distinct de celui qui a codé l'étape : il relit le code, lance les tests et `renv::status()`, et la fusion attend la correction des points bloquants. À partir de E2, les tests tournent aussi en CI (GitHub Actions) sur chaque demande de fusion, et GitHub bloque la fusion s'ils échouent. La case de l'étape est cochée dans cette demande de fusion. Les exigences et le cadrage de l'étude sont résumés en annexe.

## Objectif et critère de réussite

Livrer l'URL d'une application R Shiny déployée sur shinyapps.io qui :

1. charge, type, prétraite, explore et modélise n'importe quel fichier tabulaire CSV ou TSV (onglets 1 à 4, aucun traitement propre à BAAC hormis le bouton qui charge l'échantillon BAAC) ;
2. présente dans l'onglet 5 l'étude de cas BAAC réalisée avec cet outil.

Critère de réussite global, vérifié en E16 sur l'URL publique depuis une machine qui n'a pas servi au développement. Avec `bank-additional-full.csv` (UCI Bank Marketing, jamais chargé avant E16) :

- chargement du fichier, correction d'un type, déclaration de `unknown` comme valeur manquante ;
- imputation, regroupement des modalités rares, normalisation, dummification ;
- une analyse univariée, une analyse bivariée et le tableau des liaisons avec `y` ;
- entraînement de la logistique et de la forêt, puis matrice de confusion, Precision, Recall, F1, ROC superposées, AUC et importances.

Tout cela sans erreur ni déconnexion du serveur. L'onglet 5 affiche l'étude complète, et chaque chiffre cité se retrouve soit dans l'app, soit dans la sortie de `data-raw/03_final_models.R`.

## Contraintes retenues

- R et Shiny, `app.R` et dossier `R/` chargé automatiquement, sans golem ni rhino. Packages de l'annexe B ; tout ajout suit les conditions de l'annexe B.
- Exigences A.1 (données), A.2 (exploration, visualisations et indices du cours), A.3 (au moins deux modèles, métriques, importances), A.4 (étude de cas dans l'onglet 5), A.5 (déploiement).
- Classification binaire ; cible à plus de deux modalités traitée en un contre tous (l'utilisateur choisit la modalité positive).
- Supervisé seulement : le non supervisé (B2) et XGBoost (B1) sont des bonus, faits seulement s'il reste du temps et si les critères de l'annexe E sont remplis sur l'app déployée.
- BAAC : usagers de France métropolitaine, 2019 à 2024 ; entraînement 2019-2022, validation 2023, test 2024 ; jamais de découpage aléatoire par usager ; cible `grave` (`grav` dans {2, 3}).
- Git : une branche par étape, créée depuis `main` et supprimée après fusion ; aucun commit direct sur `main`, sauf le commit initial qui ajoute ce plan ; seul `main` est déployé.

Décisions prises pendant la planification :

- Contrat entre onglets : « recette apprise ». Le Prétraitement transmet des données nettoyées et des réglages ; les paramètres sont appris sur l'entraînement dans l'onglet Modélisation (détail plus bas).
- Données et types circulent dans un seul réactif (`dataset`, une liste `data` + `var_types`), pas dans deux. `var_types` doit rester modifiable (l'utilisateur corrige les types) ; avec deux réactifs, sa remise à zéro à l'arrivée d'un nouveau fichier passe par un observateur, et l'aval peut s'exécuter entre-temps sur le nouveau fichier avec les anciens types. Dans `mod_data`, le couple est donc stocké dans une seule `reactiveVal`, que chaque observateur (chargement, correction d'un type) remplace en une fois ; un `reactive()` qui recombine deux sources séparées ne suffit pas.
- Variables du choc (`col`, `choc`, `obs`, `obsm`) : absentes du modèle BAAC principal, ajoutées dans une variante comparée (ablation) en E12. Elles restent dans le `.rds`.
- Variables des modèles BAAC fixées par une liste blanche versionnée (`data-raw/model_vars.R`), vérifiée par assertion. L'année `an` n'en fait pas partie : le test portant sur 2024, un effet année appris sur 2019-2022 serait une extrapolation.
- Interface et étude de cas en français ; identifiants, noms de fichiers, commentaires et messages de commit en anglais (`mod_data.R` plutôt que `mod_donnees.R`, `detect_types()` plutôt que `detecter_types()`).
- `CLAUDE.md` versionné : instructions du projet pour les agents (contraintes, conventions, commandes). `.claude/settings.local.json`, propre à chaque poste, est ignoré.
- Données : aucun fichier de données ni sortie de script dans Git. Les `.rds` de `data/` sont regénérés par `data-raw/` ou transmis hors de Git.
- CSV du cours non versionnés, leurs droits de diffusion étant inconnus : chaque poste les copie dans `tests/testthat/fixtures/course/` (ignoré), et les tests qui en dépendent commencent par `skip_if_not(file.exists(...))`. Les tests de référence recopient directement les données utiles des slides (48 âges de la slide 01, table Sex × Level).
- Déploiement par `deploy.R` (versionné), lancé par la personne qui termine l'étape, toujours vers le même compte shinyapps.io et la même application, pour garder une seule URL. Le dossier `rsconnect/` (enregistrement local du déploiement) n'est pas versionné : `deployApp(forceUpdate = TRUE)` met à jour l'application du même nom sur le compte, même depuis un poste sans enregistrement.
- `.rds` lus une seule fois au démarrage (niveau global de `app.R`) et partagés entre sessions. S'ils sont absents, le bouton BAAC et l'onglet 5 affichent un message au lieu de planter.
- Onglet 5 sans boutons qui reconfigurent les autres onglets : sous chaque figure, les réglages exacts pour la rejouer dans les onglets 1 à 4, et des figures calculées par les mêmes fonctions que les onglets 3 et 4. Des boutons obligeraient le module de l'onglet 5 à piloter les entrées des autres modules, ce qui casserait le contrat entre onglets.
- Seuil de décision choisi par modèle, sur une validation interne prise dans l'entraînement (F1 maximal par défaut, curseur sur la validation) ; le test n'est évalué qu'à la demande, avec les seuils alors figés. L'AUC est le critère principal de comparaison.

## Choix techniques

| Sujet | Choix | Justification |
|---|---|---|
| Lecture de BAAC | `data.table::fread` | rapide et économe en mémoire sur 24 fichiers, gère séparateur et encodage |
| Statistiques descriptives | codées à la main en 1/n dans `R/fct_stats_base.R`, partagé par l'exploration univariée et bivariée | convention du cours, alors que `var()`, `sd()` et `cov()` divisent par n − 1 |
| Valeurs attendues des tests | recalculées indépendamment (calcul à la main sur un petit exemple, ou fonction de R de base corrigée du facteur n / (n − 1)) | les slides contiennent parfois des erreurs de résultat : elles servent de recoupement, et un écart est expliqué en commentaire du test |
| Graphiques | `ggplot2`, plus `Hmisc::histbackback` et `UsingR::scatter.with.hist` | les deux derniers pour les graphiques du cours qui en dépendent |
| Outliers | règle de Tukey (1,5 × IQR) avec les quartiles de `quantile(type = 7)`, et z-score au-delà d'un seuil réglable (3 par défaut) | quartiles de `geom_boxplot` ; l'interface signale que la slide de C02 calcule les quartiles autrement (Q1 = 35 contre 36) et prolonge les moustaches jusqu'aux extrêmes |
| Rapport de corrélation | `c_{Y\|X} = s_E / s_Y`, variances en 1/n | notation du cours, pas η² |
| Modèles | `glm(family = binomial)`, `ranger(probability = TRUE, respect.unordered.factors = "order")`, `rpart` | logistique interprétable (`quasibinomial` si les poids ne sont pas entiers : mêmes coefficients, sans avertissement) ; forêt rapide en C++ (Breiman, 2001 ; Wright et Ziegler, 2017), avec `"order"` parce que le défaut `"ignore"` traite un facteur nominal comme ordonné selon l'ordre de ses niveaux (Wright et König, 2019) ; arbre presque gratuit avec l'interface commune |
| ROC et AUC | `pROC`, `direction = "<"` et `levels` toujours fixés, jamais `"auto"` ; seule l'AUC univariée de l'alerte de fuite (E11a) est ensuite symétrisée explicitement par `max(AUC, 1 − AUC)` | le mode `"auto"` inverse la courbe d'un mauvais modèle et gonfle l'AUC (Robin et al., 2011) ; l'alerte mesure la force d'un lien, pas son sens |
| Importance | permutation (baisse d'AUC quand on permute une variable d'origine, avant la recette) pour tous les modèles ; pour la logistique, en plus, valeur absolue des coefficients standardisés, odds ratios et IC à 95 % (Wald) sur le modèle non pondéré | comparable d'un modèle à l'autre (Fisher et al., 2019) ; coefficients standardisés exigés par le cadrage ; pas d'importance par impureté de `ranger`, biaisée en faveur des variables continues et à forte cardinalité (Strobl et al., 2007) |
| Découpage dans l'app | stratifié aléatoire, groupé par identifiant (stratifié au niveau du groupe), ou « par variable » (test = lignes dont une colonne prend des valeurs données, par exemple `an = 2024`) ; validation interne de même groupement prise dans l'entraînement | le mode par variable rejoue la logique de l'étude ; la validation sert à choisir les seuils |
| Seuil de décision | un par modèle, F1 maximal sur la validation interne par défaut, réglable par un curseur sur la validation ; test évalué sur bouton, seuils figés ; AUC comme critère principal de comparaison | un seuil commun réglé sur le test rendrait le tableau P, R, F1 arbitraire, et un curseur sur le test reviendrait à choisir sur le test |
| Référence naïve | score constant (AUC = 0,5) et règle « tout positif » (Precision = prévalence, Recall = 1) dans chaque tableau comparatif | sur une classe minoritaire, Precision et F1 ne se lisent qu'à côté de ce plancher |
| Déséquilibre | pondération, sous-échantillonnage, sur-échantillonnage par duplication, sur l'entraînement seul ; dans l'étude, pondération contre « aucun rééquilibrage + seuil choisi sur 2023 » | ces corrections dégradent souvent la calibration sans gain d'AUC (van den Goorbergh et al., 2022) ; SMOTE inutile ici |
| Entraînement | bouton (`eventReactive`, comme dans le cours), barre de progression, `tryCatch` avec `showNotification` | pas de réentraînement à chaque changement d'entrée ; une erreur n'emporte pas la session |
| Niveau de référence des facteurs | modalité la plus fréquente de l'entraînement | l'ordre alphabétique des niveaux dépend de la locale |
| Modèles finaux BAAC | entraînés sur 2019-2022 ; petite grille de réglages (`mtry` et `min.node.size` pour la forêt, `cp` pour l'arbre), rééquilibrage et seuil choisis sur 2023 ; évaluation unique sur 2024 | la forêt est en général peu sensible à ses réglages, `mtry` et la taille des nœuds étant les plus influents (Probst et al., 2019) : une petite grille évite que la comparaison avec la logistique dépende des valeurs par défaut ; pas de réentraînement sur 2019-2023, pour que le seuil reste celui du modèle évalué |
| Intervalles de l'étude | bootstrap par grappes sur les accidents de 2024 | les usagers d'un même accident ne sont pas indépendants |
| `data/final_models.rds` | résultats seulement (métriques, points ROC, importances, coefficients, seuils, intervalles) | une forêt entraînée sur 400 000 lignes pèserait des centaines de Mo |
| Tests | `testthat`, modules testés avec `shiny::testServer()` | modules testés sans navigateur |

## Architecture

```
Navigateur --> shinyapps.io : app.R (navbarPage, 5 onglets)
  au démarrage : baac_sample   <- data/accidents_sample.rds
                 final_results <- data/final_models.rds

  1 Données        mod_data        (baac_sample)               --> dataset
  2 Prétraitement  mod_preprocess  (dataset)                   --> dataset_clean, recipe
  3 Exploration    mod_explore     (dataset_clean)                 uni et bi
  4 Modélisation   mod_model       (dataset_clean, recipe)
                     découpage --> fit_recipe(train) --> apply_recipe --> rééquilibrage
                     --> entraînement --> seuil sur la validation --> métriques sur le test (sur bouton)
  5 Étude de cas   mod_case_study  (baac_sample, final_results)

Hors de l'app (data-raw/, lancé à la main, mêmes fonctions de R/) :
  01_download_baac.R --> data-raw/raw/ --> 02_prepare_baac.R --> data-raw/out/accidents_full.rds
                                                            \-> data/accidents_sample.rds
  accidents_full.rds --> 03_final_models.R --> data/final_models.rds
```

## Contrat entre onglets

Signatures fixées en E2, puis respectées par toutes les étapes. Seules E1, E2 et E13 modifient `app.R`.

```
baac_sample   <- load_rds_or_null("data/accidents_sample.rds")   # niveau global de app.R
final_results <- load_rds_or_null("data/final_models.rds")

mod_data_server(id, baac_sample)
  -> dataset = reactive(list(data = <data.frame>, var_types = <chr nommé>))
mod_preprocess_server(id, dataset)
  -> list(dataset_clean = reactive(list(data = <data.frame>, var_types = <chr nommé>)),
          recipe = reactive(<list>))
mod_explore_server(id, dataset_clean)
mod_model_server(id, dataset_clean, recipe)
mod_case_study_server(id, baac_sample, final_results)
```

- `dataset` et `dataset_clean` : un seul réactif par jeu de données, pour que `data` et `var_types` changent toujours ensemble. `validate_dataset()` renvoie `TRUE` ou `FALSE` sans lever d'erreur, pour que `req()` attende en silence ; elle vérifie que `names(var_types)` est identique à `names(data)` et que les classes concordent : `"quanti"` → numérique, `"quali"` → facteur, `"id"` et `"ignore"` → classe libre. Les modules consommateurs commencent par `req(validate_dataset(...))`.
- `var_types` : vecteur nommé par les colonnes, valeurs dans `"quanti"`, `"quali"`, `"id"`, `"ignore"`.
- `recipe` reste un réactif distinct, pour qu'un changement de réglage ne relance pas l'Exploration. Il ne référence que des types ou des noms de colonnes, et `fit_recipe()` ignore un réglage qui porte sur une colonne absente.
- `data_clean` (le `data` de `dataset_clean`) : données typées, codes manquants convertis en `NA`, colonnes retirées, suppression des lignes incomplètes si l'utilisateur la choisit. Aucune opération apprise : pas d'imputation, de normalisation ni de dummification. L'Exploration n'utilise que cet objet. Chaque analyse travaille sur les cas complets des variables concernées et affiche le nombre de lignes exclues.
- `recipe` : liste des réglages choisis. `fit_recipe(recipe, train, var_types)` apprend les paramètres ; `apply_recipe(fitted, newdata)` les applique sans rien réapprendre. Ordre des étapes :
  1. outliers : seuils appris sur l'entraînement ; action « signaler », « plafonner » (entraînement et test) ou « supprimer » (lignes de l'entraînement seulement, le test n'est jamais amputé) ;
  2. imputation : médiane ou moyenne (quanti), mode ou modalité « Manquant » (quali), valeurs apprises sur l'entraînement ;
  3. modalités rares : fréquence sous un seuil dans l'entraînement, regroupées en « Autre » ; les modalités du test absentes de l'entraînement vont aussi dans « Autre », qui existe toujours comme niveau ; si « Autre » n'a aucune ligne dans l'entraînement, elles vont dans la modalité la plus fréquente de l'entraînement (avec `respect.unordered.factors = "order"`, un niveau sans ligne d'entraînement est rangé après tous les autres, donc envoyé à chaque coupure du côté du plus fort taux de positifs ; Au, 2018) ;
  4. normalisation : centrage-réduction (moyenne et écart-type en 1/n de l'entraînement) ou min-max (min et max de l'entraînement, le test peut sortir de [0, 1]) ; une colonne constante dans l'entraînement (écart-type ou étendue nulle) est retirée avec un message ;
  5. dummification : modalités de l'entraînement, one-hot ou k − 1 colonnes (référence = modalité la plus fréquente de l'entraînement) ; k − 1 imposé pour la logistique, dont l'intercept rend le one-hot colinéaire ; une variable qui dépasse un plafond de colonnes après regroupement est refusée avec un message.
  La cible n'est jamais transformée ; les lignes sans cible sont retirées avant le découpage.
- Onglet Prétraitement : l'aperçu et l'export CSV appliquent une recette ajustée sur tout `data_clean`, avec la mention « aperçu ajusté sur tout le jeu, la modélisation réajuste sur l'entraînement ».
- Onglet Modélisation : échantillon de taille réglable → découpage (entraînement, validation interne, test) → `fit_recipe(train)` → `apply_recipe` sur les trois → rééquilibrage de l'entraînement → entraînement → seuil de chaque modèle choisi sur la validation → sur bouton, prédiction et métriques sur le test avec les seuils figés.

## Étapes

### Jalon 1 : squelette, dépôt, premier déploiement

Couvre : le contrat entre onglets et un premier déploiement (A.5).

- [x] **E1. Squelette déployé**
  - Objectif : postes de travail et dépôt prêts, application vide déployée.
  - Contenu :
    - installer R 4.5.2, la dernière version signalée comme acceptée par shinyapps.io (R 4.6.2 y est refusée : « Unsupported R version 4.6.2 for operating system jammy ») : sur macOS, l'installeur `R-4.5.2-arm64.pkg` de CRAN ; sur Windows, `R-4.5.2-win.exe` (anciennes versions de CRAN), plus Git for Windows, dont Git Bash sert de terminal pour que toutes les commandes du plan soient identiques sur les deux postes ; même version sur les deux postes, vérifiée par `R --version` et enregistrée dans `renv.lock`, et le premier déploiement confirme que shinyapps.io l'accepte ; `mgcv` et `nlme` restent aux versions livrées avec R 4.5.2, pour éviter toute compilation (`nlme` 3.1-171 n'a pas de binaire macOS pour R 4.5 et demanderait gfortran) ;
    - `renv::init()`, installer les packages de l'annexe B, `renv::snapshot()` ;
    - `app.R` : `navbarPage` à 5 onglets vides, `library(Hmisc)` et `library(UsingR)` chargés pour tester leur installation sur le serveur ;
    - `.gitattributes` : `* text=auto eol=lf`, pour que les fins de ligne Windows (CRLF) ne fassent pas apparaître chaque fichier comme modifié ;
    - `.gitignore` : `.DS_Store`, `Thumbs.db`, `desktop.ini`, `.Rhistory`, `.RData`, `.Rproj.user/`, `.Renviron`, `.env`, `rsconnect/`, `Rplots.pdf`, `data/*` sauf `data/.gitkeep`, `data-raw/raw/`, `data-raw/out/`, `tests/testthat/fixtures/course/`, `.claude/settings.local.json` ; `renv/library/` est géré par le `.gitignore` de renv ; les fichiers propres à un poste vont dans son `.git/info/exclude` ;
    - `CLAUDE.md` : contraintes du projet, conventions, commandes pour lancer, tester et déployer ;
    - `deploy.R` : vérifie que la branche courante est `main`, sans modification locale et identique à `origin/main`, affiche la liste des fichiers, puis `rsconnect::deployApp(appFiles = ..., forceUpdate = TRUE)` avec seulement `app.R`, `R/`, `renv.lock` et les `.rds` de `data/` présents ;
    - `README.md` minimal : lancer, tester, déployer ;
    - `tests/testthat/setup.R`, qui charge les fonctions de `R/`, et un test de fumée (l'app se construit sans erreur) ;
    - travail sur la branche `e1-skeleton` et demande de fusion, comme pour toutes les étapes ; accès en écriture au dépôt pour l'autre membre ; après la fusion, `main` protégée sur GitHub (modifiable seulement par demande de fusion) ;
    - premier déploiement par `deploy.R` ; relever les limites réelles de l'offre gratuite (RAM, heures actives, taille du bundle) dans le README ; le délai d'inactivité reste à 15 min, non modifiable sur l'offre gratuite, et chaque réveil de l'app consomme donc au moins 15 min d'heures actives.
  - Fichiers : `app.R`, `renv.lock`, `.Rprofile`, `renv/`, `.gitignore`, `.gitattributes`, `deploy.R`, `README.md`, `CLAUDE.md`, `PLAN.md`, `data/.gitkeep`, `tests/testthat/setup.R`, `tests/testthat/test-smoke.R`.
  - Vérification :
    - `Rscript -e 'shiny::runApp()'` affiche les 5 onglets ;
    - `Rscript -e 'testthat::test_dir("tests/testthat")'` passe ;
    - l'URL shinyapps.io affiche les 5 onglets, sans erreur dans les logs au chargement de `Hmisc` et `UsingR` ;
    - les logs de déploiement ne listent que les fichiers prévus ;
    - avant le premier commit de la branche, `git status --short` ne liste que des fichiers à versionner (ni `.DS_Store`, ni `renv/library`, ni données) ;
    - `git ls-remote origin` montre `main`, et un push direct sur `main` est refusé.
  - Hors périmètre : tout contenu dans les onglets.

- [x] **E2. Contrat entre onglets**
  - Dépend de : E1.
  - Objectif : tous les modules existent avec leurs signatures définitives, pour que les étapes suivantes avancent en parallèle sans toucher `app.R`.
  - Contenu :
    - `app.R` : `options(shiny.maxRequestSize = 30 * 1024^2)` (Shiny refuse par défaut les fichiers de plus de 5 Mo, or `bank-additional-full.csv` en fait environ 5,5), lecture globale des `.rds` par `load_rds_or_null()` ;
    - `mod_data` lit un CSV avec les options par défaut et renvoie `dataset()`, colonnes quali converties en facteurs ;
    - `validate_dataset()` dans `R/fct_data.R` (noms et classes cohérents avec `var_types`) ;
    - `tests/testthat/test-generic.R` : échoue si un nom de colonne propre à BAAC (`Num_Acc`, `grav`, `catu`, `catv`, `an_nais`, `hrmn`, `id_vehicule`, `id_usager`, cherchés comme mots entiers, sans tenir compte de la casse) apparaît dans `R/` hors des fichiers de l'onglet 5 (`mod_case_study.R`, `case_study_*.R`) ; placé dès E2 pour protéger les étapes menées en parallèle ;
    - `detect_types()` : règle simple et documentée (numérique → quanti, caractère → quali, caractère à valeurs toutes distinctes ou à plus de 50 modalités → id) ; un numérique entier à valeurs toutes distinctes reste quanti mais déclenche une alerte « identifiant possible » ;
    - `R/fct_stats_base.R` : `mean_1n`, `var_1n`, `sd_1n`, `cov_1n` ;
    - `mod_preprocess` laisse passer les données (`dataset_clean = dataset`, `recipe = default_recipe()`), avec `fit_recipe()` et `apply_recipe()` en fonctions identité à leur signature finale ;
    - modules `mod_explore` (avec `mod_explore_uni` et `mod_explore_bi`), `mod_model` et `mod_case_study` en squelettes ; les onglets 3 et 4 affichent les dimensions de `dataset_clean()$data`, l'onglet 5 un message si `final_results` est absent ;
    - CI : workflow GitHub Actions qui, sur chaque demande de fusion vers `main`, installe R et les packages de `renv.lock` (`r-lib/actions/setup-r` et `r-lib/actions/setup-renv`) puis lance `testthat` ; ce test devient obligatoire dans la protection de `main` (« Require status checks »). Les tests qui dépendent des CSV du cours sont sautés en CI, faute de fichiers.
  - Fichiers : `.github/workflows/tests.yml`, `app.R`, `R/utils_load.R`, `R/fct_stats_base.R`, `R/mod_data.R`, `R/fct_data.R`, `R/mod_preprocess.R`, `R/fct_recipe.R`, `R/mod_explore.R`, `R/mod_explore_uni.R`, `R/mod_explore_bi.R`, `R/mod_model.R`, `R/mod_case_study.R`, `tests/testthat/test-types.R`, `tests/testthat/test-stats-base.R`, `tests/testthat/test-contract.R`, `tests/testthat/test-generic.R`.
  - Vérification :
    - `detect_types()` donne les types attendus sur `students.csv` (`Sex`, `Major`, `Level`, `BloodType` en quali ; `Height`, `Sleep`, `MilesHome` en quanti) et sur `chiens.csv` (4 quanti) ; une colonne d'identifiants caractère est typée id ;
    - `var_1n(c(1, 2, 3, 4)) == 1.25` ;
    - `testServer` sur `mod_data_server` renvoie un réactif dont la valeur passe `validate_dataset()` ;
    - `validate_dataset()` renvoie `FALSE` pour une liste dont `var_types` n'a pas les mêmes noms que `data`, ou annonce `"quanti"` pour une colonne non numérique ;
    - `test-generic.R` passe, et échoue si l'on ajoute temporairement `grav` dans un fichier de `R/` ;
    - l'app démarre sans les `.rds` de `data/` ;
    - dans l'app, après chargement de `students.csv`, les onglets 3 et 4 affichent les mêmes dimensions que `read.csv` ;
    - la demande de fusion de E2 affiche la CI en vert ; une demande de fusion de contrôle avec un test volontairement faux est en rouge et ne peut pas être fusionnée, puis elle est fermée sans fusion.
  - Hors périmètre : options d'import et édition des types (E5), toute transformation (E6).

- [x] **E3. Audit du schéma BAAC 2019 à 2024**
  - Dépend de : E1 (en parallèle de E2).
  - Objectif : connaître toutes les différences entre années avant d'écrire la préparation.
  - Contenu : script de téléchargement des 4 tables × 6 ans et du guide de codification ; `SCHEMA.md` en tableau année × table : colonnes, séparateur, encodage, décimale de `lat` et `long`, format de `hrmn`, nom de la clé d'accident, présence de `id_usager`, nombre de lignes, unicité de `Num_Acc` dans caractéristiques et lieux, codage des usagers en fuite, plages d'âges et de `vma`.
  - Fichiers : `data-raw/01_download_baac.R`, `data-raw/audit_baac.R` (commandes qui appuient `SCHEMA.md`), `data-raw/SCHEMA.md` (fichiers bruts dans `data-raw/raw/`, ignoré).
  - Vérification : le script se relance de zéro ; chaque point « à vérifier » de l'annexe C a une réponse appuyée par une commande ou une sortie dans `SCHEMA.md`.
  - Hors périmètre : nettoyage et jointure.

- [x] **Redéploiement J1** : après fusion de E2 et E3, `main` redéployé ; `students.csv` chargé sur l'URL publique, dimensions affichées.

### Jalon 2 : table BAAC propre, point de décision

Couvre : les données de l'étude de cas (A.4), ou la bascule sur le plan B.

- [x] **E4. Table BAAC propre et jointe**
  - Dépend de : E3.
  - Objectif : une table au niveau usager, sans fuite, prête pour l'app et pour les modèles finaux.
  - Contenu :
    - harmoniser les noms de colonnes (casse, clé de 2022) ;
    - recoder les manquants (-1, 0 selon la variable, vide, ".") ;
    - exclure les usagers en fuite, les `grav` non renseignés et les départements hors métropole ;
    - joindre caractéristiques, lieux, véhicules et usagers au niveau usager ;
    - créer `grave` et les variables dérivées (âge à partir de `an_nais`, tranche horaire, jour de la semaine, mois) ;
    - regrouper `catv` ;
    - convertir les codes en facteurs libellés ; identifiants (`Num_Acc`, `id_vehicule`, `id_usager`) stockés en caractère, pour être typés id dans l'app ;
    - aucune variable agrégée à partir de `grav` des autres usagers de l'accident.
    Deux sorties :
    - `data-raw/out/accidents_full.rds`, toutes les lignes, ignoré et non déployé ;
    - `data/accidents_sample.rds` : accidents tirés au hasard (graine fixe), stratifiés par année, avec tous leurs usagers.
  - Fichiers : `data-raw/02_prepare_baac.R`, `data-raw/baac_labels.R` (tables de libellés).
  - Vérification, imprimée par le script (contrôles en `stopifnot`) :
    - même nombre d'usagers avant et après jointure ;
    - aucun usager en double ;
    - % de `grave` par année ;
    - `accidents_sample.rds` de moins de 20 Mo, et de moins de 50 Mo en mémoire (`object.size`) ;
    - `Rscript data-raw/02_prepare_baac.R` se relance de zéro.
  - Hors périmètre : bouton BAAC (E5), modèles (E12).

- [x] **Point de décision BAAC** (fin du jalon 2) : si E4 n'est pas fusionnée, bascule sur le plan B (annexe D). Décision : E4 terminée, l'étude reste sur BAAC.

### Jalon 3 : Données et Prétraitement sur n'importe quel CSV

Couvre : le chargement et le prétraitement (A.1).

- [x] **E5. Onglet Données complet**
  - Dépend de : E2 ; le bouton BAAC est branché après la fusion de E4.
  - Objectif : importer un fichier tabulaire quelconque et corriger ses types.
  - Contenu :
    - options d'import : séparateur, en-tête, encodage, décimale, codes manquants déclarables (`-1`, `0`, `N/A`, `.`…) ;
    - aperçu en `DT` ;
    - tableau des variables (type détecté, type modifiable, nombre de modalités, % de NA, alerte de forte cardinalité, alerte « identifiant possible ») ;
    - bouton « charger l'échantillon BAAC », désactivé avec un message si l'échantillon est absent.
  - Fichiers : `R/mod_data.R`, `R/fct_data.R`, `tests/testthat/test-data.R`.
  - Vérification :
    - chargement sans erreur de `students.csv`, `chiens.csv`, d'un TSV et d'un CSV à point-virgule et virgule décimale fabriqués pour le test, d'un CSV synthétique de 10 Mo et de l'échantillon BAAC ;
    - tests : codes manquants convertis sur un CSV jouet, % de NA exact, changement de type répercuté dans `dataset()$var_types` et dans la classe de la colonne de `dataset()$data` ;
    - mémoire de la session après chargement de BAAC relevée.
  - Hors périmètre : regroupement des modalités rares (appris, donc dans E6a).

- [x] **E6a. Fonctions de la recette**
  - Dépend de : E2.
  - Objectif : implémenter `fit_recipe()` et `apply_recipe()` selon le contrat, sans interface.
  - Contenu : les 5 étapes du contrat, avec les cas limites (colonne constante, modalité inconnue, plafond de colonnes).
  - Fichiers : `R/fct_recipe.R`, `tests/testthat/test-recipe.R`.
  - Vérification par tests :
    - après centrage-réduction, moyenne 0 et écart-type (1/n) égal à 1 sur l'entraînement, et test transformé égal à `(x − μ_train) / σ_train` ;
    - paramètres égaux aux valeurs calculées à la main sur un jeu jouet ;
    - k − 1 colonnes par variable dummifiée (k en one-hot), référence = modalité la plus fréquente ;
    - une modalité inconnue du test va dans « Autre » si « Autre » a des lignes dans l'entraînement, sinon dans la modalité la plus fréquente, sans erreur ;
    - un réglage qui porte sur une colonne absente de `train` est ignoré, sans erreur ;
    - une colonne constante est retirée avec un message, sans NaN ;
    - « supprimer les outliers » ne retire aucune ligne du test ;
    - sans suppression d'outliers, `apply_recipe` sur l'entraînement redonne exactement la sortie de l'ajustement.
  - Hors périmètre : interface (E6b), rééquilibrage (E7), découpage (E10a).

- [ ] **E6b. Onglet Prétraitement**
  - Dépend de : E2 (en parallèle de E6a ; la vérification dans l'app attend E6a).
  - Objectif : l'interface qui règle la recette, en parallèle de E6a.
  - Contenu : réglages des 5 étapes, opérations non apprises de `data_clean` (colonnes retirées, suppression des lignes incomplètes), affichage des outliers par variable, aperçu et export CSV (`downloadHandler`).
  - Fichiers : `R/mod_preprocess.R`, `tests/testthat/test-preprocess.R`.
  - Vérification : `testServer` : une colonne retirée disparaît de `dataset_clean()$data` et de `dataset_clean()$var_types`, et la valeur passe `validate_dataset()` ; `recipe()` reflète les réglages ; après fusion de E6a, `students.csv` prétraité et exporté dans l'app.
  - Hors périmètre : calcul des paramètres (E6a).

- [ ] **E7. Fonctions de rééquilibrage**
  - Dépend de : E2.
  - Objectif : pondération, sous-échantillonnage et sur-échantillonnage, qui ne touchent que l'entraînement.
  - Contenu : `class_weights(y)` (poids `n / (K · n_c)`), `undersample()` et `oversample()` avec graine et ratio cible, et `balance_train(split, method)`, qui renvoie le découpage avec seul l'entraînement modifié.
  - Fichiers : `R/fct_balance.R`, `tests/testthat/test-balance.R`.
  - Vérification par tests :
    - `identical(out$test, split$test)` et `identical(out$valid, split$valid)` pour les trois méthodes ;
    - ratio de classes atteint après échantillonnage ;
    - même graine, même résultat.
  - Hors périmètre : branchement dans l'interface (E11b).

- [ ] **Redéploiement J3** : après fusion de E5, E6a, E6b et E7, sur l'URL publique, `students.csv` chargé, retypé, prétraité et exporté ; mémoire relevée après chargement de BAAC.

### Jalon 4 : Exploration complète, deux modèles et métriques

Couvre : l'exploration (A.2) et le début de la modélisation (A.3).

- [ ] **E8. Exploration univariée**
  - Dépend de : E2.
  - Objectif : les visualisations et indicateurs univariés du cours, sur n'importe quelle variable.
  - Contenu :
    - quanti : tableau statistique (`n_i`, `f_i`, `N_i`, `F_i`, par classes pour une variable continue, comme sur la slide 02), diagramme en bâtons et cumulatif, boîte à moustaches, histogramme en effectifs ou en densité (classes de Sturges, Scott ou Freedman-Diaconis via `nclass.Sturges`, `nclass.scott`, `nclass.FD`), courbe de densité, indicateurs de centre et de dispersion (fonctions de `fct_stats_base.R`) ;
    - quali : diagramme en colonnes, secteurs, table des effectifs.
  - Fichiers : `R/fct_stats_uni.R`, `R/mod_explore_uni.R`, `tests/testthat/test-stats-uni.R`.
  - Vérification : sur les 48 âges de la slide 01 recopiés dans le test, moyenne 43,625, variance 109,776, écart-type 10,477 (10,588 en n − 1, ce qui distingue les deux conventions).
  - Hors périmètre : bivarié (E9).

- [ ] **E9. Exploration bivariée et indices de liaison**
  - Dépend de : E2.
  - Objectif : les visualisations et indices bivariés du cours, codés à la main, et le tableau des liaisons avec une cible.
  - Contenu :
    - quanti/quanti : nuage de points (sous-échantillon affiché au-delà de quelques milliers de points, signalé dans l'interface), histogrammes dos à dos, nuage avec histogrammes marginaux, droite de régression `â = s_XY / s_X²`, `b̂ = ȳ − â x̄`, covariance (1/n) et `r` ;
    - quanti/quali : boîtes parallèles, `c_{Y|X} = s_E / s_Y` ;
    - quali/quali : table de contingence, profils (empilé, proportions, côte à côte), `χ²`, `Φ² = χ² / n`, `T = √(Φ² / √((r − 1)(c − 1)))`, `C = √(Φ² / (min(r, c) − 1))` ;
    - tableau des liaisons de chaque variable avec la cible choisie, avec le nom de l'indice selon les types (les indices ne sont pas sur la même échelle, l'interface le dit).
  - Fichiers : `R/fct_stats_bi.R`, `R/mod_explore_bi.R`, `tests/testthat/test-stats-bi.R`.
  - Vérification :
    - `secteursActivite.csv` : `r(EF, CA) ≈ 0,661`, `â ≈ 0,509`, `b̂ ≈ 20884` ;
    - `chiens.csv` mis en format long : `c ≈ 0,598` en 1/n, avec `s_E² + s_R² = s_Y²` vérifié (la slide donne 0,59 parce que le script du cours calcule les variances intra-classes en n − 1) ;
    - Sex × Level de `students.csv` : `χ²` égal à `chisq.test(tab, correct = FALSE)$statistic` (≈ 12,82, appel dans `suppressWarnings` car 7 cellules sur 12 ont un effectif théorique inférieur à 5), `Φ² ≈ 0,267`, `T ≈ 0,346`, `C ≈ 0,517`.
  - Hors périmètre : Spearman et carte de chaleur des corrélations (absents du cours, bonus éventuel).

- [ ] **E10a. Découpage et métriques à seuil**
  - Dépend de : E2.
  - Objectif : les fonctions de découpage et de métriques, testées, sans interface.
  - Contenu :
    - `split_data()` à trois modes : stratifié, groupé (un groupe n'est jamais de deux côtés, stratification sur « le groupe contient un positif »), par variable ; renvoie entraînement, validation interne (prise dans l'entraînement, même groupement) et test ;
    - échantillonnage par groupe quand un identifiant de groupe est donné ;
    - `confusion()`, `precision()`, `recall()`, `f1()` à un seuil donné (cas limites : aucun positif prédit, aucun positif réel).
  - Fichiers : `R/fct_split.R`, `R/fct_metrics.R`, `tests/testthat/test-split.R`, `tests/testthat/test-metrics.R`.
  - Vérification par tests :
    - métriques égales à celles d'une matrice construite à la main ;
    - aucun groupe en commun entre les trois parties ;
    - proportion de positifs à ± 2 points d'une partie à l'autre ;
    - même graine, même découpage ;
    - le mode par variable met exactement les bonnes lignes en test.
  - Hors périmètre : modèles, ROC, AUC.

- [ ] **E10b. Deux modèles dans l'onglet Modélisation**
  - Dépend de : E6a, E10a.
  - Objectif : chaîne complète dans l'app, de la cible à la matrice de confusion.
  - Contenu :
    - choix de la cible, de la modalité positive (cible recodée explicitement en 0/1) et des variables explicatives (`id` et `ignore` exclues par défaut) ;
    - répartition de la cible affichée dans l'entraînement, la validation et le test ;
    - taille d'échantillon et mode de découpage ;
    - recette ajustée sur l'entraînement ;
    - interface commune `fit_model(type, x, y, weights, ...)` et `predict_proba(model, newx)` pour `glm` et `ranger` (`probability = TRUE`, `respect.unordered.factors = "order"`) ; variable à une seule modalité dans l'entraînement retirée avec un message ;
    - matrice de confusion, Precision, Recall et F1 au seuil 0,5, sur la validation interne (le test arrive avec le bouton de E11a) ;
    - entraînement sur bouton, dans `tryCatch` avec `showNotification`.
  - Fichiers : `R/fct_models.R`, `R/mod_model.R`, `tests/testthat/test-models.R`.
  - Vérification :
    - tests : probabilités dans [0, 1], une par ligne du test, sur un jeu simulé ;
    - test : sur un jeu simulé où `y` croît avec `x`, coefficient de `x` positif, et négatif si l'on choisit l'autre modalité comme positive ;
    - test : une variable constante ou un facteur à une modalité ne fait pas planter l'entraînement ;
    - test : un facteur à 4 modalités `a`, `b`, `c`, `d` (50 lignes chacune) dont `a` et `c` sont positives, appris par `fit_model("ranger", ..., num.trees = 1, max.depth = 1, seed = 1)` (une souche ; les arguments supplémentaires sont transmis à `ranger`), a une erreur de classement nulle sur ces lignes ; la même souche apprise directement par `ranger(..., respect.unordered.factors = "ignore")` se trompe sur exactement un quart des lignes ;
    - chaîne complète sur `students.csv` (cible `Sex`) ;
    - 20 000 lignes BAAC entraînées en moins de 30 s en local.
  - Hors périmètre : ROC, AUC, `rpart`, seuils par modèle, importances, rééquilibrage (E11).

- [ ] **Redéploiement J4** : après fusion de E8, E9 et E10b, 20 000 lignes BAAC entraînées en moins de 30 s sur l'URL publique, mémoire et heures actives relevées dans le tableau de bord shinyapps.io.

### Jalon 5 : évaluation comparative et modèles finaux

Couvre : la fin de la modélisation (A.3) et les résultats de l'étude (A.4).

- [ ] **E11a. ROC, AUC, seuil et comparaison**
  - Dépend de : E10b.
  - Objectif : évaluation comparative des modèles, choix sur la validation, test évalué à la demande avec des seuils figés.
  - Contenu :
    - ROC superposées et AUC (`pROC`, `direction` et `levels` fixés), AUC comme critère principal ;
    - sur la validation interne : un curseur de seuil par modèle, initialisé au seuil de F1 maximal, avec matrice de confusion et tableau comparatif (P, R, F1 au seuil courant, AUC) ;
    - référence naïve dans le tableau : score constant (AUC = 0,5) et règle « tout positif » (Precision = prévalence, Recall = 1) ;
    - bouton « Évaluer sur le test » : fige les seuils courants et affiche le même tableau, la matrice de confusion et les ROC sur le test ; déplacer ensuite un curseur ne change plus ces résultats tant qu'on ne relance pas l'évaluation ; un nouvel entraînement efface les résultats du test, qui ne réapparaissent qu'au clic suivant ;
    - `rpart` ajouté à l'interface commune ;
    - alerte de fuite : `univariate_auc(x_train, y_train, x_eval, y_eval)` donne l'AUC de chaque variable explicative prise seule, sur les variables d'origine (avant la recette) et sans pondération, apprise sur l'entraînement et évaluée sur la validation interne, pour qu'une variable à nombreuses modalités ne soit pas gonflée par un calcul sur les mêmes lignes. Quanti : la valeur elle-même, sur les cas complets. Quali : le taux de positifs de la modalité dans l'entraînement, NA traité comme une modalité, modalité inconnue de l'entraînement remplacée par la prévalence de l'entraînement. Puis `max(AUC, 1 − AUC)` ; 0,5 pour une colonne constante, NA sans cas complet, jamais d'erreur. Au-delà de 0,95, une notification signale une liaison quasi parfaite avec la cible (fuite probable, variable dérivée de la cible ou quasi identifiante), sans bloquer l'entraînement. Même fonction que le contrôle de E12a.
  - Fichiers : `R/fct_metrics.R`, `R/fct_models.R`, `R/mod_model.R`, `tests/testthat/test-metrics.R`.
  - Vérification par tests :
    - sur un jeu jouet avec ex aequo, AUC de `pROC` égale à `W / (n₁ n₀)`, `W` étant la statistique de `wilcox.test(..., exact = FALSE)` ;
    - le Recall est non croissant quand le seuil augmente ;
    - le seuil choisi ne dépend pas des étiquettes du test ;
    - référence naïve : AUC de 0,5 pour un score constant, Precision égale à la prévalence et Recall à 1 pour la règle « tout positif » ;
    - `testServer` : aucune métrique du test n'est calculée avant le bouton, et les métriques du test ne changent pas quand un curseur bouge après l'évaluation, et un nouvel entraînement les efface ;
    - alerte de fuite : sur un jeu jouet, une variable quali qui recopie la cible et une variable quanti strictement croissante avec elle déclenchent l'alerte (AUC univariée de 1), une variable indépendante de la cible ne la déclenche pas, ni une variable quali indépendante dont chaque ligne a sa propre modalité (évaluée hors échantillon) ; `univariate_auc` vaut la même chose pour `x` et `−x` ; une colonne constante donne 0,5 et une colonne entièrement NA donne NA, sans erreur.
  - Hors périmètre : importances, rééquilibrage.

- [ ] **E11b. Importances et rééquilibrage dans l'interface**
  - Dépend de : E7, E11a.
  - Objectif : variables les plus importantes, et branchement de E7.
  - Contenu :
    - `perm_importance(predict_fn, test_raw, y, vars, n_rep, seed)` : baisse d'AUC quand on permute une variable d'origine, avec une prédiction qui inclut `apply_recipe`, calculée après le bouton « Évaluer sur le test », sur un sous-échantillon du test (taille réglable) ;
    - graphique par modèle ;
    - pour la logistique : valeur absolue des coefficients standardisés (`|β_j| × s_j`, `s_j` écart-type en 1/n de la colonne j dans l'entraînement après recette) ; odds ratios et IC à 95 % (`exp(cbind(coef(fit), confint.default(fit)))`) sur le modèle non pondéré, avec la mention que ces IC supposent des observations indépendantes ;
    - choix de la stratégie de rééquilibrage, appliquée via `balance_train()` (`quasibinomial` pour la logistique pondérée), répartition des classes affichée après rééquilibrage.
  - Fichiers : `R/fct_importance.R`, `R/mod_model.R`, `tests/testthat/test-importance.R`.
  - Vérification :
    - test sur données simulées où `y` ne dépend que de `x1` : importance de `x1` > 0, importance d'une variable de bruit proche de 0 ;
    - test : la validation et le test sont identiques avec et sans rééquilibrage ;
    - `testServer` : aucune importance n'est calculée avant le bouton « Évaluer sur le test » ;
    - temps de calcul des importances sur BAAC mesuré dans l'app déployée.
  - Hors périmètre : modèles finaux BAAC.

- [ ] **E12a. Pipeline des modèles finaux BAAC**
  - Dépend de : E4, E11a (`univariate_auc`).
  - Objectif : un script complet et contrôlé, testé sur 10 % de l'entraînement.
  - Contenu :
    - `data-raw/model_vars.R` : liste blanche des variables, spécification principale (conditions de l'accident) et ablation (+ `col`, `choc`, `obs`, `obsm`) ;
    - équipements de sécurité : d'après le guide, `secu1` à `secu3` sont trois cases qui listent jusqu'à trois équipements utilisés, sans ordre ; la case où figure un équipement n'a donc pas de sens. Le code 0 « Aucun équipement » n'a de sens qu'en `secu1` : en `secu2`, il sert de case vide (de 2019 à 2023, 35 000 à 43 500 usagers par an ont `secu2 = 0` alors que `secu1` mentionne un équipement), au même titre que -1 ; en E4, sans regarder la cible, `secu3 = 0` passe aussi de quelques centaines d'usagers par an à plus de 10 000 en 2024. Une fonction `secu_indicators()` de `02_prepare_baac.R` remplace les trois cases par six indicateurs (ceinture, casque, dispositif enfants, gilet, airbag, gants) à trois niveaux :
      - un code 1 à 7 dans l'une des cases donne « oui » pour cet équipement (7 « Gants + airbag » compte pour les gants et pour l'airbag) et « non » pour les autres ;
      - sans aucun code 1 à 7, `secu1` à 0 « Aucun équipement » ou 9 « Autre » donne « non » partout ;
      - sinon (`secu1` à 8 « Non déterminable » ou manquant, sans équipement ailleurs, environ 12 % des usagers), « Indéterminé » partout : un niveau explicite plutôt que NA, pour que l'imputation par le mode ne range pas ces usagers dans « non » ;
    - contrôle de stabilité du codage, avant de figer la liste blanche : pour chaque variable candidate, taux de NA et part de chaque modalité par année, sur 2019 à 2023 seulement (2024 n'est pas consulté) ; une variable dont le codage change d'une année à l'autre (déjà vus : `-1` de `nbv` et `vosp` 2 à 8 fois plus fréquents en 2023 qu'en 2019-2022) est recodée ou retirée, et la raison est écrite en commentaire dans `model_vars.R` ; ces décisions reposent sur le codage et le guide, jamais sur un score ;
    - sur `accidents_full.rds` : découpage 2019-2022 / 2023 / 2024, en réutilisant les fonctions de `R/` (recette, modèles, métriques) ;
    - contrôle de fuite sur 2023 : AUC univariée de chaque variable et taux de NA selon `grave` ; toute variable d'AUC univariée supérieure à 0,8 est relue avant d'être gardée ;
    - petite grille de réglages (quelques valeurs de `mtry` et `min.node.size` pour la forêt, de `cp` pour l'arbre ; logistique sans réglage), rééquilibrage (aucun ou pondération) et seuil (F1 maximal) choisis sur 2023 (critère AUC pour les réglages) par une fonction qui reçoit l'entraînement 2019-2022 et 2023, jamais 2024 ;
    - run sur 10 % de l'entraînement, temps et mémoire mesurés.
  - Fichiers : `data-raw/model_vars.R`, `data-raw/03_final_models.R`, `data-raw/02_prepare_baac.R` (indicateurs d'équipement).
  - Vérification :
    - assertion `all(vars %in% whitelist)`, aucun identifiant ni `an` parmi les variables ;
    - test : permuter les étiquettes de 2024 ne change ni les réglages, ni le seuil, ni la stratégie retenus ;
    - tableau des AUC univariées imprimé (`univariate_auc` de `R/fct_metrics.R`) ; tableau de stabilité par année 2019-2023 imprimé ; AUC sur 2023 de chaque combinaison de la grille imprimée ;
    - indicateurs d'équipement : `secu_indicators()` vérifiée par des `stopifnot` en tête de `02_prepare_baac.R` sur des usagers construits à la main (ceinture en case 2, gants + airbag, `secu1` à « Aucun équipement », `secu1` à « Non déterminable », trois cases manquantes) ; les deux sorties de E4 regénérées et ses contrôles repassés (jointure, doublons, relance de zéro, échantillon de moins de 20 Mo), `data/accidents_sample.rds` déployé changeant aussi ;
    - temps et mémoire du run à 10 % imprimés, grille comprise, et estimation pour le run complet.
  - Hors périmètre : run complet, intervalles, importances (E12b).

- [ ] **E12b. Run complet et résultats de l'étude**
  - Dépend de : E11b, E12a.
  - Objectif : résultats de modélisation de l'étude, reproductibles, sans fuite.
  - Contenu :
    - run complet, lancé après validation si l'estimation de E12a dépasse quelques minutes (sinon sous-échantillonnage de l'entraînement par accident, ou Colab) ;
    - évaluation unique sur 2024 : P, R, F1, AUC, points ROC, coefficients standardisés et odds ratios de la logistique, avec la référence naïve (prévalence de 2024) ;
    - IC à 95 % de l'AUC et des écarts d'AUC entre modèles par bootstrap par grappes (accidents de 2024 tirés avec remise, B = 1000, sur les prédictions déjà calculées) ;
    - importances par permutation sur un sous-échantillon d'accidents de 2024.
  - Fichiers : `data-raw/03_final_models.R` (sortie : `data/final_models.rds`).
  - Vérification :
    - `Rscript data-raw/03_final_models.R` se relance de zéro ;
    - métriques sur 2024 imprimées ;
    - `final_models.rds` de quelques Mo au plus.
  - Hors périmètre : rédaction de l'onglet 5.

- [ ] **Redéploiement J5** : après fusion de E11b, chaîne complète de l'onglet 4 (trois modèles, ROC, seuils, importances, rééquilibrage) sur l'URL publique, avec BAAC et `students.csv`.

### Jalon 6 : étude de cas rédigée, app complète

Couvre : l'étude de cas (A.4).

- [ ] **E13. Onglet Étude de cas**
  - Dépend de : E8, E9, E12b.
  - Objectif : l'étude de cas, au plan de l'annexe F.
  - Contenu :
    - démarche justifiée : unité usager, période 2019-2024, échantillonnage par accident, découpage temporel, liste blanche des variables, seuil choisi sur 2023, ce que montre l'ablation ;
    - partie descriptive (Q1 à Q3) calculée en direct sur l'échantillon déployé par les mêmes fonctions que les onglets 3 et 4, donc reproductible à l'identique ;
    - partie modélisation lue dans `final_results`, avec l'ablation, la comparaison pondération contre aucun rééquilibrage, et la cohérence avec la partie descriptive ;
    - pour chaque figure, les réglages exacts pour la rejouer dans les onglets 1 à 4 (fichier, variables, options) ; pour les modèles, la réplication dans l'app est approchée (échantillon, validation interne au lieu de 2023), ce que l'onglet dit ; les réglages à rejouer n'incluent jamais `grav` parmi les variables explicatives : c'est la source de la cible, et comme `grav` est quali il est sélectionné par défaut, l'onglet dit donc explicitement de le décocher et explique pourquoi en renvoyant à l'alerte de fuite de l'onglet 4 ;
    - limites : avertissement officiel sur « blessé hospitalisé », sous-déclaration des accidents, échantillonnage, dépendance entre usagers d'un même accident (traitée par le bootstrap par grappes, pas par les IC de Wald), importance par permutation faussée par les variables corrélées (Hooker et al., 2021), association n'est pas causalité ; le changement de codage de `secu3` en 2024 a été vu en E4 sur les variables explicatives, sans les étiquettes, et les indicateurs d'équipement reposent sur le guide.
  - Fichiers : `R/mod_case_study.R` (conteneur), `R/case_study_descriptive.R`, `R/case_study_models.R`, `app.R` si besoin.
  - Vérification : liste des chiffres cités, chacun associé à l'onglet ou au script qui le produit.
  - Hors périmètre : boutons qui reconfigurent les autres onglets (voir les décisions), carte.

- [ ] **E14. Relecture croisée et redéploiement complet**
  - Dépend de : E13.
  - Vérification : chaque chiffre de la liste de E13 retrouvé dans l'app déployée ou dans la sortie de `03_final_models.R` ; l'URL publique fonctionne de bout en bout.

- [ ] **Bonus, s'il reste du temps et si les critères de l'annexe E sont remplis sur l'app déployée** (entre E14 et le gel, retirés s'ils sont inachevés au gel)
  - B1 XGBoost : `R/fct_model_xgb.R`, `tests/testthat/test-model-xgb.R`, plus son enregistrement dans l'interface commune de `fct_models.R` ; s'appuyer sur la documentation de la version de `xgboost` installée par `renv` (l'interface R a changé récemment).
  - B2 ACP et k-means : `R/mod_unsupervised.R`, `R/fct_unsupervised.R`, `tests/testthat/test-unsupervised.R`, plus un sous-onglet dans `mod_model.R`.

### Jalon 7 : gel, recette, saisie de l'URL

Couvre : les critères d'acceptation (annexe E) et la saisie de l'URL (A.5).

- [ ] **E15. Gel des fonctionnalités** (après E14 et les bonus éventuels) : tag `freeze` sur `main` ; ensuite, seulement des corrections.
- [ ] **E16. Recette** (après E15) : critère de réussite global ci-dessus, depuis une autre machine, avec Bank Marketing ; heures actives restantes vérifiées.
- [ ] **E17. Saisie de l'URL** (après E16) dans le fichier des groupes du cours.

## Si le temps manque

À couper dans cet ordre :

1. Les bonus B1 et B2 : ils ne sont pas commencés.
2. `rpart` : on garde la logistique et la forêt, qui suffisent au cadrage.
3. Le mode de découpage « par variable » dans l'app : on garde le stratifié et le groupé, exigés par le cadrage.
4. La grille de réglages des modèles finaux (E12a) : on garde les réglages par défaut de `ranger` et `rpart`, et l'étude le signale.
5. L'ablation des variables du choc (E12) : on garde la spécification principale.
6. Les odds ratios et leurs IC (E11b) : on garde les coefficients standardisés, exigés par le cadrage.
7. Le bootstrap par grappes (E12b) : on garde les estimations ponctuelles, et la dépendance entre usagers est écrite dans les limites.
8. La question Q3 de l'étude de cas, réduite à trois ou quatre conditions.

On ne coupe jamais : les exigences de l'annexe A (dont les trois méthodes de rééquilibrage, les visualisations et indices du cours, les métriques, l'importance des variables), le contrat « recette apprise » et la liste blanche des variables, la gestion des erreurs, le déploiement et la recette E16.

## Risques

| Risque | Détection précoce | Parade |
|---|---|---|
| Préparation BAAC plus longue que prévu | E3 pas terminée à la fin du jalon 1, ou E4 non fusionnée au point de décision | plan B (annexe D) |
| Heures actives de l'offre gratuite épuisées en fin de projet | consommation relevée à chaque redéploiement | tests surtout en local, offre payante ponctuelle ou Posit Connect Cloud |
| Mémoire du serveur dépassée (≈ 1 Go) | mesures en E1, au redéploiement J3 et au redéploiement J4 | réduire l'échantillon BAAC, `.rds` chargés une fois, éviter les copies de données dans les réactifs, plafond de colonnes, entraînement limité en taille |
| Session qui plante sur un fichier inattendu | tests de robustesse en E2, E6a et E10b | `tryCatch` et messages, type id au-delà de 50 modalités, colonnes constantes retirées |
| `Hmisc` ou `UsingR` ne s'installent pas sur le serveur | premier déploiement en E1 | recoder les deux graphiques en `ggplot2` |
| Variance en 1/(n − 1) par inadvertance (`var`, `sd`, `cov`) | test des 48 âges (E8), décomposition de la variance (E9) | fonctions de `fct_stats_base.R` |
| Fuite de données dans la recette ou dans BAAC | tests de E6a, contrôles et assertions de E12a | contrat « recette apprise », liste blanche des variables |
| Modalité absente de l'entraînement : erreur de `predict.glm` | test dédié en E6a | modalités inconnues envoyées dans « Autre » |
| Modalité positive mal codée (`glm` modélise le second niveau) | test du signe de l'effet en E10b | recodage explicite de la cible en 0/1 |
| Temps de calcul dans l'app (forêt et permutations) | mesure en E10b et E11b sur l'app déployée | moins d'arbres dans l'app, importances sur un sous-échantillon |
| Run final trop lourd pour 8 Go de RAM | run sur 10 % en E12a | sous-échantillonner l'entraînement par accident, ou passer par Colab |
| Deux applications déployées sous deux URL | premier redéploiement depuis le second poste, s'il a lieu | `deploy.R` versionné, même nom d'application, `forceUpdate = TRUE` |
| Conflits Git entre branches | fusions qui échouent | dépendances respectées (« Dépend de »), fichiers disjoints entre étapes menées en parallèle, `app.R` modifié seulement en E1, E2 et E13 |

## Workflow Git

Une étape peut commencer dès que les étapes de sa ligne « Dépend de » sont fusionnées dans `main`. Deux étapes prêtes en même temps peuvent avancer en parallèle : leurs fichiers sont disjoints, sauf `mod_model.R` entre E11a et E11b, d'où leur dépendance. Seul le commit initial, qui ajoute ce plan, est fait directement sur `main`.

Un poste est sous macOS, l'autre sous Windows. Les commandes ci-dessous se tapent dans Git Bash sous Windows. Dans le code, les chemins sont relatifs et construits avec `file.path()`, et l'encodage des fichiers lus est toujours explicite.

Pour chaque étape, avec `e5-data-tab` comme exemple de nom de branche :

```bash
git switch main && git pull                 # partir du main à jour
git switch -c e5-data-tab                   # branche de l'étape
# ... travail, commits ...
git push -u origin e5-data-tab              # puis demande de fusion sur GitHub
# après relecture et fusion sur GitHub :
git switch main && git pull
git branch -d e5-data-tab                   # supprimer la branche locale
```

Si `main` a avancé pendant l'étape (fusion d'une étape menée en parallèle), on l'intègre dans la branche avant la demande de fusion : `git switch e5-data-tab && git merge main`. Les conflits éventuels se règlent sur la branche de l'étape, jamais sur `main`.

## Références

- Au, T. C. (2018). Random Forests, Decision Trees, and Categorical Predictors: The "Absent Levels" Problem. *JMLR*, 19(45), 1-30.
- Breiman, L. (2001). Random Forests. *Machine Learning*, 45, 5-32.
- Wright, M. N. et König, I. R. (2019). Splitting on categorical predictors in random forests. *PeerJ*, 7, e6339.
- Wright, M. N. et Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. *Journal of Statistical Software*, 77(1).
- Probst, P., Wright, M. N. et Boulesteix, A.-L. (2019). Hyperparameters and tuning strategies for random forest. *WIREs Data Mining and Knowledge Discovery*, 9(3), e1301.
- Robin, X. et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. *BMC Bioinformatics*, 12, 77.
- Strobl, C., Boulesteix, A.-L., Zeileis, A. et Hothorn, T. (2007). Bias in random forest variable importance measures. *BMC Bioinformatics*, 8, 25.
- Fisher, A., Rudin, C. et Dominici, F. (2019). All Models are Wrong, but Many are Useful. *JMLR*, 20(177).
- Hooker, G., Mentch, L. et Zhou, S. (2021). Unrestricted permutation forces extrapolation. *Statistics and Computing*, 31, 82.
- van den Goorbergh, R., van Smeden, M., Timmerman, D. et Van Calster, B. (2022). The harm of class imbalance corrections for risk prediction models. *JAMIA*, 29(9).

## Annexes : cadrage du projet

### A. Exigences

A.1 Données :
- type de chaque variable : détection automatique et correction manuelle ;
- variables qualitatives : nombre de modalités, alerte de forte cardinalité, regroupement des modalités rares ;
- outliers : détection (1,5 × IQR, z-score), affichage, choix du traitement ;
- valeurs manquantes : comptage, codes spéciaux déclarables (-1, 0, "N/A", "."…), suppression, imputation ;
- normalisation : centrage-réduction, min-max ;
- dummification : one-hot, option k − 1 colonnes ;
- déséquilibre des classes : répartition affichée, pondération, sous-échantillonnage, sur-échantillonnage, appliqués à l'entraînement seulement.

A.2 Exploration, dans un espace dédié :
- univariée, visualisations du cours. Quantitatif : tableau statistique, diagramme en bâtons, diagramme cumulatif, boîte à moustaches, histogramme (effectifs ou densité, classes de Sturges, Scott ou Freedman-Diaconis), courbe de densité. Qualitatif : diagramme en colonnes, secteurs, table des effectifs. Indicateurs de centre et de dispersion, variance en 1/n ;
- bivariée, visualisations du cours. Quanti/quanti : nuage de points, histogrammes dos à dos, nuage avec histogrammes marginaux, droite de régression. Quanti/quali : boîtes parallèles. Quali/quali : table de contingence, profils empilés, en proportions et côte à côte ;
- indices de liaison du cours : covariance et corrélation linéaire, rapport de corrélation, khi-deux, Φ², T de Tschuprow, C de Cramér ;
- tableau des liaisons de chaque variable avec la cible.

A.3 Modèles :
- au moins deux modèles (régression logistique, forêt aléatoire ; arbre de décision en troisième) ;
- entraînement depuis l'interface, choix de la cible et des variables explicatives ;
- découpage stratifié à graine fixe, avec option de regroupement par identifiant ;
- évaluation comparative : Precision, Recall, F-score, ROC, AUC, matrice de confusion, seuil réglable ;
- variables les plus importantes : importance par permutation, valeur absolue des coefficients standardisés pour la logistique.

A.4 Étude de cas : étude complète de BAAC réalisée avec l'interface, présentée dans l'onglet 5 (plan en annexe F).

A.5 Déploiement : application déployée sur shinyapps.io et fonctionnelle, URL saisie dans le fichier des groupes du cours.

Non supervisé et XGBoost : bonus, à faire seulement s'il reste du temps.

### B. Packages

`shiny`, `ggplot2`, `reshape2`, `Hmisc` et `UsingR` (ceux du cours), `data.table`, `DT`, `ranger`, `rpart`, `pROC`, `testthat`, `rsconnect`, `renv`. Écartés : plotly, tidymodels, caret. `leaflet` seulement pour une carte en bonus ; `xgboost` et `cluster` pour les bonus.

Un autre package n'est ajouté que si ni R de base ni la liste ne font déjà la même chose correctement, si le gain est réel (fonctionnalité, temps de calcul, fiabilité), s'il est justifié en une phrase dans la demande de fusion, enregistré dans `renv` et installable sur shinyapps.io. Les packages qui changent la structure de l'app (tidyverse complet, tidymodels, caret, golem, rhino) restent exclus.

### C. Données BAAC

- Source : bases annuelles des accidents corporels de la circulation routière, ONISR, sur data.gouv.fr, Licence Ouverte. Pour chaque année, 4 tables (caractéristiques, lieux, véhicules, usagers) et un guide de codification.
- Avertissement officiel : la qualification « blessé hospitalisé » n'est pas comparable avant et après 2018, et l'indicateur n'est plus labellisé par l'autorité de la statistique publique depuis 2019. On ne mélange jamais des années antérieures et postérieures à 2018.
- Cadrage : unité usager, avec l'accident, le lieu et le véhicule joints ; 2019 à 2024 ; France métropolitaine ; question : quels facteurs sont associés à la gravité de l'accident pour une personne impliquée ?
- Validation croisée éventuelle dans l'entraînement groupée par accident (`Num_Acc`).
- Variables exclues : celles qui décrivent les conséquences de l'accident ; celles qui identifient un accident (`jour`, `hrmn` brut, `lat`, `long`, `com`, `adr`, `voie`, `pr`, `pr1`, et les identifiants `Num_Acc`, `id_vehicule`, `id_usager`, `Num_Veh`), transformées en variables utiles (tranche horaire, jour de la semaine, mois, âge).
- Difficultés connues : clé `Num_Acc` commune aux 4 tables et `id_vehicule` entre véhicules et usagers ; casse des noms de colonnes variable (`Num_Veh`, `num_Veh`, `num_veh`) ; manquants codés -1 ou 0 selon la variable, vide ou "." ; usagers en fuite (2021 et après) avec sexe, âge et gravité incomplets.
- À vérifier sur les fichiers : séparateur et encodage selon l'année ; virgule décimale dans `lat` et `long` ; format de `hrmn` ; nom de la clé d'accident en 2022 ; présence de `id_usager` de 2019 à 2024 ; âges et `vma` aberrants.
- Données 2025 : publiées par l'ONISR le 30 octobre 2026, après le cadrage et l'audit E3. Elles ne sont pas utilisées (la période reste 2019 à 2024, test sur 2024) ; l'étude les cite dans ses limites comme perspective d'un second test temporel.
- Volume : plusieurs centaines de milliers d'usagers ; préparation hors de l'app (`data-raw/`), échantillon dans l'app, modèles finaux entraînés hors de l'app avec les mêmes fonctions.

### D. Plan B

Bank Marketing (UCI, version `bank-additional-full` : 41 188 lignes, 20 variables explicatives, environ 11 % de « yes ») devient l'étude de cas : `duration` exclue (connue seulement après l'appel, la documentation UCI recommande de l'écarter), `unknown` traité comme valeur manquante, `pdays = 999` comme code spécial. Les lignes sont rangées par date (mai 2008 à novembre 2010, d'après la documentation UCI) sans colonne d'année : `data-raw/` reconstitue l'année à partir des retours en arrière de `month` d'une ligne à la suivante, contrôle qu'on obtient bien la période annoncée, et l'étude découpe par période (entraînement, validation, test dans l'ordre chronologique) plutôt qu'au hasard, directement dans `data-raw/`. La prévalence de chaque partie est affichée, et un éventuel glissement entre périodes (prévalence, indicateurs macroéconomiques comme `euribor3m`) est discuté dans les limites. Seuls `data-raw/` et l'onglet 5 changent, et un autre jeu de recette est choisi dans la liste de datasets du cours.

### E. Critères d'acceptation

- l'application est accessible par URL et fonctionne sans erreur ;
- un CSV quelconque peut être chargé, typé, prétraité, exploré et modélisé de bout en bout ;
- au moins deux modèles sont comparés avec Precision, Recall, F-score, ROC et AUC ;
- les variables les plus importantes sont affichées ;
- l'étude BAAC est présentée dans l'onglet 5 : question, démarche, résultats, limites (dont l'avertissement officiel) ;
- l'URL est saisie dans le fichier des groupes.

### F. Plan de l'étude de cas

1. Données : source, période et pourquoi, métropole, unité usager, jointures, recodages, échantillonnage.
2. Question : quels facteurs sont associés à la gravité de l'accident pour une personne impliquée ?
3. Analyse descriptive, avec graphiques du cours, indices de liaison et interprétation :
   - Q1 : répartition de la gravité et évolution de 2019 à 2024 (effet du confinement en 2020) ;
   - Q2 : usagers vulnérables (piétons, cyclistes, deux-roues motorisés) face aux occupants de véhicules légers ;
   - Q3 : conditions de l'accident (agglomération, `vma`, luminosité, météo, type de route, équipement de sécurité, âge).
4. Modélisation : cible et déséquilibre, comparaison des modèles sur le test 2024, variables les plus importantes et cohérence avec la partie descriptive.
5. Limites : indicateur « blessé hospitalisé » non labellisé depuis 2019, données administratives brutes, sous-déclaration des accidents, analyse sur échantillon, corrélation n'est pas causalité.
6. Conclusions.
