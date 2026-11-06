# Code labels of the BAAC variables, from the ONISR coding guide
# (description-des-bases-de-donnees-annuelles.pdf, 21 October 2026).
# Each entry maps a code, as written in the files after trimming, to its label.
# Codes "-1" and "" are always missing, as are the codes of baac_missing_codes;
# any other code absent from its table becomes missing too and is reported by
# 02_prepare_baac.R. "0" is a level when the guide gives it a meaning ("Aucun",
# "Sans objet"...), and missing only when the guide says "Non renseigné" (trajet).

baac_labels <- list(
  lum = c(
    "1" = "Plein jour",
    "2" = "Crépuscule ou aube",
    "3" = "Nuit sans éclairage public",
    "4" = "Nuit avec éclairage public non allumé",
    "5" = "Nuit avec éclairage public allumé"
  ),
  agg = c(
    "1" = "Hors agglomération",
    "2" = "En agglomération"
  ),
  int = c(
    "1" = "Hors intersection",
    "2" = "Intersection en X",
    "3" = "Intersection en T",
    "4" = "Intersection en Y",
    "5" = "Intersection à plus de 4 branches",
    "6" = "Giratoire",
    "7" = "Place",
    "8" = "Passage à niveau",
    "9" = "Autre intersection"
  ),
  atm = c(
    "1" = "Normale",
    "2" = "Pluie légère",
    "3" = "Pluie forte",
    "4" = "Neige, grêle",
    "5" = "Brouillard, fumée",
    "6" = "Vent fort, tempête",
    "7" = "Temps éblouissant",
    "8" = "Temps couvert",
    "9" = "Autre"
  ),
  col = c(
    "1" = "Deux véhicules, frontale",
    "2" = "Deux véhicules, par l'arrière",
    "3" = "Deux véhicules, par le côté",
    "4" = "Trois véhicules et plus, en chaîne",
    "5" = "Trois véhicules et plus, collisions multiples",
    "6" = "Autre collision",
    "7" = "Sans collision"
  ),
  catr = c(
    "1" = "Autoroute",
    "2" = "Route nationale",
    "3" = "Route départementale",
    "4" = "Voie communale",
    "5" = "Hors réseau public",
    "6" = "Parc de stationnement ouvert à la circulation",
    "7" = "Route de métropole urbaine",
    "9" = "Autre"
  ),
  circ = c(
    "1" = "A sens unique",
    "2" = "Bidirectionnelle",
    "3" = "A chaussées séparées",
    "4" = "Avec voies d'affectation variable"
  ),
  vosp = c(
    "0" = "Sans objet",
    "1" = "Piste cyclable",
    "2" = "Bande cyclable",
    "3" = "Voie réservée"
  ),
  prof = c(
    "1" = "Plat",
    "2" = "Pente",
    "3" = "Sommet de côte",
    "4" = "Bas de côte"
  ),
  plan = c(
    "1" = "Partie rectiligne",
    "2" = "En courbe à gauche",
    "3" = "En courbe à droite",
    "4" = "En S"
  ),
  surf = c(
    "1" = "Normale",
    "2" = "Mouillée",
    "3" = "Flaques",
    "4" = "Inondée",
    "5" = "Enneigée",
    "6" = "Boue",
    "7" = "Verglacée",
    "8" = "Corps gras, huile",
    "9" = "Autre"
  ),
  infra = c(
    "0" = "Aucun",
    "1" = "Souterrain, tunnel",
    "2" = "Pont, autopont",
    "3" = "Bretelle d'échangeur ou de raccordement",
    "4" = "Voie ferrée",
    "5" = "Carrefour aménagé",
    "6" = "Zone piétonne",
    "7" = "Zone de péage",
    "8" = "Chantier",
    "9" = "Autre"
  ),
  situ = c(
    "0" = "Aucun",
    "1" = "Sur chaussée",
    "2" = "Sur bande d'arrêt d'urgence",
    "3" = "Sur accotement",
    "4" = "Sur trottoir",
    "5" = "Sur piste cyclable",
    "6" = "Sur autre voie spéciale",
    "8" = "Autre"
  ),
  senc = c(
    "0" = "Inconnu",
    "1" = "Repère croissant",
    "2" = "Repère décroissant",
    "3" = "Absence de repère"
  ),
  # Codes are written without leading zero in the files ("7", not "07").
  # Codes unused since 2006 (4, 5, 6, 8, 9, 11, 12, 18, 19) are left out.
  catv = c(
    "0" = "Indéterminable",
    "1" = "Bicyclette",
    "2" = "Cyclomoteur < 50 cm3",
    "3" = "Voiturette",
    "7" = "VL seul",
    "10" = "VU seul 1,5 T à 3,5 T",
    "13" = "PL seul 3,5 T à 7,5 T",
    "14" = "PL seul > 7,5 T",
    "15" = "PL > 3,5 T + remorque",
    "16" = "Tracteur routier seul",
    "17" = "Tracteur routier + semi-remorque",
    "20" = "Engin spécial",
    "21" = "Tracteur agricole",
    "30" = "Scooter < 50 cm3",
    "31" = "Motocyclette 50 à 125 cm3",
    "32" = "Scooter 50 à 125 cm3",
    "33" = "Motocyclette > 125 cm3",
    "34" = "Scooter > 125 cm3",
    "35" = "Quad léger <= 50 cm3",
    "36" = "Quad lourd > 50 cm3",
    "37" = "Autobus",
    "38" = "Autocar",
    "39" = "Train",
    "40" = "Tramway",
    "41" = "3RM <= 50 cm3",
    "42" = "3RM 50 à 125 cm3",
    "43" = "3RM > 125 cm3",
    "50" = "EDP à moteur",
    "60" = "EDP sans moteur",
    "80" = "VAE",
    "99" = "Autre véhicule"
  ),
  obs = c(
    "0" = "Sans objet",
    "1" = "Véhicule en stationnement",
    "2" = "Arbre",
    "3" = "Glissière métallique",
    "4" = "Glissière béton",
    "5" = "Autre glissière",
    "6" = "Bâtiment, mur, pile de pont",
    "7" = "Support de signalisation ou poste d'appel d'urgence",
    "8" = "Poteau",
    "9" = "Mobilier urbain",
    "10" = "Parapet",
    "11" = "Ilot, refuge, borne haute",
    "12" = "Bordure de trottoir",
    "13" = "Fossé, talus, paroi rocheuse",
    "14" = "Autre obstacle fixe sur chaussée",
    "15" = "Autre obstacle fixe sur trottoir ou accotement",
    "16" = "Sortie de chaussée sans obstacle",
    "17" = "Buse, tête d'aqueduc"
  ),
  obsm = c(
    "0" = "Aucun",
    "1" = "Piéton",
    "2" = "Véhicule",
    "4" = "Véhicule sur rail",
    "5" = "Animal domestique",
    "6" = "Animal sauvage",
    "9" = "Autre"
  ),
  choc = c(
    "0" = "Aucun",
    "1" = "Avant",
    "2" = "Avant droit",
    "3" = "Avant gauche",
    "4" = "Arrière",
    "5" = "Arrière droit",
    "6" = "Arrière gauche",
    "7" = "Côté droit",
    "8" = "Côté gauche",
    "9" = "Chocs multiples (tonneaux)"
  ),
  manv = c(
    "0" = "Inconnue",
    "1" = "Sans changement de direction",
    "2" = "Même sens, même file",
    "3" = "Entre 2 files",
    "4" = "En marche arrière",
    "5" = "A contresens",
    "6" = "En franchissant le terre-plein central",
    "7" = "Dans le couloir bus, même sens",
    "8" = "Dans le couloir bus, sens inverse",
    "9" = "En s'insérant",
    "10" = "En faisant demi-tour sur la chaussée",
    "11" = "Changeant de file à gauche",
    "12" = "Changeant de file à droite",
    "13" = "Déporté à gauche",
    "14" = "Déporté à droite",
    "15" = "Tournant à gauche",
    "16" = "Tournant à droite",
    "17" = "Dépassant à gauche",
    "18" = "Dépassant à droite",
    "19" = "Traversant la chaussée",
    "20" = "Manoeuvre de stationnement",
    "21" = "Manoeuvre d'évitement",
    "22" = "Ouverture de porte",
    "23" = "Arrêté (hors stationnement)",
    "24" = "En stationnement (avec occupants)",
    "25" = "Circulant sur trottoir",
    "26" = "Autre manoeuvre"
  ),
  motor = c(
    "0" = "Inconnue",
    "1" = "Hydrocarbures",
    "2" = "Hybride électrique",
    "3" = "Electrique",
    "4" = "Hydrogène",
    "5" = "Humaine",
    "6" = "Autre"
  ),
  # The guide describes places 1 to 9 with a drawing only.
  place = c(
    "1" = "Place 1 (conducteur)",
    "2" = "Place 2",
    "3" = "Place 3",
    "4" = "Place 4",
    "5" = "Place 5",
    "6" = "Place 6",
    "7" = "Place 7",
    "8" = "Place 8",
    "9" = "Place 9",
    "10" = "Piéton"
  ),
  catu = c(
    "1" = "Conducteur",
    "2" = "Passager",
    "3" = "Piéton"
  ),
  grav = c(
    "1" = "Indemne",
    "2" = "Tué",
    "3" = "Blessé hospitalisé",
    "4" = "Blessé léger"
  ),
  sexe = c(
    "1" = "Masculin",
    "2" = "Féminin"
  ),
  # "0" means "Non renseigné" for trajet only: see baac_missing_codes.
  trajet = c(
    "1" = "Domicile, travail",
    "2" = "Domicile, école",
    "3" = "Courses, achats",
    "4" = "Utilisation professionnelle",
    "5" = "Promenade, loisirs",
    "9" = "Autre"
  ),
  secu = c(
    "0" = "Aucun équipement",
    "1" = "Ceinture",
    "2" = "Casque",
    "3" = "Dispositif enfants",
    "4" = "Gilet réfléchissant",
    "5" = "Airbag (2RM/3RM)",
    "6" = "Gants (2RM/3RM)",
    "7" = "Gants + airbag (2RM/3RM)",
    "8" = "Non déterminable",
    "9" = "Autre"
  ),
  locp = c(
    "0" = "Sans objet",
    "1" = "Sur chaussée, à plus de 50 m du passage piéton",
    "2" = "Sur chaussée, à moins de 50 m du passage piéton",
    "3" = "Sur passage piéton sans signalisation lumineuse",
    "4" = "Sur passage piéton avec signalisation lumineuse",
    "5" = "Sur trottoir",
    "6" = "Sur accotement",
    "7" = "Sur refuge ou BAU",
    "8" = "Sur contre-allée",
    "9" = "Inconnue"
  ),
  # Codes 7 and 8 appear in the files but not in the guide: missing.
  actp = c(
    "0" = "Non renseigné ou sans objet",
    "1" = "Sens véhicule heurtant",
    "2" = "Sens inverse du véhicule",
    "3" = "Traversant",
    "4" = "Masqué",
    "5" = "Jouant, courant",
    "6" = "Avec animal",
    "9" = "Autre",
    "A" = "Monte ou descend du véhicule",
    "B" = "Inconnue"
  ),
  etatp = c(
    "1" = "Seul",
    "2" = "Accompagné",
    "3" = "En groupe"
  )
)

# Codes the guide defines as "Non renseigné", besides "-1": set to NA.
baac_missing_codes <- list(trajet = "0")

# Vehicle groups for the study (vulnerable users against car occupants).
# catv code -> group, on codes so that a label change does not break it.
catv_groups <- c(
  "1" = "Vélo", "80" = "Vélo",
  "50" = "EDP", "60" = "EDP",
  "2" = "2RM/3RM <= 50 cm3", "30" = "2RM/3RM <= 50 cm3", "41" = "2RM/3RM <= 50 cm3",
  "31" = "2RM/3RM 50 à 125 cm3", "32" = "2RM/3RM 50 à 125 cm3", "42" = "2RM/3RM 50 à 125 cm3",
  "33" = "2RM/3RM > 125 cm3", "34" = "2RM/3RM > 125 cm3", "43" = "2RM/3RM > 125 cm3",
  "3" = "Voiturette, quad", "35" = "Voiturette, quad", "36" = "Voiturette, quad",
  "7" = "VL",
  "10" = "VU",
  "13" = "PL, tracteur routier", "14" = "PL, tracteur routier", "15" = "PL, tracteur routier",
  "16" = "PL, tracteur routier", "17" = "PL, tracteur routier",
  "37" = "Bus, car", "38" = "Bus, car",
  "39" = "Train, tramway", "40" = "Train, tramway",
  "20" = "Engin spécial, tracteur agricole", "21" = "Engin spécial, tracteur agricole",
  "0" = "Autre, indéterminable", "99" = "Autre, indéterminable"
)

catv_group_levels <- c(
  "VL", "VU", "Vélo", "EDP", "2RM/3RM <= 50 cm3", "2RM/3RM 50 à 125 cm3",
  "2RM/3RM > 125 cm3", "Voiturette, quad", "PL, tracteur routier", "Bus, car",
  "Train, tramway", "Engin spécial, tracteur agricole", "Autre, indéterminable"
)

weekday_levels <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")

month_levels <- c(
  "janvier", "février", "mars", "avril", "mai", "juin",
  "juillet", "août", "septembre", "octobre", "novembre", "décembre"
)

# Time slot boundaries in hours, each slot closed on the left: [6, 9) is "06h-09h".
time_slot_breaks <- c(0, 6, 9, 12, 14, 17, 20, 24)
time_slot_levels <- c("00h-06h", "06h-09h", "09h-12h", "12h-14h", "14h-17h", "17h-20h", "20h-24h")
