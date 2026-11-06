# BAAC raw files 2019 to 2024: schema audit

Source: ONISR annual road accident databases on data.gouv.fr (Licence Ouverte), 4 tables per year and the coding guide `description-des-bases-de-donnees-annuelles.pdf` (dated 21 October 2026, covering 2005 to 2025). Files downloaded on 2026-11-06.

To reproduce, from the project root:

```bash
Rscript data-raw/01_download_baac.R   # data-raw/raw/, about 190 MB, with manifest.csv (bytes, md5)
Rscript data-raw/audit_baac.R         # prints every result quoted below
```

Each finding below names the section of the `audit_baac.R` output that supports it, in brackets.

## Files

All 24 files use the separator `;`, are valid UTF-8 on every line and have no BOM; non-empty fields are always quoted [File format]. Non-ASCII characters include accents in addresses and street names, and non-breaking spaces in identifiers (see below). Three files of 2020 differ from the others:

- `lieux`, `vehicules` and `usagers` 2020 have CRLF line endings (one CR byte per line); every other file has LF endings;
- `lieux` and `vehicules` 2020 write empty fields without quotes (`;;`, or `;` at the end of a line), on 47 595 and 80 445 lines; everywhere else, an empty field is written `""`.

`fread` reads both forms, and an unquoted empty field gives `""` when columns are read as character, as for a quoted one.

Number of rows:

| year | caracteristiques | lieux | vehicules | usagers |
|---|---:|---:|---:|---:|
| 2019 | 58 840 | 58 840 | 100 710 | 132 977 |
| 2020 | 47 744 | 47 744 | 81 066 | 105 295 |
| 2021 | 56 518 | 56 518 | 97 315 | 129 248 |
| 2022 | 55 302 | 55 302 | 94 493 | 126 662 |
| 2023 | 54 822 | 70 860 | 93 585 | 125 789 |
| 2024 | 54 402 | 70 248 | 92 678 | 125 187 |

## Columns

Same columns and same order every year, with two exceptions [Columns]:

| table | columns (2024) | differences |
|---|---|---|
| caracteristiques | `Num_Acc`, `jour`, `mois`, `an`, `hrmn`, `lum`, `dep`, `com`, `agg`, `int`, `atm`, `col`, `adr`, `lat`, `long` | 2022: the key is named `Accident_Id` |
| lieux | `Num_Acc`, `catr`, `voie`, `v1`, `v2`, `circ`, `nbv`, `vosp`, `prof`, `pr`, `pr1`, `plan`, `lartpc`, `larrout`, `surf`, `infra`, `situ`, `vma` | none |
| vehicules | `Num_Acc`, `id_vehicule`, `num_veh`, `senc`, `catv`, `obs`, `obsm`, `choc`, `manv`, `motor`, `occutc` | none |
| usagers | `Num_Acc`, `id_usager`, `id_vehicule`, `num_veh`, `place`, `catu`, `grav`, `sexe`, `an_nais`, `trajet`, `secu1`, `secu2`, `secu3`, `locp`, `actp`, `etatp` | 2019 and 2020: no `id_usager` |

The vehicle code is spelled `num_veh` in every file from 2019 to 2024. The spellings `Num_Veh` and `num_Veh` appear only in the guide.

## Keys and identifiers

Accident key [Accident key]:

- 12 characters, starting with the year (`201900000001`), unique in `caracteristiques` every year;
- every `Num_Acc` of `vehicules` and `usagers` exists in `caracteristiques`, and `lieux` covers exactly the same accidents;
- `lieux` has one row per accident from 2019 to 2022, but several from 2023 on [Duplicated Num_Acc in lieux]: 15 870 accidents in 2023 and 15 645 in 2024 have 2 to 5 rows, one per road (for example an accident at the corner of rue de Rivoli and rue Saint-Florentin). The rows are never identical; `voie`, `v1`, `v2`, `circ`, `nbv`, `vosp`, `prof`, `pr`, `pr1`, `plan`, `lartpc`, `larrout`, `surf`, `infra`, `situ` and `vma` can differ, `catr` never does. A plain join of `lieux` would duplicate the users of these accidents.

Vehicle and user identifiers [Vehicle and user identifiers], [Identifier format and uniqueness across years]:

- `id_vehicule` and `id_usager` are numbers written with non-breaking spaces (U+00A0) as thousands separators (`138 306 524`): they must be read as text, not as integers;
- `id_vehicule` is unique in `vehicules`, within each year and across the 6 years (559 847 vehicles); every `id_vehicule` of `usagers` exists in `vehicules`;
- `id_usager` (2021 to 2024) is unique within and across years;
- without `id_usager`, 101 rows of `usagers` in 2019 and 63 in 2020 are identical on every column to another row (passengers of the same vehicle with the same description): a row identifier has to be built for these two years;
- vehicles with no user: 2 040 in 2019, 1 899 in 2020, then 6, 0, 40 and 24 from 2021 to 2024. According to the guide, a vehicle without a user is a fugitive driver who was not created in the database.

## Values

| point | finding | section |
|---|---|---|
| `lat`, `long` | decimal comma in every file (`48,8962100`); 7 decimals in 2019 and 2020, 10 in 2021 and 2022, 8 in 2023 and 2024 | [lat and long], [Birth years of users over 100 in 2019, and decimals of lat] |
| other decimals | `lartpc` and `larrout` use a decimal point in 2019 and a decimal comma from 2020 | [Decimal separators] |
| geographic outliers | among metropolitan accidents, 1 point outside the bounding box of mainland France and Corsica in 2019 (`11,0000000 11,0000000`) and 3 in 2024 (Corsica, latitude and longitude swapped), none at (0, 0) | [Coordinates of metropolitan accidents] |
| `hrmn` | always `HH:MM` (`01:30`), every row of every year | [hrmn] |
| `dep` | 2019 writes departments 1 to 9 without the leading zero (`2` instead of `02`); `2A` and `2B` for Corsica every year; overseas codes 971 to 978 and 986 to 988 | [Department codes] |
| overseas accidents | 5 790 (2019, counting the unpadded codes as metropolitan gives 2 824), 2 623, 2 978, 2 922, 3 181, 3 344 | [Departments outside metropolitan France], [Coordinates of metropolitan accidents] |

Missing values [Missing value codes], [Whitespace around values]:

- `-1` is written `" -1"`, with a leading space inside the quotes, in most coded columns (`atm`, `col`, `circ`, `vma`, `senc`, `catv`, `obs`, `grav`, `sexe`, `secu1`...). `fread` keeps spaces inside quotes, so values must be trimmed before any comparison;
- empty cells: `adr` (2022 on), `voie`, `v1`, `v2`, `lartpc`, `larrout`, `occutc`, and `an_nais` from 2021;
- `"."` and `"N/A"` only in the free text columns `adr`, `voie` and `v2`;
- `0` means "not filled in" for some variables only: "non renseigné" for `trajet`, "non renseigné ou sans objet" for `actp`, "sans objet" for `vosp`, `obs` and `locp`, "aucun" for `infra`, `situ`, `obsm` and `choc`, "inconnu(e)" for `senc`, `manv` and `motor`, and "aucun équipement" for `secu1` to `secu3`. Recoding has to follow the guide variable by variable.

## Fugitive users

No column flags them [grav, sexe and an_nais of users], [Drivers with sexe and age missing, 2021 and later]:

- before 2021, no user has `sexe = -1` or a missing `an_nais`;
- from 2021, users with `sexe = -1` and an empty `an_nais` appear: 3 060, 2 742, 2 422 and 2 390 from 2021 to 2024, almost all drivers (`catu = 1`);
- their `grav` is `1` (unharmed) or `-1`: `-1` for 60, 240 and 116 of them from 2021 to 2023, never in 2024 (for the drivers among them: 60, 239, 113, and `grav = 1` for all 2 384 of 2024). Their gravity is not observed, and keeping them would add a block of unharmed users whose real state is unknown;
- `grav = -1` concerns almost only these users: 60, 240 and 116 of the 60, 241 and 118 users with `grav = -1` from 2021 to 2023, none in 2024.

Rule that identifies them in the files: `sexe = -1` and `an_nais` empty, from 2021.

Apart from them, 7, 132, 176 and 189 users from 2021 to 2024 have an empty `an_nais` with a known `sexe` [Users with an empty an_nais but sexe filled in]: their age is missing, but nothing marks them as fugitive.

## Ages and speed limits

Age computed as `an - an_nais` [Age at the accident (an - an_nais)], [Birth years of users over 100 in 2019, and decimals of lat]:

| year | min | median | max | over 100 | over 110 |
|---|---:|---:|---:|---:|---:|
| 2019 | 0 | 36 | 119 | 177 | 176 |
| 2020 | 0 | 35 | 120 | 18 | 15 |
| 2021 | 0 | 34 | 109 | 4 | 0 |
| 2022 | 0 | 35 | 109 | 7 | 0 |
| 2023 | 0 | 35 | 110 | 9 | 0 |
| 2024 | 0 | 36 | 110 | 6 | 0 |

No negative age. In 2019, the users over 100 were born in 1900 (133), 1901 (43) or 1911 (1): 1900 and 1901 look like default values and should be treated as missing.

`vma` [vma]:

| year | -1 | 0 | 1 to 9 | not a multiple of 10 | over 130 | max |
|---|---:|---:|---:|---:|---:|---:|
| 2019 | 881 | 1 | 82 | 168 | 19 | 800 |
| 2020 | 740 | 0 | 65 | 139 | 12 | 500 |
| 2021 | 875 | 0 | 55 | 123 | 21 | 901 |
| 2022 | 897 | 0 | 64 | 156 | 14 | 700 |
| 2023 | 4 578 | 0 | 0 | 95 | 0 | 130 |
| 2024 | 3 630 | 1 | 94 | 263 | 29 | 900 |

The most frequent values are 50, 80, 30, 90, 70 and 110 km/h. Values above 130, and from 1 to 9, look like entry errors. 2023 has no value above 130 or below 10, but five times more `-1` than the other years: the cleaning seems to differ for that year.

## Answers to the points to check (PLAN.md, annex C)

| point | answer |
|---|---|
| separator and encoding by year | `;` and UTF-8 for all 24 files, no BOM |
| decimal comma in `lat` and `long` | yes, every year; also in `lartpc` and `larrout` from 2020 |
| format of `hrmn` | `HH:MM` every year |
| accident key in 2022 | `Accident_Id` in `caracteristiques` 2022 only; `Num_Acc` in the 3 other tables of 2022 |
| `id_usager` from 2019 to 2024 | absent in 2019 and 2020, present and unique from 2021 |
| uniqueness of `Num_Acc` | unique in `caracteristiques` every year; in `lieux` unique until 2022, several rows per accident in 2023 and 2024 |
| fugitive users | no flag; `sexe = -1` and empty `an_nais` from 2021, `grav` set to 1 or -1 |
| ages and `vma` | ages up to 120 (176 over 110 in 2019); `vma` with `-1`, values from 1 to 9 and up to 901 |
| case of `Num_Veh` | `num_veh` in every file |

## Consequences for E4

- read every column as character, trim spaces, then recode by variable with the guide;
- rename `Accident_Id` to `Num_Acc` for 2022;
- strip the non-breaking spaces from `id_vehicule` and `id_usager`, and build a user identifier for 2019 and 2020;
- `lieux` 2023 and 2024: choose one row per accident before the join (rule to define in E4), otherwise the users are duplicated;
- pad `dep` to two digits in 2019 before filtering on metropolitan France;
- exclude fugitive users with the rule above, and the remaining `grav = -1`;
- treat ages above 110 and `vma` outside 10 to 130 as missing.
