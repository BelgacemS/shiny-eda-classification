# Download the BAAC tables 2019 to 2024 and the coding guide into data-raw/raw/.
# Usage, from the project root: Rscript data-raw/01_download_baac.R
# Files already present are kept; delete data-raw/raw/ to start from scratch.
# Source: https://www.data.gouv.fr/datasets/bases-de-donnees-annuelles-des-accidents-corporels-de-la-circulation-routiere-annees-de-2005-a-2024
# (ONISR, Licence Ouverte).

raw_dir <- file.path("data-raw", "raw")
base_url <- "https://www.data.gouv.fr/api/1/datasets/r/"

# data.gouv.fr resource ids. The link follows a republished file, so the
# manifest records size and md5 of what was actually downloaded.
resources <- utils::read.csv(text = "
year,table,id
2019,caracteristiques,e22ba475-45a3-46ac-a0f7-9ca9ed1e283a
2019,lieux,2ad65965-36a1-4452-9c08-61a6c874e3e6
2019,vehicules,780cd335-5048-4bd6-a841-105b44eb2667
2019,usagers,36b1b7b3-84b4-4901-9163-59ae8a9e3028
2020,caracteristiques,07a88205-83c1-4123-a993-cba5331e8ae0
2020,lieux,e85c41f7-d4ea-4faf-877f-ab69a620ce21
2020,vehicules,a66be22f-c346-49af-b196-71df24702250
2020,usagers,78c45763-d170-4d51-a881-e3147802d7ee
2021,caracteristiques,85cfdc0c-23e4-4674-9bcd-79a970d7269b
2021,lieux,8a4935aa-38cd-43af-bf10-0209d6d17434
2021,vehicules,0bb5953a-25d8-46f8-8c25-b5c2f5ba905e
2021,usagers,ba5a1956-7e82-41b7-a602-89d7dd484d7a
2022,caracteristiques,5fc299c0-4598-4c29-b74c-6a67b0cc27e7
2022,lieux,a6ef711a-1f03-44cb-921a-0ce8ec975995
2022,vehicules,c9742921-4427-41e5-81bc-f13af8bc31a0
2022,usagers,62c20524-d442-46f5-bfd8-982c59763ec8
2023,caracteristiques,104dbb32-704f-4e99-a71e-43563cb604f2
2023,lieux,8bef19bf-a5e4-46b3-b5f9-a145da4686bc
2023,vehicules,146a42f5-19f0-4b3e-a887-5cd8fbef057b
2023,usagers,68848e2a-28dd-4efc-9d5f-d512f7dbe66f
2024,caracteristiques,83f0fb0e-e0ef-47fe-93dd-9aaee851674a
2024,lieux,228b3cda-fdfb-4677-bd54-ab2107028d2d
2024,vehicules,fd30513c-6b11-4a56-b6dc-5ac87728794b
2024,usagers,f57b1f58-386d-4048-8f78-2ebe435df868
", colClasses = "character")
resources$file <- file.path(raw_dir, resources$year, paste0(resources$table, ".csv"))

guide <- data.frame(
  year = NA_character_, table = "guide",
  id = "8ef4c2a3-91a0-4d98-ae3a-989bde87b62a",
  file = file.path(raw_dir, "description-des-bases-de-donnees-annuelles.pdf")
)
resources <- rbind(resources, guide)

download_resource <- function(id, dest) {
  if (file.exists(dest)) {
    return(invisible(FALSE))
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  # Write to a temporary name first, so that an interrupted download is not kept.
  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp))
  status <- utils::download.file(paste0(base_url, id), tmp, mode = "wb", quiet = TRUE, method = "libcurl")
  if (status != 0 || file.size(tmp) == 0) {
    stop("Download failed for resource ", id, call. = FALSE)
  }
  file.rename(tmp, dest)
  invisible(TRUE)
}

# A failed redirect can return an HTML page with status 200.
check_not_html <- function(path) {
  first <- readBin(path, "raw", n = 200)
  text <- rawToChar(first[first != as.raw(0)])
  if (grepl("<html|<!doctype", text, ignore.case = TRUE, useBytes = TRUE)) {
    stop(path, " is an HTML page, not the expected file.", call. = FALSE)
  }
}

options(timeout = max(600, getOption("timeout")))
for (i in seq_len(nrow(resources))) {
  new <- download_resource(resources$id[i], resources$file[i])
  check_not_html(resources$file[i])
  message(if (new) "downloaded " else "kept       ", resources$file[i])
}

manifest <- data.frame(
  year = resources$year,
  table = resources$table,
  resource_id = resources$id,
  file = resources$file,
  bytes = file.size(resources$file),
  md5 = unname(tools::md5sum(resources$file))
)
utils::write.csv(manifest, file.path(raw_dir, "manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("Manifest written to ", file.path(raw_dir, "manifest.csv"))
