# ============================================================
# Detection of Brazilian Author Affiliations in SciELO Records
# ============================================================

library(stringi)

# -------------------------------
# Input files (SciELO .txt)
# -------------------------------

files <- list.files(
  path = ".",
  pattern = "\\.txt$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No SciELO .txt files found in the current directory.")
}

cat("Files found:\n")
print(files)

# -------------------------------
# Dictionary: Brazilian affiliation terms
# -------------------------------

br_terms <- c(
  "brazil", "brasil", "brazilian", "brasileir",
  "capes", "cnpq", "finep",
  "fapesp", "faperj", "fapemig", "fapesb", "facepe",
  "fapesc", "fapergs", "fapeam", "fapdf",
  "usp", "unicamp", "unesp", "unifesp",
  "ufrj", "ufmg", "ufrgs", "ufpr", "ufpe",
  "ufba", "ufc", "ufrn", "ufsc", "ufsm",
  "uff", "ufpa", "ufabc", "ufscar",
  "fiocruz", "oswaldo cruz", "butantan",
  "inpe", "lncc", "lnbio", "embrapa", "inca",
  "instituto adolfo lutz", "incor", "ibge",
  "hospital albert einstein",
  "hospital sírio-libanês", "hospital sirio libanes",
  "hospital das clínicas", "hospital de clínicas",
  "hcfmusp", "hc usp", "hospital universitario"
)

pattern <- paste(br_terms, collapse = "|")

# -------------------------------
# Read SciELO file
# -------------------------------

read_scielo_file <- function(path) {
  
  message("Processing file: ", basename(path))
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  
  records <- list()
  current <- list(fields = list())
  current_field <- NULL
  
  is_field_header <- function(x) grepl("^[A-Z0-9]{2} ", x)
  
  for (ln in lines) {
    
    # End of record
    if (trimws(ln) == "ER") {
      if (length(current$fields) > 0) {
        records[[length(records) + 1]] <- current
      }
      current <- list(fields = list())
      current_field <- NULL
      next
    }
    
    # New field
    if (is_field_header(ln)) {
      tag <- substr(ln, 1, 2)
      value <- trimws(substr(ln, 4, nchar(ln)))
      current$fields[[tag]] <- c(value)
      current_field <- tag
      next
    }
    
    # Continuation line
    if (!is.null(current_field) && nzchar(trimws(ln))) {
      current$fields[[current_field]] <-
        c(current$fields[[current_field]], trimws(ln))
    }
  }
  
  records
}

# -------------------------------
# Detect Brazilian affiliation (C1)
# -------------------------------

has_brazil <- function(rec, pattern) {
  
  if (is.null(rec$fields[["C1"]])) return(FALSE)
  
  any(stringi::stri_detect_regex(
    rec$fields[["C1"]],
    pattern,
    opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
  ))
}

# -------------------------------
# SciELO -> RIS
# -------------------------------

scielo_to_ris <- function(rec) {
  
  f <- rec$fields
  get <- function(tag) if (is.null(f[[tag]])) character(0) else f[[tag]]
  
  # ---- TITLE
  ti <- paste(get("TI"), collapse = " ")
  
  # ---- AUTHORS
  au_raw <- get("AU")
  au <- character(0)
  if (length(au_raw)) {
    au <- paste0("AU  - ", au_raw)
  }
  
  # ---- ABSTRACT
  ab <- ""
  if (length(get("AB"))) {
    ab <- paste0("AB  - ", paste(get("AB"), collapse = " "))
  }
  
  # ---- KEYWORDS
  kw <- ""
  if (length(get("DE"))) {
    kw <- paste0("KW  - ", paste(get("DE"), collapse = "; "))
  }
  
  # ---- ADDRESSES
  ad <- ""
  if (length(get("C1"))) {
    ad <- paste0("AD  - ", paste(get("C1"), collapse = "; "))
  }
    
    # ---- DOI
    do <- ""
    if (length(get("DI"))) {
      do <- paste0("DO  - ", get("DI")[1])
  }
  
    ris <- c(
      "TY  - JOUR",
      paste0("TI  - ", ti),
      au,
      ab,
      paste0("JO  - ", paste(get("SO"), collapse = " ")),
      paste0("PY  - ", paste(get("PY"), collapse = "")),
      kw,
      ad,
      do,
      "ER  - "
    )
  
  ris[nzchar(ris)]
}

# -------------------------------
# SciELO -> CSV row
# -------------------------------

scielo_to_row <- function(rec) {
  
  f <- rec$fields
  get1 <- function(tag) if (is.null(f[[tag]])) "" else paste(f[[tag]], collapse = " || ")
  
  data.frame(
    Title = get1("TI"),
    Abstract = get1("AB"),
    Authors = get1("AU"),
    Journal = get1("SO"),
    Year = get1("PY"),
    Keywords = get1("DE"),
    Author_Addresses = get1("C1"),
    stringsAsFactors = FALSE
  )
}

# -------------------------------
# Run pipeline
# -------------------------------

records_by_file <- lapply(files, read_scielo_file)
names(records_by_file) <- basename(files)

all_records <- unlist(records_by_file, recursive = FALSE)

brazil_records <- Filter(function(r) has_brazil(r, pattern), all_records)

cat("\nTotal records:", length(all_records), "\n")
cat("Brazil records:", length(brazil_records), "\n")

# -------------------------------
# Counts per file
# -------------------------------

count_table <- data.frame(
  File = names(records_by_file),
  Total = integer(length(records_by_file)),
  Brazilian = integer(length(records_by_file)),
  Excluded = integer(length(records_by_file)),
  stringsAsFactors = FALSE
)

for (i in seq_along(records_by_file)) {
  recs <- records_by_file[[i]]
  total <- length(recs)
  br <- sum(vapply(recs, has_brazil, logical(1), pattern = pattern))
  
  count_table$Total[i] <- total
  count_table$Brazilian[i] <- br
  count_table$Excluded[i] <- total - br
}

print(count_table)
write.csv(count_table, "scielo_brazil_counts_per_file.csv", row.names = FALSE)

# -------------------------------
# Flow diagram function
# -------------------------------

draw_flow <- function(total, brazil, excluded, title, file_out) {
  
  png(file_out, width = 900, height = 600)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))
  
  rect(1, 5, 4, 7); text(2.5, 6, paste("Total\n", total))
  rect(6, 7, 9, 9); text(7.5, 8, paste("Brazil\n", brazil))
  rect(6, 3, 9, 5); text(7.5, 4, paste("Excluded\n", excluded))
  
  arrows(4, 6, 6, 8, length = 0.1)
  arrows(4, 6, 6, 4, length = 0.1)
  
  title(main = title, cex.main = 1.3)
  dev.off()
}

# -------------------------------
# Flow diagrams per file
# -------------------------------

for (i in seq_len(nrow(count_table))) {
  
  f <- count_table$File[i]
  
  draw_flow(
    total = count_table$Total[i],
    brazil = count_table$Brazilian[i],
    excluded = count_table$Excluded[i],
    title = paste("Flow —", f),
    file_out = paste0("flow_", f, ".png")
  )
}

# -------------------------------
# Combined flow diagram
# -------------------------------

draw_flow(
  total = sum(count_table$Total),
  brazil = sum(count_table$Brazilian),
  excluded = sum(count_table$Excluded),
  title = "Flow — All SciELO Files Combined",
  file_out = "flow_ALL_FILES.png"
)

# -------------------------------
# Export RIS
# -------------------------------

ris_lines <- unlist(lapply(brazil_records, scielo_to_ris))
writeLines(ris_lines, "scielo_brazil_only.ris")
cat("RIS exported: scielo_brazil_only.ris\n")

# -------------------------------
# Export CSV
# -------------------------------

csv_df <- do.call(rbind, lapply(brazil_records, scielo_to_row))

write.csv(
  csv_df,
  "scielo_brazil_only.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("CSV exported: scielo_brazil_only.csv\n")

# -------------------------------
# Final summary
# -------------------------------

cat("\n====================\n")
cat("FINAL SUMMARY (SciELO)\n")
cat("====================\n")
cat("Input files:", length(files), "\n")
cat("Total records processed:", length(all_records), "\n")
cat("Records with Brazilian affiliation:", length(brazil_records), "\n")
cat("Outputs:\n")
cat(" - scielo_brazil_only.ris\n")
cat(" - scielo_brazil_only.csv\n")
cat(" - scielo_brazil_counts_per_file.csv\n")
cat(" - flow_*.png (per file)\n")
cat(" - flow_ALL_FILES.png\n")