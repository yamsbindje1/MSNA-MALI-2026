# Builds the READY_FOR_R variables of the tag dictionary (called from 10).
# Excluded options only leave the numerator, no answer = NA; anything missing is skipped and logged.

dico_na <- if (exists("values_to_NA")) values_to_NA else c(NA, "NA", "", "SL", -999, 999)
dico_chr <- function(x) { x <- as.character(x); x[x %in% dico_na] <- NA_character_; x }
dico_codes <- function(z) { z <- as.character(z); if (length(z) == 0 || is.na(z) || !nzchar(trimws(z))) return(character(0)); trimws(strsplit(z, ";")[[1]]) }
dico_dummies <- function(df, var) grep(paste0("^", gsub(".", "\\.", var, fixed = TRUE), "\\."), names(df), value = TRUE)
dico_ticked <- function(df, var, codes) {
  v <- dico_dummies(df, var); v <- v[sub("^[^.]*\\.", "", v) %in% codes]
  if (!length(v)) return(rep(FALSE, nrow(df)))
  rowSums(sapply(v, function(k) suppressWarnings(as.numeric(dico_chr(df[[k]]))) %in% 1)) > 0
}

# sub-groups: dictionary label + value -> dataset column / codes
dico_subgroups <- tibble::tribble(
  ~label,                                  ~value,        ~col,                  ~codes,
  "hoh_gender (sexe du chef de m\u00e9nage)",   "female",      "hoh_gender",          "female",
  "hoh_gender (sexe du chef de m\u00e9nage)",   "male",        "hoh_gender",          "male",
  "sexe de l'individu",                    "female",      "ind_gender",          "female",
  "sexe de l'individu",                    "male",        "ind_gender",          "male",
  "handicap (WG-SS niveau 3)",             "oui",         "wgq_dis_3",           "1",
  "quintile de revenu",                    "0-20 %",      "cm_income_total_qtl", "0%-20%",
  "quintile de revenu",                    "60-100 %",    "cm_income_total_qtl", "60%-80%;80%-100%",
  "resp_gender (sexe du r\u00e9pondant)",       "female",      "resp_gender",         "female",
  "resp_gender (sexe du r\u00e9pondant)",       "male",        "resp_gender",         "male",
  "tranche d'\u00e2ge",                         "0_to_5_y",    "ind_age_wgq_cat",     "0_to_5_y",
  "tranche d'\u00e2ge",                         "6_to_17_y",   "ind_age_wgq_cat",     "6_to_17_y",
  "tranche d'\u00e2ge",                         "18_to_59_y",  "ind_age_wgq_cat",     "18_to_59_y",
  "tranche d'\u00e2ge",                         "60_more",     "ind_age_wgq_cat",     "60_more",
  "milieu de r\u00e9sidence",                   "rural",       "setting",             "rural",
  "milieu de r\u00e9sidence",                   "urban",       "setting",             "urban",
  "milieu de r\u00e9sidence",                   "camp_formal", "setting",             "camp_formal",
  "milieu de r\u00e9sidence",                   "camp_informal", "setting",           "camp_informal"
)
# labels handled elsewhere (analysis group, filter, export): not a sub-group of the variable
dico_subgroup_ignored <- c("hoh_dis_group (groupe de population)", "niveau g\u00e9ographique", "besoin de sant\u00e9 satisfait", "cpa_preferred_modality_1 (croisement)")

# denominateur_filtre (XLSForm subset) -> logical vector; NULL if not translatable
dico_filter <- function(df, f) {
  if (is.na(f) || !nzchar(trimws(f)) || grepl("^aucun", trimws(f))) return(rep(TRUE, nrow(df)))
  e <- f
  e <- gsub("\\bif\\(", "ifelse(", e)
  e <- gsub("not\\(", "!(", e)
  e <- gsub("selected\\(\\s*([A-Za-z0-9_]+)\\s*,\\s*'([^']+)'\\s*\\)", "SEL('\\1','\\2')", e)
  e <- gsub("\\b([a-z][a-z0-9_]+)\\s*=\\s*'([^']+)'", "EQ('\\1','\\2')", e)
  e <- gsub("\\b([a-z][a-z0-9_]+)\\s*(>=|>)\\s*([0-9]+)", "NUM('\\1','\\2',\\3)", e)
  e <- gsub("\\b([a-z][a-z0-9_]+)\\s*=\\s*([0-9]+)", "EQ('\\1','\\2')", e)
  e <- gsub("\\band\\b", "&", e); e <- gsub("\\bor\\b", "|", e)
  need <- function(v) if (!v %in% names(df)) stop("filter column not in the data: ", v)
  SEL <- function(v, c) { need(v); z <- dico_chr(df[[v]]); !is.na(z) & (z == c | grepl(paste0("(^| )", c, "( |$)"), z)) }
  EQ  <- function(v, c) { need(v); dico_chr(df[[v]]) %in% c }
  NUM <- function(v, op, n) { need(v); z <- suppressWarnings(as.numeric(dico_chr(df[[v]]))); (if (op == ">=") z >= n else z > n) %in% TRUE }
  out <- eval(parse(text = e))
  if (!is.logical(out) || length(out) != nrow(df)) stop("filter not translatable: ", f)
  out %in% TRUE
}

# reads the dictionary; returns the rows of the READY_FOR_R variables
dico_read <- function(file, sheet = "Dictionnaire") {
  d <- readxl::read_excel(file, sheet = sheet, skip = 1, col_types = "text")
  pick <- function(prefix) { n <- names(d)[startsWith(names(d), prefix)]; if (!length(n)) stop("dictionary: column ", prefix, " not found"); n[1] }
  d <- dplyr::rename(d, vs = !!pick("variable_source"), dfil = !!pick("denominateur_filtre"))
  d[d$statut_R %in% "READY_FOR_R" & !is.na(d$nom_dans_analyse), ]
}

# builds one variable from its dictionary rows; stop() with the reason when it cannot
dico_build_one <- function(G, df, lp, survey, choices) {
  key <- c("vs", "options_numerateur", "regle_combinaison", "options_exclues", "valeur_sortie", "dfil", "desag1_variable", "desag1_valeur")
  ign <- G$desag1_variable %in% dico_subgroup_ignored   # population groups / national value: handled by the analysis, not here
  G$desag1_variable[ign] <- NA; G$desag1_valeur[ign] <- NA
  G <- G[!duplicated(G[, key]), ]                       # national and regional rows describe the same variable
  if (any(duplicated(G$valeur_sortie))) stop("the dictionary rows of this variable disagree (same valeur_sortie, different rule)")
  r <- G[1, ]; nm <- r$nom_dans_analyse
  vs <- dico_codes(r$vs); if (!length(vs)) stop("variable_source empty")
  src <- if (r$niveau_calcul %in% "boucle \u2192 m\u00e9nage") lp else df
  miss <- vs[!vs %in% names(src)]; if (length(miss)) stop("source variable not in the data: ", paste(miss, collapse = ", "))
  vtype <- function(v) { t <- survey$type[match(v, survey$name)]; if (is.na(t)) "" else sub(" .*", "", t) }
  # sub-group
  sg <- NULL
  if (!is.na(r$desag1_variable) && !r$desag1_variable %in% dico_subgroup_ignored) {
    sg <- dico_subgroups[dico_subgroups$label %in% r$desag1_variable & dico_subgroups$value %in% r$desag1_valeur, ]
    if (nrow(sg) != 1) stop("unknown sub-group: ", r$desag1_variable, " = ", r$desag1_valeur)
    if (!sg$col %in% names(df)) stop("sub-group column not in the data: ", sg$col)
  }
  kf <- dico_filter(df, r$dfil)
  keep <- kf
  if (!is.null(sg)) keep <- keep & dico_chr(df[[sg$col]]) %in% dico_codes(sg$codes)
  # ---- copy of an existing variable (sub-group / Top N) ----
  if (length(vs) == 1 && startsWith(nm, paste0(vs, "_")) && (!is.null(sg) || r$type_ligne %in% "Top N")) {
    return(list(kind = if (vtype(vs) == "select_multiple") "copy_sm" else "copy", var = vs, keep = keep, kf = kf))
  }
  # codes must exist in the choices of the source variable(s)
  allc <- unique(unlist(lapply(vs, function(v) { l <- sub("^select_(one|multiple)\\s+", "", survey$type[match(v, survey$name)]); choices$name[choices$list_name %in% l] })))
  if (length(allc)) {
    used <- unique(c(unlist(lapply(G$options_numerateur[!G$type_ligne %in% "Top N"], dico_codes)), unlist(lapply(G$options_exclues, dico_codes))))
    bad <- setdiff(used, c(allc, "1"))
    if (length(bad)) stop("code(s) not in the Kobo choices: ", paste(bad, collapse = ", "))
  }
  regles <- G$regle_combinaison
  answered <- function(v) !is.na(dico_chr(src[[v]]))
  # ---- household value from the loop ----
  if (any(regles %in% "Agr\u00e9gation loop")) {
    z <- dico_chr(lp[[vs]]); ind <- ifelse(is.na(z), NA_real_, as.numeric(z %in% dico_codes(r$options_numerateur)))
    dt <- data.table::data.table(uuid = as.character(lp$uuid), x = ind)[!is.na(x)][, .(x = as.numeric(any(x == 1))), by = uuid]
    v <- dt$x[match(as.character(df$uuid), dt$uuid)]
    return(list(kind = "value", v = v, keep = keep, kf = kf, method = "agregation_boucle"))
  }
  # ---- yes / no pair (Toutes sauf + Complement): dnk/pnta only = kept in the denominator ----
  if (all(c("Toutes sauf", "Compl\u00e9ment") %in% regles)) {
    ex <- dico_codes(G$options_exclues[regles == "Toutes sauf"][1]); no <- dico_codes(G$options_numerateur[regles == "Compl\u00e9ment"][1])
    y <- n <- a <- rep(FALSE, nrow(df))
    for (x in vs) { cs <- sub("^[^.]*\\.", "", dico_dummies(df, x)); y <- y | dico_ticked(df, x, setdiff(cs, ex)); n <- n | dico_ticked(df, x, no); a <- a | answered(x) }
    v <- ifelse(y, "yes", ifelse(n, "no", ifelse(a, "dnk_pnta", NA_character_)))
    return(list(kind = "value", v = v, keep = keep, kf = kf, method = "oui_non"))
  }
  # ---- several categories (one row per valeur_sortie) ----
  if (nrow(G) > 1 && length(unique(G$valeur_sortie)) == nrow(G)) {
    x <- vs[1]; z <- dico_chr(df[[x]]); v <- ifelse(is.na(z), NA_character_, "other")
    hit_any <- rep(FALSE, nrow(df))
    for (k in rev(seq_len(nrow(G)))) {
      hit <- if (vtype(x) == "select_multiple") dico_ticked(df, x, dico_codes(G$options_numerateur[k])) else z %in% dico_codes(G$options_numerateur[k])
      v[hit] <- G$valeur_sortie[k]; hit_any <- hit_any | hit
    }
    if (all(G$valeur_sortie %in% c("yes", "no"))) v[!hit_any & !is.na(z)] <- "dnk_pnta"
    return(list(kind = "value", v = v, keep = keep, kf = kf, method = "categories"))
  }
  # ---- OR over several variables ----
  if (length(vs) > 1) {
    cs <- dico_codes(r$options_numerateur)
    if (identical(cs, "1")) {
      m <- sapply(vs, function(x) suppressWarnings(as.numeric(dico_chr(src[[x]]))))
      v <- ifelse(rowSums(!is.na(m)) > 0, as.numeric(rowSums(m == 1, na.rm = TRUE) > 0), NA_real_)
    } else {
      hit <- a <- rep(FALSE, nrow(df))
      for (x in vs) { z <- dico_chr(df[[x]]); a <- a | !is.na(z); hit <- hit | (if (vtype(x) == "select_multiple") dico_ticked(df, x, cs) else z %in% cs) }
      v <- ifelse(a, as.numeric(hit), NA_real_)
    }
    return(list(kind = "value", v = v, keep = keep, kf = kf, method = "union_variables"))
  }
  # ---- one variable ----
  x <- vs; z <- dico_chr(src[[x]])
  if (vtype(x) == "select_multiple") {
    cs <- if (regles[1] %in% "Toutes sauf") setdiff(sub("^[^.]*\\.", "", dico_dummies(df, x)), dico_codes(r$options_exclues)) else dico_codes(r$options_numerateur)
    if (!length(dico_dummies(df, x))) stop("no dummy column ", x, ".<code>")
    v <- ifelse(is.na(z), NA_real_, as.numeric(dico_ticked(df, x, cs)))
  } else {
    cs <- if (regles[1] %in% "Toutes sauf") setdiff(unique(stats::na.omit(z)), dico_codes(r$options_exclues)) else dico_codes(r$options_numerateur)
    v <- ifelse(is.na(z), NA_real_, as.numeric(z %in% cs))
  }
  if (r$valeur_sortie %in% "yes") v <- ifelse(is.na(v), NA_character_, ifelse(v == 1, "yes", "no"))
  list(kind = "value", v = v, keep = keep, kf = kf, method = if (vtype(x) == "select_multiple") "options_choix_multiple" else "options_choix_unique")
}

# creates the variables in md (household) and lp (individual); returns list(md, lp, log)
c10_from_dictionary <- function(md, lp, file, survey, choices, apply_filters = TRUE) {
  rules <- dico_read(file)
  log <- list()
  for (nm in unique(rules$nom_dans_analyse)) {
    G <- rules[rules$nom_dans_analyse == nm, ]
    lvl <- G$niveau_calcul[1]; ind <- lvl %in% "individu"
    df <- if (ind) lp else md
    entry <- list(indicateur = nm, ids = paste(G$id, collapse = ", "), niveau = lvl, methode = NA_character_,
                  statut = "created", raison = NA_character_, n_valeurs = NA_integer_, n_retires_par_filtre = NA_integer_)
    if (nm %in% names(df)) {
      entry$statut <- "skipped"; entry$raison <- "variable already in the data (never overwritten)"
    } else {
      res <- tryCatch(dico_build_one(G, df, lp, survey, choices), error = function(e) e)
      if (inherits(res, "error")) {
        entry$statut <- "skipped"; entry$raison <- conditionMessage(res)
      } else if (res$kind == "copy_sm") {
        dum <- dico_dummies(df, res$var)
        df[[nm]] <- ifelse(res$keep, dico_chr(df[[res$var]]), NA_character_)
        for (v in dum) df[[paste0(nm, ".", sub("^[^.]*\\.", "", v))]] <- ifelse(res$keep, df[[v]], NA)
        entry$methode <- "copie_choix_multiple"; entry$n_valeurs <- sum(!is.na(df[[nm]]))
      } else {
        v <- if (res$kind == "copy") df[[res$var]] else res$v
        removed <- sum(!is.na(v) & !res$kf)
        out <- if (apply_filters) res$keep else (res$keep | !res$kf)   # without filters: only the sub-group limits the copy
        v[!out] <- NA
        df[[nm]] <- v
        entry$methode <- if (res$kind == "copy") "copie" else res$method
        entry$n_valeurs <- sum(!is.na(v)); entry$n_retires_par_filtre <- removed
      }
    }
    if (ind) lp <- df else md <- df
    log[[nm]] <- as.data.frame(entry, stringsAsFactors = FALSE)
  }
  list(md = md, lp = lp, log = do.call(rbind, log))
}
