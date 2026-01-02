###############################################################################
# ANALYZE TREESLAB'S PAPERS
# Last update 2024-22-29
###############################################################################

library(bib2df)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(purrr)

#NOTE: Install package bibliometrix from github!
#      If in trouble, use the dev branch!
library(bibliometrix)

set.seed(123)

# Configuration.
in_dir <-
  "/home/alber/Documents/trees_lab/relatorios/bibliometry/trees_papers/queries/query_20251226/data"

stopifnot("Input directory not found!" = dir.exists(in_dir))

# Bibtex file with TREES Lab publication list.
bib_file <- "/home/alber/Documents/trees_lab/relatorios/bibliometry/zotero/treeslab.bib"

stopifnot("bib file not found!" = file.exists(bib_file))

out_dir <-
  "/home/alber/Documents/trees_lab/relatorios/bibliometry/trees_papers/report/slides"

stopifnot("Output directory not found!" = dir.exists(out_dir))

# Figure size.
h <- 105
w <- 148.5
u <- "mm"

field_names <- c(
  "abstracts"        = "AB",
  "cited_references" = "CR",
  "keyword_authors"  = "DE",
  "keyword_plus"     = "ID",
  "titles"           = "TI"
)



#--- Utilitary functions ----

# Util function. Export a tibble into a latex file.
table2latex <- function(x, file_path) {
  x %>%
    kableExtra::kable(
      format = "latex",
      booktabs = TRUE,
      digits = 2
    ) %>%
    cat(file = file_path, append = FALSE)
  invisible(file_path)
}

# Util function. Process a bibliometrix table.
process_table <- function(x, int_cols = NA, double_cols = NA, text_width = 80) {
  x <-
    x %>%
    magrittr::set_colnames(trimws(colnames(.))) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), as.character),
      dplyr::across(tidyselect::everything(), trimws),
      dplyr::across(tidyselect::everything(),
                \(x) stringr::str_trunc(x, width = text_width))
    )
  if (all(!is.na(int_cols)))
    x <- dplyr::mutate(x, dplyr::across(tidyselect::all_of(int_cols), 
                                        as.integer))
  if (all(!is.na(double_cols)))
    x <- dplyr::mutate(x, dplyr::across(tidyselect::all_of(double_cols), 
                                        as.double))
  return(x)
}



#---- Analyze TREES Lab publication list ----



# Export table with number of documents per type.
bib_tb <-
  bib_file %>%
  bib2df::bib2df() %>%
  dplyr::mutate(YEAR = as.integer(YEAR))

bib_tb %>%
  dplyr::count(CATEGORY) %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  table2latex(file_path = file.path(out_dir, "tables",
                                    "summary_doc_type_total.tex"))

p <- bib_tb %>%
  dplyr::count(CATEGORY, YEAR) %>%
  dplyr::filter(!(CATEGORY %in% c("MISC", "TECHREPORT"))) %>%
  dplyr::arrange(CATEGORY, YEAR) %>%
  ggplot2::ggplot(ggplot2::aes(x = YEAR, y = n, color = CATEGORY)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(x = "Year", y = "Number of publications")

ggplot2::ggsave(
  filename = file.path(out_dir, "figures", "AnnualScientProd_total.png"),
  plot = p,
  width = w, height = h, units = u
)



#---- Analyze TREES Lab publications with DOI ----



#---- Read and merge Scopus and Web of Science data ----

# Scopus.
scopus_files <- list.files(
  path = in_dir,
  pattern = ".csv$",
  full.names = TRUE
)

scopus_df <- bibliometrix::convert2df(
  scopus_files,
  dbsource = "scopus",
  format = "csv"
)

# Web of Science.
wos_files <- list.files(
  path = in_dir,
  pattern = ".txt$",
  full.names = TRUE
)

wos_df <- bibliometrix::convert2df(
  wos_files,
  dbsource = "wos",
  format = "plaintext"
)

# Union of Scopus and Web of Science.
data_df <- bibliometrix::mergeDbSources(scopus_df, wos_df,
  remove.duplicated = TRUE
)

data_df <-
  data_df #%>%
  # dplyr::filter(#DT %in% c("ARTICLE", "DATA PAPER", "ARTICLE; DATA PAPER"),
  #               #PY %in% 2001:2023,
  #               LA %in% c("ENGLISH", "PORTUGUESE", "SPANISH"))

save(data_df, file = file.path("output", "papers_treeslab.RData"))

# Results.
analysis_bm <- bibliometrix::biblioAnalysis(data_df)



#---- Summary of the analysis ----

analysis_summary <- summary(object = analysis_bm, k = 20)

# Export summary table to latex table.
analysis_summary %>%
  magrittr::extract2("MainInformationDF") %>%
  dplyr::as_tibble() %>%
  dplyr::filter(Description %in%
    c(
      "Timespan", "Sources (Journals, Books, etc)", "Documents",
      "Annual Growth Rate %", "Authors", "Authors of single-authored docs",
      "International co-authorships %", "Co-Authors per Doc",
      "Author's Keywords (DE)", "References", "Document Average Age",
      "Average citations per doc"
    )) %>%
  process_table() %>%
  table2latex(file.path(out_dir, "tables", "summary_main_information.tex"))

# Export table with number of documents per type.
analysis_summary %>%
  magrittr::extract2("MainInformationDF") %>%
  dplyr::as_tibble() %>%
  dplyr::filter(Description %in%
    c("article", "article; proceedings paper", "biographical-item", 
      "conference paper", "correction", "data paper", "editorial", 
      "editorial material", "erratum", "letter", "note", "review", 
      "short survey")) %>%
  process_table(int_cols = "Results") %>%
  dplyr::arrange(dplyr::desc(Results)) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_doc_type_doi.tex"))

# Export table with most productive authors. 
analysis_summary %>%
  magrittr::extract2("MostProdAuthors") %>%
  dplyr::select(1:2) %>%
  process_table(int_cols = "Articles") %>%
  dplyr::slice_max(n = 10, order_by = Articles) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_prod_authors.tex"))

# Export table with most productive authors (fractionalized).
analysis_summary %>%
  magrittr::extract2("MostProdAuthors") %>%
  dplyr::select(3:4) %>%
  process_table(double_cols = "Articles Fractionalized") %>%
  dplyr::slice_max(n = 10, order_by = `Articles Fractionalized`) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_prod_authors_frac.tex"))

# Export table with most used journals.
analysis_summary %>%
  magrittr::extract2("MostRelSources") %>%
  process_table(int_cols = "Articles", text_width = 60) %>%
  dplyr::slice_max(n = 10, order_by = Articles) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_rel_sources.tex"))

# Export table with most cited papers.
analysis_summary %>%
  magrittr::extract2("MostCitedPapers") %>%
  process_table(int_cols = "TC", double_cols = c("TCperYear", "NTC")) %>%
  dplyr::slice_max(n = 10, order_by = TC) %>%
  dplyr::select(-DOI) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_cited_papers.tex"))

# Export table with most relevant keywords.
analysis_summary %>%
  magrittr::extract2("MostRelKeywords") %>%
  dplyr::select(1:2) %>%
  process_table(int_cols = "Articles") %>%
  dplyr::slice_max(n = 10, order_by = Articles) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_rel_keywords.tex"))

# Export table with most relevant keywords (keyword plus).
analysis_summary %>%
  magrittr::extract2("MostRelKeywords") %>%
  dplyr::select(3:4) %>%
  process_table(int_cols = "Articles") %>%
  dplyr::slice_max(n = 10, order_by = Articles) %>%
  table2latex(file_path = file.path(out_dir, "tables", 
                                    "summary_most_rel_keywords_kwplus.tex"))



# Get base plots.
base_plots <- plot(analysis_bm, k = 5)

vapply(names(base_plots), FUN.VALUE = character(1), function(plot_name) {
    file_name <- file.path(out_dir, "figures", paste0(plot_name, ".png"))
    pplot <- base_plots[[plot_name]] + ggplot2::ggtitle(NULL)
    ggplot2::ggsave(
        plot = pplot,
        filename = file_name,
        width = w, height = h, units = u
    )
    return(file_name)
})



# Conceptual structure.

con_stru <-
  bibliometrix::conceptualStructure(
    M = data_df, 
    field = "ID", 
    method = "CA", 
    stemming = TRUE, 
    minDegree = 3, 
    clust = "auto" ,
    labelsize = 10,
    documents = 20,
    ngrams = 2
  )

ggplot2::ggsave(
    plot = con_stru[["graph_terms"]],
    filename = file.path(out_dir, "figures", "con_stru_map.png"),
    width = w * 2, height = h * 2, units = u
  )

ggplot2::ggsave(
    plot = con_stru[["graph_documents_Contrib"]],
    filename = file.path(out_dir, "figures", "con_stru_map_contrib.png"),
    width = w * 2, height = h * 2, units = u
  )

ggplot2::ggsave(
    plot = con_stru[["graph_documents_TC"]],
    filename = file.path(out_dir, "figures", "con_stru_map_cited.png"),
    width = w * 2, height = h * 2, units = u
  )



# Networks.

analysis_type <- as.data.frame(tibble::tribble(
  ~analysis, ~network,
  "collaboration", "authors",
  "collaboration", "universities",
  "co-citation", "references",
  "coupling", "authors",
  "coupling", "sources",
  "co-occurrences", "authors",
  "co-occurrences", "sources",
  "co-occurrences", "keywords",
  "co-occurrences", "author_keywords",
  # NOTE: These analysis throw errors!
  # "co-occurrences", "abstracts",
  # "co-occurrences", "titles",
  # "coupling", "countries",
  # "co-citation", "sources",
  # "co-citation", "authors",
  # "collaboration", "countries",
))
rownames(analysis_type) <-
  paste0(analysis_type$network, ";", analysis_type$analysis)

bnetwork_lst <-
    apply(analysis_type, MARGIN = 1, FUN = function(x) {
        tryCatch({
            bibliometrix::biblioNetwork(
                M = data_df,
                analysis = x[["analysis"]],
                network = x[["network"]]
            )
        }, error = function(e){
                print(e)
                return(NULL)
            })
    })
bnetwork_lst <- bnetwork_lst[!vapply(bnetwork_lst, is.null, logical(1))]

for (analysis in names(bnetwork_lst)) {
  # NOTE: sources co-occurrences throws an error
  if (analysis %in% "sources;co-occurrences") {
    next
  }
  nplot <-
    networkPlot(
      bnetwork_lst[[analysis]],
      n = 50,
      type = "auto",
      normalize="association", 
      labelsize = 1,
      Title = stringr::str_to_title(gsub(";", " ", analysis)),
      size.cex = TRUE, 
      size = 10,
      remove.multiple = TRUE,
      edgesize = 3, 
      #labelsize = 1,
      label.cex = TRUE,
      label.n = 30,
      edges.min = 2
    )
  png(filename = file.path(
    out_dir, "figures",
    paste0("bnet_", gsub(";", "_", analysis), ".png")
  ))
  plot(nplot[["graph"]])
  dev.off()
}



# Thematic map.

# Parameters for the thematic map plots.
tmap_params <- list(
    field = c("DE", "ID", "TI", "AB"),
    n.labels = c(1, 5, 1, 5)
)

tmap_ls <- 
    purrr::pmap(tmap_params, bibliometrix::thematicMap, M = data_df, n = 250, 
    minfreq = 5, size = 0.5, repel = TRUE, stemming = TRUE, subgraphs = FALSE)

vapply(tmap_ls, FUN.VALUE = character(1), function(tmap) {
    params_df <- tmap[["params"]]
    field_abb <- params_df[params_df$params == "field", "values"]
    field_name <- names(which(field_names == field_abb)) 
    file_name <- file.path(out_dir, "figures", 
        paste0("thematic_map_", field_name, ".png"))
    ggplot2::ggsave(
        plot = tmap[["map"]], 
        filename = file_name, 
        width = w, height = h, units = u
    )
    invisible(file_name)
})


# Thematic evolution

time_breaks <-
    analysis_summary %>% 
    magrittr::extract2("MainInformationDF") %>%
    dplyr::as_tibble() %>%
    dplyr::filter(Description == "Timespan") %>%
    dplyr::pull(Results) %>%
    stringr::str_split_1(pattern = ":") %>%
    as.integer() %>%
    -1 %>%
    (function(x) {
        seq(from = x[1], to = x[2], by = 5)[-1]
    })

tevol_params <- list(
    field = c("ID", "DE", "TI", "AB")
)

tevol_ls <- purrr::pmap(tevol_params, bibliometrix::thematicEvolution,
    M = data_df, years = time_breaks, n = 250, minFreq = 2
)

lapply(tevol_ls, function(tevol){
    params_df <- tevol[["params"]]
    field_abb <- params_df[params_df$params == "field", "values"]
    field_name <- names(which(field_names == field_abb)) 
    fnames <- list()
    for (y in seq(time_breaks)) {
        file_name <- file.path(out_dir, "figures", 
            paste0("thematic_evolution_", y, ".png"))
        ggplot2::ggsave(plot = tevol[["TM"]] [[y]] [["map"]], 
            filename = file_name, width = w, heigh = h, units = u)
        fnames <- append(fnames, file_name)
    }
    invisible(fnames)
})




t_evolution <- bibliometrix::thematicEvolution(
  M = data_df,
  field = "ID",
  years = time_breaks,
  n = 250,
  minFreq = 2
)

for (y in seq(time_breaks)) {
    ggplot2::ggsave(
        plot = t_evolution[["TM"]] [[y]] [["map"]],
        filename = file.path(out_dir, "figures",
                             paste0("thematic_evolution_", y, ".png")),
        width = w, height = h, units = u
    )
}








# Coupling map.

analysis_type <- c("documents", "authors", "sources")

couplingmap_w <- lapply(analysis_type, function(x) {
  bibliometrix::couplingMap(
    M = wos_df,
    analysis = x,
    field = field_names["cited_references"],
    impact.measure = "local"
  )
})

for (i in seq_along(couplingmap_w)) {
  ggplot2::ggsave(
    plot = couplingmap_w[[i]][["map"]],
    filename = file.path(
      out_dir, "figures",
      paste0("couplingmap_w_", analysis_type[i], ".png")
    ),
    width = w, height = h, units = u
  )
}



# Plot number of documentes per year and journal.

(data_df %>%
  bibliometrix::sourceGrowth(top = 5) %>%
  tidyr::pivot_longer(
    cols = -tidyselect::one_of("Year"),
    names_to = "Sources",
    values_to = "Documents"
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(
    x = Year,
    y = Documents,
    color = Sources
  )) +
  ggplot2::geom_point(ggplot2::aes(
    x = Year,
    y = Documents,
    color = Sources,
    shape = Sources
  )) +
  ggplot2::ylab("Cumulate occurrences")) %>%
  ggplot2::ggsave(
    filename = file.path(out_dir, "figures", "sources_production_over_time.png"),
    width = w, height = h, units = u
  )


# Replace missing figures with dummies.
figures <- file.path(out_dir, "figures",
    c(
        "bnet_authors_coupling.png",
        "bnet_references_co-citation.png",
        "bnet_sources_coupling.png",
        "couplingmap_w_authors.png",
        "couplingmap_w_documents.png",
        "couplingmap_w_sources.png"
    )
)

if (!all(file.exists(figures))) {
    for (fig in figures[!file.exists(figures)]) {
        if (tools::file_ext(fig) == "png") {
            png(filename = fig)
            plot(1:10, type = "l", axes = FALSE, xaxt = "n", yaxt = "n",
                xlab = "", ylab = "",
                lwd = 5)
            lines(10:1, lwd = 5)
            dev.off()
        }
    }
}

