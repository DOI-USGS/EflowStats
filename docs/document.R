Rd2md::ReferenceManual(outdir = "docs/")


outfile <- "docs/Reference_Manual_EflowStats.md"

text <- readLines(outfile)

headings <- text[grepl("^# `.*", text)]

headings <- gsub("`", "", gsub("# `", "", headings))

toc <- paste0("- [", headings, "](#", headings, ")")

text <- c("# Vignettes",
          "- [Introduction](intro.md)",
          "- [Discrepencies](packageDiscrepencies.md)",
          "",
          "# Function Reference",
          "",
          toc,
          "",
          text[2:length(text)])

error_line <- which(grepl(".*```$", text) & !grepl("^```,*", text))

text[error_line] <- gsub("```", "", text[error_line])

text <- c(text[1:error_line], "```", text[error_line + 1:length(text)])

text <- text[!is.na(text)]

writeLines(text, outfile)

rmarkdown::render("vignettes/intro.Rmd", "md_document", output_file = "../docs/intro.md")
rstudioapi::restartSession()

rmarkdown::render("vignettes/packageDiscrepencies.Rmd", "md_document", output_file = "../docs/packageDiscrepencies.md")
rstudioapi::restartSession()
