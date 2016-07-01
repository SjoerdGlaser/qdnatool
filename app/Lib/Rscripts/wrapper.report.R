report <- function(file.name, title, number.answeroptions, key, item.names,
                   correct.frequency, correct.percentage,
                   frequency.answer.options, percentage.answer.options,
                   corrected.item.tot.cor, corrected.item.tot.cor.answ.option,
                   cronbach, student.scores, categories) {
  rmarkdown::render("Rapport_subcategories.Rmd", output_format = "html_document",
                    output_file = paste0(file.name,".html"))
  rmarkdown::render("Rapport_subcategories.Rmd", output_format = "pdf_document",
                    output_file = paste0(file.name,".pdf"))
}