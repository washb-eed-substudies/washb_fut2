# This code chunk will run after the document is rendered to PDF
library(pdftools)
library(here)

# Path to your existing PDF page
existing_pdf <- here("figures/FUT2_enrollment-flowchart.pdf")

# Path to the output PDF from this R Markdown document
output_pdf <- here("EE-BD-FU2-supplementary-figures.pdf")

# Combine the PDFs
pdf_combine(c(output_pdf, existing_pdf), output = "EE-BD-FUT2-supplementary-figures.pdf")