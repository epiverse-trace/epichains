# Copy the front matter of paper.qmd into the rendered paper.md, as JOSS
# reads author and affiliation metadata from the YAML header.
qmd <- readLines("paper.qmd", encoding = "UTF-8")
delims <- which(qmd == "---")[1:2]
front_matter <- qmd[delims[1]:delims[2]]
body <- readLines("paper.md", encoding = "UTF-8")
writeLines(c(front_matter, "", body), "paper.md", useBytes = TRUE)
