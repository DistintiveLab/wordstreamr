## code to prepare `d3minjs` dataset goes here

download.file("https://cdnjs.cloudflare.com/ajax/libs/d3/7.9.0/d3.min.js",
"inst/htmlwidgets/lib/d3.min.js")

download.file("https://github.com/huyen-nguyen/wordstream-library/raw/refs/heads/master/js/wordstream.js",
              "inst/htmlwidgets/lib/wordstream.js")

#usethis::use_data(d3minjs, overwrite = TRUE)
