library(whisker)
template <- 
  'Hello {{name}}
You have just won ${{value}}!
{{#in_ca}}
Well, ${{taxed_value}}, after taxes.
{{/in_ca}}
'

data <- list( name = "Chris"
              , value = 10000
              , taxed_value = 10000 - (10000 * 0.4)
              , in_ca = TRUE
)

text <- whisker.render(template, data)
cat(text)


writeLines(whisker.render(template, data), "./output.toml")