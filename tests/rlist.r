

yaml.header <- function() {
  cat("%YAML 1.2\n")
  cat("---\n")
  cat("#   MC: multiple choice\n")
  cat("#   NUM: numerical answer\n")
  cat("#   TF: true/false questions\n")
  cat("# Use python yamltocsv.py question_file.yaml > question_file.csv to create the\n")
  cat("# vUWS ready file.\n\n")
  cat("questions:\n\n")
}


yaml.out <- function(question, parameters) {
  if (question$type == "NUM") {
    return(yaml.NUM.out(question, parameters))
  }
  if (question$type == "MC") {
    return(yaml.MC.out(question, parameters))
  }
}

yaml.NUM.out <- function(question, parameters) {
  cat("    - type: ", question$type, "\n")
  cat("      known.parameters: ", question$known.parameters, "\n")
  cat("      set.parameters: '`r ", parameters, "`'\n")
  cat("      question: ", question$question, "\n")
  cat("      answer: ", question$answer, "\n")
  cat("      tolerance: ", question$tolerance, "\n\n")
}


yaml.MC.out <- function(question, parameters) {
  cat("    - type: ", question$type, "\n")
  cat("      known.parameters: ", question$known.parameters, "\n")
  cat("      set.parameters: '`r ", parameters, "`'\n")
  cat("      question: ", question$question, "\n")
  cat("      choices: ", question$answer, "\n")
  for (choice in question$choices) {
    cat("       - ", choice, "\n")
  }
  cat("      correct_answer: ", question$correct_answer, "\n\n")
}


yaml.footer <- function() {
  cat("...\n")
}
