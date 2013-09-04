

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


yaml.out <- function(question, parameters = "") {
  if (question$type == "NUM") {
    return(yaml.NUM.out(question, parameters))
  }
  if (question$type == "MC") {
    return(yaml.MC.out(question, parameters))
  }
}

yaml.NUM.out <- function(question, parameters) {
  cat("    - type: '", question$type, "'\n", sep="")
  cat("      known.parameters: '", question$known.parameters, "'\n", sep="")
  cat("      set.parameters: '`r ", parameters, "`'\n", sep="")
  cat("      question: '", question$question, "'\n", sep="")
  cat("      answer: '", question$answer, "'\n", sep="")
  cat("      tolerance: '", question$tolerance, "'\n\n", sep="")
}


yaml.MC.out <- function(question, parameters) {
  cat("    - type: '", question$type, "'\n", sep="")
  cat("      known.parameters: '", question$known.parameters, "'\n", sep="")
  cat("      set.parameters: '`r ", parameters, "`'\n", sep="")
  cat("      question: '", question$question, "'\n", sep="")
  cat("      choices:\n", sep="")
  for (choice in question$choices) {
    cat("       - '", choice, "'\n", sep="")
  }
  cat("      correct_answer: ", question$correct_answer, "\n\n", sep="")
}


yaml.footer <- function() {
  cat("...\n", sep="")
}
