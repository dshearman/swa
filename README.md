# Course Material for 300958 Social Web Analytics

This project contains the materials required to run the UWS Unit 300958 Social
Web Analytics.

The project has the following structure:

- bin: all output from lecture compilation
- code: code samples that are useful
- html: html files for presenting the content
- include: common files needed for lecture compilation
- information: information about the unit
- labs: source for lab sheets
- lectures: souce files for lectures
- templates: the structure of the lectures slides
- tests: source files for tests

## Building lecture slides

To build a set of lecture slides, run the command:
```{sh}
make weekXX.slides
```
where XX is the week number.

The resulting PDF is located at bin/300958.weekXX.slides.pdf

To build a set of lecture notes, run the command:
```{sh}
make weekXX.notes
```
where XX is the week number.

The resulting PDF is located at bin/300958.weekXX.notes.pdf

## Building lab notes

Lab notes are compiled by making the HTML version of the Rmd file.
To make the week 3 lab notes:
```{sh}
make labs/300958.week03.lab.Rmd
```

