

zero.nas <- function(score) {
  score[is.na(score)] = 0
  return(score)
}

participated <- function(score) {
  return(!is.na(score))
}


quizzes = c(
  "Introduction.to.R.and.accessing.Social.Web.data..Total.Pts..4...90432",
  "Simple.Exposure.Analysis..Total.Pts..4...91280",
  "Text.Mining.1..Indexing.and.Querying.Text..Total.Pts..4...92110",
  "Graphs.1..Introduction.to.graphs.and.their.parameters..Total.Pts..4...92917",
  "Visualization.of.Social.Web.Data..Total.Pts..4...93939",
  "Text.Mining.2..Clustering..Total.Pts..4...95031",
  "Graphs.2..Link.Analysis..Total.Pts..4...95456",
  "Trends..Total.Pts..4...96097") 

project = "Group.Project..Total.Pts..30...101193"
exam = "Exam..Total.Pts..56...103209"


# marks from vUWS
all.marks <- read.csv("gc_300958_2013_spr_fullgc_2013-11-28-16-30-55.csv")
## add missing project marks
## late exam mark

# list from platformWeb
students <- read.csv("300958 Social Web Analytics.csv", skip=1)

marks <- merge(students, all.marks, by.x="Student.Number", by.y="Username", all.x=TRUE, all.y=FALSE)
# combine all variations of the class test

# Amendments
## Reviewed exam marks
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17012834"] = 22
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17212683"] = 25
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17228350"] = 24
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17282303"] = 25
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17504788"] = 26
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17537041"] = 22
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17542047"] = 25
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17259822"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17461984"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17504270"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "16733406"] = 24
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17013707"] = 28
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17207052"] = 28
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17240244"] = 28
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17275301"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17278384"] = 33
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17344207"] = 26
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17360210"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17455132"] = 34
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17517270"] = 29
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17527330"] = 23
marks$Exam..Total.Pts..56...103209[marks$Student.Number == "16972504"] = 23


## missing project
marks$Group.Project..Total.Pts..30...101193[marks$Student.Number == "17575788"] = 10
marks$Group.Project..Total.Pts..30...101193[marks$Student.Number == "17423830"] = 10
marks$Group.Project..Total.Pts..30...101193[marks$Student.Number == "16665037"] = 14
marks$Group.Project..Total.Pts..30...101193[marks$Student.Number == "16861318"] = 14


sum.top.5 <- function(quiz.marks) {
  sorted.marks = sort(quiz.marks, decreasing=TRUE, na.last=TRUE)
  return(sum(sorted.marks[1:5], na.rm=TRUE))
}

quiz.marks = ceiling(subset(marks, select=quizzes))
project.mark = ceiling(zero.nas(subset(marks, select=project, drop=TRUE)))
exam.mark = ceiling(subset(marks, select=exam, drop=TRUE)*50/56)

total.quiz.mark = zero.nas(apply(quiz.marks,1,sum.top.5))

raw.assess.mark = total.quiz.mark + project.mark
raw.exam.mark = exam.mark

final.mark = raw.assess.mark + raw.exam.mark


results = data.frame(Student.Number = marks$Student.Number, Student.Name = marks$Student.Name, raw.assess.mark = raw.assess.mark, raw.exam.mark = raw.exam.mark, final.mark = final.mark)
  
CF = which((raw.exam.mark < 20) & (final.mark >= 50))

borderline = which((final.mark < 50) & (final.mark > 47))

## check these marks
results[borderline,]
results[CF,]



compute.grade <- function(marks) {
  if (is.na(marks$raw.exam.mark)) {
    return("AF")
  }
  if (marks$final.mark < 50) {
    return("F")
  }
  if ((marks$raw.exam.mark < 20) & (marks$final.mark >= 50)) {
    return("CF")
  }
  if ((marks$final.mark >= 50) & (marks$final.mark < 65)) {
    return("P")
  }
  if ((marks$final.mark >= 65) & (marks$final.mark < 75)) {
    return("C")
  }
  if ((marks$final.mark >= 75) & (marks$final.mark < 85)) {
    return("D")
  }
  if ((marks$final.mark >= 85)) {
    return("HD")
  }
  return("Missing")
}

## compute grades
grades = sapply(split(results, rownames(results)),compute.grade)
## order the grades
grades = factor(grades, c("AF","CF","F","P","C","D","HD"), ordered = TRUE)

table(grades)

withdrawal.rate <- function(grades) {
  return(mean(grades == "AF"))
}

cf.rate <- function(grades) {
  s = grades[grades != "AF"]
  return(mean(s == "CF"))
}

high.fail.rate <- function(grades) {
  return(mean((grades == "F") | (grades == "CF")))
}

grade.skew <- function(grades) {
  return(sum((grades == "HD") | (grades == "D"))/sum((grades == "P") | (grades == "C")))
}


withdrawal.rate(grades)
cf.rate(grades)
high.fail.rate(grades)
grade.skew(grades)


#admin = FALSE
admin = TRUE

if (admin == TRUE) {

# Report results
# produce desired table
admin <- data.frame(student.number=results$Student.Number,
                    student.name=results$Student.Name,
                    raw.assess.mark=results$raw.assess.mark,
                    raw.exam.mark=results$raw.exam.mark,
                    override.grade=as.character(""))

name.order = order(admin$student.name)
admin = admin[name.order,]

write.table(admin, file="300958.results.2013S.csv", sep=",", row.names=FALSE, na = "",
            col.names=c("Student Number","Student Name",
              "Raw Assess Mark","Raw Exam Mark","Override Grade"))


}
