

zero.nas <- function(score) {
  score[is.na(score)] = 0
  return(score)
}

participated <- function(score) {
  return(!is.na(score))
}


quizzes = c(
    "Introduction.to.R.and.accessing.Social.Web.data..Total.Pts..4...122678",
    "Simple.Exposure.Analysis..Total.Pts..4...122679",     
    "Graphs.1..Introduction.to.graphs.and.their.parameters..Total.Pts..4...152126",
    "Text.Mining.1..Indexing.and.Querying.Text..Total.Pts..4...151353",
    "Visualization.of.Social.Web.Data..Total.Pts..4...153499",
    "Text.Mining.2..Clustering..Total.Pts..4...155321",
    "Graphs.2..Link.Analysis..Total.Pts..4...156347",
    "Trends..Total.Pts..4...157272"                                               
)

project = "Submit.Group.Project..Total.Pts..30...122681"
exam = "Exam..Total.Pts..64...122689"


# marks from vUWS
#all.marks <- read.csv("gc_300958_2014_spr_fullgc_2014-11-24-16-58-08.csv")
all.marks <- read.csv("gc_300958_2014_spr_fullgc_2014-11-25-10-51-10.csv")
## add missing project marks
## late exam mark

# list from platformWeb
students <- read.csv("300958, Social Web Analytics.csv", skip=1)

marks <- merge(students, all.marks, by.x="Student.Number", by.y="Username", all.x=TRUE, all.y=FALSE)
# combine all variations of the class test

# Amendments
## Reviewed exam marks
#marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17012834"] = 22

## missing project
#marks$Group.Project..Total.Pts..30...101193[marks$Student.Number == "17575788"] = 10


## deferred exams
#marks$Exam..Total.Pts..56...103209[marks$Student.Number == "17513223"] = 13


sum.top.5 <- function(quiz.marks) {
  sorted.marks = sort(quiz.marks, decreasing=TRUE, na.last=TRUE)
  return(sum(sorted.marks[1:5], na.rm=TRUE))
}

quiz.marks = ceiling(subset(marks, select=quizzes))
project.mark = ceiling(zero.nas(subset(marks, select=project, drop=TRUE)))
exam.mark = ceiling(subset(marks, select=exam, drop=TRUE)*50/60)
cf.grades = c()
f.grades = c()

exam.mark[cf.grades] = floor(exam.mark[cf.grades]*60/64)
exam.mark[f.grades] = floor(exam.mark[f.grades]*60/64)

total.quiz.mark = zero.nas(apply(quiz.marks,1,sum.top.5))

raw.assess.mark = total.quiz.mark + project.mark
raw.exam.mark = exam.mark

final.mark = apply(cbind(raw.assess.mark, raw.exam.mark), 1, sum, na.rm=TRUE)


results = data.frame(Student.Number = marks$Student.Number, Student.Name = marks$Student.Name, raw.assess.mark = raw.assess.mark, raw.exam.mark = raw.exam.mark, final.mark = final.mark)
  
#CF = which((raw.exam.mark < 20) & (final.mark >= 50))

#borderline = which((final.mark < 50) & (final.mark > 47))

## check these marks
#results[borderline,]
#results[CF,]



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


compute.grade <- function(marks) {
    marks$grade = "Missing"
 
    marks$grade[marks$final.mark < 50] = "F"
    marks$grade[(marks$final.mark >= 50) & (marks$final.mark < 65)] = "P"
    marks$grade[(marks$final.mark >= 65) & (marks$final.mark < 75)] = "C"
    marks$grade[(marks$final.mark >= 75) & (marks$final.mark < 85)] = "D"
    marks$grade[(marks$final.mark >= 85)] = "HD"

    marks$grade[is.na(marks$raw.exam.mark)] = "AF"
    marks$grade[(marks$raw.exam.mark < 20) & (marks$final.mark >= 50)] = "CF"
    
    marks$grade = factor(marks$grade, c("Missing","AF","CF","F","P","C","D","HD"), ordered = TRUE)
    return(marks)
}

## compute grades
#grades = sapply(split(results, rownames(results)),compute.grade)
## order the grades
#grades = factor(grades, c("AF","CF","F","P","C","D","HD"), ordered = TRUE)

results = compute.grade(results)

table(results$grade)

subset(results, grade == "CF")


cf.grades = which(results$grade == "CF")
f.grades = which(results$grade == "F")

exam.mark2 = floor(subset(marks, select=exam, drop=TRUE)*50/64)
exam.mark[cf.grades] = exam.mark2[cf.grades]
exam.mark[f.grades] = exam.mark2[f.grades]
raw.exam.mark = exam.mark
final.mark = apply(cbind(raw.assess.mark, raw.exam.mark), 1, sum, na.rm=TRUE)
results = data.frame(Student.Number = marks$Student.Number, Student.Name = marks$Student.Name, raw.assess.mark = raw.assess.mark, raw.exam.mark = raw.exam.mark, final.mark = final.mark)
results = compute.grade(results)

table(results$grade)
subset(results, grade == "CF")


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


withdrawal.rate(results$grade)
cf.rate(results$grade)
high.fail.rate(results$grade)
grade.skew(results$grade)



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




