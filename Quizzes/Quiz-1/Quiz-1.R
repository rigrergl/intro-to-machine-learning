# Name: Rigre R. Garciandia
# NetID: RRG190004
# Section: CS 4375.002

fname <- c(
  "Lenny",
  "John",
  "Ugun",
  "Ulysses",
  "Homer",
  "Alec",
  "Hellen",
  "Eren",
  "Levi",
  "Erwin"
)

lname <- c(
  "Smith",
  "Norwood",
  "Lowood",
  "Udin",
  "Gallagher",
  "Watson",
  "DelCastillo",
  "Yaeger",
  "Strauss",
  "Smith"
)

zipcode <- c(
  75093,
  75093,
  75094,
  75095,
  75096,
  75097,
  75098,
  75099,
  75094,
  75095
)

gpa <- c(
 3.2,
 3.3,
 3.1,
 3.1,
 3.1,
 3.1,
 3.1,
 3.1,
 3.6,
 2.3
)

students.data <- data.frame(fname, lname, zipcode, gpa)

students.data[students.data$gpa >= 3.2,]
students.data[students.data$zipcode == 75093,]