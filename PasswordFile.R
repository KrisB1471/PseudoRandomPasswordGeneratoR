## Pseudo-Random Password Generator
### Source Credit: https://www.youtube.com/watch?v=CM-sKNebC0o
setwd("C:/Users/krisb/Desktop/Stuff/Password")

#### 1) Create password function
#set.seed(1471) 
passWord <- function(LENGHT) {
  symbolsA <- c("+", "*", "%", "&", "/", "(", ")", "=", 
                "@", "#", "?", "'", "^", "!", "$", "?", 
                "-", "_", ".", ":", ",", ";", "<", ">")
  symbolsB <- c("?", "?", "?", "?", "|", "?", "?", "`", 
                "~", "?", "[", "]", "{", "}") #less common symbols -> but could be a list of anything
  numbers10 <- c(0:9)
  pwChars <- c(letters, LETTERS, symbolsA, symbolsB, numbers10)
  #setting probabilities for each element of pwChars
  probabilities <- c(rep(0.40, 26), rep(0.20, 26), rep(0.10, 24), rep(0.10, 14), rep(0.20, 10)) #can be easily modified/ ajusted
  pWord <- paste0(sample(pwChars, size = LENGHT, replace = TRUE, prob = probabilities
                         #, useHash : 
                         #logical indicating if the hash-version of the algorithm should be used. Can only be used for replace = FALSE, prob = NULL, and size <= n/2, and really should be used for large n, as useHash=FALSE will use memory proportional to n.
                         ), collapse = "")
  return(pWord)
}

#### 2) Set password length and number of passwords desired
pwLenght <- 16 #can be easily modified/ adjusted
pwNumber <- 1000 #can be easily modified/ adjusted

#### 3) Create a list based on pwNumber
xy <- vector("list", pwNumber)

#### 4) Creating a for-loop that calls passWord function -> 1)
counter <- pwNumber
for (pwNumber in 1:pwNumber) {
  dfPassword <- passWord(pwLenght)
  xy[(pwNumber)] <- dfPassword
  counter <- counter-1
  print(dfPassword)
}

#### 5) Place in dataframe and rename column to Passwords
dfPw <- data.frame(matrix(unlist(xy), ncol = 10,
                          byrow = TRUE), stringsAsFactors = FALSE)
colnames(dfPw) <- c("Passwords 1-100", "Passwords 101-200", "Passwords 201-300", "Passwords 301-400", "Passwords 401-500",
                    "Passwords 501-600", "Passwords 601-700", "Passwords 701-800", "Passwords 801-900", "Passwords 901-1000")
dfPw

#### Output to a text file (Notepad)
write.table(dfPw, "C:\\Users\\krisb\\Desktop\\Stuff\\Password\\PasswordFile.txt", sep = "\t")














