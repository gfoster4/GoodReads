#Setting the working directory
setwd("~/Documents/Current_Classes/R_Programming/Internship Project")

#Importing the cleaned csv file
books <- read.csv("books.csv")

#Making sure the imported object is a data frame
class(books)

#Deleting the ISBN and ISBN13 columns
books[,5:6] #Here are the columns to be deleted
books <- books[,-5:-6]
colnames(books) #Checking to make sure the two columns have been removed

#Initial Look into the data
summary(books)

#We can see that the publication date is in a character format, keeping us from
#performing proper analysis on that column. Let's fix that.
typeof(books$publication_date)
head(books$publication_date)
#We can convert the character strings into dates with the lubridate package

library(stringr)
nchar(head(books$publication_date))
str_sub(books$publication_date, 5, 10) <- ""
books$publication_date <- as.numeric(books$publication_date)
books$publication_date
typeof(books$publication_date)


#Renaming the dates column to something more convenient
colnames(books)
newnames <- c("bookID", "title", "authors", "average_rating", "lang_code", "num_pages", "num_ratings", "num_text_reviews", "pub_date", "publisher")
colnames(books) <- newnames

#Quick look at the cleaned data
summary(books)

#Without removing extreme cases, we have the following baseline data

#The average rating for books in this data set is 3.934
#The average number of pages for books in this data set is 299
#The average number of ratings for books in this data set is 17,936
#The average publication date for books in this data set is 2000, with the 
#earliest being 1900 and the most recent being 2020. 

#Let's take an initial look at our variables with the built-in histogram function
hist(books$num_pages) #The majority of books are under 500 pages in length
hist(books$num_text_reviews) #The vast majority of books have under 5000 text reviews
hist(books$num_ratings) # Virtually all books in the dataset have somewhere between 0 and 1000 ratigns 
hist(books$average_rating) #Most books have an average rating somewhere between 3.5 and 4.5



#Let's see if we can find any relationships between the variables, using appropriate 
#x and y-axis limitations to view the bulk of the data the 1000 page limit was obtained with common sense
#after viewing this plot. 
hist(books$num_pages)


#The various ylim values were obtained using the +-1.5(IQR) formula, using common sense
#when necessary

#There doesn't appear to be a clear relationship between number of pages and average rating
plot(books$num_pages, books$average_rating, xlim=c(0,1000), ylim=c(2.5,5))

#It looks like as the book pages reach about 600-700 pages the number of reviews begin to taper off
plot(books$num_pages, books$num_ratings, xlim=c(0,1000), ylim=c(0,7335)) #7335 ylim was obtained using the 1.5(IQR) formula, with the IQR for number of ratings being 4890
plot(books$num_pages, books$num_text_reviews, xlim=c(0,1000), ylim=c(9,343))

#The only relationship that can be seen at a glance between book length
#and date of publication is that prior to the late 1950s, books
#logged into GoodReads do not surpass 1000 pages. Most books stored in the 
#GoodReads database appear to be under 1000 pages in general.
plot(books$num_pages, books$pub_date, xlim=c(0,1000))
plot(books$num_pages, books$pub_date, xlim=c(0,1500))

#Creating a multivariate plot, which I can only do if I can find a distinct grouping variable
#The only distinct grouping variable that comes to mind is the language

unique(books$lang_code)
books$lang_code <- factor(books$lang_code, ordered=TRUE, levels=c("eng", "en-US", "fre", "spa", "en-GB", "mul", "grc", "enm", "en-CA", "ger", "jpn", "ara", "nl", 
                                                                  "zho", "lat", "por", "srp", "ita", "rus", "msa", "glg", "wel", "swe", "nor", "tur", "gla", "ale"))

#Taking out unnecessary variables
mvbooks<-books[,-1:-3]
mvbooks<-mvbooks[,-7]
mvbooks<-mvbooks[,-4:-5]

colnames(mvbooks)
mvbooks2 <- mvbooks[,c(2,4,3,1)]
colnames(mvbooks2)
summary(mvbooks2)


# library(lattice)
# ms <- mvbooks2[sample(nrow(books), 5000),]
# parallelplot(~ms, group = lang_code, data = ms, horizontal.axis=FALSE)

library(cdparcoord)
mm <- discretize(mvbooks2,nlevels=15) 
discparcoord(mm,k=5000,saveCounts=FALSE,name="test") 

#This interactive plot would be more useful if there were fewer years and page numbers smushed together.
#Let's focus on the years column first

summary(mvbooks2$pub_date)

#We can use the 1.5(IQR) strategy to figure out which years are considered likely outliers
#The IQR of pub_date is 2005-1998, or 7 years. 
#7(1.5)=10.5. 2005+10.5=2010.5, or 2011 rounding up.
# 1998-10.5=1987.5, or 1987 rounding down. 
#Let's just look at books published between 1987 and 2011.

library(tidyverse)

mvbooks3 <- mvbooks2 %>% filter(pub_date >= 1987, pub_date <= 2011)
summary(mvbooks3)


#page number column
summary(mvbooks3$num_pages)
mvbooks3 <- mvbooks3 %>% filter(num_pages <= 746)
summary(mvbooks3)

#Now we can use the mvbooks3 dataset to make a more useful multivariate plot.
mm <- discretize(mvbooks3,nlevels=100) 
discparcoord(mm,k=5000,saveCounts=FALSE,name="GoodReads Analysis", minFreq = 1) 

#Using this interactive visualization, I figured out that there is a book in the dataset published in 
#1989, with fewer than 3 pages but with an average GoodReads rating of 4 or greater. (Insert video)
#Let's see if we can find this item using the sqlr package

library(sqldf)

sqldf("select * from books where pub_date == 1989 and average_rating >= 4 and num_pages <= 3")

#While it appears that we were mislead about the German title, the visualization did lead us to discover an apparent error in the
#dataset. The Day Before Midnight in reality has 434 pages according to google books, and the Feynman Lectures on Physics 
#actually has 384 pages, according to GoodReads as of current.

#No matter how well you clean and visualize a data set, if the raw data is faulty there is nothing you can do but get better data.

#Let's see if we can find any other interesting books, starting with the one with the most pages

books[which.max(books$num_pages), ] #The book in the dataset with the most number of pages is a 5 volume set of the Complete Aubrey/Maturin Novels

#Now let's see which book had the most number of text reviews

books[which.max(books$num_text_reviews),] #Ahh lovely... Twilight...

#Lowest rated?

books[which.min(books$average_rating),] #Ok that's cheating a bit, this book has a 0 avg rating but also doesn't have any ratings...

#Lowest rated WITH ratings

book_rated <- books %>% filter(num_ratings>0)

book_rated[which.min(book_rated$average_rating),] #Much more interesting, a Puzzle Pack from 2005 haha

#I wonder which book has the highest rating?

book_rated[which.max(book_rated$average_rating),] #Also cheating a bit, since it only has one rating

#Highest rated book with atleast 100 reviews

book_extra_rated <- books %>% filter(num_ratings>100)
book_extra_rated[which.max(book_extra_rated$average_rating),] #The Complete Calvin and Hobbes reigns supreme, apparently

#Just for fun, let's make a random book generator that uses titles from the GoodReads dataset we are working with

gimmeABook <- function(){
  range <- c(1:11127)
  rando <- sample(range, 1)
  books[rando,]
}

gimmeABook()
gimmeABook()
gimmeABook()
gimmeABook()

#It works!! As a book lover this function is surprisingly fun

#Write author and title search functions##############

