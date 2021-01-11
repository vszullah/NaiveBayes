# NaiveBayes
library(NLP)
library(tm)
library(stringr)
library(caret)
library(dplyr)
library(katadasaR)
library(tau)
library(parallel)
library(readxl)
library(stopwords)

#memanggil data#
dataytri <- read_excel("D:/VITA/Semester 7/Metode Data Tidak Terstruktur/TUBES/dataytri.xlsx")

#mengubah file ke dalam corpus#
corpusdataytri <- Corpus(VectorSource(dataytri$Tweet))

#mengubah semua huruf kapital menjadi huruf kecil#
dataytri_casefolding <- tm_map(corpusdataytri, content_transformer(tolower))

#menghapus url pada dokumen#
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
dataytri_URL <- tm_map(dataytri_casefolding, content_transformer(removeURL))

#menghapus mention pada dokumen#
remove.mention <- function(x)gsub("@\\S+","",x)
dataytri_mention <- tm_map(dataytri_URL, remove.mention)

#menghapus hashtag pada dokumen#
remove.hashtag <- function(x)gsub("#\\S+","",x)
dataytri_hashtag <- tm_map(dataytri_mention, remove.hashtag)

#menghapus tanda baca#
dataytri_punctuation <- tm_map(dataytri_hashtag, content_transformer(removePunctuation))

#menghapus nomor#
dataytri_nonumber <- tm_map(dataytri_punctuation, content_transformer(removeNumbers))

#merubah ke bentuk kata dasar#
stem_text <- function(text, mc.cores = 1)
{
  stem_string <- function(str)
  {str <- tokenize(x = str)
  str <- sapply(str, katadasaR)
  str <- paste(str, collapse = "")
  return(str)
  }
  x <- mclapply(X = text, FUN = stem_string, mc.cores = mc.cores)
  return(unlist(x))
}
dataytri_stemming <- tm_map(dataytri_nonumber, stem_text)

#menghapus kata-kata tidak penting#
cStopwordID <- readLines("D:/VITA/Semester 7/Metode Data Tidak Terstruktur/stopwords1.csv")
dataytri_stopword <- tm_map(dataytri_stemming, removeWords, cStopwordID)

#menghapus spasi yang berlebihan#
dataytri_whitespace <- tm_map(dataytri_stopword, stripWhitespace)

#menyimpan dokumen yang sudah bersih ke dalam format csv#
databersih <- data.frame(text = unlist(sapply(dataytri_whitespace, '[')), stringsAsFactors = F)
write.csv(databersih, file = "D:/VITA/Semester 7/Metode Data Tidak Terstruktur/TUBES/cleandataytri.csv")

library(tokenizers)
corpusytribersih <- Corpus(VectorSource(databersih$text))
inspect(corpusytribersih[1:10])

teks=corpusytribersih
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(teks)

library(wordcloud)
teks<-as.character(teks)
wordcloud(teks, colors=brewer.pal(8, "Dark2"))

#Naive Bayes
library(e1071)
#Klasifikasi Data
library(caret)
#Membaca Fungsi get_nrc
library(syuzhet)

charytri <- as.character(databersih$text)
s <- get_nrc_sentiment(charytri)

#Mengklasifikasi Data
dataytri_combine <- cbind(databersih$text,s)
par(mar=rep(3,4))
a <- barplot(colSums(s),col=rainbow(15),ylab='count',main='Sentiment Analysis Rewind Indonesia 2020')
brplt <-a
