library(Rstem)
# Takes a string of text and returns a vector of word tokens

tokenize <- function(text) {
  # Assum anything surrounded by spaces is a word.
  strsplit(text, " ")	
}

lower_case <- function(text) {
  # Inputs a string and returns the string with all lowercase letters
  tolower(text)
}

remove_numbers <- function(text) {
  # Not using this now. I wantto leave numbers in
}

remove_punctuation <- function(text) {
  # Removes any punctuation from a string of text
  gsub("[^\\w\\s]", "", text, perl=TRUE)
}

stem_words <- function(text) {
  # Break text into a vector
  string_vec <- strsplit(text, " ")
  # strsplit returns a list, we unlist it here
  stem_vec <- wordStem(unlist(string_vec))
  paste(stem_vec, collapse=" ")
}

remove_profanity <- function(text) {
  # Swear words pulled from http://www.bannedwordlist.com/lists/swearWords.txt
  # Regex is my own with lots of help from Google
  # !!! Be sure to run lower_case first, as this regex assumes lower case

  profane_regex <- paste("shit(ty)?\\b\\s?|",
                         "fuck(ing)?(ed)?(en)?\\b\\s?|",
                         "cunt\\b\\s?|",
                         "\\bpussy\\b\\s?|",
                         "\\bass\\b\\s?|",
                         "asshole\\b\\s?|",
                         "blowjob\\b\\s?|",
                         "blow job\\b\\s?|",
                         sep="")
  gsub(profane_regex, "", text, perl=TRUE)  
}

remove_stop_words <- function(text) {
  gsub(stop_word_regex, "", text)
}

#remove_non_ascii <- function(text) {
#  # Something causes the gsub to not return anything, we need
#  # to explicitely return here
#  cleaned <- gsub("â|€|œ", "", text)
#  print("foobar")
#  print(cleaned)
#  return(cleaned)
#}

remove_non_ascii <- function(text) {
  # Explicitly converting to UTF-8, then to ASCII seems to do the trick
  iconv(text, "UTF-8", sub="")
  iconv(text, "UTF-8", "ASCII", sub="")
}

convert_to_utf8 <- function(text) {
  # This removes strange characters
  print(text)
  iconv(text, "UTF-8", sub="")
  print(text)
  iconv(text, "UTF-8", "ASCII", sub="")
  print(text)
  return(text)
}

clean_line <- function(text) {
  as_ascii <- remove_non_ascii(text)
  lower <- lower_case(as_ascii)
  clean <- remove_profanity(lower)
  non_punctuated <- remove_punctuation(clean)
  non_stop <- remove_stop_words(non_punctuated)
}

get_tokens <- function(text) {
  as_ascii <- remove_non_ascii(text)
  lower <- lower_case(as_ascii)
  clean <- remove_profanity(lower)
  non_punctuated <- remove_punctuation(clean)
  non_stop <- remove_stop_words(non_punctuated)
  tokenize(non_stop)
}


stop_word_regex <- paste("\\ba\\b\\s?|",
                         "\\babout\\b\\s?|",
                         "\\babove\\b\\s?|",
                         "\\bacross\\b\\s?|",
                         "\\bafter\\b\\s?|",
                         "\\bafterwards\\b\\s?|",
                         "\\bagain\\b\\s?|",
                         "\\bagainst\\b\\s?|",
                         "\\ball\\b\\s?|",
                         "\\balmost\\b\\s?|",
                         "\\balone\\b\\s?|",
                         "\\balong\\b\\s?|",
                         "\\balready\\b\\s?|",
                         "\\balso\\b\\s?|",
                         "\\balthough\\b\\s?|",
                         "\\balways\\b\\s?|",
                         "\\bam\\b\\s?|",
                         "\\bamong\\b\\s?|",
                         "\\bamongst\\b\\s?|",
                         "\\bamoungst\\b\\s?|",
                         "\\bamount\\b\\s?|",
                         "\\ban\\b\\s?|",
                         "\\band\\b\\s?|",
                         "\\banother\\b\\s?|",
                         "\\bany\\b\\s?|",
                         "\\banyhow\\b\\s?|",
                         "\\banyone\\b\\s?|",
                         "\\banything\\b\\s?|",
                         "\\banyway\\b\\s?|",
                         "\\banywhere\\b\\s?|",
                         "\\bare\\b\\s?|",
                         "\\baround\\b\\s?|",
                         "\\bas\\b\\s?|",
                         "\\bat\\b\\s?|",
                         "\\bback\\b\\s?|",
                         "\\bbe\\b\\s?|",
                         "\\bbecame\\b\\s?|",
                         "\\bbecause\\b\\s?|",
                         "\\bbecome\\b\\s?|",
                         "\\bbecomes\\b\\s?|",
                         "\\bbecoming\\b\\s?|",
                         "\\bbeen\\b\\s?|",
                         "\\bbefore\\b\\s?|",
                         "\\bbeforehand\\b\\s?|",
                         "\\bbehind\\b\\s?|",
                         "\\bbeing\\b\\s?|",
                         "\\bbelow\\b\\s?|",
                         "\\bbeside\\b\\s?|",
                         "\\bbesides\\b\\s?|",
                         "\\bbetween\\b\\s?|",
                         "\\bbeyond\\b\\s?|",
                         "\\bbill\\b\\s?|",
                         "\\bboth\\b\\s?|",
                         "\\bbottom\\b\\s?|",
                         "\\bbut\\b\\s?|",
                         "\\bby\\b\\s?|",
                         "\\bcall\\b\\s?|",
                         "\\bcan\\b\\s?|",
                         "\\bcannot\\b\\s?|",
                         "\\bcant\\b\\s?|",
                         "\\bco\\b\\s?|",
                         "\\bcomputer\\b\\s?|",
                         "\\bcon\\b\\s?|",
                         "\\bcould\\b\\s?|",
                         "\\bcouldnt\\b\\s?|",
                         "\\bcry\\b\\s?|",
                         "\\bde\\b\\s?|",
                         "\\bdescribe\\b\\s?|",
                         "\\bdetail\\b\\s?|",
                         "\\bdo\\b\\s?|",
                         "\\bdone\\b\\s?|",
                         "\\bdown\\b\\s?|",
                         "\\bdue\\b\\s?|",
                         "\\bduring\\b\\s?|",
                         "\\beach\\b\\s?|",
                         "\\beg\\b\\s?|",
                         "\\beight\\b\\s?|",
                         "\\beither\\b\\s?|",
                         "\\beleven\\b\\s?|",
                         "\\belse\\b\\s?|",
                         "\\belsewhere\\b\\s?|",
                         "\\bempty\\b\\s?|",
                         "\\benough\\b\\s?|",
                         "\\betc\\b\\s?|",
                         "\\beven\\b\\s?|",
                         "\\bever\\b\\s?|",
                         "\\bevery\\b\\s?|",
                         "\\beveryone\\b\\s?|",
                         "\\beverything\\b\\s?|",
                         "\\beverywhere\\b\\s?|",
                         "\\bexcept\\b\\s?|",
                         "\\bfew\\b\\s?|",
                         "\\bfifteen\\b\\s?|",
                         "\\bfify\\b\\s?|",
                         "\\bfill\\b\\s?|",
                         "\\bfind\\b\\s?|",
                         "\\bfire\\b\\s?|",
                         "\\bfirst\\b\\s?|",
                         "\\bfive\\b\\s?|",
                         "\\bfor\\b\\s?|",
                         "\\bformer\\b\\s?|",
                         "\\bformerly\\b\\s?|",
                         "\\bforty\\b\\s?|",
                         "\\bfound\\b\\s?|",
                         "\\bfour\\b\\s?|",
                         "\\bfrom\\b\\s?|",
                         "\\bfront\\b\\s?|",
                         "\\bfull\\b\\s?|",
                         "\\bfurther\\b\\s?|",
                         "\\bget\\b\\s?|",
                         "\\bgive\\b\\s?|",
                         "\\bgo\\b\\s?|",
                         "\\bhad\\b\\s?|",
                         "\\bhas\\b\\s?|",
                         "\\bhasnt\\b\\s?|",
                         "\\bhave\\b\\s?|",
                         "\\bhe\\b\\s?|",
                         "\\bhence\\b\\s?|",
                         "\\bher\\b\\s?|",
                         "\\bhere\\b\\s?|",
                         "\\bhereafter\\b\\s?|",
                         "\\bhereby\\b\\s?|",
                         "\\bherein\\b\\s?|",
                         "\\bhereupon\\b\\s?|",
                         "\\bhers\\b\\s?|",
                         "\\bherse\\b\\s?|",
                         "\\bhim\\b\\s?|",
                         "\\bhimse\\b\\s?|",
                         "\\bhis\\b\\s?|",
                         "\\bhow\\b\\s?|",
                         "\\bhowever\\b\\s?|",
                         "\\bhundred\\b\\s?|",
                         "\\bi\\b\\s?|",
                         "\\bie\\b\\s?|",
                         "\\bif\\b\\s?|",
                         "\\bin\\b\\s?|",
                         "\\binc\\b\\s?|",
                         "\\bindeed\\b\\s?|",
                         "\\binterest\\b\\s?|",
                         "\\binto\\b\\s?|",
                         "\\bis\\b\\s?|",
                         "\\bit\\b\\s?|",
                         "\\bits\\b\\s?|",
                         "\\bitse\\b\\s?|",
                         "\\bkeep\\b\\s?|",
                         "\\blast\\b\\s?|",
                         "\\blatter\\b\\s?|",
                         "\\blatterly\\b\\s?|",
                         "\\bleast\\b\\s?|",
                         "\\bless\\b\\s?|",
                         "\\bltd\\b\\s?|",
                         "\\bmade\\b\\s?|",
                         "\\bmany\\b\\s?|",
                         "\\bmay\\b\\s?|",
                         "\\bme\\b\\s?|",
                         "\\bmeanwhile\\b\\s?|",
                         "\\bmight\\b\\s?|",
                         "\\bmill\\b\\s?|",
                         "\\bmine\\b\\s?|",
                         "\\bmore\\b\\s?|",
                         "\\bmoreover\\b\\s?|",
                         "\\bmost\\b\\s?|",
                         "\\bmostly\\b\\s?|",
                         "\\bmove\\b\\s?|",
                         "\\bmuch\\b\\s?|",
                         "\\bmust\\b\\s?|",
                         "\\bmy\\b\\s?|",
                         "\\bmyse\\b\\s?|",
                         "\\bname\\b\\s?|",
                         "\\bnamely\\b\\s?|",
                         "\\bneither\\b\\s?|",
                         "\\bnever\\b\\s?|",
                         "\\bnevertheless\\b\\s?|",
                         "\\bnext\\b\\s?|",
                         "\\bnine\\b\\s?|",
                         "\\bno\\b\\s?|",
                         "\\bnobody\\b\\s?|",
                         "\\bnone\\b\\s?|",
                         "\\bnoone\\b\\s?|",
                         "\\bnor\\b\\s?|",
                         "\\bnot\\b\\s?|",
                         "\\bnothing\\b\\s?|",
                         "\\bnow\\b\\s?|",
                         "\\bnowhere\\b\\s?|",
                         "\\bof\\b\\s?|",
                         "\\boff\\b\\s?|",
                         "\\boften\\b\\s?|",
                         "\\bon\\b\\s?|",
                         "\\bonce\\b\\s?|",
                         "\\bone\\b\\s?|",
                         "\\bonly\\b\\s?|",
                         "\\bonto\\b\\s?|",
                         "\\bor\\b\\s?|",
                         "\\bother\\b\\s?|",
                         "\\bothers\\b\\s?|",
                         "\\botherwise\\b\\s?|",
                         "\\bour\\b\\s?|",
                         "\\bours\\b\\s?|",
                         "\\bourselves\\b\\s?|",
                         "\\bout\\b\\s?|",
                         "\\bover\\b\\s?|",
                         "\\bown\\b\\s?|",
                         "\\bpart\\b\\s?|",
                         "\\bper\\b\\s?|",
                         "\\bperhaps\\b\\s?|",
                         "\\bplease\\b\\s?|",
                         "\\bput\\b\\s?|",
                         "\\brather\\b\\s?|",
                         "\\bre\\b\\s?|",
                         "\\bsame\\b\\s?|",
                         "\\bsee\\b\\s?|",
                         "\\bseem\\b\\s?|",
                         "\\bseemed\\b\\s?|",
                         "\\bseeming\\b\\s?|",
                         "\\bseems\\b\\s?|",
                         "\\bserious\\b\\s?|",
                         "\\bseveral\\b\\s?|",
                         "\\bshe\\b\\s?|",
                         "\\bshould\\b\\s?|",
                         "\\bshow\\b\\s?|",
                         "\\bside\\b\\s?|",
                         "\\bsince\\b\\s?|",
                         "\\bsincere\\b\\s?|",
                         "\\bsix\\b\\s?|",
                         "\\bsixty\\b\\s?|",
                         "\\bso\\b\\s?|",
                         "\\bsome\\b\\s?|",
                         "\\bsomehow\\b\\s?|",
                         "\\bsomeone\\b\\s?|",
                         "\\bsomething\\b\\s?|",
                         "\\bsometime\\b\\s?|",
                         "\\bsometimes\\b\\s?|",
                         "\\bsomewhere\\b\\s?|",
                         "\\bstill\\b\\s?|",
                         "\\bsuch\\b\\s?|",
                         "\\bsystem\\b\\s?|",
                         "\\btake\\b\\s?|",
                         "\\bten\\b\\s?|",
                         "\\bthan\\b\\s?|",
                         "\\bthat\\b\\s?|",
                         "\\bthe\\b\\s?|",
                         "\\btheir\\b\\s?|",
                         "\\bthem\\b\\s?|",
                         "\\bthemselves\\b\\s?|",
                         "\\bthen\\b\\s?|",
                         "\\bthence\\b\\s?|",
                         "\\bthere\\b\\s?|",
                         "\\bthereafter\\b\\s?|",
                         "\\bthereby\\b\\s?|",
                         "\\btherefore\\b\\s?|",
                         "\\btherein\\b\\s?|",
                         "\\bthereupon\\b\\s?|",
                         "\\bthese\\b\\s?|",
                         "\\bthey\\b\\s?|",
                         "\\bthick\\b\\s?|",
                         "\\bthin\\b\\s?|",
                         "\\bthird\\b\\s?|",
                         "\\bthis\\b\\s?|",
                         "\\bthose\\b\\s?|",
                         "\\bthough\\b\\s?|",
                         "\\bthree\\b\\s?|",
                         "\\bthrough\\b\\s?|",
                         "\\bthroughout\\b\\s?|",
                         "\\bthru\\b\\s?|",
                         "\\bthus\\b\\s?|",
                         "\\bto\\b\\s?|",
                         "\\btogether\\b\\s?|",
                         "\\btoo\\b\\s?|",
                         "\\btop\\b\\s?|",
                         "\\btoward\\b\\s?|",
                         "\\btowards\\b\\s?|",
                         "\\btwelve\\b\\s?|",
                         "\\btwenty\\b\\s?|",
                         "\\btwo\\b\\s?|",
                         "\\bun\\b\\s?|",
                         "\\bunder\\b\\s?|",
                         "\\buntil\\b\\s?|",
                         "\\bup\\b\\s?|",
                         "\\bupon\\b\\s?|",
                         "\\bus\\b\\s?|",
                         "\\bvery\\b\\s?|",
                         "\\bvia\\b\\s?|",
                         "\\bwas\\b\\s?|",
                         "\\bwe\\b\\s?|",
                         "\\bwell\\b\\s?|",
                         "\\bwere\\b\\s?|",
                         "\\bwhat\\b\\s?|",
                         "\\bwhatever\\b\\s?|",
                         "\\bwhen\\b\\s?|",
                         "\\bwhence\\b\\s?|",
                         "\\bwhenever\\b\\s?|",
                         "\\bwhere\\b\\s?|",
                         "\\bwhereafter\\b\\s?|",
                         "\\bwhereas\\b\\s?|",
                         "\\bwhereby\\b\\s?|",
                         "\\bwherein\\b\\s?|",
                         "\\bwhereupon\\b\\s?|",
                         "\\bwherever\\b\\s?|",
                         "\\bwhether\\b\\s?|",
                         "\\bwhich\\b\\s?|",
                         "\\bwhile\\b\\s?|",
                         "\\bwhither\\b\\s?|",
                         "\\bwho\\b\\s?|",
                         "\\bwhoever\\b\\s?|",
                         "\\bwhole\\b\\s?|",
                         "\\bwhom\\b\\s?|",
                         "\\bwhose\\b\\s?|",
                         "\\bwhy\\b\\s?|",
                         "\\bwill\\b\\s?|",
                         "\\bwith\\b\\s?|",
                         "\\bwithin\\b\\s?|",
                         "\\bwithout\\b\\s?|",
                         "\\bwould\\b\\s?|",
                         "\\byet\\b\\s?|",
                         "\\byou\\b\\s?|",
                         "\\byour\\b\\s?|",
                         "\\byours\\b\\s?|",
                         "\\byourself\\b\\s?|",
                         "\\byourselves\\b\\s?",
                         sep="")

