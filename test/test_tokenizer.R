library(testthat)
source("../R/tokenizer.R")

context("Cleaning text")

describe("Converting to lower case", {

  it("Should convert to lower case", {
    sentence <- "This is Sparta"
    clean    <- "this is sparta"
    expect_equal(lower_case(sentence), clean)
  })
})

# Decided to leave numbers in to cat things like 2 live crue, 3 blind mice, 12:00 oclock, etc...
#describe("Removing numbers", {
#  it("Should remove numbers", {
#    sentence <- "3 blind mice went 2 school"
#    clean  <- "blind mice went school"
#    expect_equal(remove_numbers(sentence), clean)
#  })
#})

describe("Removing punctuation", {
  it("Should remove all punctuation", {
    sentence <- "With a comma, this is sparta!"
    clean    <- "With a comma this is sparta"
    expect_equal(remove_punctuation(sentence), clean)
  })
})

describe("Removing swear words", {

  it("Should remove fuck", {
    sentence <- "What the fuck is this"
    clean    <- "What the is this"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove fucking", {
    sentence <- "The fucking car won't start"
    clean    <- "The car won't start"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove shit", {
    sentence <- "This car is shit to me"
    clean    <- "This car is to me"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove shitty", {
    sentence <- "This car is shitty to me"
    clean    <- "This car is to me"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove pussy", {
    sentence <- "You're such a pussy he said"
    clean    <- "You're such a he said"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should not remove pussycat", {
    sentence <- "This is my pussycat"
    clean    <- "This is my pussycat"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove asshole", {
    sentence <- "You are an asshole he said"
    clean    <- "You are an he said"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove blowjob", {
    sentence <- "I want a blowjob right now"
    clean    <- "I want a right now" 
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove blow job", {
    sentence <- "I want a blow job right now"
    clean    <- "I want a right now" 
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should not remove blow", {
    sentence <- "the wind did blow"
    clean    <- "the wind did blow"    
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove ass", {
    sentence <- "An ass you are"
    clean    <- "An you are" 
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should not remove crass", {
    sentence <- "don't be crass"
    clean    <- "don't be crass"
    expect_equal(remove_profanity(sentence), clean)
  })

  it("Should remove cunt", {
    sentence <- "the word cunt is offensive"
    clean    <- "the word is offensive" 
    expect_equal(remove_profanity(sentence), clean)
  })
})

describe("Removing stop words", {
  it("should remove a", {
    sentence <- "a red zebra"
    clean    <- "red zebra"
    expect_equal(remove_stop_words(sentence), clean)
  })
  
  it("should not remove apple", {
    sentence <- "apple of my eye"
    clean    <- "apple eye"
    expect_equal(remove_stop_words(sentence), clean)
  })
})

describe("Stemming words", {
  it("should stem words", {
    sentence <- "search searched searching"
    clean    <- c("search search search")
    expect_equal(stem_words(sentence), clean)
  })
})

context("Non ascii characters")

describe("Removing non ascii characters", {
  it("Should return only non-ascii characters", {
    
    sentence <- "after pagan â€œgodsâ€ went"
    clean    <- "after pagan gods went"
    expect_equal(remove_non_ascii(sentence), clean)
  })
})

# context("Tokens from strings")
# 
# describe("Recieving tokens from lines of text", {
#   it("Should return three tokens from a three-word sentence", {
#     sentence <- "This is sparta"
#     tokens   <- list(c("This", "is", "sparta"))
#     expect_equal(tokenize(sentence), tokens)
#   })
# 
#   it("Should get clean tokens", {
#     # lower case, no profanity, no punctuation
#     sentence <- "This, 2, fucking is Sparta!!"
#     tokens <- list(c("2", "sparta"))
#     expect_equal(get_tokens(sentence), tokens)
#   })
# 
#   it("Should get tokens without stop words", {
#     sentence <- "This is sparta and it is green and a cow"
#     tokens   <- list(c("sparta", "green", "cow"))
#   })
#   
#   it("Should stem words", {
#     sentence <- "search searched searching"
#     clean    <- c("search", "searched", "searching")
#     expect_equal(get_tokens(sentence), clean)
#   })
# })
  
