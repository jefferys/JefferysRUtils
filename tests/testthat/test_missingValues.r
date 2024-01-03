describe( "fixNull()", {
   describe( "Default behavior with only required parrameter 'x", {
      it( "Returns x unchanged if not null", {
         x <- c(1,2)
         expect_equal( x, fixNull( x ))
      })
      it( "Returns NULL if x is NULL", {
         x <- NULL
         expect_equal( x, fixNull( x ))
      })
   })
   describe( "When only the parameter `val=` is specified", {
      replacementValue <- 42

      it( "returns the specified value if x is null", {
         x <- NULL
         expect_equal( replacementValue, fixNull( x, val= replacementValue ))
      })
      it( "returns x if x is not null", {
         x <- list()
         expect_equal( x, fixNull( x, val= replacementValue ))
      })
   })
   describe( "When only the parameter `func=` is specified, without parameters", {
      funcVal <- "Final Answer"
      myFunc <- function() { funcVal }
      describe( "As a bare function", {
         it( "returns the result of calling the function if x is null", {
            x <- NULL
            expect_equal( funcVal, fixNull( x, func= myFunc ))
         })
         it( "returns x if x is not null", {
            x <- ""
            expect_equal( x, fixNull( x, func= myFunc ))
         })
         it( "Does not call the function if x is not null", {
            x <- 42
            expect_silent(
               expect_equal( x, fixNull( x, func= stop )))
         })
      })
      describe( "As a string function name", {
         it( "returns the result of calling the function if x is null", {
            x <- NULL
            expect_equal( funcVal, fixNull( x, func= "myFunc" ))
         })
         it( "returns x if x is not null", {
            x <- ""
            expect_equal( x, fixNull( x, func= "myFunc" ))
         })
         it( "Does not call the function if x is not null", {
            x <- NaN
            expect_silent(
               expect_equal( x, fixNull( x, func= "stop" )))
         })
      })
      describe( "As a function definition", {
         it( "returns the result of calling the function if x is null", {
            x <- NULL
            expect_equal( funcVal, fixNull( x, func= function() { funcVal } ))
         })
         it( "returns x if x is not null", {
            x <- ""
            expect_equal( x, fixNull( x, func= function() { funcVal } ))
         })
         it( "Does not call the function if x is not null", {
            x <- NaN
            expect_silent(
               expect_equal( x, fixNull( x, func= function() { stop() } )))
         })
      })
   })
   describe( "When val= and func= are both specified, without parameters", {
      it( "Returns val if x is null, ignoring the returned value from the function", {
         x <- NULL
         replacementVal <- c(1,2,3)
         expect_equal( replacementVal,
                       fixNull( x, val=replacementVal, func= function(x) {111} ))
      })
      it( "Still calls the function when x is null", {
         x <- NULL
         replacementVal <- "No, ME!"
         expect_warning(
            expect_equal( replacementVal, fixNull( x, val=replacementVal, func= "warning" )))
      })
      it( "returns x if x is not null without calling function", {
         x <- ""
         replacementValue= c( "not", "me ")
         expect_silent(
            expect_equal( x, fixNull( x, val=replacementValue, func= stop )))

      })
      it( "Does not call the function if x is non-null", {
         x <- NA
         replacementVal <- "Not used!"
         expect_silent(
            expect_equal( x, fixNull( x, val= replacementVal, func= "stop" )))
      })
   })
   describe( "When extra parameters are provided", {
      it( "they are ignored if `func` not set, regardless of `x` or `val`", {
         replacementVal <- "don't care"

         x <- "not NULL"
         expect_equal( x, fixNull( x, extraParam= 42 ))
         expect_equal( x, fixNull( x, val= replacementVal, bob= 42 ))

         x= NULL
         expect_equal( NULL, fixNull( x, extraParam= 42 ))
         expect_equal( replacementVal, fixNull( x, val= replacementVal, bob= 42 ))
      })
      it( "it is an error if not used by the specified `func`, but only if x is null", {
         myFunc <- function() { "the answer" }
         replacementVal <- "try this"

         x= "not NULL"
         expect_equal( x, fixNull( x, func= "myFunc", notUsed="me" ))
         expect_equal( x, fixNull( x, func= myFunc, val= replacementVal, notUsed= "me" ))
         expect_equal( x, fixNull( x, val= replacementVal, func= function() { "the answer" }, notUsed= "me" ))
         x= NULL
         expect_error( fixNull( x, func= myFunc, notUsed="me" ))
         expect_error( fixNull( x, val= replacementVal, func= "myFunc", notUsed= "me" ))
         expect_error( fixNull( x, func= function() { "the answer" }, val= replacementVal, notUsed= "me" ))
      })
      it( "it passes them on to the called function, if `x` is non-null, regardless of `val`", {
         replacementVal= NA

         x= "not NULL"
         expect_equal( x, fixNull( x, func= "paste",
                                   "one", "two", sep= "-" ))
         expect_equal( x, fixNull( x, func= "paste", val= replacementVal,
                                   "one",  "two", sep= "-" ))

         x= NULL
         # Must explicitly state val=NULL if unnamed parameters!
         expect_equal( "one-two", fixNull( x, val= NULL, func= "paste",
                                           "one", "two", sep= "-" ))
         expect_equal( replacementVal, fixNull( x, func= "paste", val= replacementVal,
                                "one", "two", sep= "-" ))
         expect_warning(
            expect_equal( replacementVal,
                          fixNull( x, func= "warning", val= replacementVal,
                                   "one", "two" )),
            "onetwo"
         )

      })
   })
})
describe( "ifElseMore()", {
   describe( "Test value is TRUE", {
      it( "Returns true case without more values set", {
         expect_equal( "trueCase", ifElseMore( TRUE, "trueCase", "falseCase" ))
      })
      it( "Returns true case with more values set", {
         expect_equal( "trueCase", ifElseMore( TRUE, "trueCase", "falseCase", na="naCase", null="nullCase" ))
      })
   })
   describe( "Test value is FALSE", {
      it( "Returns false case without more values set", {
         expect_equal( "falseCase", ifElseMore( FALSE, "trueCase", "falseCase" ))
      })
      it( "Returns true case with more values set", {
         expect_equal( "falseCase", ifElseMore( FALSE, "trueCase", "falseCase", na="naCase", null="nullCase" ))
      })
   })
   describe( "Test value is NA", {
      it( "Returns na without more values set", {
         expect_true( is.na( ifElseMore( NA, "trueCase", "falseCase" )))
         expect_true( is.na( ifElseMore( NA_character_, "trueCase", "falseCase" )))
      })
      it( "Returns null case with more values set", {
         expect_equal( "naCase", ifElseMore( NA, "trueCase", "falseCase", na="naCase", null="nullCase" ))
         expect_equal( "naCase", ifElseMore( NA_character_, "trueCase", "falseCase", na="naCase", null="nullCase" ))
      })
   })
   describe( "Test value is NULL", {
      x <- NULL
      it( "Returns null without more values set", {
         expect_null( ifElseMore( x, "trueCase", "falseCase" ))
         expect_null( ifElseMore( NULL, "trueCase", "falseCase" ))
      })
      it( "Returns true case with more values set", {
         expect_equal( "nullCase", ifElseMore( x, "trueCase", "falseCase", na="naCase", null="nullCase" ))
         expect_equal( "nullCase", ifElseMore( NULL, "trueCase", "falseCase", na="naCase", null="nullCase" ))
      })
   })
})