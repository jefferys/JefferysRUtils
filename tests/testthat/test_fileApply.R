context("Testing the file*Apply family of functions.")

describe( "fileBlockApply()", {

   # textFile and textFileContent
   textFile <- tempfile( "textFile", fileext= "txt" )
   textFileContent <- c( "One line", "Two lines.", "", "Four" )
   writeLines( textFileContent, textFile )

   # integerFile and integerFileContent
   integerFile <- tempfile( "integerFile", fileext= "txt" )
   integerFileContent <- as.character( 1:10 )
   writeLines( integerFileContent, integerFile )

#   dimensions:
#      chunkSize: default/(just)bigger than file/same size as file/even fit/uneven fit/errors
#      FUN: named / explicit
#      ...(extra FUN parameters) none/some/some, out-of-order
#      grow model: default/errors/(1/2 growStart) * (1/1.5/2 growX) * (0/1 growAdd)
#      FUN type: select/transform (1 to 1)/transform(1 to list)
#      unlist: default/TRUE/FALSE
#      con: fileName/fileConnection/zipFile/errors
#      Not testing .* passthrough to readLines

   describe( "Setup for testing", {
      it( "created a text file with the expected content", {
         expect_true( file.exists( textFile ))
         expect_equal( readLines( textFile ), textFileContent )
      })
      it( "created a text file with the expected content", {
         expect_true( file.exists( integerFile ))
         expect_equal( readLines( integerFile ), integerFileContent )
      })
   })

   describe( "Smoke test with simple defaults", {
      it ( "Returns the file contents when applying the 'c' function", {
         got <- fileBlockApply( textFile, "c" )
         want <- textFileContent
         expect_equal( got, want )
      })
   })

   describe( "Examples work as expected", {
      it( "Transformation function 'nchar' works when applied", {
         got <- fileBlockApply( textFile, "nchar" )
         want <- nchar(textFileContent)
         expect_equal(got, want)
      })
      it( "Select function 'intersect' works when applied", {
         lookingFor <- c( "BOB", textFileContent[3], textFileContent[1] )
         got <- fileBlockApply( textFile, "intersect", lookingFor )
         want <- c( textFileContent[1], textFileContent[3] )
         expect_equal(got, want)
      })
      it( "Functions can have extra parameters", {
         got <- fileBlockApply( textFile, "grep", pattern="line", value=TRUE )
         want <- grep("line", textFileContent, value=TRUE)
         expect_equal(got, want)
      })
      it( "Block structure is preserved with unlist=FALSE", {
         got <- fileBlockApply( textFile, "nchar", chunkSize=2, unlist=FALSE )
         want <- list(nchar(textFileContent[1:2]), nchar(textFileContent[3:4]))
         expect_equal(got, want)
      })
      it( "Functions can be in-line definitions", {
         got <- fileBlockApply( integerFile, function (x) {
               y <- as.numeric(x)
               y[y > 5]
            }, chunkSize= 2
         )
         y <- as.numeric(integerFileContent)
         want <- y[y > 5]
         expect_equal(got, want)
      })
      it( "Block structure works with uneven chunks; may return empty elements", {
         got <- fileBlockApply( integerFile, function (x) {
            y <- as.numeric(x)
            y[y > 5]
         }, chunkSize= 4, unlist=FALSE
         )
         y <- as.numeric(integerFileContent)
         want <- list( (y[1:4])[(y[1:4]) > 5],
                       (y[5:8])[(y[5:8]) > 5],
                       (y[9:10])[(y[9:10]) > 5]
         )
         expect_equal(got, want)
      })
      it( "Uneven chunks and growing a too-small initial list", {
         got <- fileBlockApply( integerFile, function (x) {
            y <- as.numeric(x)
            y[y > 5]
         }, chunkSize= 4, growStart= 1, growX= 1, growAdd= 1, unlist=FALSE
         )
         y <- as.numeric(integerFileContent)
         want <- list( (y[1:4])[(y[1:4]) > 5],
                       (y[5:8])[(y[5:8]) > 5],
                       (y[9:10])[(y[9:10]) > 5]
         )
         expect_equal(got, want)
      })
      it( "Manually combining blocks", {
         got <- sum( fileBlockApply( integerFile, "length", chunkSize= 4))
         want <- length(integerFileContent)
         expect_equal(got, want)

         got <- sum( fileBlockApply( integerFile, function (x) sum(as.integer(x)),
                                     chunkSize= 4))
         want <- sum( as.integer( integerFileContent ))
         expect_equal(got, want)
      })
      it( "Can use connections", {
         con <- textConnection(integerFileContent)
         got <- sum( fileBlockApply( con, "length", chunkSize= 4))
         want <- length(integerFileContent)
         expect_equal(got, want)
      })
   })
   describe( "Filtering works", {
      it( "Works with logical returning function", {
         got <- fileBlockApply( integerFile, function (x) { as.numeric(x) > 5 },
                                chunkSize= 2, filter=TRUE
         )
         # Original should be character still
         want <- integerFileContent[as.numeric(integerFileContent) > 5]
         expect_equal(got, want)
      })
      it( "Works with index returning functions", {
         # Returns last line of each block of 3, including last line of
         # left-over block of 1
         got <- fileBlockApply( integerFile, function (x) { length(x) },
                                chunkSize= 3, filter=TRUE)
         want <- integerFileContent[c(3,6,9,10)] # These are strings!
         expect_equal(got, want)
      })
   })
   describe( "Throws errors when expected", {
      describe( "parameter errors", {
         wantErrorRE <- "chunkSize may not be 0\\."
         expect_error( fileBlockApply( chunkSize= 0 ), wantErrorRE)

         wantErrorRE <- "growStart must be >= 1\\."
         expect_error( fileBlockApply( growStart= 0), wantErrorRE)

         wantErrorRE <- "growX must be >= 1\\."
         expect_error( fileBlockApply( growX= 0 ), wantErrorRE)

         wantErrorRE <- "growAdd must be >= 0\\."
         expect_error( fileBlockApply( growAdd= -1 ), wantErrorRE)

         wantErrorRE <- "must have growX > 1 or growAdd > 0\\."
         expect_error( fileBlockApply( growX= 1, growAdd= 0 ), wantErrorRE)
      })
   })
})

describe( "fileLineApply()", {

   # textFile and textFileContent
   textFile <- tempfile( "textFile", fileext= "txt" )
   textFileContent <- c( "One line", "Two lines.", "", "Four" )
   writeLines( textFileContent, textFile )

   # integerFile and integerFileContent
   integerFile <- tempfile( "integerFile", fileext= "txt" )
   integerFileContent <- as.character( 1:10 )
   writeLines( integerFileContent, integerFile )

   #   dimensions:
   #      chunkSize: default/(just)bigger than file/same size as file/even fit/uneven fit/errors
   #      FUN: named / explicit
   #      ...(extra FUN parameters) none/some/some, out-of-order
   #      grow model: default/errors/(1/2 growStart) * (1/1.5/2 growX) * (0/1 growAdd)
   #      FUN type: select/transform (1 to 1)/transform(1 to list)
   #      unlist: default/TRUE/FALSE
   #      con: fileName/fileConnection/zipFile/errors
   #      Not testing .* passthrough to readLines

   describe( "Setup for testing", {
      it( "created a text file with the expected content", {
         expect_true( file.exists( textFile ))
         expect_equal( readLines( textFile ), textFileContent )
      })
      it( "created a text file with the expected content", {
         expect_true( file.exists( integerFile ))
         expect_equal( readLines( integerFile ), integerFileContent )
      })
   })

   describe( "Smoke test with simple defaults", {
      it ( "Returns the file contents when applying the 'c' function", {
         got <- fileLineApply( textFile, "c" )
         want <- textFileContent
         expect_equal( got, want )
      })
   })

   describe( "Examples work as expected", {
      it( "Transformation function 'toupper' works when applied", {
         got <- fileLineApply( textFile, "toupper" )
         want <- toupper(textFileContent)
         expect_equal(got, want)
      })
      it( "Select function 'intersect' works when applied", {
         lookingFor <- c( "BOB", textFileContent[3], textFileContent[1] )
         got <- fileLineApply( textFile, "intersect", lookingFor )
         want <- list(textFileContent[1], character(0), textFileContent[3], character(0) )
         expect_equal(got, want)
      })
      it( "Select function 'intersect' works when applied without simplification", {
         lookingFor <- c( "BOB", textFileContent[3], textFileContent[1] )
         got <- fileLineApply( textFile, "intersect", lookingFor, .simplify= FALSE )
         want <- list(textFileContent[1], character(0), textFileContent[3], character(0) )
         expect_equal(got, want)
      })
      it( "Need to unset simplify if want to keep structure/attributes", {
         got <- fileLineApply( textFile, "structure", class="foo", .simplify=FALSE)
         want <- as.list(rep( "foo", 4 ))
         expect_equal( lapply(got, "class"), want)

         got <- fileLineApply( textFile, "structure", class="foo")
         want <- as.list(rep( "character", 4 ))
         expect_equal( lapply(got, "class"), want)
      })
      it( "Block structure is preserved with unlist=FALSE", {
         got <- fileLineApply( textFile, "nchar", chunkSize=2, unlist=FALSE )
         want <- list(nchar(textFileContent[1:2]), nchar(textFileContent[3:4]))
         expect_equal(got, want)
      })
      it( "Uneven chunks and growing a too-small initial list", {
         got <- fileLineApply( textFile, "nchar", chunkSize=2,
                               growStart= 1, growX= 1, growAdd= 1,unlist=FALSE )
         want <- list(nchar(textFileContent[1:2]), nchar(textFileContent[3:4]))
         expect_equal(got, want)
      })

      it( "Block structure is preserved with elements as list if unlist= FALSE and .simplify= FALSE ", {
         got <- fileLineApply( textFile, "nchar", chunkSize=2, unlist=FALSE, .simplify=FALSE )
         want <- list( as.list( nchar( textFileContent[1:2] )), as.list( nchar( textFileContent[3:4] )))
         expect_equal(got, want)
      })
      it( "Can use connections", {
         con <- textConnection(integerFileContent)
         got <- fileLineApply( con, "length", chunkSize= 4)
         want <- sapply( integerFileContent, "length", USE.NAMES=FALSE )
         expect_equal(got, want)
         close(con)

         con <- textConnection(integerFileContent)
         got <- fileLineApply( con, "length", chunkSize= 4, .simplify=FALSE)
         want <- lapply( integerFileContent, "length" )
         expect_equal(got, want)
         close(con)
      })
      describe( "Filtering works", {
         it( "Works with logical returning function", {
            got <- fileLineApply( textFile, function (x) {
                  lengths(strsplit(x, "\\s+")) == 1
               }, chunkSize= 2, filter=TRUE
            )
            want <- textFileContent[lengths(strsplit(textFileContent, "\\s+")) == 1]
            expect_equal(got, want)
         })
      })
   })
   describe( "Throws errors when expected", {
      describe( "parameter errors", {
         wantErrorRE <- "chunkSize may not be 0\\."
         expect_error( fileLineApply( chunkSize= 0 ), wantErrorRE)

         wantErrorRE <- "growStart must be >= 1\\."
         expect_error( fileLineApply( growStart= 0), wantErrorRE)

         wantErrorRE <- "growX must be >= 1\\."
         expect_error( fileLineApply( growX= 0 ), wantErrorRE)

         wantErrorRE <- "growAdd must be >= 0\\."
         expect_error( fileLineApply( growAdd= -1 ), wantErrorRE)

         wantErrorRE <- "must have growX > 1 or growAdd > 0\\."
         expect_error( fileLineApply( growX= 1, growAdd= 0 ), wantErrorRE)
      })
   })
})
