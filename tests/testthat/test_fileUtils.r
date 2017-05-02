context( "Testing file utils" )

describe( "makeTempFile", {
   describe("Minimal behavior", {
      it( "Smoke tests", {
         expect_silent( makeTempFile() )
      })
      it( "Creates a file and returns its name", {
         file <- makeTempFile()
         expect_true( file.exists( file ))
      })
   })
   describe( "Creates file is empty by default", {
      file <- makeTempFile()
      it( "Returns an empty file", {
         got <- file.size(file)
         want <- 0
         expect_equal( got, want )

         got <- readLines(file)
         want <- character(0)
         expect_equal( got, want )
      })
   })
   describe("lines= parameter", {
      describe( "Normal value handling of specified character vectors", {
         it( "Can recover the content", {
            content <- c( "Line 1", "This is line 2", "3" )
            file <- makeTempFile( content )

            got <- readLines(file)
            want <- content
            expect_equal( got, want )
         })
      })
      describe( "Fallback default behavior", {
         file <- makeTempFile()
         it( "The empty file has NO content", {
            got <- file.size(file)
            want <- 0
            expect_equal( got, want )
         })
         it( "Reading empty files back recapitulates the empty input", {
            # Note - this might be a "testing the system test"
            got <- readLines(file)
            want <- character(0)
            expect_equal( got, want )
         })
      })
      describe( "Corner cases", {
         it( "Can write empty lines", {
            content <- c( "", "", "" )
            file <- makeTempFile( content )

            got <- readLines(file)
            want <- content
            expect_equal( got, want )
         })
         it( "Can write just one line", {
            content <- "Make me a file please !"
            file <- makeTempFile( lines= content )

            got <- readLines(file)
            want <- content
            expect_equal( got, want )
         })
         it( "Can write one empty line", {
            file <- makeTempFile( lines= "" )

            got <- readLines(file)
            want <- ""
            expect_equal( got, want )
         })
         it( "Can write single space", {
            file <- makeTempFile( ' ' )

            got <- readLines(file)
            want <- " "
            expect_equal( got, want )
         })
         it( "Can create empty files", {
            file <- makeTempFile( character(0) )

            got <- readLines(file)
            want <- character(0)
            expect_equal( got, want )
         })
      })
   })
   describe("eol= parameter", {
      describe( "Normal default behavior", {
         it( "Uses the platform default as a line ending", {
            # Can't just read characters, as readLines auto-converts line endings
            content <- c( "L1", "L2" )
            file <- makeTempFile( content )

            got <- scan( file, character(), sep="\n", quiet= TRUE )
            expect_equal( got, content )
         })
      })
      describe( "Non-platform EOL", {
         it( "eol= allows for other platform line endings.", {
            content <- c( "Line 1", "This is line 2", "3" )
            file <- makeTempFile( content, eol= '\r' )

            # Any of CR, LF, or CRLF are used as line ending automatically.
            got <- readLines(file)
            want <- content
            expect_equal( got, want )
         })
         it( "eol= allows for non-standard line ending characters", {
            content <- c( "Line 1", "This is line 2", "3" )
            file <- makeTempFile( content, eol= ' ' )

            got <- scan( file, character(), sep=" ", quiet= TRUE )
            # No EOL terminating file, so scan reads an extra empty string at end.
            want <- c( "Line", "1", "This", "is", "line", "2", "3", "" )
            expect_equal( got, want )
         })
         it( "eol= allows for non-standard multi-character line endings", {
            file <- makeTempFile( c("one fish", "two fish"), eol=" <>< " )
            got <- readLines( file, warn= FALSE )
            want <- "one fish <>< two fish <>< "
            expect_equal( got, want)
         })
      })
   })
   describe("namePrefix=, tempDir=, and extension= parameters", {
      content <- c("one fish", "two fish")

      describe( "Normal default behavior", {
         it( "Defaults to file name of temp*.txt in tempdir()", {
            gotFileName <- makeTempFile( content )
            wantRE <- file.path(tempdir(), 'temp.+.txt$' )
            expect_match( gotFileName, wantRE )
         })
         it("generates different file names on each call", {
            fileName1 <- makeTempFile( content )
            fileName2 <- makeTempFile( content )
            expect_false( fileName1 == fileName2 )
         })
      })
      describe( "Changing the temp file name's conserved parts", {
         dir <- tempdir()
         nestedDir <- file.path( tempdir(), "innerDir" )
         dir.create(nestedDir)

         it( "can set different dir, prefix, and extension file name parts", {
            gotFileName <- makeTempFile( content, "deleteMe", ".text", tempDir <- nestedDir )
            wantRE <- file.path( nestedDir, 'deleteMe.+.text$' )
            expect_match( gotFileName, wantRE )
         })
         it( "can set dir, prefix, and extension file name parts, and still get unique file", {
            gotFileName1 <- makeTempFile( content, "deleteMe", ".text", tempDir <- nestedDir )
            gotFileName2 <- makeTempFile( content, "deleteMe", ".text", tempDir <- nestedDir )
            expect_false( gotFileName1 == gotFileName2 )
         })

      })
      describe( "Errors", {
         it( "Is an error if the tempDir does not exist.", {
            wantRE <- "tempDir= directory does not exist: 'Bob'"
            expect_error( makeTempFile( content, tempDir= 'Bob' ), wantRE )
         })
      })
   })
})

describe( "slurp()", {
   normalArray <- c("# File Description", "Data Line",
                    "# Commented out",    "",          "Empty Line Prior and after", "")
   normalFile <- tempfile( "slurpTest.normal" )
   writeLines( normalArray, normalFile )

   emptyArray <- character(0)
   emptyFile <- tempfile( "slurpTest.empty" )
   file.create( emptyFile )

   noTextArray <- c("")
   noTextFile <- tempfile( "slurpTest.noText")
   writeLines( noTextArray, noTextFile )

   onlyCommentArray <- c("   ## Comment", "# maybe comment", "## BOB", "   ##")
   onlyCommentFile <- tempfile( "slurpTest.onlyComment")
   writeLines( onlyCommentArray, onlyCommentFile )

   describe( "default behavior", {
      it( "reads files as expected", {
         expect_equal( slurp( normalFile      ), normalArray      )
         expect_equal( slurp( emptyFile       ), emptyArray       )
         expect_equal( slurp( noTextFile      ), noTextArray      )
         expect_equal( slurp( onlyCommentFile ), onlyCommentArray )
      })
   })
   describe( "commentString= option", {
      it( "reads files as expected", {
         got <- slurp( normalFile, commentString = '#' )
         want <- normalArray[c(-1, -3)]
         expect_equal( got, want )
         expect_equal( slurp( normalFile, commentString = '##'), normalArray )
         expect_equal( slurp( emptyFile, commentString = '#' ), emptyArray )
         expect_equal( slurp( emptyFile, commentString = '##' ), emptyArray )
         expect_equal( slurp( noTextFile, commentString = '#' ), noTextArray )
         expect_equal( slurp( noTextFile, commentString = '##' ), noTextArray )
         expect_equal( slurp( onlyCommentFile, commentString = '#' ), emptyArray )
         got <- slurp( onlyCommentFile, commentString = '##' )
         want <- onlyCommentArray[c(-1, -3, -4)]
         expect_equal( got, want )
      })
      it( "handles empty or NA commentStrings", {
         expect_equal( slurp( normalFile, commentString = ""), normalArray )
         expect_equal( slurp( normalFile, commentString = NA), normalArray )
      })
   })
   describe( "skipLines= option", {
      it( "skips n lines when told too", {
         expect_equal( slurp( normalFile, skipLines = 0), normalArray[1:6])
         expect_equal( slurp( normalFile, skipLines = 1), normalArray[2:6])
         expect_equal( slurp( normalFile, skipLines = 2), normalArray[3:6])
         expect_equal( slurp( normalFile, skipLines = 3), normalArray[4:6])
         expect_equal( slurp( normalFile, skipLines = 4), normalArray[5:6])
         expect_equal( slurp( normalFile, skipLines = 5), normalArray[6:6])
      })
      it( "handles skipping all lines", {
         expect_equal( slurp( normalFile, skipLines = 6), character(0))
      })
      it( "handles deleting more lines than kept", {
         wantWarningRE <- "Skipping more lines, 10, than lines to skip, 6."
         expect_warning( got <- slurp( normalFile, skipLines = 10), wantWarningRE )

         expect_equal( got, character(0))
      })
   })
   describe( "interaction between skipLines= and commentString=", {
      it( "removes comments before deleting lines", {
         got <- slurp( normalFile, commentString = '#', skipLines = 1 )
         want <- normalArray[c(-1, -2, -3)]
         expect_equal( got, want )
         got <- slurp( normalFile, commentString = '#', skipLines = 2 )
         want <- normalArray[c(-1, -2, -3, -4)]
         expect_equal( got, want )
         got <- slurp( normalFile, commentString = '#', skipLines = 3 )
         want <- normalArray[c(-1, -2, -3, -4, -5)]
         expect_equal( got, want )
      })
      it( "handles skipping all lines", {
         got <- slurp( normalFile, commentString = '#', skipLines = 4 )
         want <- character(0)
         expect_equal( got, want )
      })
      it( "handles deleting more lines than kept", {
         wantWarningRE <- "Skipping more lines, 5, than lines to skip, 4."
         expect_warning( got <- slurp( normalFile, commentString = '#', skipLines = 5 ), wantWarningRE)
         want <- character(0)
         expect_equal( got, want )
      })
   })

   unlink( c(normalFile, emptyFile, noTextFile, onlyCommentFile) )
})

describe( "mergeFiles()", {
   describe( "Files without headers", {
      contentA <- c( "One fish,", "two fish," )
      contentB <- c( "red fish,", "blue fish." )
      inFileA <- makeTempFile( contentA, "inFileA" )
      inFileB <- makeTempFile( contentB, "inFileB" )
      outFile <- tempfile( pattern= "mergedOut", fileext= ".txt" )
      emptyFileA <- makeTempFile()
      emptyFileB <- makeTempFile()
      blankFileA <- makeTempFile( c("",""))
      blankFileB <- makeTempFile( c(""))

      describe("Minimal behavior", {
         it( "Smoke tests", {
            fileList <- c(inFileA, inFileB)

            expect_silent( got <- mergeFiles( fileList ))

            expect_true( file.exists( got ))
         })
      })
      describe( "inFiles= (mandatory parameter)", {
         describe( "Default merging of files (without headers)", {
            describe( "Normal text file merging", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList )

               it( "Cat files to a temp file, prefixing <filename> and tab.", {
                  got <- readLines(outFileName)
                  want <- c( paste( inFileA, contentA, sep="\t" ),
                             paste( inFileB, contentB, sep="\t" ))
                  expect_equal(got, want)
               })
               it( "Default filename has expected name", {
                  got <- outFileName
                  wantRE <- file.path( tempdir(), 'merged.+\\.tmp$' )
                  expect_match( got, wantRE )
               })
            })
            describe( "weird text file merging",{
               it( "Creates an empty file if no files to merge", {
                  fileName <- mergeFiles( character(0) )

                  expect_true( file.exists( fileName ))

                  got <- readLines(fileName)
                  want <- character(0)
                  expect_equal(got, want)
               })
               it( "Can merge just 'one' input file", {
                  outFileName <- mergeFiles( inFileA )

                  got <- readLines(outFileName)
                  want <- paste( inFileA, contentA, sep="\t" )
                  expect_equal(got, want)
               })
               it( "Does not record any line for empty files by default", {
                  outFileName <- mergeFiles( c(emptyFileA, emptyFileB) )
                  got <- readLines(outFileName)
                  want <- character(0)
                  expect_equal(got, want)
               })
               it( "Keeps empty lines even when file is all empty lines", {
                  outFileName <- mergeFiles( c(blankFileA, blankFileB) )
                  got <- readLines(outFileName)
                  want <- paste0( c(blankFileA, blankFileA, blankFileB), sep= "\t")
                  expect_equal(got, want)

                  outFileName <- mergeFiles( c(blankFileB) )
                  got <- readLines(outFileName)
                  want <- paste0( c(blankFileB), sep= "\t")
                  expect_equal(got, want)
               })
               it( "Handles multiple kinds of weird input files", {
                  outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA) )
                  got <- readLines(outFileName)
                  want <- c( paste0( c(blankFileA, blankFileA, blankFileB), sep= "\t"),
                             paste( inFileA, contentA, sep="\t" ))
                  expect_equal(got, want)
               })
            })
         })
      })
      describe( "outFile= optional output file name parameter", {
         it( "Overrides default output file name when used", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile )

            expect_equal( outFileName, outFile )
         })
         it( "Outputs expected content to the named file", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile= outFile )

            got <- readLines(outFile)
            want <- c( paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
      })
      describe( "keepEmpty= boolean option for reporting blank files", {
         it( "Adds a line for empty files when set true.", {
            fileList <- c(emptyFileA, emptyFileB)
            mergeFiles( fileList, outFile, keepEmpty= TRUE )

            got <- readLines( outFile )
            want <- paste0( c(emptyFileA, emptyFileB), sep= "\t" )
         })
      })
      describe( "delim= text option for filename column delimiter", {
         it(  "Uses delim to separate the prefixed filename column", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, delim=' ' )

            got <- readLines(outFileName)
            want <- c( paste( inFileA, contentA, sep=" " ),
                       paste( inFileB, contentB, sep=" " ))
            expect_equal(got, want)
         })
         it(  "+ output=. Works as expected if also specify output= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFile <- tempfile()
            outFileName <- mergeFiles( fileList, outFile= outFile, delim=':' )

            got <- readLines(outFile)
            want <- c( paste( inFileA, contentA, sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
         it(  "+ keepEmpty=. Works as expected if also specify keepEmpty= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFileName <- mergeFiles( fileList, keepEmpty= TRUE, delim=':' )

            got <- readLines(outFileName)
            want <- c( paste( inFileA, contentA, sep=":" ),
                       paste( emptyFileA, "", sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
      })
   })
   describe( "Files with headers (header= is set to non-null)", {
      header <- "DESC\tTHING"
      contentA <- c("One\tfish,", "two\tfish,")
      contentB <- c("red\tfish,", "blue\tfish,")
      inFileA <- makeTempFile( c( header, contentA ))
      inFileB <- makeTempFile( c( header, contentB ))
      outFile <- tempfile( pattern= "mergedOutWithHeader", fileext= ".txt" )
      emptyFileA <- makeTempFile(header)
      emptyFileB <- makeTempFile(header)
      blankFileA <- makeTempFile( c(header, "", ""))
      blankFileB <- makeTempFile( c(header, ""))

      describe("Minimal behavior", {
         it( "Smoke tests", {
            fileList <- c(inFileA, inFileB)

            expect_silent( got <- mergeFiles( fileList ))

            expect_true( file.exists( got ))
         })
      })
      describe( "inFiles= (mandatory parameter)", {
         describe( "Merging of normal files with header)", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, headerLines=1 )

            it( "Cat files to a temp file, prefixing <filename> and tab.", {
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ),
                          paste( inFileB, contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Default filename has expected name", {
               got <- outFileName
               wantRE <- file.path( tempdir(), 'merged.+\\.tmp$' )
               expect_match( got, wantRE )
            })
         })

      })
   })
})