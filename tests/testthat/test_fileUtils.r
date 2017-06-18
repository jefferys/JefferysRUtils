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
   describe( "REQ inFiles= Files without headers (headerLines= 0)", {
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
      describe( "Default merging of files (without headers)", {
         describe( "Normal no-header text file merging", {
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
         describe( "Weird no-header text file merging",{
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
            it( "+ keepEmpty, Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          keepEmpty= TRUE )
               got <- readLines(outFileName)
               want <- c( paste0( c(blankFileA, blankFileA, emptyFileA, blankFileB), sep= "\t"),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe("Errors", {
            it( "Generates an error if no files to merge", {
               wantRE <- 'Must specify at least one input file.'
               expect_error( mergeFiles( character(0) ), wantRE )
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
            expect_equal(got, want)
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
      describe( "names= replacement for source file names", {
         describe( "Normal names", {
            it( "changes nothing to use explicit default names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= fileList )

               got <- readLines(outFileName)
               want <- c( paste( inFileA, contentA, sep="\t" ),
                          paste( inFileB, contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are simple alphabetic names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Amy", "Bob") )

               got <- readLines(outFileName)
               want <- c( paste( "Amy", contentA, sep="\t" ),
                          paste( "Bob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "Weird names", {
            it( "assigns names if they contain spaces", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("A m y", "B o b") )

               got <- readLines(outFileName)
               want <- c( paste( "A m y", contentA, sep="\t" ),
                          paste( "B o b", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are numbers", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c(-123, 42) )

               got <- readLines(outFileName)
               want <- c( paste( -123, contentA, sep="\t" ),
                          paste( 42, contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they contain unicode", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Åmy", "ßob") )

               got <- readLines(outFileName)
               want <- c( paste( "Åmy", contentA, sep="\t" ),
                          paste( "ßob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are empty strings or contain delimiter (bad idea...)", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("", "\t") )

               got <- readLines(outFileName)
               want <- c( paste( "", contentA, sep="\t" ),
                          paste( "\t", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "weird text file merging (only normal names...",{
            it( "Can merge just 'one' input file", {
               outFileName <- mergeFiles( inFileA, names="A" )

               got <- readLines(outFileName)
               want <- paste( "A", contentA, sep="\t" )
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), names= c("EA", "EB") )
               got <- readLines(outFileName)
               want <- character(0)
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               outFileName <- mergeFiles( c(blankFileA, blankFileB), names= c("BA", "BB") )
               got <- readLines(outFileName)
               want <- paste0( c("BA", "BA", "BB"), sep= "\t")
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), names= "BB" )
               got <- readLines(outFileName)
               want <- paste0( "BB", sep= "\t")
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy") )
               got <- readLines(outFileName)
               want <- c( paste0( c("BA", "BA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "+keepEmpty, when handling multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy"), keepEmpty= TRUE )
               got <- readLines(outFileName)
               want <- c( paste0( c("BA", "BA", "EA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ keepEmpty=", {
            it( "Lines for empty files have correct names, when set true.", {
               fileList <- c(emptyFileA, emptyFileB)
               mergeFiles( fileList, outFile, keepEmpty= TRUE, names= c("Amy", "Bob") )

               got <- readLines( outFile )
               want <- paste0( c("Amy", "Bob"), sep= "\t" )
               expect_equal(got, want)
            })
         })
         describe( "Errors", {
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, names="bob" ), wantRE)
            })
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, names=c("bob","bob","bob")), wantRE)
            })
         })
      })
      describe( "headerLines= 0 (explicit default)", {
         fileList <- c(inFileA, inFileB)
         outFileName <- mergeFiles( fileList, headerLines= 0 )

         it( "Changes nothing to explicitly specify header lines are 0", {
            got <- readLines(outFileName)
            want <- c( paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
      })
      describe( "colName= 'FILE'", {
         fileList <- c(inFileA, inFileB)

         it( "Changes nothing to explicitly specify colName as default value", {
            outFileName <- mergeFiles( fileList, colName= 'FILE' )

            got <- readLines(outFileName)
            want <- c( paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
         it( "Indeed, changes nothing to change colName", {
            outFileName <- mergeFiles( fileList, colName= 'some other column name!' )

            got <- readLines(outFileName)
            want <- c( paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
      })


   })
   describe( "REQ inFiles= Files with one line headers (headerLines = 1)", {
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

            expect_silent( got <- mergeFiles( fileList, headerLines=1L  ))

            expect_true( file.exists( got ))
         })
      })
      describe( "Default merging of files (with headers)", {
         describe( "Normal with-header text files merging)", {
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
         describe( "Weird with-header text file merging", {
            it( "Can merge just 'one' input file", {
               outFileName <- mergeFiles( inFileA, headerLines=1L  )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), headerLines=1L  )
               got <- readLines(outFileName)
               want <- paste( "FILE", header, sep="\t")
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               outFileName <- mergeFiles( c(blankFileA, blankFileA, blankFileB), headerLines=1L  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), headerLines=1L  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines=1L  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
            expect_equal(got, want)
            })
            it( "+ keepEmpty, Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines=1L, keepEmpty=TRUE  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( emptyFileA, "", sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe("Errors", {
            it( "Generates an error if no files to merge", {
               wantRE <- 'Must specify at least one input file.'
               expect_error( mergeFiles( character(0), headerLines=1 ), wantRE )
            })
         })
      })
      describe( "outFile= optional output file name parameter", {
         it( "Overrides default output file name when used", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile, headerLines= 1 )

            expect_equal( outFileName, outFile )
         })
         it( "Outputs expected content to the named file", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile= outFile, headerLines= 1 )

            got <- readLines(outFile)
            want <- c( paste(  "FILE",   header, sep="\t" ),
                       paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
      })
      describe( "keepEmpty= boolean option for reporting blank files", {
         it( "Adds a line for empty files when set true.", {
            fileList <- c(emptyFileA, emptyFileB)
            mergeFiles( fileList, outFile, keepEmpty= TRUE, headerLines= 1 )

            got <- readLines( outFile )
            want <- c( paste(  "FILE",   header, sep="\t" ),
                       paste0( c(emptyFileA, emptyFileB), sep= "\t" ))
            expect_equal(got, want)
         })
      })
      describe( "delim= text option for filename column delimiter", {
         it(  "Uses delim to separate the prefixed filename column", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, delim=' ', headerLines= 1 )

            got <- readLines(outFileName)
            want <- c( paste(  "FILE",   header, sep=" " ),
                       paste( inFileA, contentA, sep=" " ),
                       paste( inFileB, contentB, sep=" " ))
            expect_equal(got, want)
         })
         it(  "+ output=. Works as expected if also specify output= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFile <- tempfile()
            outFileName <- mergeFiles( fileList, outFile= outFile, delim=':', headerLines= 1 )

            got <- readLines(outFile)
            want <- c( paste(  "FILE",   header, sep=":" ),
                       paste( inFileA, contentA, sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
         it(  "+ keepEmpty=. Works as expected if also specify keepEmpty= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFileName <- mergeFiles( fileList, keepEmpty= TRUE, delim=':', headerLines= 1 )

            got <- readLines(outFileName)
            want <- c( paste(  "FILE",   header, sep=":" ),
                       paste( inFileA, contentA, sep=":" ),
                       paste( emptyFileA, "", sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
      })
      describe( "names= replacement for source file names", {
         describe( "Normal names", {
            it( "changes nothing to use explicit default names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= fileList, headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ),
                          paste( inFileB, contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are simple alphabetic names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Amy", "Bob"), headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "Amy", contentA, sep="\t" ),
                          paste( "Bob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "Weird names", {
            it( "assigns names if they contain spaces", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("A m y", "B o b"), headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( "A m y", contentA, sep="\t" ),
                          paste( "B o b", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are numbers", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c(-123, 42), headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( -123,  contentA, sep="\t" ),
                          paste( 42,    contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they contain unicode", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Åmy", "ßob"), headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "Åmy", contentA, sep="\t" ),
                          paste( "ßob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are empty strings or contain delimiter (bad idea...)", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("", "\t"), headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "",    contentA, sep="\t" ),
                          paste( "\t",  contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "weird text file merging (only normal names...",{
            it( "Can merge just 'one' input file", {
               outFileName <- mergeFiles( inFileA, names="A", headerLines= 1 )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "A",   contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), names= c("EA", "EB"),
                                          headerLines= 1 )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               outFileName <- mergeFiles( c(blankFileA, blankFileB), names= c("BA", "BB"),
                                          headerLines= 1 )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header,      sep="\t" ),
                          paste0( c("BA", "BA", "BB"), sep= "\t"))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), names= "BB", headerLines= 1 )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( "BB", sep= "\t"))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy"), headerLines= 1 )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("BA", "BA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "+keepEmpty, when handling multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy"), keepEmpty= TRUE,
                                          headerLines= 1 )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("BA", "BA", "EA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ keepEmpty=", {
            it( "Lines for empty files have correct names, when set true.", {
               fileList <- c(emptyFileA, emptyFileB)
               mergeFiles( fileList, outFile, keepEmpty= TRUE, names= c("Amy", "Bob"),
                           headerLines= 1 )

               got <- readLines( outFile )
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("Amy", "Bob"), sep= "\t" ))
               expect_equal(got, want)
            })
         })
         describe( "Errors", {
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, headerLines= 1, names="bob" ), wantRE)
            })
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, headerLines= 1, names=c("bob","bob","bob")), wantRE)
            })
         })
      })
      describe( "colName= header name parameter", {
         describe( "Normal with-header text files merging)", {
            columnName <- "The File"
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, headerLines=1, colName= columnName )

            it( "Cat files to a temp file, prefixing <filename> and tab.", {
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
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
         describe( "Weird with-header text file merging", {
            it( "Can merge just 'one' input file", {
               columnName <- "The head"
               outFileName <- mergeFiles( inFileA, headerLines=1L, colName= columnName )

               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               columnName <- ""
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), headerLines=1L,
                                          colName= columnName )
               got <- readLines(outFileName)
               want <- paste( columnName, header, sep="\t")
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               columnName <- "BOB"
               outFileName <- mergeFiles( c(blankFileA, blankFileA, blankFileB),
                                          headerLines=1L, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), headerLines=1L,
                                          colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               columnName <- "∂ß∫"

               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines=1L, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ keepEmpty", {
            it( "handles keepEmpty and multiple kinds of weird input files", {
               columnName <- '   '
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines=1L, keepEmpty=TRUE, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( emptyFileA, "", sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ names=", {
            it( "assigns names when they are simple alphabetic names", {
               fileList <- c(inFileA, inFileB)
               columnName <- "Bob_bob-bob"
               outFileName <- mergeFiles( fileList, names= c("Amy", "Bob"), headerLines= 1,
                                          colName= columnName )

               got <- readLines(outFileName)
               want <- c( paste(  columnName, header, sep="\t" ),
                          paste( "Amy", contentA, sep="\t" ),
                          paste( "Bob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
      })
   })
   describe( "REQ inFiles= Files with one line headers (headerLines = 3)", {
      header <- c("DESC\tTHING", "col\tcol", "A\tB" )
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

            expect_silent( got <- mergeFiles( fileList, headerLines= 3L  ))

            expect_true( file.exists( got ))
         })
      })
      describe( "Default merging of files (with headers)", {
         describe( "Normal with-header text files merging)", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, headerLines= 3L )

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
         describe( "Weird with-header text file merging", {
            it( "Can merge just 'one' input file", {
               outFileName <- mergeFiles( inFileA, headerLines= 3L  )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), headerLines= 3L  )
               got <- readLines(outFileName)
               want <- paste( "FILE", header, sep="\t")
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               outFileName <- mergeFiles( c(blankFileA, blankFileA, blankFileB), headerLines= 3L  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), headerLines= 3L  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "+ keepEmpty, Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines= 3L, keepEmpty=TRUE  )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( emptyFileA, "", sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe("Errors and Warnings", {
            it( "Generates an error if no files to merge", {
               wantRE <- 'Must specify at least one input file.'
               expect_error( mergeFiles( character(0), headerLines= 3L ), wantRE )
            })
            it( "Generates an error if fewer lines than header in any file being merged", {
               shortFile <- makeTempFile("One line only, please.")
               emptyFile <- makeTempFile()
               wantRE <- 'Not enough lines in file to have expected header: ".+"'

               expect_error( mergeFiles( shortFile, headerLines= 3L ), wantRE )

               expect_error( mergeFiles( emptyFile, headerLines= 1L ), wantRE )
            })
            it( "Generates a warning if header lines don't match", {
               sillyFile <- makeTempFile( c("This is not the header you were looking for", "oops"))
               alsoShortFile <- makeTempFile( c("One line only, please.", "oops-oops"))
               shortFile <- makeTempFile( c("One line only, please.", "oops"))
               shorterFile <- makeTempFile("One line only, please.")

               wantRE <- 'File headings differ between first file and ".+"\\.'

               expect_silent( mergeFiles( c(shortFile, shorterFile), headerLines= 1L) )
               expect_silent( mergeFiles( c(shorterFile, shortFile), headerLines= 1L) )
               expect_warning( mergeFiles( c(shorterFile, sillyFile), headerLines= 1L ), wantRE )
               expect_warning( mergeFiles( c(sillyFile, shorterFile), headerLines= 1L ), wantRE )
               expect_silent( mergeFiles( c(shortFile, alsoShortFile), headerLines= 1L) )
               expect_silent( mergeFiles( c(alsoShortFile, shortFile), headerLines= 1L) )
               expect_warning( mergeFiles( c(shortFile, alsoShortFile), headerLines= 2L), wantRE )
               expect_warning( mergeFiles( c(alsoShortFile, shortFile), headerLines= 2L ), wantRE )
            })
         })
      })
      describe( "outFile= optional output file name parameter", {
         it( "Overrides default output file name when used", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile, headerLines= 3L )

            expect_equal( outFileName, outFile )
         })
         it( "Outputs expected content to the named file", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, outFile= outFile, headerLines= 3L )

            got <- readLines(outFile)
            want <- c( paste(  "FILE",   header, sep="\t" ),
                       paste( inFileA, contentA, sep="\t" ),
                       paste( inFileB, contentB, sep="\t" ))
            expect_equal(got, want)
         })
      })
      describe( "keepEmpty= boolean option for reporting blank files", {
         it( "Adds a line for empty files when set true.", {
            fileList <- c(emptyFileA, emptyFileB)
            mergeFiles( fileList, outFile, keepEmpty= TRUE, headerLines= 3L )

            got <- readLines( outFile )
            want <- c( paste(  "FILE",   header, sep="\t" ),
                       paste0( c(emptyFileA, emptyFileB), sep= "\t" ))
            expect_equal(got, want)
         })
      })
      describe( "delim= text option for filename column delimiter", {
         it(  "Uses delim to separate the prefixed filename column", {
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, delim=' ', headerLines= 3L )

            got <- readLines(outFileName)
            want <- c( paste(  "FILE",   header, sep=" " ),
                       paste( inFileA, contentA, sep=" " ),
                       paste( inFileB, contentB, sep=" " ))
            expect_equal(got, want)
         })
         it(  "+ output=. Works as expected if also specify output= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFile <- tempfile()
            outFileName <- mergeFiles( fileList, outFile= outFile, delim=':', headerLines= 3L )

            got <- readLines(outFile)
            want <- c( paste(  "FILE",   header, sep=":" ),
                       paste( inFileA, contentA, sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
         it(  "+ keepEmpty=. Works as expected if also specify keepEmpty= ", {
            fileList <- c(inFileA, emptyFileA, inFileB)
            outFileName <- mergeFiles( fileList, keepEmpty= TRUE, delim=':', headerLines= 3L )

            got <- readLines(outFileName)
            want <- c( paste(  "FILE",   header, sep=":" ),
                       paste( inFileA, contentA, sep=":" ),
                       paste( emptyFileA, "", sep=":" ),
                       paste( inFileB, contentB, sep=":" ))
            expect_equal(got, want)
         })
      })
      describe( "names= replacement for source file names", {
         describe( "Normal names", {
            it( "changes nothing to use explicit default names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= fileList, headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ),
                          paste( inFileB, contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are simple alphabetic names", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Amy", "Bob"), headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "Amy", contentA, sep="\t" ),
                          paste( "Bob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "Weird names", {
            it( "assigns names if they contain spaces", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("A m y", "B o b"), headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE",   header, sep="\t" ),
                          paste( "A m y", contentA, sep="\t" ),
                          paste( "B o b", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are numbers", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c(-123, 42), headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( -123,  contentA, sep="\t" ),
                          paste( 42,    contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they contain unicode", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("Åmy", "ßob"), headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "Åmy", contentA, sep="\t" ),
                          paste( "ßob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
            it( "assigns names if they are empty strings or contain delimiter (bad idea...)", {
               fileList <- c(inFileA, inFileB)
               outFileName <- mergeFiles( fileList, names= c("", "\t"), headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "",    contentA, sep="\t" ),
                          paste( "\t",  contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "weird text file merging (only normal names...",{
            it( "Can merge just 'one' input file", {
               outFileName <- mergeFiles( inFileA, names="A", headerLines= 3L )

               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste( "A",   contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), names= c("EA", "EB"),
                                          headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               outFileName <- mergeFiles( c(blankFileA, blankFileB), names= c("BA", "BB"),
                                          headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header,      sep="\t" ),
                          paste0( c("BA", "BA", "BB"), sep= "\t"))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), names= "BB", headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( "BB", sep= "\t"))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy"), headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("BA", "BA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "+keepEmpty, when handling multiple kinds of weird input files", {
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          names= c("BA","EA","BB","Amy"), keepEmpty= TRUE,
                                          headerLines= 3L )
               got <- readLines(outFileName)
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("BA", "BA", "EA", "BB"), sep= "\t"),
                          paste( "Amy", contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ keepEmpty=", {
            it( "Lines for empty files have correct names, when set true.", {
               fileList <- c(emptyFileA, emptyFileB)
               mergeFiles( fileList, outFile, keepEmpty= TRUE, names= c("Amy", "Bob"),
                           headerLines= 3L )

               got <- readLines( outFile )
               want <- c( paste(  "FILE", header, sep="\t" ),
                          paste0( c("Amy", "Bob"), sep= "\t" ))
               expect_equal(got, want)
            })
         })
         describe( "Errors", {
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, headerLines= 3L, names="bob" ), wantRE)
            })
            it ("errors if names and file list are not the same size", {
               fileList <- c(inFileA, inFileB)

               wantRE <- "Parameters inFiles= and names= must be vectors of the same length\\."
               expect_error( mergeFiles( fileList, headerLines= 3L, names=c("bob","bob","bob")), wantRE)
            })
         })
      })
      describe( "colName= header name parameter", {
         describe( "Normal with-header text files merging)", {
            columnName <- "The File"
            fileList <- c(inFileA, inFileB)
            outFileName <- mergeFiles( fileList, headerLines= 3L, colName= columnName )

            it( "Cat files to a temp file, prefixing <filename> and tab.", {
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
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
         describe( "Weird with-header text file merging", {
            it( "Can merge just 'one' input file", {
               columnName <- "The head"
               outFileName <- mergeFiles( inFileA, headerLines= 3L, colName= columnName )

               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
            it( "Does not record any line for empty files by default", {
               columnName <- ""
               outFileName <- mergeFiles( c(emptyFileA, emptyFileB), headerLines= 3L,
                                          colName= columnName )
               got <- readLines(outFileName)
               want <- paste( columnName, header, sep="\t")
               expect_equal(got, want)
            })
            it( "Keeps empty lines even when file is all empty lines", {
               columnName <- "BOB"
               outFileName <- mergeFiles( c(blankFileA, blankFileA, blankFileB),
                                          headerLines= 3L, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)

               outFileName <- mergeFiles( c(blankFileB), headerLines= 3L,
                                          colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileB, "", sep="\t" ))
               expect_equal(got, want)
            })
            it( "Handles multiple kinds of weird input files", {
               columnName <- "∂ß∫"

               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines= 3L, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ keepEmpty", {
            it( "handles keepEmpty and multiple kinds of weird input files", {
               columnName <- '   '
               outFileName <- mergeFiles( c(blankFileA, emptyFileA, blankFileB, inFileA),
                                          headerLines= 3L, keepEmpty=TRUE, colName= columnName )
               got <- readLines(outFileName)
               want <- c( paste(  columnName,   header, sep="\t" ),
                          paste( blankFileA, c("", ""), sep="\t" ),
                          paste( emptyFileA, "", sep="\t" ),
                          paste( blankFileB, "", sep="\t" ),
                          paste( inFileA, contentA, sep="\t" ))
               expect_equal(got, want)
            })
         })
         describe( "+ names=", {
            it( "assigns names when they are simple alphabetic names", {
               fileList <- c(inFileA, inFileB)
               columnName <- "Bob_bob-bob"
               outFileName <- mergeFiles( fileList, names= c("Amy", "Bob"), headerLines= 3L,
                                          colName= columnName )

               got <- readLines(outFileName)
               want <- c( paste(  columnName, header, sep="\t" ),
                          paste( "Amy", contentA, sep="\t" ),
                          paste( "Bob", contentB, sep="\t" ))
               expect_equal(got, want)
            })
         })
      })
   })
})

describe( "is.absolutePath( paths ) - Test for absolute path", {
   describe( "Smoke test", {
      it( "Can be run without error using all defaults", {
         expect_silent( is.absolutePath( "" ))
      })
   })
   describe( "paths= REQ - File paths to check", {
      files <- list(
         normal= list(
            val= list(
               singleAbs= "/",
               singleNotAbs= "bob",
               multiAbs= c( "\\", "/bob", "\\bob" ),
               multiNotAbs= c( "bob\\bob", "bob/bob", "bob" ),
               multiMixed= c( "/", "bob" ),
               tilde= c( "~", "~/bob", "~bob", "bob~bob" ),
               windowsAbs= c(  "c:/", "D:\\", "C:\\bob", "d:/bob" ),
               currentDot= c( c(".", "./", ".\\", "./bob", ".\\bob" )),
               updirDotsAbs= c( c("..", "../", "..\\", "../bob", "..\\bob" )),
               updirDotsNotAbs= c( c("bob/..", "bob/../bob", "bob\\..", "bob\\..\\bob" ))
            ),
            want= list(
               singleAbs= TRUE,
               singleNotAbs= FALSE,
               multiAbs= c(T, T, T),
               multiNotAbs= c(F, F, F),
               multiMixed= c(T, F),
               tilde= c( T, T, T, F ),
               windowsAbs= c(T, T, T, T ),
               currentDot= c(T, T, T, T, T ),
               updirDotsAbs= c( T, T, T, T, T ),
               updirDotsNotAbs= c( F, F, F, F )
            )
         ),
         weird= list(
            val= list(
               null= NULL,
               none= character( 0 ),
               empty= "",
               list= list( x= "hello" ),
               anNA= NA,
               NAs= c(NA, NA, "NA", "/NA"),
               ints= c( 1L, 2L, NA ),
               utfNonSep= c( "BETA_ß_", "ß", "/BETA_ß_", "\\ß" ),
               invalidButTrue= "/[](){}''|;:::Invalid but absolute!"
            ),
            want= list(
               null= logical(0),
               none= logical( 0 ),
               empty= FALSE,
               list= FALSE,
               anNA= FALSE,
               NAs= c(F, F, F, T),
               ints= c(F, F, F),
               utfNonSep= c(F,F,T,T),
               invalidButTrue= TRUE
            )
         )
      )

      describe( "Default value", {
         it ("Has expected return value", {
            succeed( "No default to test" )
         })
         it ("Has expected side effects", {
            succeed( "No default to test" )
         })
      })
      describe( "Normal values", {
         values <- files$normal$val
         want <- files$normal$want
         tests <- names( values )

         it ( "Is test data set up correctly", {
            expect_equal( tests, names( want ))
         })
         it ("Has expected return value", {
            for (test in tests) {
               expect_equal( is.absolutePath( paths= values[[test]] ), want[[test]],
                             info= test )
            }
         })
         it ("Has expected side effects", {
            succeed( "No side efects to test" )
         })
      })
      describe( "Weird values", {
         values <- files$weird$val
         want <- files$weird$want
         tests <- names( values )

         it ( "Is test data set up correctly", {
            expect_equal( tests, names( want ))
         })
         it ("Has expected return value", {
            for (test in tests) {
               expect_equal( is.absolutePath( paths= values[[test]] ), want[[test]],
                             info= test )
            }
         })
         it ("Has expected side effects", {
            succeed( "No side efects to test" )
         })
      })
      describe( "Bad values", {
         it ("Fails as expected for bad values", {
            succeed( "No bad values to test" )
         })
         it ("Has no side effects with error exit", {
            succeed( "No bad values to test" )
         })
      })
   })
})

describe( "read.tsv( file ) - Tab separated value file reader", {
   tsvFile <- makeTempFile( ext=".tsv", lines= c(
      "H1\tH2\tH3",
      "A\t1\tTRUE",
      "B\t2\tFALSE"
   ))
   tsvDF <- data.frame( H1=c("A", "B"), H2=c(1, 2), H3=c(TRUE, FALSE),
                        stringsAsFactors=FALSE)
   describe( "Smoke test", {
      it( "Can be run without error using all defaults", {
         expect_silent( read.tsv(tsvFile) )
      })
   })
   describe( "Parameter testing", {
      describe( "'file=' parameter (required)", {
         it( "Loads data from specified file into a data frame", {
            got <- read.tsv(tsvFile)
            want <- tsvDF
            expect_equal( got, want )
         })
         describe( "Errors thrown for bad values", {
            it( "Errors if file not found.", {
               badFile <- tempfile()
               expect_false( file.exists( badFile ))
               errorRE <- "No such file '.+'\\."
               expect_error( read.tsv( badFile ), errorRE )
            })
            it( "Errors if file is empty.", {
               badFile <- makeTempFile( lines="", eol="" )
               errorRE <- "no lines available in input"
               expect_error( read.tsv( badFile ), errorRE )
            })
         })
         describe( "Weird file contents work", {
            it( "Works if file has only a header line", {
               file <- makeTempFile( ext=".tsv", lines= c(
                  "H1\tH2\tH3"
               ))
               df <- data.frame( H1=logical(0), H2=logical(0), H3=logical(0),
                                 stringsAsFactors= FALSE )

               got <- read.tsv( file )
               want <- df
               expect_equal( got, want )
            })
            it( "Works if file has only one column", {
               file <- makeTempFile( ext=".tsv", lines= c(
                  "H1",
                  "A",
                  "B"
               ))
               df <- data.frame( H1=c("A", "B"), stringsAsFactors=FALSE)

               got <- read.tsv( file )
               want <- df
               expect_equal( got, want )
            })
            it( "Works if file has only one column, and it is just a header", {
               file <- makeTempFile( ext=".tsv", lines= c( "H1" ))
               df <- data.frame( H1=logical(0), stringsAsFactors=FALSE)
               got <- read.tsv( file )
               want <- df
               expect_equal( got, want )
            })
         })
      })
      describe( "'header= FALSE' parameter", {
         it( "Works with explicit default.", {
            got <- read.tsv(tsvFile, header=TRUE)
            want <- tsvDF
            expect_equal( got, want )
         })
         it( "Can read files without header correctly", {
            noHeadTsvFile <- makeTempFile( ext=".tsv", lines= c(
               "A\t1\tTRUE",
               "B\t2\tFALSE"
            ))
            noHeadTsvDF <- data.frame( V1=c("A", "B"), V2=c(1, 2), V3=c(TRUE, FALSE),
                                       stringsAsFactors= FALSE )

            got <- read.tsv( noHeadTsvFile, header=FALSE )
            want <- noHeadTsvDF
            expect_equal( got, want )
         })
         describe( "Weird file contents work", {
            it( "Works if file has only a data line", {
               file <- makeTempFile( ext=".tsv", lines= c(
                  "A\t1\tTRUE"
               ))
               df <- data.frame( V1="A", V2=1, V3=TRUE, stringsAsFactors= FALSE )
               got <- read.tsv( file, header= FALSE )
               want <- df
               expect_equal( got, want )
            })
            it( "Works if file has only one column", {
               file <- makeTempFile( ext=".tsv", lines= c("A", "B") )
               df <- data.frame( V1=c("A", "B"), stringsAsFactors=FALSE)

               got <- read.tsv( file, header=FALSE )
               want <- df
               expect_equal( got, want )
            })
            it( "Works if file has only one column, and it is just a data element", {
               file <- makeTempFile( ext=".tsv", lines= c( "A" ))
               df <- data.frame( V1="A", stringsAsFactors=FALSE)

               got <- read.tsv( file, header=FALSE )
               want <- df
               expect_equal( got, want )
            })
         })
      })
      describe( "'sep= \"\t\"' parameter", {
         it( "Can read files with alternate separator", {
            ssvFile <- makeTempFile( ext=".ssv", lines= c(
               "H1 H2 H3",
               "A 1 TRUE",
               "B 2 FALSE"
            ))
            ssvDF <- data.frame( H1=c("A", "B"), H2=c(1, 2), H3=c(TRUE, FALSE),
                                 stringsAsFactors=FALSE)

            got <- read.tsv( ssvFile, sep=" " )
            want <- ssvDF
            expect_equal( got, want )
         })
         it( "Works with explicit default.", {
            got <- read.tsv(tsvFile, sep="\t")
            want <- tsvDF
            expect_equal( got, want )
         })
         it( "Works with header= FALSE.", {
            noHeadSsvFile <- makeTempFile( ext=".ssv", lines= c(
               "A 1 TRUE",
               "B 2 FALSE"
            ))
            noHeadSsvDF <- data.frame( V1=c("A", "B"), V2=c(1, 2), V3=c(TRUE, FALSE),
                                 stringsAsFactors=FALSE)

            got <- read.tsv(noHeadSsvFile, header=FALSE, sep=" ")
            want <- noHeadSsvDF
            expect_equal( got, want )
         })
      })
      describe( "'stringsAsFactors= FALSE' parameter", {
         it( "Can read string columns in as factor.", {
            tsvFactorFile <- makeTempFile( ext=".ssv", lines= c(
               "H1\tH2\tH3",
               "A\t1\tTRUE",
               "B\t2\tFALSE"
            ))
            tsvFactorDF <- data.frame( H1=c("A", "B"), H2=c(1, 2), H3=c(TRUE, FALSE))

            got <- read.tsv( tsvFactorFile, stringsAsFactors= TRUE )
            want <- tsvFactorDF
            expect_equal( got, want )
         })
         it( "Works with explicit default.", {
            got <- read.tsv(tsvFile, stringsAsFactors= FALSE)
            want <- tsvDF
            expect_equal( got, want )
         })
         it( "Works with non-default settings", {
            noHeadTsvFactorFile <- makeTempFile( ext=".ssv", lines= c(
               "A 1 TRUE",
               "B 2 FALSE"
            ))
            noHeadTsvFactorDF <- data.frame( V1=c("A", "B"), V2=c(1, 2), V3=c(TRUE, FALSE))

            got <- read.tsv(noHeadTsvFactorFile, header= FALSE, sep=" ", stringsAsFactors= TRUE)
            want <- noHeadTsvFactorDF
            expect_equal( got, want )
         })
      })
      describe( "'...' - pass through to read.tables(...) works", {
         it( "Can set column names", {
            tsvFile <- makeTempFile( ext=".tsv", lines= c(
               "H1\tH2\tH3",
               "A\t1\tTRUE",
               "B\t2\tFALSE"
            ))
            newColDF <- data.frame( A=c("A", "B"), B=c(1, 2), C=c(TRUE, FALSE),
                                 stringsAsFactors=FALSE)
            got <- read.tsv(tsvFile, col.names=c( "A", "B", "C" ))
            want <- newColDF
            expect_equal( got, want )
         })
         it( "Works with other parmaeter changes", {
            noHeadTsvFactorFile <- makeTempFile( ext=".ssv", lines= c(
               "A 1 TRUE",
               "B 2 FALSE"
            ))
            newColnoHeadTsvFactorDF <- data.frame( A=c("A", "B"), B=c(1, 2), C=c(TRUE, FALSE))
            got <- read.tsv(noHeadTsvFactorFile, header=FALSE, sep=" ", stringsAsFactors=TRUE,
                            col.names=c( "A", "B", "C" ))
            want <- newColnoHeadTsvFactorDF
            expect_equal( got, want )
         })
         it( "Errors if parameter not used by read.tables", {
            wantErrorRE <- "unused argument"
            expect_error( read.tsv( tsvFile, foo="bar" ), wantErrorRE )
         })
      })
   })

})
