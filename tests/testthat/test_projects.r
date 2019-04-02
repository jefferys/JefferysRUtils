context( "Testing projects" )

describe( "useResultsDir", {
   describe("Minimal behavior", {
      it( "Smoke test", {
         expect_silent( useResultsDir( tempfile(), "subDir" ))
      })
      it( "Creates a directory for outputing files into and returns it", {
         base <- tempfile()
         sub <- "subDir"

         got <- useResultsDir( base, sub )
         want <- file.path( base, sub )
         expect_equal( got, want )
         expect_true( dir.exists( got ))
      })
      it( "Returns a directory that can be written into.", {
         base <- tempfile()
         sub <- "aSubDir"
         newDir <- useResultsDir( base, sub )
         aFile <- file.path( newDir, "test.txt" )

         expect_false( file.exists( aFile ))
         file.create( aFile )
         expect_true( file.exists( aFile ))
      })
   })
   describe("Recursive base directory creation", {
      it( "Creates base recursively when at least two levels of base missing", {
         baseOfBase <- tempfile()
         expect_false( dir.exists( baseOfBase ))
         baseDir <- file.path( baseOfBase, "subDir" )
         got <- useResultsDir( baseDir, "dir1" )
         want <- file.path( baseDir, "dir1" )
         expect_equal( got, want )
         expect_true( dir.exists( got ))
      })
      it( "Creates base if just the terminal directory missing", {
         baseOfBase <- tempfile()
         dir.create( baseOfBase, recursive= TRUE )
         expect_true( dir.exists( baseOfBase ))
         baseDir <- file.path( baseOfBase, "someSub" )
         got <- useResultsDir( baseDir, "dir1" )
         want <- file.path( baseDir, "dir1" )
         expect_equal( got, want )
         expect_true( dir.exists( got ))
      })
      it( "Works if base already exists", {
         baseDir <- tempfile()
         dir.create( baseDir, recursive= TRUE )
         expect_true( dir.exists( baseDir ))
         got <- useResultsDir( baseDir, "dir1" )
         want <- file.path( baseDir, "dir1" )
         expect_equal( got, want )
         expect_true( dir.exists( got ))
      })
   })
   describe( "Base directory may be relative or absolute", {
      it( "Works if base dir is absolute", {
         baseDir <- tempfile()
         expect_true( is.absolutePath( baseDir ))
         got <- useResultsDir( baseDir, "dir1" )
         want <- file.path( baseDir, "dir1" )
         expect_equal( got, want )
         expect_true( dir.exists( got ))
      })
      it( "Works if base dir is relative", {
         oldwd <- getwd()
         on.exit( setwd( oldwd ), add= TRUE )
         newwd <- tempfile()
         dir.create( newwd, recursive= TRUE )
         setwd( newwd )

         relBase <- file.path( "some", "dir" )
         expect_false( is.absolutePath( relBase ))
         got <- useResultsDir( relBase, "dir1" )
         want <- file.path( relBase, "dir1" )
         expect_equal( got, want )
         expect_true( dir.exists( got ))

         setwd( oldwd )
      })
   })
   describe( "Rerun", {
      it( "Returns outDir with warning message if outDir already exists", {
         base <- tempfile()
         outDir <- file.path( base, "rerunDir" )
         dir.create( outDir, recursive= TRUE )
         expect_true( dir.exists( outDir ))
         wantRE <- paste0( "Warning - Rerun is reusing existing directory: \"",
                           outDir, "\"\\." )
         expect_warning( got <- useResultsDir( base, "rerunDir", rerun= TRUE ), wantRE )
         expect_equal( got, outDir )
      })
   })
   describe( "Errors", {
      describe( "When rerun= FALSE", {
         it( "Is an error if the dir to be created already exists", {
            base <- tempfile()
            dir <- "aDir"
            outDir <- file.path( base, dir )
            dir.create( outDir, recursive= TRUE )
            wantRE <- paste0( "Aborting: Output dir already exists: \"",
                              outDir, "\".\tSet rerun= TRUE to overwrite exising output.")
            expect_error( useResultsDir(base, dir), wantRE )
         })
         it( "Is an error if can't create the base directory", {
            base <- tempfile()
            dir.create( base )
            fileBase <- file.path( base, "aFile" )
            file.create( fileBase )
            expect_true( file.exists( fileBase ))
            expect_false( dir.exists( fileBase ))

            wantErrorRE = "Aborting: Error creating base directory: \"" %p% fileBase %p% "\"."
            wantWarningRE = "already exists"
            expect_warning(
               expect_error( useResultsDir( fileBase, "aDir" ), wantErrorRE ),
               wantWarningRE
            )
         })
         it( "Is an error if can't create the outDir named directory", {
            base <- tempfile()
            dir.create( base )
            dir <- "aFile"
            fileDir <- file.path( base, dir )
            file.create( fileDir )
            expect_true( file.exists( fileDir ))
            expect_false( dir.exists( fileDir ))

            wantErrorRE = "Aborting: Error creating output directory: \"" %p% fileDir %p% "\"."
            wantWarningRE = "already exists"
            expect_warning(
               expect_error( useResultsDir( base, dir ), wantErrorRE ),
               wantWarningRE
            )
         })
      })
      describe( "When rerun= TRUE", {
         it( "Is an error if outDir does not already exist (base dir not created if not existing)", {
            base <- tempfile()
            dir <- "aDir"
            wantDir <- file.path( base, dir )
            wantErrorRE <- "Aborting: Dir not found: \"" %p% wantDir %p%
            "\".\tOn rerun, output directory path must exist."

            expect_false( dir.exists( base ))
            expect_error( useResultsDir(base, dir, rerun= TRUE ), wantErrorRE )
            expect_false( dir.exists( base ))
         })
      })
   })
   describe( "Examples", {
      it( "Is runable as is and results are correctly specified", {
         baseDir <- tempfile()
         outDirName <- "run-1"
         expect_true( FALSE == dir.exists( file.path( baseDir, outDirName )))
         myOutDir <- useResultsDir( baseDir, outDirName )
         expect_true( TRUE == dir.exists( file.path( baseDir, outDirName )))
         expect_warning( expect_true( myOutDir == useResultsDir( baseDir, outDirName, rerun=TRUE )))
      })
   })
})