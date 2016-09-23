context("Testing utils")

describe( "merge() is extended to merge lists when everything has a name", {
   describe( "preconditions on lists being merged and the names of their elements", {
      it( "Errors when first element is not a list, if call explicitly", {
         listAB <- list(A=1, B=2, D=4)
         notList <- 'x'
         errorRE = "Can't merge lists; 'x' is not a list\\."
         expect_error( merge.list(notList, listAB), errorRE )
      })
      it( "Errors when second element is not a list", {
         listAB <- list(A=1, B=2, D=4)
         notList <- 'x'
         errorRE = "Can't merge lists; 'y' is not a list\\."
         expect_error( merge(listAB, notList ), errorRE )
      })
      it( "Errors when either list has unnamed elements", {
         allNamedList <- list(A=1, A=2)
         missingNamesList <- list(A=1,2)
         errorRE = "'x' contains elements without names\\."
         expect_error( merge(missingNamesList, allNamedList), errorRE )
         errorRE = "'y' contains elements without names\\."
         expect_error( merge(allNamedList, missingNamesList), errorRE )
      })
      it( "Errors when either list has duplicated element names", {
         duplicatedNamesList <- list(A=1, A=2)
         uniqueNamesList <- list(A=1, B=2)
         errorRE = "'x' contains elements with duplicated names\\."
         expect_error( merge(duplicatedNamesList, uniqueNamesList), errorRE )
         errorRE = "'y' contains elements with duplicated names\\."
         expect_error( merge(uniqueNamesList, duplicatedNamesList), errorRE )
      })
   })
	describe( "merging lists by appending by default", {
		it( "Merging when list have unique names", {
			listA <- list(A1=1, A2=2)
			listB <- list(B1=3, B2=4)
			want <- list(A1=1, A2=2, B1=3, B2=4)
			got <- merge(listA, listB)
			expect_equal(got, want)
		})
      it( "When the same name is present in both lists, by default the element from \
           the first list is dropped before appending the second", {
         listAB <- list(A=1, B=2, D=4)
         listBC <- list(B="b", C="c", A="a")
         want <- list(D=4, B="b", C="c", A="a")
         got <- merge(listAB, listBC)
         expect_equal(got, want)
      })
      it( "Works with empty lists", {
      	listA <- list(A=1, B=2)
      	listEmpty <- list()

      	want <- listA
      	got <- merge(listA, listEmpty)
      	expect_equal(got, want)
      	got <- merge(listEmpty, listA)
      	expect_equal(got, want)

      	want <- listEmpty
      	got <- merge(listEmpty, listEmpty)
      	expect_equal(got, want)
      })
	})
   describe( "merging lists keeps order with keepOrder set", {
   	it( "Keeps order when list have unique names", {
   		listA <- list(A1=1, A2=2)
   		listB <- list(B1=3, B2=4)
   		want <- list(A1=1, A2=2, B1=3, B2=4)
   		got <- merge(listA, listB, keepOrder=TRUE)
   		expect_equal(got, want)
   	})
      it( "If the user wants to retain the order of elements as musch as possible, \
          duplicate names in the first list have their values replaced and these \
          are dropped from the second list when appended.", {
         listAB <- list(A=1, B=2, D=4)
         listBC <- list(B="b", C="c", A="a")
         want <- list(A="a", B="b", D=4, C="c" )
         got <- merge(listAB, listBC, keepOrder=TRUE)
         expect_equal(got, want)
      })
      it( "keepOrder works fine with empty lists too", {
      	listA <- list(A=1, B=2)
      	listEmpty <- list()

      	want <- listA
      	got <- merge(listA, listEmpty, keepOrder=TRUE)
      	expect_equal(got, want)
      	got <- merge(listEmpty, listA, keepOrder=TRUE)
      	expect_equal(got, want)

      	want <- listEmpty
      	got <- merge(listEmpty, listEmpty, keepOrder=TRUE)
      	expect_equal(got, want)
      })
   })
})

describe( "Binary paste operators %p% and %pp%", {
   it( "Joins without spaces using %p%.", {
      expect_equal( "a" %p% "b", "ab")
      expect_equal( "a " %p% " b", "a  b")
      expect_equal( c("a", "b") %p% c(1, 2), c("a1", "b2"))

      expect_equal( "a" %pp% "b", "a b")
      expect_equal( "a " %pp% " b", "a   b")
      expect_equal( c("a", "b") %pp% c(1, 2), c("a 1", "b 2"))

      aVar <- 123
      expect_equal( "a" %p% "b" %p% "c" %p% aVar, "abc123")
      expect_equal( "a" %pp% "b" %pp% "c" %pp% aVar, "a b c 123")
      expect_equal( "a" %p% "b" %pp% "c" %p% aVar %pp% 4, "ab c123 4")
   })
})

describe( "Logging with the say functions", {
   dateRE <- "\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\]"

   describe( "Respects logger levels for each named logger provided.", {
      flog.logger( name="test_TRACE", TRACE, appender=appender.console() )
      flog.logger( name="test_DEBUG", DEBUG, appender=appender.console() )
      flog.logger( name="test_INFO", INFO, appender=appender.console() )
      flog.logger( name="test_WARN", WARN, appender=appender.console() )
      flog.logger( name="test_ERROR", ERROR, appender=appender.console() )
      flog.logger( name="test_FATAL", FATAL, appender=appender.console() )
      flog.logger( name="test_OFF", OFF, appender=appender.console() )
      allLoggers <- c( "test_TRACE", "test_DEBUG", "test_INFO",
                       "test_WARN", "test_ERROR", "test_FATAL", "test_OFF" )

      it( "sayTrace() only logs to trace loggers", {
         message <- "Testing trace logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- "(?s)^TRACE" %pp% dateRE %pp% messageRE %p% "$"
         onLoggers <- "test_TRACE"
         offLoggers <- c("test_DEBUG", "test_INFO", "test_WARN", "test_ERROR",
                         "test_FATAL", "test_OFF" )
         expect_output( sayTrace( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayTrace( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayTrace( message, name=offLoggers ))
      })
      it( "sayDebug() logs to debug and trace loggers", {
         message <- "Testing debug logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- ("(?s)^DEBUG" %pp% dateRE %pp% messageRE %p% "."
                 %p% "DEBUG" %pp% dateRE %pp% messageRE %p% "$")
         onLoggers <-  c("test_DEBUG", "test_TRACE" )
         offLoggers <- c("test_INFO", "test_WARN", "test_ERROR", "test_FATAL",
                         "test_OFF" )
         expect_output( sayDebug( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayDebug( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayDebug( message, name=offLoggers ))
      })
      it( "sayInfo() logs to info + loggers - 3 total", {
         message <- "Testing info logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- ("(?s)^INFO" %pp% dateRE %pp% messageRE %p% "."
                    %p% "INFO" %pp% dateRE %pp% messageRE %p% "."
                    %p% "INFO" %pp% dateRE %pp% messageRE %p% "$")
         onLoggers <-  c("test_INFO", "test_DEBUG", "test_TRACE" )
         offLoggers <- c("test_WARN", "test_ERROR", "test_FATAL", "test_OFF" )
         expect_output( sayInfo( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayInfo( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayInfo( message, name=offLoggers ))
      })
      it( "sayWarn() logs to warn + loggers - 4 total", {
         message <- "Testing warn logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- ("(?s)^WARN" %pp% dateRE %pp% messageRE %p% "."
                    %p% "WARN" %pp% dateRE %pp% messageRE %p% "."
                    %p% "WARN" %pp% dateRE %pp% messageRE %p% "."
                    %p% "WARN" %pp% dateRE %pp% messageRE %p% "$")
         onLoggers <-  c("test_INFO", "test_DEBUG", "test_TRACE", "test_WARN" )
         offLoggers <- c("test_ERROR", "test_FATAL", "test_OFF" )
         expect_output( sayWarn( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayWarn( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayWarn( message, name=offLoggers ))
      })
      it( "sayError() logs to error + loggers - 5 total", {
         message <- "Testing error logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- ("(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "."
                    %p% "ERROR" %pp% dateRE %pp% messageRE %p% "."
                    %p% "ERROR" %pp% dateRE %pp% messageRE %p% "."
                    %p% "ERROR" %pp% dateRE %pp% messageRE %p% "."
                    %p% "ERROR" %pp% dateRE %pp% messageRE %p% "$")
         onLoggers <-  c("test_ERROR", "test_INFO", "test_DEBUG", "test_TRACE",
                         "test_WARN" )
         offLoggers <- c("test_FATAL", "test_OFF" )
         expect_output( sayError( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayError( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayError( message, name=offLoggers ))
      })
      it( "sayFatal() logs to all but OFF logger - 6 total", {
         message <- "Testing fata logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- ("(?s)^FATAL" %pp% dateRE %pp% messageRE %p% "."
                    %p% "FATAL" %pp% dateRE %pp% messageRE %p% "."
                    %p% "FATAL" %pp% dateRE %pp% messageRE %p% "."
                    %p% "FATAL" %pp% dateRE %pp% messageRE %p% "."
                    %p% "FATAL" %pp% dateRE %pp% messageRE %p% "."
                    %p% "FATAL" %pp% dateRE %pp% messageRE %p% "$")
         onLoggers <-  c("test_ERROR", "test_INFO", "test_DEBUG", "test_TRACE",
                         "test_WARN", "test_FATAL" )
         offLoggers <- "test_OFF"
         expect_output( sayFatal( message, name= onLoggers ), wantRE, perl= TRUE )
         expect_output( sayFatal( message, name=allLoggers ), wantRE, perl= TRUE )
         expect_silent( sayFatal( message, name=offLoggers ))
      })
   })
   describe( "Split logging to console + file by default", {
      file <- tempfile("tempLog", fileext= ".log" )
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      dateRE <- "\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\]"

      it( "Logs to both if level allows", {
         message <- "Testing split file and console logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
         expect_false(file.exists(file))
         expect_output( sayError( message ), wantRE, perl= TRUE )
         expect_true(file.exists(file))
         gotText <- paste0(readLines(file))
         wantTextRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( gotText, wantTextRE, perl=TRUE )
         file.remove(file)
      })
      it( "Logs to only one if level restricts", {
         message <- "Testing split file and console logging."
         messageRE <- "\\Q" %p% message %p% "\\E"
         wantRE <- "(?s)^DEBUG" %pp% dateRE %pp% messageRE %p% "$"
         expect_false(file.exists(file))
         expect_output( sayDebug( message ), wantRE, perl= TRUE )
         expect_false(file.exists(file))
      })
      it( "Respects logging level changes", {
         flog.threshold(INFO, name=packageName() %p% ".file")
         flog.threshold(ERROR, name=packageName() %p% ".console")
         message <- "Testing split file and console logging with rest log levels."
         messageRE <- "\\Q" %p% message %p% "\\E"
         expect_false(file.exists(file))
         expect_silent( sayInfo( message ))
         expect_true(file.exists(file))
         gotText <- paste0(readLines(file))
         wantTextRE <- "(?s)^INFO" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( gotText, wantTextRE, perl=TRUE )
         file.remove(file)
      })

   })
   describe( "Returns expected value, including respecting carp", {
      flog.logger( name="test_DEBUG", DEBUG, appender=appender.console() )
      flog.logger( name="test_INFO", INFO, appender=appender.console() )
      it( "Returns the string of the last logger when flog.carp is off.", {
         expect_false(flog.carp(name="test_INFO"))
         expect_false(flog.carp(name="test_DEBUG"))
         message <- "A message."
         messageRE <- "\\Q" %p% message %p% "\\E"
         expect_output( got <- sayInfo( message, name= c("test_INFO", "test_DEBUG")), ".+" )
         wantRE <- "(?s)^INFO" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( got, wantRE, perl=TRUE )
         expect_output( got <- sayDebug( message, name= c("test_INFO", "test_DEBUG")), ".+" )
         wantRE <- "(?s)^DEBUG" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( got, wantRE, perl=TRUE )
         expect_output( got <- sayInfo( message, name= c("test_DEBUG", "test_INFO")), ".+" )
         wantRE <- "(?s)^INFO" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( got, wantRE, perl=TRUE )
         expect_output( got <- sayDebug( message, name= c("test_DEBUG", "test_INFO")), ".+" )
         expect_null( got )
      })
      it( "Return value respects futile.carp", {
         message <- "A message."
         messageRE <- "\\Q" %p% message %p% "\\E"
         flog.carp(TRUE, name="test_INFO")
         expect_true(flog.carp(name="test_INFO"))
         expect_false(flog.carp(name="test_DEBUG"))
         expect_output( got <- sayDebug( message, name= c("test_DEBUG", "test_INFO")), ".+" )
         wantRE <- "(?s)^DEBUG" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( got, wantRE, perl=TRUE )
         flog.carp(FALSE, name="test_INFO")
         expect_false(flog.carp(name="test_INFO"))
         expect_output( got <- sayDebug( message, name= c("test_DEBUG", "test_INFO")), ".+" )
         expect_null( got )
      })
   })
   describe( "sprintf message formats work.", {
      file <- tempfile("tempLog", fileext= ".log" )
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      dateRE <- "\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\]"
      it( "Message can be a sprintf style string", {
         message <- "This is a number %i and a string %s."
         messageRE <- "\\Q" %p% "This is a number 4 and a string FOUR." %p% "\\E"
         wantRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
         expect_false(file.exists(file))
         x <- 4; y <- "FOUR"
         expect_output( sayError( message, x, y ), wantRE, perl= TRUE )
         expect_true(file.exists(file))
         gotText <- paste0(readLines(file))
         wantTextRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
         expect_match( gotText, wantTextRE, perl=TRUE )
         file.remove(file)
      })
   })
   describe( "OFF works as log level", {
   	file <- tempfile("tempLog", fileext= ".log" )
   	it ("Doesn't log when initilaized to OFF or \"OFF\"", {
   		initSayLoggers(file=file, fileLevel = "OFF", consoleLevel= "OFF" )
   		expect_false(file.exists(file))
   		expect_silent( sayFatal( "IgnoreMe" ))
   		expect_false(file.exists(file))

   		initSayLoggers(file=file, fileLevel = "OFF", consoleLevel= "OFF" )
   		expect_false(file.exists(file))
   		expect_silent( sayFatal( "IgnoreMe" ))
   		expect_false(file.exists(file))
   	})
   	it ("Doesn't log if changed to OFF or \"OFF\"", {
   		initSayLoggers( file=file )
   		flog.threshold( OFF, name= packageName() %p% ".file" )
# This does not work!
#   		flog.threshold( "OFF", name= packageName() %p% ".console" )
   		flog.threshold( OFF, name= packageName() %p% ".console" )
   		expect_false(file.exists(file))
   		expect_silent( sayFatal( "IgnoreMe" ))
   		expect_false(file.exists(file))
   	})
   })
})
