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
   dateRE <- "\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d\\]"

   it( "Logs to both if level allows", {
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      message <- "Testing split file and console logging."
      messageRE <- "\\Q" %p% message %p% "\\E"
      expect_false(file.exists(file))
      expect_output( sayError( message ), messageRE, perl= TRUE )
      expect_true(file.exists(file))
      gotText <- paste0(readLines(file))
      wantFileRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
      expect_match( gotText, wantFileRE, perl=TRUE )
      file.remove(file)
   })
   it( "Respects layoyt changes", {
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG,
         consoleLayout = layout.format("~l [~t] ~m"),
         fileLayout= layout.format("~m")
      )
      message <- "Testing split file and console logging."
      messageRE <- "\\Q" %p% message %p% "\\E"
      timeStampMessageRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
      expect_false(file.exists(file))
      expect_output( sayError( message ), timeStampMessageRE, perl= TRUE )
      expect_true(file.exists(file))
      gotText <- paste0(readLines(file))
      expect_match( gotText, messageRE, perl=TRUE )
      file.remove(file)
   })
   it( "Logs to only one if level restricts", {
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      message <- "Testing split file and console logging."
      messageRE <- "\\Q" %p% message %p% "\\E"
      expect_false(file.exists(file))
      expect_output( sayDebug( message ), messageRE, perl= TRUE )
      expect_false(file.exists(file))
   })
   it( "Logs to only one if level restricts, if set as string", {
      initSayLoggers(file=file, fileLevel = "INFO", consoleLevel= "DEBUG" )
      message <- "Testing split file and console logging."
      messageRE <- "\\Q" %p% message %p% "\\E"
      expect_false(file.exists(file))
      expect_output( sayDebug( message ), messageRE, perl= TRUE )
      expect_false(file.exists(file))
   })
   it( "Respects logging level changes", {
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      flog.threshold(INFO, name=packageName() %p% ".file")
      flog.threshold(ERROR, name=packageName() %p% ".console")
      message <- "Testing split file and console logging with rest log levels."
      messageRE <- "\\Q" %p% message %p% "\\E"
      expect_false(file.exists(file))
      expect_silent( sayInfo( message ))
      expect_true(file.exists(file))
      gotText <- paste0(readLines(file))
      wantFileRE <- "(?s)^INFO" %pp% dateRE %pp% messageRE %p% "$"
      expect_match( gotText, wantFileRE, perl=TRUE )
      file.remove(file)
   })
   it( "Respects logging level changes as strings", {
      initSayLoggers(file=file, fileLevel = INFO, consoleLevel= DEBUG )
      flog.threshold("INFO", name=packageName() %p% ".file")
      flog.threshold("ERROR", name=packageName() %p% ".console")
      message <- "Testing split file and console logging with rest log levels."
      messageRE <- "\\Q" %p% message %p% "\\E"
      expect_false(file.exists(file))
      expect_silent( sayInfo( message ))
      expect_true(file.exists(file))
      gotText <- paste0(readLines(file))
      wantFileRE <- "(?s)^INFO" %pp% dateRE %pp% messageRE %p% "$"
      expect_match( gotText, wantFileRE, perl=TRUE )
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
      wantFileRE <- "(?s)^ERROR" %pp% dateRE %pp% messageRE %p% "$"
      expect_false(file.exists(file))
      x <- 4; y <- "FOUR"
      expect_output( sayError( message, x, y ), messageRE, perl= TRUE )
      expect_true(file.exists(file))
      gotText <- paste0(readLines(file))
      expect_match( gotText, wantFileRE, perl=TRUE )
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
