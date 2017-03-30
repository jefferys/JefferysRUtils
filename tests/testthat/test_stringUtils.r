context( "Testing string utils" )

describe( "Binary paste operators %p% and %pp%", {
   it( "Joins without spaces using %p%.", {
      expect_equal( "a" %p% "b", "ab")
      expect_equal( "a " %p% " b", "a  b")
      expect_equal( c("a", "b") %p% c(1, 2), c("a1", "b2"))
   })
   it( "Joins with spaces using %pp%.", {
      expect_equal( "a" %pp% "b", "a b")
      expect_equal( "a " %pp% " b", "a   b")
      expect_equal( c("a", "b") %pp% c(1, 2), c("a 1", "b 2"))
   })
   it( "Joins using content of variables", {
      aVar <- 123
      expect_equal( "a" %p% "b" %p% "c" %p% aVar, "abc123")
      expect_equal( "a" %pp% "b" %pp% "c" %pp% aVar, "a b c 123")
      expect_equal( "a" %p% "b" %pp% "c" %p% aVar %pp% 4, "ab c123 4")
   })
})

describe( 'regexprMatches()', {
   it( "Retrieves multi-group matched text across a vector of strings", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>.+)"
      data <- c('name = Stuart R. Jefferys', 'email=srj@unc.edu')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data)
      want<-do.call(cbind, list(key=c('name', 'email'), value=c('Stuart R. Jefferys', 'srj@unc.edu')))
      expect_equal(got, want)
   })

   it( "Retrieves mutli-group matched text from a single string", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>.+)"
      data <- c('name = Stuart R. Jefferys')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data)
      want<-do.call(cbind, list(key=c('name'), value=c('Stuart R. Jefferys')))
      expect_equal(got, want)
   })

   it( "Retrieves single-group matched text across a vector of strings", {
      regExp <- ".*?(?<num>\\d+).*"
      data <- c('Embedded 123 number', '0234')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data)
      want<-do.call(cbind, list(num=c('123', '0234')))
      expect_equal(got,want)
   })

   it( "Retrieves empty text string by default when doesn't match", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>\\d+)"
      data <- c('id = 0123', 'email=srj@unc.edu')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data)
      want<-do.call(cbind, list(key=c('id', ''), value=c('0123', '')))
      expect_equal(got, want)
   })

   it( "Retrieves NA if matches nothing and use.na is specified", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>\\d+)"
      data <- c('id = 0123', 'email=srj@unc.edu')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data, use.na=TRUE)
      want<-do.call(cbind, list(key=c('id', NA), value=c('0123', NA)))
      expect_equal(got, want)
   })

   it( "Retrieves empty text string by default if matches but captures no text", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>\\d*)"
      data <- c('id = 0123', 'email=srj@unc.edu')

      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data)
      want<-do.call(cbind, list(key=c('id', 'email'), value=c('0123', '')))
      expect_equal(got, want)
   })
   it( "Still retrieves empty text string if matches but captures no text when use.na is specified", {
      regExp <- "(?<key>.+?)\\s*=\\s*(?<value>\\d*)"
      data <- c('id = 0123', 'email=srj@unc.edu')

      # Still want '', not NAs
      matchResults <- regexpr(regExp, data, perl= TRUE)
      got<-regexprMatches(matchResults, data, use.na=TRUE)
      want<-do.call(cbind, list(key=c('id', 'email'), value=c('0123', '')))
      expect_equal(got, want)
   })
})

describe( 'regexprCapture()', {
   it( "Captures text from data using capture expressions", {
      regExp <- "\\s*(?<name>.*?)\\s*<\\s*(?<email>.+)\\s*>\\s*"
      data <- c( 'Stuart R. Jefferys <srj@unc.edu>',
                 'nonya business <nobody@nowhere.com>',
                 'no email',
                 '<just@an.email>' )
      got <- regexprCapture(regExp, data)
      want <- cbind( c("Stuart R. Jefferys", "nonya business", "", ""),
                     c("srj@unc.edu", "nobody@nowhere.com", "", "just@an.email"))
      colnames(want) <- c("name","email")
      expect_equal( got, want )
   })
   it( "Can be set to use NA for non matching groups instead of empty strings", {
      regExp <- "\\s*(?<name>.*?)\\s*<\\s*(?<email>.+)\\s*>\\s*"
      data <- c( 'Stuart R. Jefferys <srj@unc.edu>',
                 'nonya business <nobody@nowhere.com>',
                 'no email',
                 '<just@an.email>' )
      got <- regexprCapture( regExp, data, use.na= TRUE )
      want <- cbind( c("Stuart R. Jefferys", "nonya business", NA, ""),
                     c("srj@unc.edu", "nobody@nowhere.com", NA, "just@an.email"))
      colnames(want) <- c("name","email")
      expect_equal( got, want )
   })
})

describe( "templateFill() when as.R is FALSE", {
   it("Smoke tests", {
      var1<-"filled"
      expect_equal(templateFill('{{var1}}'), "filled")
   })
   it("Fills single variable mustache templates with caller variables", {
      templateText <- c(
         "At the end comes {{var1}}.",
         "{{var2}} comes at the start",
         "Also, {{var2}} comes in the middle."
      )
      var1 <- "END"
      var2 <- "A WORD"
      got<-templateFill(templateText);
      want <- c(
         "At the end comes END.",
         "A WORD comes at the start",
         "Also, A WORD comes in the middle."
      )
      expect_equal(got, want)
   })
   it("Fills multi variable mustache templates with caller variables", {
      templateText <- c(
         "{{var1}} and ({{var2}} or {{var3}})",
         "{{var1}}{{var2}}{{var3}}"
      )
      var1 <- "one"
      var2 <- "two"
      var3 <- "three"
      got<-templateFill(templateText);
      want <- c(
         "one and (two or three)",
         "onetwothree"
      )
      expect_equal(got, want)
   })
   it("Fills a template only string with a caller variable", {
      templateText <- "{{var1}}"
      var1 <- "one"
      got<-templateFill(templateText);
      want <- "one"
      expect_equal(got, want)
   })
   it("Leaves non-template containing strings alone", {
      templateText <- c( "{{ Start only.", "End only. }}", "{{", "}}" )
      got<-templateFill(templateText);
      want <- templateText
      expect_equal(got, want)
   })
   it("Leaves empty strings alone", {
      templateText <- c( "", "Nothing", "" )
      got<-templateFill(templateText);
      want <- templateText
      expect_equal(got, want)
   })
   it("Leaves original template strings alone", {
      originalTemplateText <- "{{var1}}"
      var1 <- "one"
      templateFill(originalTemplateText);
      want <- "{{var1}}"
      expect_equal(originalTemplateText, want)

      originalTemplateText <- c(
         "At the end comes {{var1}}.",
         "{{var2}} comes at the start",
         "Also, {{var2}} comes in the middle."
      )
      var1 <- "one"
      var2 <- "two"
      templateFill(originalTemplateText);
      want <- c(
         "At the end comes {{var1}}.",
         "{{var2}} comes at the start",
         "Also, {{var2}} comes in the middle."
      )
      expect_equal(originalTemplateText, want)
   })
   it("Works with non-default template delimiters", {
      templateText <- c(
         "At the end comes <[var1|.",
         "<[var2| comes at the start",
         "Also, <[var2| comes in the middle."
      )
      var1 <- "END"
      var2 <- "A WORD"
      got <- templateFill(templateText, delim = c( "<[", "|" ) );
      want <- c(
         "At the end comes END.",
         "A WORD comes at the start",
         "Also, A WORD comes in the middle."
      )
      expect_equal(got, want)
   })
   it("Works when variables are supplied via the env", {
      templateText <- c(
         "{{var1}} and ({{var2}} or {{var3}})",
         "{{var1}}{{var2}}{{var3}}"
      )
      var1 <- "one"
      var2 <- "two"
      var3 <- "three"
      env=new.env(parent=environment())
      env$var1 = 'A'
      env$var2 = 'B'
      env$var3 = 'C'
      got<-templateFill(templateText, envir=env);
      want <- c(
         "A and (B or C)",
         "ABC"
      )
      expect_equal(got, want)
   })
})

describe( "templateFill() when as.R is TRUE", {
   securityWarn_RE <- "Potential security risk"
   it("Interprets simple code", {
      # Variable look up
      var1 <- "filled"
      expect_warning( got <- templateFill('{{var1}}', as.R= TRUE), securityWarn_RE )
      expect_equal(got, var1)

      expect_warning( got <- templateFill('{{1 + 1}}', as.R= TRUE), securityWarn_RE )
      expect_equal(got, as.character(1 + 1))
   })
   it("Runs single code mustache templates in (same) caller frame", {
      templateText <- c(
         'At the end comes {{code1 <- "END"; code1}}.',
         '{{code2 <- "A WORD"; code2}} comes at the start',
         "Also, {{code2}} comes in the middle."
      )
      expect_warning( got <- templateFill(templateText, as.R= TRUE), securityWarn_RE )
      want <- c(
         "At the end comes END.",
         "A WORD comes at the start",
         "Also, A WORD comes in the middle."
      )
      expect_equal(got, want)
   })
   it("Runs multi variable mustache templates in (same) caller frame", {
      templateText <- c(
         "{{x<-1; 1}} and ({{x+1}} or {{x+2}})",
         "{{x}}{{x*2}}{{x*2+1}}"
      )
      expect_warning( got<-templateFill(templateText, as.R=TRUE), securityWarn_RE )
      want <- c(
         "1 and (2 or 3)",
         "123"
      )
      expect_equal(got, want)
   })
   it("Fills a template only string with a R code result", {
      templateText <- "{{f<-function(x) {\n   100 * x + 1\n}\nx<-3\nf(x)}}"
      expect_warning( got<-templateFill(templateText, as.R=TRUE), securityWarn_RE )
      want <- "301"
      expect_equal(got, want)
   })
   it("Leaves non-template containing strings alone if as.R set", {
      templateText <- c( "{{ Start only.", "End only. }}", "{{", "}}" )
      expect_warning( got<-templateFill(templateText, as.R= TRUE), securityWarn_RE )
      want <- templateText
      expect_equal(got, want)
   })
   it("Leaves empty strings alone if as.R set", {
      templateText <- c( "", "Nothing", "" )
      expect_warning( got<-templateFill(templateText, as.R= TRUE), securityWarn_RE )
      want <- templateText
      expect_equal(got, want)
   })
   it("Leaves original template strings alone", {
      originalTemplateText <- "{{f<-function(x) {\n   100 * x + 1\n}\nx<-3\nf(x)}}"
      expect_warning( ignoreMe <- templateFill(originalTemplateText, as.R= TRUE), securityWarn_RE )
      want <- "{{f<-function(x) {\n   100 * x + 1\n}\nx<-3\nf(x)}}"
      expect_equal(originalTemplateText, want)
   })
   it("Runs code with non-default template delimiters", {
      templateText <- c(
         'At the end comes <<<code1 <- "END"; code1????.',
         '<<<code2 <- "A WORD"; code2???? comes at the start',
         "Also, <<<code2???? comes in the middle."
      )
      expect_warning( got<-templateFill(templateText, as.R= TRUE, delim=c( '<<<', '????' )), securityWarn_RE )
      want <- c(
         "At the end comes END.",
         "A WORD comes at the start",
         "Also, A WORD comes in the middle."
      )
      expect_equal(got, want)
   })
   it("Runs code in the supplied env if its specified", {
      templateText <- "{{f<-function(x) {\n   y * x + 1\n}\nx<-3\nf(x)}}"
      env=new.env(parent=environment())
      env$y = 1000
      expect_warning( got<-templateFill(templateText, as.R=TRUE, envir=env), securityWarn_RE )
      want <- "3001"
      expect_equal(got, want)
   })
})

describe( "templateFill() exception handling with and without as.R", {
   securityWarn_RE <- "Potential security risk"
   it("Dies if too many open default delimiters.", {
      wantErrorRE = paste(
         "Too many \\{\\{ found in template text element 1.",
         "Probably missing one or more \\}\\}.", sep=" "
      )
      templateText <- c( "{{var1}}{{var1}}{{" )
      var1<-"dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many close default delimiters.", {
      wantErrorRE = paste(
         "Too many \\}\\} found in template text element 1.",
         "Probably missing one or more \\{\\{.", sep=" "
      )
      templateText <- c( "{{var1}}{{var1}}}}" )
      var1<-"dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many open default delimiters, not in first string", {
      wantErrorRE = paste(
         "Too many \\{\\{ found in template text element 2.",
         "Probably missing one or more \\}\\}.", sep=" "
      )
      templateText <- c( "Good: {{var1}}", "Bad: {{var2}}{{", "}}. oops" )
      var1<-"dummy"
      var2<-"aslo dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many close default delimiters, not in first string", {
      wantErrorRE = paste(
         "Too many \\}\\} found in template text element 2.",
         "Probably missing one or more \\{\\{.", sep=" "
      )
      templateText <- c( "Good: {{var1}}", "Bad: {{var1}}}}", "{{. oops" )
      var1<-"dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if nested open and close default delimiters", {
      wantErrorRE = paste(
         "Nested delimiters not allowed: \\{\\{ occurs again before \\}\\}",
         "in string 1.", sep=" "
      )
      templateText <- c( "{{var1{{var1}}var1}}" )
      var1<-"dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if nested open and close default delimiters, not in first string", {
      wantErrorRE = paste(
         "Nested delimiters not allowed: \\{\\{ occurs again before \\}\\}",
         "in string 2.", sep=" "
      )
      templateText <- c( "{{var1}} is ok.", "But this is bad: {{var1{{var1}}var1}}" )
      var1<-"dummy"
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if out-of order open and close default delimiters (close before open)", {
      wantErrorRE = "\\}\\} before \\{\\{ in string 1."
      templateText <- c( "}}var1{{Must match in nubmer to trigger this." )
      var1 <- NULL;
      expect_error( templateFill(templateText), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, as.R= TRUE), wantErrorRE), securityWarn_RE)
   })

   # user delimiters

   it("Dies if too many open user delimiters.", {
      securityWarn_RE <- "Potential security risk"
      wantErrorRE = paste(
         "Too many <<< found in template text element 1.",
         "Probably missing one or more \\|.", sep=" "
      )
      templateText <- c( "<<<var1|<<<var1|<<<" )
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<<<', '|')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<<<', '|'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many close user delimiters.", {
      wantErrorRE = paste(
         "Too many \\| found in template text element 1.",
         "Probably missing one or more <<<.", sep=" "
      )
      templateText <- c( "<<<var1|<<<var1||" )
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<<<', '|')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<<<', '|'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many open user delimiters, not in first string", {
      wantErrorRE = paste(
         "Too many < found in template text element 2.",
         "Probably missing one or more \\!\\!\\!.", sep=" "
      )
      templateText <- c( "Good: <var1!!!", "Bad: <<<var2!!!", "!!!!!!. oops" )
      var1<-"dummy"
      var2<-"aslo dummy"
      expect_error( templateFill(templateText, delim=c('<', '!!!')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<', '!!!'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if too many close user delimiters, not in first string", {
      wantErrorRE = paste(
         "Too many \\!\\!\\! found in template text element 2.",
         "Probably missing one or more <.", sep=" "
      )
      templateText <- c( "Good: <var1!!!", "Bad: <var1!!!!!!", "<. oops" )
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<', '!!!')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<', '!!!'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if nested open and close user delimiters", {
      wantErrorRE = paste(
         "Nested delimiters not allowed: < occurs again before \\!\\!\\!",
         "in string 1.", sep=" "
      )
      templateText <- c( "<var1<var1!!!var1!!!" )
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<', '!!!')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<', '!!!'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if nested open and close user delimiters, not in first string", {
      wantErrorRE = paste(
         "Nested delimiters not allowed: <<< occurs again before \\|",
         "in string 2.", sep=" "
      )
      templateText <- c( "<<<var1| is ok.", "But this is bad: <<<var1<<<var1|var1|" )
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<<<', '|')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<<<', '|'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })
   it("Dies if out-of order open and close user delimiters (close before open)", {
      wantErrorRE = "\\| before <<< in string 1."
      templateText <- c( "|var1<<< Must match in number to trigger this." )
      var1 <- NULL;
      expect_error( templateFill(templateText, delim=c('<<<', '|')), wantErrorRE)
      expect_warning( expect_error( templateFill(templateText, delim=c('<<<', '|'), as.R= TRUE), wantErrorRE), securityWarn_RE)
   })

   it("Dies if specify too few delimiters", {
      wantErrorRE = "delim= must have exactly two elements."
      templateText <- "<<var1>>"
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<<')), wantErrorRE)
   })
   it("Dies if specify too many delimiters", {
      wantErrorRE = "delim= must have exactly two elements."
      templateText <- "<<var1>>"
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('<<', '>>', '<<>>')), wantErrorRE)
   })
   it("Dies if open and close delimiters are same", {
      wantErrorRE = "delim= must have different open and close elements"
      templateText <- "||var1||"
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('||', '||')), wantErrorRE)
   })
   it("Dies if open delimiter embeded in the close delimiters", {
      wantErrorRE = "Can't have one of the delimiters embeded in the other."
      templateText <- "|var1||"
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('|', '||')), wantErrorRE)
   })
   it("Dies if close delimiter embeded in the open delimiters", {
      wantErrorRE = "Can't have one of the delimiters embeded in the other."
      templateText <- "$||var1||"
      var1<-"dummy"
      expect_error( templateFill(templateText, delim=c('$||', '||')), wantErrorRE)
   })
   it("Dies if R variable not found (as.R = FALSE)", {
      wantErrorRE <- "object \'noSuchVar\' not found"
      expect_error(templateFill('{{noSuchVar}}'), wantErrorRE)
   })
   it("Dies if R code fails (as.R = TRUE)", {
      wantErrorRE <- "object \'noSuchVar\' not found"
      expect_warning( expect_error(templateFill('{{noSuchVar}}', as.R= TRUE), wantErrorRE), securityWarn_RE)

      wantErrorRE <- "could not find function.+noSuchFunction"
      expect_warning( expect_error(templateFill('{{noSuchFunction()}}', as.R= TRUE), wantErrorRE), securityWarn_RE)
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
