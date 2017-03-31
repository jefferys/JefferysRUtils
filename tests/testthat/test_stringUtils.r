context( "Testing stringUtils.R" )

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

describe( "toChar", {
   describe( "Required parameter x=", {
      it( "Convets single element input to vector of char", {
         data <- "ABC"
         want <- c("A", "B", "C")
         got <- toChar( data )
         expect_equal(got, want)

         data <- 123
         want <- c("1", "2", "3")
         got <- toChar( data )
         expect_equal(got, want)
      })
      it( "Converts vector element input to list of vector of char", {
         data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- list( c("A", "B", "C"), c("A", "B", "C"),
                       c("A", "\u00df", "B"), c("1", "2", "3"), "", "x", NA_character_ )
         names(want) <- data
         got <- toChar( data )
         expect_equal(got, want)
      })
      it( "For NULL and empty vector input returns an empty vector", {
         data <- NULL
         want <- character(0)
         got <- toChar( data )
         expect_equal(got, want)

         data <- character(0)
         want <- character(0)
         got <- toChar( data )
         expect_equal(got, want)
      })
   })
   describe( "Optional parameter drop= TRUE", {
      it( "Has the expected default (TRUE)", {
         data <- "ABC"
         want <- toChar( data )
         got <- toChar( data, drop=TRUE )
         expect_equal(got, want)

         data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- toChar( data )
         names(want) <- data
         got <- toChar( data, drop=TRUE )
         expect_equal(got, want)
      })
      it( "Allows changing single element input to produce a list. Vector unchanged", {
         data <- "ABC"
         want <- list("ABC"=c("A", "B", "C"))
         got <- toChar( data, drop=FALSE )
         expect_equal(got, want)

         data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- toChar( data, drop=TRUE )
         got <- toChar( data, drop=FALSE )
         expect_equal(got, want)
      })
   })
   describe( "Optional parameter use.names= TRUE", {
      it( "Has the expected default (TRUE)", {
         data <- "ABC"
         want <- toChar( data )
         got <- toChar( data, use.names= TRUE )
         expect_equal(got, want)

         data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- toChar( data )
         got <- toChar( data, use.names= TRUE )
         expect_equal(got, want)
      })
      it( "Allows producing list output without a name. Vector unchanged", {
         data <- "ABC"
         want <- toChar( data, use.names=TRUE )
         got <- toChar( data, use.names=FALSE )
         expect_equal(got, want)

         data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- list( c("A", "B", "C"), c("A", "B", "C"),
                       c("A", "\u00df", "B"), c("1", "2", "3"), "", "x", NA_character_ )
         got <- toChar( data, use.names=FALSE )
         expect_equal(got, want)
      })
   })
   describe( "Parameter interactions", {
      describe( "use.names= && drop=", {
         it( "Reports single and multi-string inputs as unnamed lists", {
            data <- "ABC"
            want <- list( c("A", "B", "C" ))
            got <- toChar( data, use.names=FALSE, drop=FALSE )
            expect_equal(got, want)

            data <- c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
            want <- list( c("A", "B", "C"), c("A", "B", "C"),
                          c("A", "\u00df", "B"), c("1", "2", "3"), "", "x", NA_character_ )
            got <- toChar( data, use.names=FALSE, drop=FALSE )
            expect_equal(got, want)
         })
      })
   })
})

describe( "revString", {
   describe( "Required parameter x", {
      it( "Reverses single element input strings", {
         data <- "ABC"
         want <- "CBA"
         got <- revString(data)
         expect_equal(got, want)
      })
      it( "Reverses each string in a vector", {
         data <- c( "ABC", "ABC", "A\u00dfB", 123, "", "x", NA )
         want <- c( "CBA", "CBA", "BßA", "321", "", "x", NA )
         got <- revString(data)
         expect_equal(got, want)
      })
      it( "Returns character(0) for NULL or empty vector input", {
         data <- NULL
         want <- character(0)
         got <- revString(data)
         expect_equal(got, want)

         data <- character(0)
         want <- character(0)
         got <- revString(data)
         expect_equal(got, want)
      })
   })
})

describe( "commonPrefix", {
   describe( "Required parameter x", {
      it( "Returns single element as its own commonPrefix", {
         data <- "ABC"
         want <- "ABC"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- "A"
         want <- "A"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- ""
         want <- ""
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- NA
         want <- NA_character_
         got <- commonPrefix( data )
         expect_equal( got, want )
      })
      it( "Returns common prefix if all are the same string", {
         data <- c( "ABC", "ABC", "ABC")
         want <- "ABC"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "A", "A", "A")
         want <- "A"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "", "", "")
         want <- ""
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( NA, NA, NA)
         want <- NA_character_
         got <- commonPrefix( data )
         expect_equal( got, want )
      })
      it( "Returns common prefix if of different length, shortest complete", {
         data <- c( "ABC", "AB")
         want <- "AB"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "ABC", "AB", "A")
         want <- "A"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "AB", "A", "")
         want <- ""
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "AB", "", NA)
         want <- NA_character_
         got <- commonPrefix( data )
         expect_equal( got, want )
      })
      it( "Returns common prefix up to first difference, case insensitive", {
         data <- c( "ABC", "ABD")
         want <- "AB"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "ABC", "ABc")
         want <- "AB"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "ABC", "AB", "AC")
         want <- "A"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "ABC", "AB", "Ab")
         want <- "A"
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "ABC", "BC", "C")
         want <- ""
         got <- commonPrefix( data )
         expect_equal( got, want )

         data <- c( "AB", "ab", "ABC" )
         want <- ""
         got <- commonPrefix( data )
         expect_equal( got, want )
      })
   })
   describe( "Optional parameter ignoreCase= FALSE", {
      it( "Case insensitive prefix match enabled by ignoreCase= TRUE", {
         data <- c( "ABC", "ab*" )
         want <- "ab"
         got <- commonPrefix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "A", "a", "AB" )
         want <- "a"
         got <- commonPrefix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "ABC")
         want <- "abc"
         got <- commonPrefix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "bcd")
         want <- ""
         got <- commonPrefix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "abc", NA)
         want <- NA_character_
         got <- commonPrefix( data, ignoreCase= TRUE )
         expect_equal( got, want )
      })
      it( "defaults to ignoreCase= FALSE", {
         data <- c( "ABC", "ab*" )
         want <- commonPrefix( data, ignoreCase= FALSE )
         got <- commonPrefix( data )
         expect_true( identical(got, want ))

         want <- commonPrefix( data, ignoreCase= TRUE )
         got <- commonPrefix( data )
         expect_false( identical(got, want ))
      })
   })
   describe( "Optional parameter dropNA= FALSE", {
      it( "Ignores NA values if dropNA set", {
         data <- c( "AB", NA, "ABC" )
         want <- "AB"
         got <- commonPrefix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "ABD", NA, "ABC" )
         want <- "AB"
         got <- commonPrefix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "abc", NA, "ABC" )
         want <- ""
         got <- commonPrefix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "abc", NA, NA, NA )
         want <- "abc"
         got <- commonPrefix( data, dropNA= TRUE )
         expect_equal( got, want )
      })
      it( "defaults to dropNA= FALSE", {
         data <- c( "ABC", NA )
         want <- commonPrefix( data, dropNA= FALSE )
         got <- commonPrefix( data )
         expect_true( identical(got, want ))

         want <- commonPrefix( data, dropNA= TRUE )
         got <- commonPrefix( data )
         expect_false( identical(got, want ))
      })
   })
   describe( "Parameter interactions", {
      describe( "ignoreCase= && dropNA=", {
         data <- c( "AB", NA, "ABC" )
         want <- "ab"
         got <- commonPrefix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "AB", NA, "abc" )
         want <- "ab"
         got <- commonPrefix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABD", NA, "ABC" )
         want <- "ab"
         got <- commonPrefix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "abD", NA, "aBC" )
         want <- "ab"
         got <- commonPrefix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", NA, NA, NA )
         want <- "abc"
         got <- commonPrefix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )
      })
   })
})

describe( "commonSuffix", {
   describe( "Required parameter x", {
      it( "Returns single element as its own commonSuffix", {
         data <- "ABC"
         want <- "ABC"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- "A"
         want <- "A"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- ""
         want <- ""
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- NA
         want <- NA_character_
         got <- commonSuffix( data )
         expect_equal( got, want )
      })
      it( "Returns common suffix if all are the same string", {
         data <- c( "ABC", "ABC", "ABC")
         want <- "ABC"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "A", "A", "A")
         want <- "A"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "", "", "")
         want <- ""
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( NA, NA, NA)
         want <- NA_character_
         got <- commonSuffix( data )
         expect_equal( got, want )
      })
      it( "Returns common suffix if of different length, shortest complete", {
         data <- c( "ABC", "BC")
         want <- "BC"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "ABC", "BC", "C")
         want <- "C"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "AB", "B", "")
         want <- ""
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "AB", "", NA)
         want <- NA_character_
         got <- commonSuffix( data )
         expect_equal( got, want )
      })
      it( "Returns common suffix up to first difference, case insensitive", {
         data <- c( "ABC", "DBC")
         want <- "BC"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "ABC", "aBC")
         want <- "BC"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "ABC", "BC", "AC")
         want <- "C"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "ABC", "BC", "bC")
         want <- "C"
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "ABC", "AB", "A")
         want <- ""
         got <- commonSuffix( data )
         expect_equal( got, want )

         data <- c( "BC", "bc", "ABC" )
         want <- ""
         got <- commonSuffix( data )
         expect_equal( got, want )
      })
   })
   describe( "Optional parameter ignoreCase= FALSE", {
      it( "Case insensitive suffix match enabled by ignoreCase= TRUE", {
         data <- c( "ABC", "*bc" )
         want <- "bc"
         got <- commonSuffix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "B", "b", "AB" )
         want <- "b"
         got <- commonSuffix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "ABC")
         want <- "abc"
         got <- commonSuffix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "bcd")
         want <- ""
         got <- commonSuffix( data, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", "abc", NA)
         want <- NA_character_
         got <- commonSuffix( data, ignoreCase= TRUE )
         expect_equal( got, want )
      })
      it( "defaults to ignoreCase= FALSE", {
         data <- c( "ABC", "*bc" )
         want <- commonSuffix( data, ignoreCase= FALSE )
         got <- commonSuffix( data )
         expect_true( identical(got, want ))

         want <- commonSuffix( data, ignoreCase= TRUE )
         got <- commonSuffix( data )
         expect_false( identical(got, want ))
      })
   })
   describe( "Optional parameter dropNA= FALSE", {
      it( "Ignores NA values if dropNA set", {
         data <- c( "BC", NA, "ABC" )
         want <- "BC"
         got <- commonSuffix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", NA, "DBC" )
         want <- "BC"
         got <- commonSuffix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "abc", NA, "ABC" )
         want <- ""
         got <- commonSuffix( data, dropNA= TRUE )
         expect_equal( got, want )

         data <- c( "abc", NA, NA, NA )
         want <- "abc"
         got <- commonSuffix( data, dropNA= TRUE )
         expect_equal( got, want )
      })
      it( "defaults to dropNA= FALSE", {
         data <- c( "ABC", NA )
         want <- commonSuffix( data, dropNA= FALSE )
         got <- commonSuffix( data )
         expect_true( identical(got, want ))

         want <- commonSuffix( data, dropNA= TRUE )
         got <- commonSuffix( data )
         expect_false( identical(got, want ))
      })
   })
   describe( "Parameter interactions", {
      describe( "ignoreCase= && dropNA=", {
         data <- c( "BC", NA, "ABC" )
         want <- "bc"
         got <- commonSuffix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "BC", NA, "abc" )
         want <- "bc"
         got <- commonSuffix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "DBC", NA, "ABC" )
         want <- "bc"
         got <- commonSuffix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "abC", NA, "dBC" )
         want <- "bc"
         got <- commonSuffix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )

         data <- c( "ABC", NA, NA, NA )
         want <- "abc"
         got <- commonSuffix( data, dropNA= TRUE, ignoreCase= TRUE )
         expect_equal( got, want )
      })
   })
})