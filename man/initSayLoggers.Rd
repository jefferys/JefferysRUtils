% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FutileLoggerUtils.R
\name{initSayLoggers}
\alias{initSayLoggers}
\title{Initialize default loggers}
\usage{
initSayLoggers(
  file = packageName(env = parent.frame()) \%p\% ".log",
  fileLevel = WARN,
  consoleLevel = INFO,
  fileLayout = layout.format("~l [~t] ~m"),
  consoleLayout = layout.format("~m")
)
}
\arguments{
\item{file}{The name of the log file, by default \file{"\var{package}.log"}
where \var{package} is guessed using \code{link{packageName(env=parent.frame())}}.}

\item{fileLevel}{Only messages at least this important will be saved to the
log file, by default \code{WARN}.}

\item{consoleLevel}{Only messages at least this important will be printed to the
console, by default \code{INFO}.}

\item{fileLayout}{The layout string to use with messages logged to the log
file. By default this See \code{\link[futile.logger]{flog.layout}}}

\item{consoleLayout}{The layout string to use with messages logged to the
console. See \code{\link[futile.logger]{flog.layout}}}
}
\value{
Nothing, called only for its side effect of initializing loggers.
}
\description{
Sets up the default \code{"\var{package}.console"} and
\code{"\var{package}.file"} loggers. Use the \code{say\var{level}} function
to log simultaneously to them. If you log from a script or a console and
don't specify the log file, it will write to the probably hidden file
\file{".log"}. Allowed log levels are, in order of decreasing importance:
\code{OFF}, \code{FATAL}, \code{ERROR}, \code{WARN}, \code{INFO},
\code{DEBUG}, \code{TRACE}. Messages logged to the console will be just the
messages by default, messages logged to a file will be prepended with the
log level and the date. This can be changed by modifying the layout
parameters.
}
\examples{
\dontrun{
  # Default settings
  initSayLoggers()

  # Won't be logged
  sayDebug('Initialized logging.')

  # Logged only to console, not file
  sayInfo('Welcome!')
  #> Welcome!

  # Logged to console and file.
  sayWarn("This conversation is being monitored.")
  #> This conversation is being monitored.
  #> [In log file]
  #> WARN [YYYY-MM-DD hh::mm::ss TZ] This conversation is being monitored.
}
}
\seealso{
futile.logger::flog.layout, futile.logger::flog.logger
}
