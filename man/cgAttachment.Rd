% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{cgAttachment}
\alias{cgAttachment}
\title{Get a single primary Attachment}
\usage{
cgAttachment(class, filename, Oid, un, pw, org)
}
\arguments{
\item{class}{class requesting attachment from}

\item{filename}{output file name, names not ending in ".jpg" will have value appended.}

\item{Oid}{Oid of the attachment}

\item{un}{api username}

\item{pw}{api password}

\item{org}{orginization API ID ie 'PittsburghPA'}
}
\value{
A saved jpeg image
}
\description{
Get a single primary Attachment
}
\examples{
\dontrun{
cgAttachment("cgFacilitiesClass",
    Oid = 31459,
    un = "fakeUn",
    pw = "fakePwd",
    org = "AnytownUSA")
}
}