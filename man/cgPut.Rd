% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postData.R
\name{cgPut}
\alias{cgPut}
\title{Update records in Cartegraph}
\usage{
cgPut(class, body, un, pw, org,
  base_url = "https://cgweb06.cartegraphoms.com/")
}
\arguments{
\item{class}{cartegraph class or class and attachment ie 'cgSigns_cgAttachmentsClass', must inlcude the Oid column}

\item{body}{DataFrame/tibble to be turned into JSON object}

\item{un}{api username}

\item{pw}{api password}

\item{org}{orginization API ID ie 'PittsburghPA'}

\item{base_url}{API Base URL (defaulted to "https://cgweb06.cartegraphoms.com/")}
}
\value{
http status
}
\description{
Update records in Cartegraph
}
\examples{
\dontrun{
df <- data.frame(Oid = 242283165,
     AddressNumberField = 4771,
     StreetField = '123 Main Street')

cgPut("cgSignsClass", df, "fakeUN", "fakePW", "AnyTownUSA")
}
}
