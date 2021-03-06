% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appendCypher.R, R/appendCypherToDF.R,
%   R/appendCypherToList.R, R/commit.R, R/newTransaction.R
\name{appendCypher}
\alias{appendCypher}
\alias{appendCypherToDF}
\alias{appendCypherToList}
\alias{transactions}
\alias{commit}
\alias{newTransaction}
\title{Transactions}
\usage{
appendCypher(transaction, query, ...)

appendCypherToDF(transaction, query, ...)

appendCypherToList(transaction, query, ...)

commit(transaction)

newTransaction(graph)
}
\arguments{
\item{transaction}{A transaction object.}

\item{query}{A character string.}

\item{...}{A named list. Optional parameters to pass to the query in the form key = value, if applicable.}

\item{graph}{A graph object.}
}
\value{
\code{newTransaction} returns a transaction object. Both \code{appendCypher} and \code{commit} return \code{NULL}. \code{appendCypherToDF} returns a \code{data.frame} and \code{appendCypherToList} returns a \code{list}
}
\description{
Transactions
}
\examples{
\dontrun{
graph = startGraph("http://localhost:7474/db/data/")
clear(graph)

data = data.frame(Origin = c("SFO", "AUS", "MCI"),
                  FlightNum = c(1, 2, 3),
                 Destination = c("PDX", "MCI", "LGA"))


query = "
MERGE (origin:Airport {name:{origin_name}})
MERGE (destination:Airport {name:{dest_name}})
CREATE (origin)<-[:ORIGIN]-(:Flight {number:{flight_num}})-[:DESTINATION]->(destination)
"

t = newTransaction(graph)

for (i in 1:nrow(data)) {
  origin_name = data[i, ]$Origin
  dest_name = data[i, ]$Dest
  flight_num = data[i, ]$FlightNum
  
  appendCypher(t, 
               query, 
               origin_name = origin_name, 
               dest_name = dest_name, 
               flight_num = flight_num)
}

r <- cypherToDF(t, "MATCH (o:Airport)<-[:ORIGIN]-(f:Flight)-[:DESTINATION]->(d:Airport)
                RETURN o.name, f.number, d.name")

commit(t)

cypher(graph, "MATCH (o:Airport)<-[:ORIGIN]-(f:Flight)-[:DESTINATION]->(d:Airport)
       RETURN o.name, f.number, d.name")
}

}
