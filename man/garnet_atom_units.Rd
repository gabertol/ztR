% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/garnet_atomic_unit.R
\name{garnet_atom_units}
\alias{garnet_atom_units}
\title{garnet_atom_units}
\usage{
garnet_atom_units(database, weights = garnet_EW())
}
\arguments{
\item{database}{A database with mineral specimen,sample and list of elements with the SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,FeO,MnO,MgO,CaO for garnet}

\item{weights}{A dataframe with elements weight,number of cation and anions for each oxide,crystallographic site for garnet. The function EW_garnet() provides the element weights but another DF with different values can input}
}
\value{
a dataframe with the atomic units for each element including SiO2,TiO2,Al2O3,Cr2O3,Fe2O3,FeO,MnO,MgO,CaO
}
\description{
garnet_atom_units
}
\examples{
A<-garnet_benchmark_01()
garnet_atom_units(A)

}
