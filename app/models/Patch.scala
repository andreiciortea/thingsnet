package models

import org.w3.banana._

case class Patch[Rdf <: RDF](removed: Rdf#Graph, added: Rdf#Graph)