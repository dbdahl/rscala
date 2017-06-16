.DollarNames.ScalaCachedReference <- function(x, pattern="") {
  type <- x[['type']]
  findMethods <- function() x[['interpreter']] %!% '
    import reflect.runtime.{universe => ru}
    ru.typeOf[@{type}].members.map(_.toString).toSet.filter(_.startsWith("method ")).map(_.substring(7)).filter(!_.startsWith("$")).toArray
   '
  grep(pattern, findMethods(), value=TRUE)
}

.DollarNames.ScalaInterpreterReference <- .DollarNames.ScalaCachedReference

.DollarNames.ScalaInterpreter <- function(x, pattern="") NULL

.DollarNames.ScalaInterpreterItem <- .DollarNames.ScalaInterpreter

