#' @export
#' @importFrom utils .DollarNames
#'
.DollarNames.rscalaReference <- function(x, pattern="") {
  snippet <- '
    import reflect.runtime.{universe => ru}
    ru.typeOf[@{type}].members.map(_.toString).toSet.filter(_.startsWith("method ")).map(_.substring(7)).filter(!_.startsWith("$")).toArray
  '
  snippet <- sub("@\\{type\\}",x[['type']],snippet)
  methods <- scalaInvoke(x[['details']], snippet, list())
  grep(pattern, methods, value=TRUE)
}
