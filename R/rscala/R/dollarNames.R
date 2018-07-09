#' @export
#' @importFrom utils .DollarNames
#'
.DollarNames.rscalaReference <- function(x, pattern="") {
  snippet <- '
    import reflect.runtime.{universe => ru}
    ru.typeOf[@{type}].members.map(_.toString).toSet.filter(_.startsWith("method ")).map(_.substring(7)).filter(!_.startsWith("$")).toArray
  '
  env <- attr(x,"rscalaReferenceEnvironment")
  snippet <- sub("@\\{type\\}",env[['type']],snippet)
  methods <- scalaInvoke(env[['details']], snippet, list())
  grep(pattern, methods, value=TRUE)
}
