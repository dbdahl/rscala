#' @export
#' @importFrom utils .DollarNames
#'
.DollarNames.rscalaReference <- function(x, pattern="") {
  # There is a bug that crashes the traditional Windows GUI.  Just bug-out in that case.
  if ( ( .Platform$OS.type == "windows" ) && ( .Platform$GUI != "RStudio" ) ) return(character(0))
  snippet <- '
    import reflect.runtime.{universe => ru}
    ru.typeOf[@{type}].members.map(_.toString).toSet.filter(_.startsWith("method ")).map(_.substring(7)).filter(!_.startsWith("$")).toArray
  '
  env <- attr(x,"rscalaReferenceEnvironment")
  snippet <- sub("@\\{type\\}",env[['type']],snippet)
  methods <- scalaInvoke(env[['details']], snippet, list(), parent.frame(1))
  grep(pattern, methods, value=TRUE)
}
