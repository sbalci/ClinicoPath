#' Table One
#'
#' @return Table
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#' @importFrom tableone CreateTableOne
#'


tableoneClass <- if (requireNamespace("jmvcore")) R6::R6Class("tableoneClass",
    inherit = tableoneBase, private = list(.run = function() {

        # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



        if (is.null(self$options$vars)) {

            # ToDo Message ----


            todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Table One, which is almost always used in clinicopathological research manuscripts.
                          <br><br>
                          Select the 'Variables' you want to include in the table. Numeric, Ordinal, and Categorical variables are allowed.
                          <br><br>
                          This tool uses tableone package. Please cite the packages and jamovi using references below.
                          "

            html <- self$results$todo
            html$setContent(todo)

        } else {

            todo <- ""
        html <- self$results$todo
        html$setContent(todo)


        # Prepare Data ----

            varsName <- self$options$vars

            data <- jmvcore::select(self$data, c(varsName))
            data <- jmvcore::naOmit(data)


            # Select Style ----

            sty <- self$options$sty

            if (sty == "t1") {

            # tableone ----

            mytable <- tableone::CreateTableOne(data = data)

            self$results$tablestyle1$setContent(mytable)


            } else if (sty == "t2") {


            # gtsummary ----

                mytable <- gtsummary::tbl_summary(data = data)
                mytable <- gtsummary::as_kable_extra(mytable)

                self$results$tablestyle2$setContent(mytable)


            }


            # Results ----



    }
    }
    ))
