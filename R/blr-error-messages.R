#' @importFrom stats family
blr_check_model <- function(model) {

	model_class  <- class(model)[1] == "glm"
	model_family <- isTRUE(family(model)$family == "binomial")

	fmla <- deparse(formula(model))
	data <- deparse(model$call$data)

	if (!model_class) {

		cat("Hmmm.. Looks like you have specified an incorrect model. The below steps might be helpful:

* Check if you have used the", "glm()", "function to build the model.
* If you have never used it before, you can learn more by typing", "?glm", "or", "help(glm)", "in the Console.\n\n",

"Please specify the model in the below format:\n\n",

"glm(", fmla, ", data = ", data, ", family = binomial(link = 'logit'))\n\n",

"Happy modeling :)\n")

    stop("", call. = FALSE)

  }

  if (!model_family) {

  	cat("The error distribution is not", "binomial", ". Please specify the error distribution using the", "family", "argument in", "glm()", ".\n",

"Please specify the model in the below format:",

"glm(", fmla, ", data = ", data, ", family = binomial(link = 'logit')",

"Happy modeling :)\n")

  }

}

blr_check_data <- function(data) {

  df         <- is.data.frame(data)
  data_class <- class(data)
  data_name  <- deparse(substitute(data))

  if (!df) {

    cat(data_name, "must either be a", "data.frame", "or a", "tibble", "but you have used a", data_class, "vector. Use the", "class()", "function to check the type of", data_name, " as shown below:\n\n", paste0("class(", data_name, ")"), "\n\n If", data_name, "is a column in a data set, use the name of the data set as the input.", "\n\n Type", "?data.frame", "or", "?tibble", "to learn how to create and use them.\n")

    stop("", call. = FALSE)
  }

}

blr_check_logic <- function(logic) {

  lval        <- is.logical(logic)
  logic_class <- class(logic)
  logic_name  <- deparse(substitute(logic))

  if (!lval) {

    cat("\n *", logic_name, "can take only 2 values, either", "TRUE", "or", "FALSE", "\n * You have used", logic, "\n * Replace",  logic, "with either", "TRUE", "or", "FALSE", "\n\n")

    stop("", call. = FALSE)

  }

}


blr_check_values <- function(value, lower, upper) {

  valid       <- (value >= lower & value <= upper)
  value_class <- class(value)
  value_name  <- deparse(substitute(value))

  if (!valid) {

    cat("\n")
    cat(value_name, "can take on values between", lower , "and", upper, "only.", "You have used", value, ",", "please specify a value between", lower , "and", upper, "only.", "\n\n")

    stop("", call. = FALSE)

  }

}

blr_check_npredictors <- function(model, min) {

  n <- length(coefficients(model))
    
  if (n < min) {

    cat("\n")
    cat("Hello there.. the model contains only one predictor. For stepwise selection, please specify a model with at least 2 predictors. \n\n")

    stop("", call. = FALSE)

  }

}

blr_check_gtable <- function(table) {

	gclass      <- class(table) == "blr_gains_table"
	table_name  <- deparse(substitute(table))
	table_class <- class(table)

	if (!gclass) {

		cat(table_name, "is not the correct input. Use", "blr_gains_table()", "to create the input for this function.\n\n", "Type", "?blr_gains_table", "to learn more.\n\n")

		stop("", call. = FALSE)

	}

}

blr_check_varnames <- function(data, column) {

  data_name <- deparse(substitute(data))
  resp      <- deparse(substitute(column))
  k         <- resp %in% names(data)

  if (!k) {

    cat("Uh oh...", resp, "is not a column in", data_name, ". Please check the column names using: \n\n", "* names()", "\n", "* colnames()", "\n\n")

    stop("", call. = FALSE)
  }

}



