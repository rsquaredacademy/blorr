#' @importFrom checkmate check_class check_tibble check_data_frame check_number check_logical check_true check_choice
#' @importFrom rlang quo_name
blr_check_model <- function(model) {

	model_class  <- check_class(model, c("glm", "lm"))
	model_family <- check_true(family(model2)$family == "binomial")

	fmla <- deparse(formula(model))
	data <- deparse(model$call$data)

	if (model_class != TRUE) {

		cat("Hmmm.. Looks like you have specified an incorrect model. The below steps might be helpful:

* Check if you have used the", crayon::bold$blue("glm()"), "function to build the model.
* If you have never used it before, you can learn more by typing", crayon::bold$red("?glm"), "or", crayon::bold$red("help(glm)"), "in the Console.\n\n",

"Please specify the model in the below format:\n\n",

"glm(", fmla, ", data = ", data, ", family = binomial(link = 'logit'))\n\n",

"Happy modeling :)\n")

    stop("", call. = FALSE)

  }

  if (model_family != TRUE) {

  	cat("The error distribution is not", crayon::bold$blue("binomial"), ". Please specify the error distribution using the", crayon::bold$red("family"), "argument in", crayon::bold$blue("glm()"), ".\n",

"Please specify the model in the below format:",

"glm(", fmla, ", data = ", data, ", family = binomial(link = 'logit')",

"Happy modeling :)\n")

  }

}

blr_check_data <- function(data) {

  tib <- check_tibble(data)
  df  <- check_data_frame(data)
  data_class <- class(data)
  data_name <- deparse(substitute(data))

  if (tib != TRUE & df != TRUE) {

    cat(crayon::bold$red(data_name), "must either be a", crayon::bold$blue("data.frame"), "or a", crayon::bold$blue("tibble"), "but you have used a", crayon::bold$blue(data_class), "vector. Use the", crayon::bold$blue("class()"), "function to check the type of", crayon::bold$red(data_name), " as shown below:\n\n", crayon::bold$blue(paste0("class(", data_name, ")")), "\n\n If", crayon::bold$red(data_name), "is a column in a data set, use the name of the data set as the input.", "\n\n Type", crayon::bold$red("?data.frame"), "or", crayon::bold$red("?tibble"), "to learn how to create and use them.\n")

    stop("", call. = FALSE)
  }

}

blr_check_logic <- function(logic) {

  lval <- check_logical(logic)
  logic_class <- class(logic)
  logic_name <- deparse(substitute(logic))

  if (lval != TRUE) {

    cat("\n *", crayon::bold$red(logic_name), "can take only 2 values, either", crayon::bold$blue("TRUE"), "or", crayon::bold$blue("FALSE"), "\n * You have used", crayon::bold$red(logic), "\n * Replace",  crayon::bold$blue(logic), "with either", crayon::bold$blue("TRUE"), "or", crayon::bold$blue("FALSE"), "\n\n")

    stop("", call. = FALSE)

  }

}


blr_check_values <- function(value, lower, upper) {

  valid <- check_number(value, lower = lower, upper = upper)
  value_class <- class(value)
  value_name <- deparse(substitute(value))

  if (valid != TRUE) {

    cat("\n")
    cat(crayon::bold$red(value_name), "can take on values between", crayon::bold$red(lower) , "and", crayon::bold$red(upper), "only.", "You have used", paste0(crayon::bold$blue(value), ","), "please specify a value between", crayon::bold$red(lower) , "and", crayon::bold$red(upper), "only.", "\n\n")

    stop("", call. = FALSE)

  }

}

blr_check_npredictors <- function(model, min) {

  n <-
    model %>%
    coefficients() %>%
    length()

  if (n < min) {

    cat("\n")
    cat("Hello there.. the model contains only one predictor. For stepwise selection, please specify a model with at least 2 predictors. \n\n")

    stop("", call. = FALSE)

  }

}

blr_check_gtable <- function(table) {

	gclass <- check_class(table, "blr_gains_table")
	table_name <- deparse(substitute(table))
	table_class <- class(table)

	if (gclass != TRUE) {

		cat(crayon::bold$red(table_name), "is not the correct input. Use", crayon::bold$blue("blr_gains_table()"), "to create the input for this function.\n\n", "Type", crayon::bold$blue("?blr_gains_table"), "to learn more.\n\n")

		stop("", call. = FALSE)

	}

}

blr_check_varnames <- function(data, column) {

  data_name <- deparse(substitute(data))
  resp <- enquo(column)
  k <- check_choice(quo_name(resp), choices = names(data))

  if (k != TRUE) {

    cat("Uh oh...", crayon::bold$red(quo_name(resp)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

}



