
# Helper functions ------------------------------------------------------------
get_phase_names <- function(filters) {
    if (is.null(names(filters)) | any(names(filters) == "")) {
        phase_names <- as.character(1:length(filters))
    } else {
        phase_names <- names(filters)
    }
    return(phase_names)
}

get_argument_names <- function(fun) {
    return(names(as.list(args(fun))))
}

# mk_phase_template <- function(filters){
mk_phase_template <- function(filters, crit_list){
    # Construct table
    phase_table_cols <- c("Phase", "Criteria", "Before(N)", "After(N)",
                          "Removed(N)")
    phase_out <- matrix("", nrow = length(filters),
                        ncol = length(phase_table_cols))
    phase_out <- as.data.frame(phase_out, stringsAsFactors = FALSE)
    colnames(phase_out) <- phase_table_cols

    # Record phase names
    phase_names <- get_phase_names(filters)
    phase_out[, "Phase"] <- phase_names

    # Record criteria for each phase
    sum_crit <- function(x) {
        out <- as.character(unlist(x))
        out[is.na(out)] <- "NA"
        out <- paste(out, collapse = "; ")
        out <- ifelse(out == "NA", NA, out)
        return(out)
    }
    phase_out[, "Criteria"] <- sapply(crit_list, sum_crit)
    return(phase_out)
}

mk_crit_template <- function(filters, crit_list) {
    # Create skeleton table
    crit_table_cols <- c("Phase", "Criteria",
                         "Excluded Complete(N)",
                         "Excluded In-phase(N)")
    crit_out <- matrix("",
                       nrow = length(unlist(filters)),
                       ncol = length(crit_table_cols))
    crit_out <- as.data.frame(crit_out, stringsAsFactors = FALSE)
    colnames(crit_out) <- crit_table_cols

    # Get phase names
    phase_names <- get_phase_names(filters)

    row_idx <- 1
    for(i in 1:length(filters)) {

        for(j in 1:length(filters[[i]])) {
            # Fill phase name
            crit_out[row_idx, "Phase"] <- phase_names[i]

            # Fill criteria name
            crit_out[row_idx, "Criteria"] <- crit_list[[i]][[j]]

            # Advance to the next row
            row_idx <- row_idx + 1
        }
    }
    return(crit_out)
}

# TODO come back to this and figure out how to implement the argument list
# TODO update references later in function to include an argument list
rec_filters <- function(filter_fun, data,  phase_idx, cume_crit_idx,
                        filter_desc, args_list = NULL) {

    # NOTE when adding functionality to be more specific with arguments -
        # consider
        # using names of args_list and names of functions being called to
        # specify to specify when and where to use arguments. This will be a
        # good place to subset args_list to a set of argument pertinent for
        # each function called by filter_data -- will have to figure out how to
        # extract or capture function names uniformly though. This might be
        # done in parallel with the sort of
        # list structures we've been making throughout other parts of this
        # function.
    # IDEA could also make it optional to provide argument names not tied
        # to individual functions to reduce the amount of coding one has to do.

    # Get all argument names from the input function
    arg_idx <- names(args_list) %in% get_argument_names(filter_fun)
    args_list <- args_list[arg_idx]

    # Set up a system of reference
    data[, ncol(data) + 1] <- 1:nrow(data)
    ref_idx <- 1:nrow(data)

    # Create a filtered dataset
    # filt_data <- filter_fun(data)
    filt_data <- do.call(filter_fun,
                         args = append(list(data),
                                       args_list))

    # 1. ------
    # Row IDs to remove (last column in data will match ref_idx)
    remove_idx <- setdiff(ref_idx, filt_data[, ncol(data)])

    # Check that filtering function has induced no other changes in data
    data <- data[-remove_idx, ]
    rownames(data) <- NULL
    rownames(filt_data) <- NULL
    if(!identical(data, filt_data)) {
        stop(paste0("Filtering function '", filter_desc,
                    "' produced an unexpected output. All functions in '...' ",
                    "should take 'data' as an argument and produce a similar ",
                    "output, but with the same or fewer rows"), call. = FALSE)
    }

    # 2. -------
    # Calc # to remove a) total dataset, b) phase-appropriate dataset
    n_tote <- length(remove_idx)
    n_phase <- length(setdiff(remove_idx, cume_crit_idx))

    # 3. -------
    # Update phase index
    phase_idx <- unique(c(phase_idx, remove_idx))

    return(list(n_tote = n_tote, n_phase = n_phase, phase_idx = phase_idx))
}

#' Apply and document inclusion/exclusion criteria
#'
#' This function is designed to assist in the application and documentation
#' of inclusion and exclusion criteria to clinical or epidemiologic datasets.
#' It allows the analyst to define a series of functions, each corresponding
#' to one criteria, and then apply them to the dataset.
#' It is expected that in many analyses certain inclusion/exclusion criteria
#' or groups of criteria may be applied sequentially. This function
#' allows analysts to specify filtering functions in \code{...} in the order
#' that they should be applied. The subsequent output of \code{filter_data}
#' is a \code{list} containing the newly filtered
#' dataset and a report of total observations removed for each criteria.
#'
#' The input of this function is an unfiltered dataset (\code{data}) and
#' a series of functions or list(s) of functions accepted by \code{...}. Each
#' function specified in \code{...} should apply one inclusion/exclusion
#' criteria to \code{data}. Functions specified in
#' \code{...} should take \code{data} as the input, remove observations (rows)
#' that failed to meet a certain inclusion criteria (or which meet a certain
#' exclusion criteria), and return a \code{data.frame} of the same or
#' fewer rows as \code{data}.
#'
#' Each functions may be supplied to \code{...} as either 1) a single
#' function; or 2) as a list of functions. Each argument captured by
#' \code{...} will be treated as a separte phase of the data-filtering process,
#' with each phase being applied sequentially to the dataset. This may affect
#' the number of observations listed as failing individual inclusion/exclusion
#' criteria within reports contained in \code{filter_data}'s output. It will
#' not affect the the filtered dataset generated in \code{filter_data}'s output.
#'
#' Several criteria may be applied simultaneously as part of a single filtering
#' "phase". To do this, multiple functions may be specified within a \code{list}
#' captured by \code{...}. Functions supplied directly to \code{...}
#' will be treated as their own individual phase in the data-filtering process.
#'
#' Descriptions of inclusion/exclusion criteria may be specified by supplying
#' functions to \code{...} as named arguments or named lists.
#'
#' @param data a \code{data.frame} containing a dataset to be filtered
#' @param ... functions or lists of functions to be applied to \code{data}. Each
#' function must be able to take \code{data} as an argument and remove
#' observations (rows) failing a user-defined exclusion criteria
#' @return A \code{list} containing the filtered dataset, a criteria-specific
#' report, and a phase-specific report
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(A = 1:10, B = LETTERS[1:10],
#'                    stringsAsFactors = FALSE)
#' filter_data(data,
#'             list(remove_A_equals_2 = function(x) x[x[, 1] != 2, ],
#'                  remove_A_equals_8 = function(x) x[x[, 1] != 8, ]),
#'             list(remove_B_equals_E = function(x) x[x[, 2] != "E", ]))
#' }
filter_data <- function(data, ..., args_list = NULL) {
    # TODO create ability to take function arguments within functions provided by ...
    # Convert ... into a list
    filters <- list(...)

    if(!is.data.frame(data)) {
        stop("'data' must be of class 'data.frame'", call. = FALSE)
    }

    # Check that all elements of ... are lists or functions
    if (!all(lapply(filters, class) %in% c("list", "function"))) {
        stop(paste0("All objects passed to '...' must be of class 'function' ",
                    "or 'list'"), call. = FALSE)
    }

    # Check that lists passed to ... contain only functions
    idx <- unlist(lapply(filters, is.list))
    if (sum(idx) != 0) {
        for (a_list in filters[idx]) {
            if(any(unlist(lapply(a_list, class)) != "function")) {
                stop(paste0("All lists passed to '...' ",
                            "must have a depth = 1 and be composed ",
                            "entirely of functions", call. = FALSE))
            }
        }
    }

    # Create a list of criteria names mirroring the structure of
    # ..., such that it may be referenced when building output tables
    limit_string <- function(x) {
        ifelse(nchar(x) < 38,
               x,
               paste0(substr(x, 1, 37), "..."))
    }
    uneval_filters <- eval(substitute(alist(...)))
    crit_list <- filters

    for(i in 1:length(filters)) {
        if (length(filters[[i]]) > 1) {
            for(j in 1:length(filters[[i]])) {
                if (is.null(names(filters[[i]])) ||
                    names(filters[[i]])[j] == "") {
                    crit_list[[i]][[j]] <-
                        limit_string(
                            as.character(as.list(uneval_filters[[i]])[-1])[j])
                } else {
                    crit_list[[i]][[j]] <- names(filters[[i]])[j]
                }
            }
        } else if (!is.null(names(filters)) && names(filters)[i] != "") {
            crit_list[[i]] <- names(filters)[i]
        } else {
            crit_list[[i]] <-
                limit_string(deparse(uneval_filters[[i]],
                                     width.cutoff = 40, nlines = 1))
        }
    }

    # Create output table templates
    phase_table <- mk_phase_template(filters, crit_list)
    crit_table <- mk_crit_template(filters, crit_list)

    ############### Create starting indexes ##############
    # Create empty index for all records to be eliminated
    cume_crit_idx <- NULL
    # Start editing out on first row
    out_row_idx <- 1

    # Phase level ###########################################################
    for (i in 1:length(filters)) {

        # Create/reset phase_idx
        phase_idx <- NULL

        # Criterium level #####################################################
        if (is.list(filters[[i]])) { # For phase with multiple criteria #######
            for (j in 1:length(filters[[i]])) {

                # Update phase_idx and calc N entries removed from
                # total dataset and phase-specific data
                crit_res <- rec_filters(filters[[i]][[j]], data, phase_idx,
                                        cume_crit_idx, crit_list[[i]][[j]],
                                        args_list = args_list)

                # Update phase index
                phase_idx <- crit_res[["phase_idx"]]
                # Update criteria table
                crit_table[out_row_idx, 3] <- crit_res[["n_tote"]]
                crit_table[out_row_idx, 4] <- crit_res[["n_phase"]]

                out_row_idx <- out_row_idx + 1

            }
        } else { # For phase with single criteria ############################

            # Update phase_idx and calc N entries removed from
            # total dataset and phase-specific data
            crit_res <- rec_filters(filters[[i]], data, phase_idx,
                                    cume_crit_idx, crit_list[[i]],
                                    args_list = args_list)

            # Update phase index
            phase_idx <- crit_res[["phase_idx"]]

            # Update criteria table
            crit_table[out_row_idx, 3] <- crit_res[["n_tote"]]
            crit_table[out_row_idx, 4] <- crit_res[["n_phase"]]

            # Update which row we are recording on
            out_row_idx <- out_row_idx + 1

        } # End Criteria Level ################################################

        # Record the number of entries remaining before the data is filtered
        # during this phase
        phase_table[i, 3] <- length(setdiff(1:nrow(data), cume_crit_idx))


        # Update list of all observations that should be excluded
        cume_crit_idx <- unique(c(cume_crit_idx, phase_idx))

        # Record the number of entries after this phase
        phase_table[i, 4] <- length(setdiff(1:nrow(data), cume_crit_idx))

        # Record the total number of entries removed in this phase
        phase_table[i, 5] <- as.numeric(phase_table[i, 3]) -
            as.numeric(phase_table[i, 4])
    }

    out <- list(data = data[-cume_crit_idx, ],
                crit_table = crit_table,
                phase_table = phase_table)
    return(out)
}
