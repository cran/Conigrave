#' autoModel
#'
#' autoModel uses a genetic algorithm to optimize regression models for increased explained variance. Overly complicated models are penalized for adding additional regression terms in order to combat over-fitting.
#' @param data a data.frame or imputationList.
#' @param outcome the colname of the dependent variable.
#' @param genepool a vector. The genepool is the vector of variables names which shall be used to generate models. If not set, the genepool defaults to all variables in the supplied dataset other than the outcome variable.
#' @param extinction a numeric. The algorithm will stop when no improvement has been made for this number of generations.
#' @param children a numeric. The number of models to test in each generation.
#' @param penalty a numeric. Model fitness will be reduced by this number for each regression coefficient. This results in a handicap for overly complicated models.
#' @param samples a numeric. The number of sub-samples in which to test stability of r-squared.
#' @param include a vector of colnames which must be included as predictors in each model.
#' @param exclude a vector of colnames to be removed from the genepool.
#' @param set.seed a numeric. If this argument is provided, the algorithm will use the given seed in order to present reproducible results.
#' @examples autoModel(mtcars,"mpg",set.seed = 2)
#' @export autoModel
#' @importFrom mitools MIcombine imputationList
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble arrange desc
#' @importFrom stats runif
#' @importFrom utils capture.output txtProgressBar setTxtProgressBar
#' @importFrom stringr str_count str_split
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot aes geom_smooth geom_point theme_classic scale_size_continuous labs theme element_rect
#' @details 'autoModel' is a genetic algorithm which mutates regression models (predicting a specified outcome) in order to maximize r-squared (the explained variance).\cr \cr
#' The algorithm tests models at random. In each generation, it produces 'children' using the current best model as a seed. Each child of the previous winner will, on average, lose and gain a predictor. In each child, predictors have a smaller chance to gain or lose an interaction term.
#' Over successive generations selecting seeds with larger r-squares causes a drift towards models which explain more variance.\cr\cr
#' Without intervention this algorithm generates very complicated models, e.g. 15 way interactions, in which all variance is explained.
#' These overly-complicated models are almost certainly useless for explaining phenomenon outside of the training dataset.
#' Generally, these models do no more than describe the exact configuration of the dataset in which they evolved.
#' In order to deal with this situation, models are penalized for every predictor. This means that increased complexity will not be preferred unless it contributes substantially to the model's r-squared.\cr \cr
#' When the algorithm has failed to improve model fitness over many successive generations, it stops and returns the best model.
#' It also presents the history of all previous winners. The algorithm tests the stability of each of these winners on multiple sub-samples (75\% of rows with replacement).
#' Stability is equal to 1, minus the standard deviation of the r-squares in each sub sample, divided by the r-square statistic of the model in question.
#' Stability can range from 1 to negative values (if the standard deviation of sub-sample r-squares was larger than the model's r-squared).
#' @return A list containing a tibble with all the best models the algorithm found, the summary results of the best model, and a plot tracking the algorithms' performance.

autoModel <- function(data,
                      outcome,
                      genepool = NULL,
                      extinction = 30,
                      children = 20,
                      penalty = 0.03,
                      samples = 5,
                      include = c(),
                      exclude = c(),
                      set.seed = NULL) {
        debug = F
        verbose = F

        outcome = gsub(" ",".",outcome) #convert spaces to dots for tibble users.
        include = gsub(" ",".",include)
        exclude = gsub(" ",".",exclude)

        if(extinction < 5){
                stop("'extinction' must be set to 5 or more.")
        }

        if (!is.null(set.seed)) {
                set.seed(set.seed)
        }

        ###Set up variables for imputationList
        if (c("amelia") %in% class(data)) {
                data = imputationList(data$imputations)
        }

        if ("imputationList" %in% class(data)){
                imps = T
        } else{
                imps = F
                data = data.frame(data)
        }


        ##is genepool missing?
        if (is.null(genepool)) {
                if (imps == T) {
                        genepool = names(data$imputations[[1]])
                } else{
                        genepool = names(data)
                }

        }
        genepool = genepool[!genepool %in% c(outcome, exclude)] # remove outcome and exclude

        include_names = c(str_split(include, ",")) %>% unlist()

        check_names(x = c(genepool, outcome, include_names, exclude),
                    data = data)
        #stop if columns are non-numeric
        test_class = c(outcome, genepool)
        if (imps == T) {
                classes_non_num = lapply(test_class, function(x)
                        is.numeric(data$imputations[[1]][, x])) %>% unlist
        } else{
                classes_non_num = lapply(test_class, function(x)
                        is.numeric(data[, x])) %>% unlist
        }

        if (any(!classes_non_num)) {
                bad_classes = test_class[!classes_non_num]
                stop(paste0(
                        "The following variables are not numeric: ",
                        paste(bad_classes, collapse = ", ")
                ),
                call. = F)
        }


        if (imps == T) {
                # for(i in seq_along(data$imputations)){
                #         data$imputations[[i]] = data.frame(sapply(data$imputations[[i]],function(x){
                #         as.numeric(as.character(x))
                #         }))
                # } #this is too slow. have disabled for now until it can be sped up.

        } else{
                data = data.frame(sapply(data, function(x) {
                        as.numeric(as.character(x))
                }))
        }

        #########functions
        addChild = function(seed) {
                #function to generate children at random based on previous winner
                child = c()
                while (length(child) < 1) {
                        schance = (0.5 / length(seed)) #there is a large chance that seed will have a member removed
                        for (w in seq_along(seed)) {
                                if (runif(1) > schance) {
                                        # go through seed, on average, keep most of them
                                        child = append(child, seed[w])
                                }
                        }

                        for (g in seq_along(genepool)) {
                                if (runif(1) < (1 / length(genepool))) {
                                        #go through genepool, there is a small chance that one will be added to child
                                        child =  append(child, genepool[g])
                                }
                        }

                        for (g in seq_along(child)) {
                                #go along child
                                if (runif(1) < (0.35 / length(child))) {
                                        #if small chance, add a genepool to interactors
                                        tointeract = c(child[g],
                                                       sample(genepool, 1)) %>%
                                                str_split(",") %>% #split existing interactions
                                                unlist() %>%
                                                unique() #get rid of duplicates
                                        if (length(tointeract) > 1) {
                                                tointeract = tointeract[order(tointeract)] #order alphabetically

                                        }

                                        child[g] = paste(tointeract,
                                                         collapse = ",") #then collapse again at random
                                }

                        }
                        for (g in seq_along(child)) {
                                ###remove interaction at random
                                if ("," %in% str_split(child[g], "")[[1]]) {
                                        if (runif(1) < (1 / length(child))) {
                                                int = str_split(child[g], ",")[[1]]
                                                int = sample(int,
                                                             length(int) - 1)
                                                int = paste(int,
                                                            collapse = ",")
                                        }
                                }

                        }
                        if (length(include) > 0) {
                                child = append(child, include)
                        }
                        child = unlist(unique(na.omit(child)))
                }

                return(child[order(child)])
        }


        addFamily = function(seed) {
                kids = list()
                kids[[1]] = seed
                expiry = 0
                while (length(kids) < children &
                       expiry < children * 10) {
                        length = length(kids)
                        tryCatch({
                                newchild = addChild(seed)
                                if (!is.null(newchild)) {
                                        kids[[length(kids) + 1]] = newchild
                                }
                                kids = unique(kids)
                        })
                        if (length(kids) == length) {
                                expiry = expiry + 1
                        } else{
                                expiry = 0
                        }

                }
                kids
        }



        getResult = function(outcome, genes, data = data) {
                #function to evaluate randomly generated child variables

                string = paste(
                        "with(",
                        deparse(substitute(data)),
                        ",lm(",
                        "scale(",
                        outcome,
                        ")",
                        "~",
                        sep = ""
                )

                for (i in seq_along(genes)) {
                        if (!"," %in% str_split(genes[i], "")[[1]]) {
                                string = paste(string,
                                               "scale(",
                                               genes[i],
                                               ")",
                                               "+",
                                               sep = "")
                        } else{
                                commas = table(str_split(genes[i], "")[[1]])[","][[1]]
                                int.genes <-
                                        str_split(genes[i], ",")[[1]]
                                int = paste("scale(",
                                            int.genes[1],
                                            ")*",
                                            sep = "")
                                for (c in 2:length(int.genes)) {
                                        int = paste(int,
                                                    "scale(",
                                                    int.genes[c],
                                                    ")*",
                                                    sep = "")
                                }
                                int = substring(int, 1, nchar(int) - 1)
                                string = paste(string, int, "+", sep = "")

                        }

                } #with(data,summary(lm(scale(flourishing)~scale(ITC)+scale(SelfEsteem)))$r.squared)
                string = substring(string, 1, nchar(string) - 1)
                string = paste(string, "))", sep = "")

                if (class(data) == "imputationList") {
                        #print(outcome)
                        #print(genes)
                        stringr = paste(
                                "mean(unlist(with(data,summary(",
                                substring(string, 11),
                                "$adj.r.squared)))",
                                sep = ""
                        )
                        stringlm = paste("MIcombine(", string, ")", sep = "")
                        #print("get summary table")
                        #print(stringlm)
                        invisible(capture.output(result <-
                                                         summary(eval(
                                                                 parse(text = stringlm)
                                                         ))))
                        #print("get r squared")
                        result$adj.r.squared = eval(parse(text = stringr))
                } else{
                        result = summary(eval(parse(text = string)))
                }
                return(result)
        }

        cure_result = function(outcome,
                               genes,
                               data) {
                result = NULL
                tryCatch({
                        result <- getResult(outcome, genes, data)
                }, error = function(e) {

                })

                if (!is.null(result)) {
                        if (class(data) == "imputationList") {
                                length = length(result$results) - 1
                                r2 = result$adj.r.squared[[1]]
                        } else {
                                r2 =  result$adj.r.squared
                                length = length(result$aliased) - 1
                                if (all(!is.na(result))) {
                                        #get rid of results with NAs in model.
                                        if (any(
                                                !names(result$aliased) %in% rownames(
                                                        result$coefficients
                                                )
                                        )) {
                                                r2 = NA
                                                length = NA
                                        }

                                }
                        }
                } else{
                        r2 = NA
                        length = NA
                }
                fitness = r2 - penalty * length
                out = cbind(r2, length, fitness)
                return(out)
        }

        add_seed = function() {
                #this function keeps creating a starting winner until one is viable.
                winners = sample(genepool, 2)
                result = NULL
                expiry = 0
                while (is.null(result) & expiry < 1000) {
                        child = addChild(winners)
                        result = cure_result(outcome, child, data)
                        expiry = expiry + 1
                }

                if (expiry < 1000) {
                        return(winners)
                } else{
                        stop("After 1000 attempts, no viable regressions could be found.")
                }

        }

        ##engine of function:
        message(
                paste0(
                        "The algorithm will finish when it has failed to improve fitness for ",
                        extinction,
                        " consecutive generations."
                )
        )

        winners = add_seed() ##create initial winners
        result_history = list()
        expiry = 0
        old_result = cure_result(outcome, winners, data)[3]

        pb = txtProgressBar(min = 0,
                            max = extinction,
                            style = 3)
        while (expiry < extinction) {
                #print(expiry)
                results = list()
                kids = addFamily(winners)

                for (r in seq_along(kids)) {
                        #print(kids[[r]])
                        results[[length(results) + 1]] = cure_result(outcome, kids[[r]], data)
                }

                results = do.call(rbind, results)
                win_location = which.max(results[, 3])

                expiry = expiry + 1
                win_location = which.max(results[, 3])
                win_result = results[win_location, 3]
                if (!is.na(win_result)) {
                        if (is.na(old_result)) {
                                old_result = -1 #this helps reset system in case of few viable regressions being found
                        }
                        if (win_result > old_result) {
                                expiry = 0
                                winners = kids[[win_location]]
                                winners = winners[order(winners)] # alphabetize winners.
                                winners = winners[order(str_count(winners, ","))] # put interaction terms at end
                                if (verbose == T) {
                                        print(winners)
                                }
                                old_result = win_result

                        }
                }
                new_row = cbind(paste0(paste0(winners, collapse = "+")),
                                results[win_location, 1],
                                results[win_location, 2],
                                results[win_location, 3])
                result_history[[length(result_history) + 1]] = new_row
                setTxtProgressBar(pb, expiry)
        }
        close(pb)
        ##preparing results
        result_history = data.frame(do.call(rbind, result_history))
        names(result_history) = c("call", "R2", "length", "fitness")
        result_history$R2 = result_history$R2 %>%
                as.character() %>%
                as.numeric() %>%
                round(3)
        result_history$fitness = result_history$fitness %>%
                as.character() %>%
                as.numeric() %>%
                round(3)
        result_history$generation = seq_along(result_history$R2)
        message("assessing stability...")
        if (imps == T) {
                sample_data = lapply(data$imputations, function(z) {
                        lapply(1:samples, function(x) {
                                z[sample(seq_along(z[, 1]),
                                         round(nrow(z) * 0.75, 0),
                                         replace = T),]
                        }) %>% imputationList
                })

                result_history$stability = lapply(seq_along(result_history$call), function(x) {
                        call = as.character(result_history$call[x]) %>%
                                str_split("\\+")

                        r2 = lapply(seq_along(sample_data), function(y) {
                                cure_result(outcome, call[[1]], data = sample_data[[y]])[1]
                        }) %>% unlist

                        1 - (sd(r2) / result_history$R2[x])
                }) %>%
                        unlist %>%
                        round(3)

        } else{
                sample_data = lapply(1:samples, function(x) {
                        data[sample(seq_along(data[, 1]),
                                    round(nrow(data) * 0.75, 0),
                                    replace = T),]
                })

                result_history$stability = lapply(seq_along(result_history$call), function(x) {
                        call = as.character(result_history$call[x]) %>%
                                str_split("\\+")

                        r2 = lapply(seq_along(sample_data), function(y) {
                                cure_result(outcome, call[[1]], data = sample_data[[y]])[1]
                        }) %>% unlist

                        1 - (sd(r2) / result_history$R2[x])
                }) %>%
                        unlist %>%
                        round(3)
        }


        result_history$length = as.numeric(as.character(result_history$length))
        #set up NULL variables
        .=NULL
        generation = NULL
        R2=NULL
        fitness = NULL
        #plot
        plot <- ggplot(result_history, aes(generation, R2, size = length)) +
                geom_smooth(color = "black", fill = "#78909c",alpha =1, method = "loess", na.rm = T) + geom_point(color = "#e91e63",alpha = .9)  + theme_classic() +
                scale_size_continuous(range = c(0.1, 2)) + labs(x = "Generation", size = "Model\nlength") +
                theme(panel.background = element_rect(fill = "#eceff1"))

        result_history = as_tibble(result_history[,-5]) %>%
                unique %>%
                arrange(desc(fitness))
        result_history$call = paste0(outcome, "~", result_history$call) %>%
                gsub(",", "*", .)
        return(list(
                result_history = result_history[, c(1, 3, 4, 5, 2)],
                result = getResult(outcome, winners, data = data),
                plot = plot
        ))
}
