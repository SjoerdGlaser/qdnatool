# Get necessary packages
library(psych)
library(psy)
library(plyr)

Score <- function(key, input.answers, n.answer.op, item.names = NULL, category) {
  # Calculates standard psychometric properties of an exam, notably the
  # percentage correct and the item rest correlation (IRC) per item and
  # per answer option and the cronbach's alpha for the whole exam.
  #
  # Args:
  #   key: Matrix of 0's and 1's. key[i,j] implies wether answer option i
  #        to item j is right (1) or wrong (0). If a row (item) consists of
  #        only 0s, the item is interpreted as graded manually.
  #        Should be at least of length 3 (3 items), there is no maximum length.
  #   input.answers: Ungraded matrix of answers. input.answers[i,j] is
  #                  the answer of student (i) to item (j). Should consist of
  #                  at least 3 rows (items) and 2 columns (students).
  #                  Number of columns should be equal to the length as number
  #                  of columns of key and n.answer.op. There is no maximum.
  #   n.anwswer.op: Vector with number of answer options per item
  #                 Length should be equal to number of columns of key and
  #                 input.answers. There is no maximum.
  #   item.names: If desired, a categorical vector of the items names. If NULL,
  #               the items are names "Item 1", "Item 2", etc.
  #               Length should be equal to number of columns of key and
  #               input.answers. There is no maximum.
  #   category: vector of integers that indicate which item belongs to which
  #             category to do the same analysis for each sub category.
  #             If it consists of a vector of 1's, no subcategories are analyzed.
  #             Length should be equal to number of columns of key and
  #             input.answers. There is no maximum.
  # Returns:
  #   Either one list if no subcategories are present. Otherwise a list of lists
  #   is returned: one for the overall analysis and one for each sub category.
  #   Each lists consists of the following:
  #     n.stud: Integer, number of students
  #     n.item: Integer, number of items
  #     cronbach: Numeric, cronbach's alpha
  #     item.sum: Vector, with sum score per item
  #     key: As defined in input
  #     n.answer.op: As defined in input
  #     input.correct: Matrix[i,j] with the scored answer of student (j) to item
  #                    (i). 0 = wrong answer, 1 = right.
  #     item.perc: Vector of percentage correct per item
  #     item.tot.cor: Vector of Item Rest Correlation (IRC) per item
  #     freq.answer.op: Matrix[i,j] of frequency of students answering
  #                     option i to item j
  #            (only if any multiple choice items are present, else returns a 0)
  #     perc.answer.op: Matrix[i,j] of percentage of students answering
  #                     option i to item j
  #            (only if any multiple choice items are present, else returns a 0)
  #     answer.op.tot.cor: Matrix[i,j] IRC of answer option i to item j
  #            (only if any multiple choice items are present, else returns a 0)
  #   These arguments are equal for all lists. The first list also consists of:
  #     err: which categories has less than 3 items. Analyses are not ran.
  #     sub.cat: which categories have 3 or more items. Analyses are ran.

  # Create Correct/Incorrect Matrix
  if (is.null(item.names)){
    item.names <- paste("Item ",1:ncol(input.answers),sep = "")
  }
  colnames(input.answers) <- item.names
  input.correct <- input.answers

  # Count amount of items per category
  cat.items <- as.matrix(count(category))
  colnames(cat.items) <- c("category", "n.items")
  # save which subcategories to process
  sub.cat <- which(cat.items[,2] > 2, arr.ind = TRUE)
  # save for which categories to produce a warning
  err <- data.frame("category" = which(cat.items[,2] <= 2, arr.ind = TRUE))

  # Fill in Correct/Incorrect Matrix
  for (j in 1: ncol(input.answers)) {
    if (any(key[, j] != 0)) {
    # If no key is supplied for a question,
    # item is seen as manually graded and input.answers is used directly
      input.correct[, j] <- as.numeric(input.answers[, j] %in% which(key[, j] == 1))
    }
  }

  # Run the analysis
  results <- Analyse(key, input.answers, input.correct, n.answer.op)

  #If multiple categories are present, repeat the analysis for every category
  if(any(category > 1)){
    results <- list(results)
    for(c in 1:length(sub.cat)){
      sel <- which(category == sub.cat[c])
      results <- c(results, list(Analyse(matrix(key[, sel], ,length(sel)),
                                         matrix(input.answers[, sel], ,length(sel), dimnames = list(NULL, item.names[sel])),
                                         matrix(input.correct[, sel], , length(sel), dimnames = list(NULL, item.names[sel])),
                                         n.answer.op[sel]))
                  )
    }
  }
  results[[1]][[(length(results[[1]]) + 1)]] <- err
  names(results[[1]])[length(results[[1]])] <- "error"
  results[[1]][[(length(results[[1]]) + 1)]] <- sub.cat
  names(results[[1]])[length(results[[1]])] <- "subcats"
  return(results)
}

Analyse <- function(key, input.answers, input.correct, n.answer.op) {
  # Do the actual analyses as decribed above.
  # Args:
  #  Same as above
  #
  # Returns:
  #  The lists as described above

  n.stud <- nrow(input.answers)
  n.item <- ncol(input.answers)

  # Create frequency and percentage matrix for total scores
  item.sum <- colSums(input.correct)
  item.perc <- round(item.sum / n.stud * 100, digits = 1)

  # Calculate percentage per answer option. Only if any non 0s are present in key
  if(any(key != 0)) {
    freq.answer.op <- matrix(, max(n.answer.op) + 1, n.item)

    for (j in 1 : n.item){
      if (any(key[, j] != 0)){
        freq.answer.op[,j] <- table(factor(input.answers[, j],
                                           levels = 0:max(n.answer.op)))
      }
    }

    colnames(freq.answer.op) <- colnames(input.correct)
    rownames(freq.answer.op) <- c("Times_Answer_Missing",
                                  paste("Times", LETTERS[1:max(n.answer.op)],
                                        "answered", sep = "_"))

    # Percentage answered per answer option per questions
    perc.answer.op <- round(freq.answer.op / n.stud * 100, digits = 1)

  } else {
    freq.answer.op <- 0
    perc.answer.op <- 0
  }

  # Calculate corrected item tot correlation per item
  if (n.stud > 1 & n.item > 2){
     item.tot.cor <- numeric()
     suppressWarnings(
     # If no one or everyone answered an item correctly, R returns NA and a warning
       for (j in 1 : n.item) {
         item.tot.cor <- c(item.tot.cor, cor(input.correct[, j], rowSums(input.correct[, -j]) ))
       }
     )
    # Correct for when all students answered correctly or incorrectly
    item.tot.cor[is.na(item.tot.cor)] <- 0
    item.tot.cor <- round(item.tot.cor, digits = 3)
    names(item.tot.cor) <- colnames(input.correct)

    # Create frequency matrix and correct item total cor for each answer option
    # only if any non 0's are present in key
    if (any(key != 0)) {
      # Calculate corrected item total correlation per answer option
      answer.op.tot.cor <- matrix(, max(n.answer.op) + 1, n.item)

      suppressWarnings(
        for (i in 0:max(n.answer.op)) {
	        for (j in 1:n.item) {
            if (any(key[, j] != 0)) {
              answer.op.tot.cor[i + 1, j] <- round(cor(as.numeric(input.answers[, j] == i),
                                                   rowSums(input.correct[, -j])),
                                                   digits = 3)
              if (is.na(answer.op.tot.cor[i + 1, j])) {
               answer.op.tot.cor[i + 1, j] <- 0
				  		}
            } else {
              answer.op.tot.cor[i + 1, j] <- NA
            }
	        }
        }
      )

    rownames(answer.op.tot.cor) <- c("Times_Answer_Missing",
      	                             paste("Times", LETTERS[1:max(n.answer.op)],
                                     "answered", sep = "_"))
    colnames(answer.op.tot.cor) <- colnames(input.correct)
    }

    if (all(key == 0)) {
      answer.op.tot.cor <- 0
    }

		# Computes Cronbach's Alpha
		cronbach <- round(cronbach(input.correct)$alpha, digits = 3)

  } else { # If no enought items or students are present
    cronbach <- 0
    item.tot.cor <- 0
    answer.op.tot.cor <- 0

  }

  list(n.stud = n.stud, n.item = n.item, cronbach = cronbach,
       item.sum = item.sum, key = key, n.answer.op = n.answer.op,
       input.correct = input.correct, item.perc = item.perc,
       item.tot.cor = item.tot.cor, freq.answer.op = freq.answer.op,
       perc.answer.op = perc.answer.op, answer.op.tot.cor = answer.op.tot.cor)
}
