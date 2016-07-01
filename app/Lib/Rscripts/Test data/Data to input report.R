full <- Analyse(key, input.answers, number.answeroptions)
cronbach <- full[[1]]
correct.frequency <- full[[3]]
correct.percentage <- full[[4]]
corrected.item.tot.cor <- full[[5]]
frequency.answer.options <- full[[6]]
percentage.answer.options <- full[[7]]
corrected.item.tot.cor.answ.option <- full[[8]]
student.scores <- sample(2:ncol(key), 50, T) # Give student random scores
item.names <- paste("Item", 1:ncol(key))
title <- 'Random Test data'
file.name <- 'RandomFile'


c1 <- Analyse(matrix(key[, category == 1], ncol = sum(category == 1)),
	input.answers[, category == 1], number.answeroptions[category == 1])

c2 <- Analyse(matrix(key[, category == 2], ncol = sum(category == 2)),
	input.answers[, category == 2], number.answeroptions[category == 2])

categories = list(
	list(name = 'nameCat1',
		cronbach = c1[[1]],
		corrected.item.tot.cor = c1[[5]],
		corrected.item.tot.cor.answ.option = c1[[8]],
		items = which(category == 1),
		student.scores = sample(1:sum(category == 1), 50, T)
	),
	list(name = 'nameCat2',
		cronbach = c2[[1]],
		corrected.item.tot.cor = c2[[5]],
		corrected.item.tot.cor.answ.option = c2[[8]],
		items = which(category == 2),
		student.scores = sample(0:sum(category == 2), 50, T)
	))

#categories <- list()

rm(list =
		setdiff(ls(), c('cronbach', 'correct.frequency', 'correct.percentage', 'title',
			'corrected.item.tot.cor', 'frequency.answer.options', 'file.name',
			'percentage.answer.options', 'corrected.item.tot.cor.answ.option',
			'categories', 'key', 'number.answeroptions', 'student.scores', 'item.names'))
	)

save.image(file = '/Volumes/fmg-public/Psychologie/Onderwijsinstituut/Psychologie tweedejaarspracticumgroepen/Persoonlijke Folders/Sjoerd/qdnatool GIT - categories/app/Lib/Rscripts/Test data/Random data.RData')

load('/Volumes/fmg-public/Psychologie/Onderwijsinstituut/Psychologie tweedejaarspracticumgroepen/Persoonlijke Folders/Sjoerd/qdnatool GIT - categories/app/Lib/Rscripts/Test data/Random data.RData')
