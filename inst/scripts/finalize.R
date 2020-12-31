alea <- sample(c(1, sample(2:length(questions), (alternatives-1))), alternatives)
questions <- questions[alea]
solutions <- solutions[alea]
explanations <- explanations[alea]