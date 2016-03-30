fit <- glm(input$y ~ input$shocks+input$avoidances,family=binomial(link='logit'), data=input)
summary(fit)
