output$binom_shape <- renderPlot({
  vistributions::vdist_binom_plot(input$binom_n, input$binom_p)
})

output$bprob_plot <- renderPlot({
  if (input$bprob_tail != 'interval') {
    vistributions::vdist_binom_prob(input$bprob_n, input$bprob_p, input$bprob_s, input$bprob_tail)
  } else {
    vistributions::vdist_binom_prob(input$bprob_n, input$bprob_p,
      c(input$bprob_tail_1, input$bprob_tail_2), input$bprob_tail)
  }

})

output$bperc_plot <- renderPlot({
  vistributions::vdist_binom_perc(input$bperc_n, input$bperc_p, input$bperc_tp, input$bperc_tail)
})
