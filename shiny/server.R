library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_graf(input$obcina.vnos, input$vrsta)
  })
})

narisi_graf = function(obcina.vnos, vrsta){
  if (vrsta == "OŠ ali manj"){
    graf = ggplot(brezposelnost %>% filter(obcina == obcina.vnos, stopnja.izobrazbe == vrsta)) +
      aes(x = leto, y = stevilo.brezposelnih) +
      geom_col(position = "dodge", fill = "lightblue") +
      labs(
        x = "leto",
        y = "število brezposelnih",
        title = paste("Število brezposelnih po letih za občino:", obcina.vnos, sep = " ")
      )
    print(graf)
  } 
  else if (vrsta == "srednje poklicno izobraževanje"){
    graf = ggplot(brezposelnost %>% filter(obcina == obcina.vnos, stopnja.izobrazbe == vrsta)) +
      aes(x = leto, y = stevilo.brezposelnih) +
      geom_col(position = "dodge", fill = "lightblue") +
      labs(
        x = "leto",
        y = "število brezposelnih",
        title = paste("Število brezposelnih po letih za občino:", obcina.vnos, sep = " ")
      )
    print(graf)
  }
  else if (vrsta == "strokovno, splošno izobraževanje"){
    graf = ggplot(brezposelnost %>% filter(obcina == obcina.vnos, stopnja.izobrazbe == vrsta)) +
      aes(x = leto, y = stevilo.brezposelnih) +
      geom_col(position = "dodge", fill = "lightblue") +
      labs(
        x = "leto",
        y = "število brezposelnih",
        title = paste("Število brezposelnih po letih za občino:", obcina.vnos, sep = " ")
      )
    print(graf)
  }
  else if (vrsta == "izobraževanje prve, druge, tretje stopnje"){
    graf = ggplot(brezposelnost %>% filter(obcina == obcina.vnos, stopnja.izobrazbe == vrsta)) +
      aes(x = leto, y = stevilo.brezposelnih) +
      geom_col(position = "dodge", fill = "lightblue") +
      labs(
        x = "leto",
        y = "število brezposelnih",
        title = paste("Število brezposelnih po letih za občino:", obcina.vnos, sep = " ")
      )
    print(graf)
  }
  else if (vrsta == "Skupaj"){
    graf = ggplot(brezposelnost %>% filter(obcina == obcina.vnos, stopnja.izobrazbe == vrsta)) +
      aes(x = leto, y = stevilo.brezposelnih) +
      geom_col(position = "dodge", fill = "lightblue") +
      labs(
        x = "leto",
        y = "število brezposelnih",
        title = paste("Število brezposelnih po letih za občino:", obcina.vnos, sep = " ")
      )
    print(graf)
  }
  else {
    print("NAPAKA")
  }
  
}

