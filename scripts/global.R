<<<<<<< HEAD
shinyServer(function(input, output,session) {
  formulaText<-reactive({
    paste("Stars",input$Stars, input$Factors)
  })
  output$caption<-renderText({formulaText()})
  output$starsPlot <- renderPlot({
    if (input$Factors=="TV"){
      dodgepic<-ggplot(tv[which(tv$stars==input$Stars),], aes(x=city, y=cn,fill=TV)) +geom_bar(stat="identity",position=input$BarPlotType,width=0.7)+
        scale_fill_hue(c=45, l=70)+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14))}
    else if (input$Factors=="Noise"){ 
      dodgepic<-ggplot(n[which(n$stars==input$Stars),], aes(x=city, y=cn,fill=Noise)) +geom_bar(stat="identity",position=input$BarPlotType,width=0.7)+
        scale_fill_manual(values=c("#E69F00","#CC79A7","#56B4E9","#D55E00"))+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
                                                                                                                                              legend.title=element_text(size=14))}
    else if (input$Factors=="WiFi"){ 
      dodgepic<-ggplot(wf[which(wf$stars==input$Stars),], aes(x=city, y=cn,fill=WiFi)) +geom_bar(stat="identity",position=input$BarPlotType,width=0.7)+
        scale_fill_manual(values=c("#66CC99","#CC6666", "#9999CC"))+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14))}
    else if (input$Factors=="Happy_Hour"){ 
      dodgepic<-ggplot(h[which(h$stars==input$Stars),], aes(x=city, y=cn,fill=Happy_Hour)) +geom_bar(stat="identity",position=input$BarPlotType,width=0.7)+
        scale_fill_hue(c=45, l=70)+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14))}  
    else {
      dodgepic<-ggplot(p[which(p$stars==input$Stars),], aes(x=city, y=cn,fill=Price_Range)) +geom_bar(stat="identity",position=input$BarPlotType,width=0.7)+
        scale_fill_brewer()+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14))}    
    print(dodgepic)
  })
  output$reviewPlot<-renderPlot({
    revplot<-ggplot(rv[which(rv$stars==input$Stars),], aes(x=city, y=review_cn,fill=city)) +geom_bar(stat="identity")+coord_polar(theta='y')+
      theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())
    print(revplot)
    })
  output$reviewPlot1<-renderPlot({
    revplot1<-ggplot(rv[which(rv$stars==input$Stars),], aes(x=city, y=review_cn,fill=city)) +geom_bar(stat="identity",position="dodge",width=0.7)+
      scale_fill_discrete(name="review count")+scale_x_discrete("city")+scale_y_continuous("stars count")+theme(axis.text=element_text(size=14),legend.text=element_text(size=14),legend.title=element_text(size=14))
    print(revplot1)
  })  
  output$view<-renderDataTable({
    if (input$Factors=="TV"){tv[which(tv$stars==input$Stars),]}
    else if (input$Factors=="Noise"){n[which(n$stars==input$Stars),]}
    else if (input$Factors=="WiFi"){wf[which(wf$stars==input$Stars),]}
    else if (input$Factors=="Happy_Hour"){h[which(h$stars==input$Stars),]}
    else {p[which(p$stars==input$Stars),]}
  })
  output$view2<-renderDataTable({
    rv[which(rv$stars==input$Stars),]
  })
  output$table<-renderDataTable({
    new_data
  })
  output$view_rest<-renderDataTable({
    rest_rv[1:input$review_count,]
  })
 
  output$view_stars<-renderDataTable({
    if (input$city=="no"){
    test_fs<-filter(test_s,stars==input$Stars) ##parse star variable!
    names(test_fs)<-c("name","stars","stars_count")
    test_fs<-as.data.frame(test_fs)
    test_fstop<-arrange(test_fs,desc(stars_count))    
    }
    else if (input$city=="yes"){
      test_fs1<-filter(test_s1,stars==input$Stars) ##parse star variable!
      names(test_fs1)<-c("name","city","stars","stars_count")
      test_fs1<-as.data.frame(test_fs1)
      test_fstop<-arrange(test_fs1,desc(stars_count))    
    }
    print(test_fstop)
  })
  
  output$reviews_p<-renderPlot({
    t<-ggplot(review_plot, aes(x=stars, y=review)) +geom_bar(stat="identity",width=0.2,fill=I("#66CC99"))+
      theme(axis.text=element_text(size=14),legend.text=element_text(size=14))
  print(t)
    })  
  output$summary_r <- renderPrint({
    dataset1 <- group_by(review_plot,stars)
    s<-summarise(dataset1,sum(review))
    names(s)<-c("reviews","counts")
    print(s)
    })
  output$stars_p<-renderPlot({
    m <- qplot(stars,data=stars_plot,geom="histogram",fill=I("#CC6666"))
    print(m)
  })  
  output$summary_s <- renderPrint({
    dataset <- group_by(stars_plot,stars)    
    s1<-summarise(dataset,length(stars))
    names(s1)<-c("stars","counts")
    print(s1)}) 
  
  
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
})

=======
'__author__' ='Yang Liu(yl3296)'


library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
books <<- list("1Star" = "newt1",
               "2Star" = "newt2",
               "3Star" = "newt3",
               "4Star" = "newt4",
               "5Star" = "newt5")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
if (!(book %in% books))
    stop("Unknown book")
  
  a = read.table(sprintf("./%s.txt", book))
  idx = a$word
  a = a[,2]
  names(a) = idx
  sqrt(a)
  
  
})
>>>>>>> 32a5bdeb132f9df5691075cc085d8d67327bfcfc
