#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # -----------------------------------------------------------------------
    # Files submit  ---------------------------------------------------------
    
    # data dictionnary
    output$dictionnary <- renderDT({
      datatable(data_dictionnary, rownames = F)
    })
    
    # Upload
    raw <- reactive({
        req(input$file1)
        as.data.frame(openxlsx::read.xlsx(input$file1$datapath, 1, colNames = TRUE))#as.data.frame(read_excel(input$file1$datapath, 1))
    })
    
    # Clean 
    cleaned <- reactive({
        nom_c <- c("id", noms_colnames)
        data <- raw()
        data_clean <- data[, intersect(colnames(data), nom_c)]
        data_clean[, setdiff(intersect(colnames(data), nom_c), c('id', 'age.at.onset'))] <- apply(data_clean[, setdiff(intersect(colnames(data), nom_c), c('id', 'age.at.onset'))], 2, function(x) as.numeric(gsub(' ', '', x)))
        data_clean <- data_clean[complete.cases(data_clean), ]
        
        data_clean
    })
    
    # Results test DM 
    dm <- reactive({
        nom_c <- c("id", noms_colnames)
        erreurs <- NULL 
        data <- raw()
        data_clean <- cleaned()
        
        if(length(intersect(nom_c, colnames(data))) < length(nom_c)) erreurs <- rbind(erreurs, data.frame(x = ifelse(length(setdiff(nom_c, colnames(data))) == 1, 
                                                                                                                     paste0("The following column is missing : ", setdiff(nom_c, colnames(data))), 
                                                                                                                     paste0("The following columns are missing : ", paste(setdiff(nom_c, colnames(data)), collapse = ' + ')))))
        
        if(length(intersect(colnames(data), 'id')) == 1){
            if(sum(is.na(data$id))+sum(data$id %in% c('', ' ', NA)) > 0) erreurs <- rbind(erreurs,  data.frame(x = "Some subjects id are missing"))
            if(length(data$id) > length(unique(data$id))) erreurs <- rbind(erreurs, data.frame(x = paste0("Some subjects id are duplicated : ", paste(data$id[duplicated(data$id)], collapse = ' ; '))))
        }
        
        if(length(intersect(colnames(data), 'age.at.onset')) == 1){
            if(any(data$age.at.onset < 0 | data$age.at.onset > 125, na.rm = T)) erreurs <- rbind(erreurs, data.frame(x =  paste0("Age at onset not possible : ", paste(sort(unique(round(subset(data, age.at.onset < 0 | age.at.onset > 125)$age.at.onset, 1))), collapse = ' ; '))))
        }
        
        if(length(intersect(colnames(data), setdiff(nom_c, c('id', 'age.at.onset')))) >= 1){
            temp <- data[, intersect(colnames(data), setdiff(nom_c, c('id', 'age.at.onset')))]
            
            erreurs <- rbind(erreurs, do.call('rbind', lapply(1:ncol(temp), function(x){ 
                if(length(temp[, x][-which(as.numeric(gsub(' ', '', temp[, x])) %in% c(0, 1))]) > 0) data.frame( x = paste0('\t\tDifferent(s) value(s) from 0 and/or 1 for ', colnames(temp)[x], ' : ', paste(sort(unique(temp[, x][-which(as.numeric(gsub(' ', '', temp[, x])) %in% c(0, 1))] )), collapse = ' ; ')))
            })))
        }
        
        # data_clean <- data[, intersect(colnames(data), nom_c)]
        # data_clean[, setdiff(intersect(colnames(data), nom_c), c('id', 'age.at.onset'))] <- apply(data_clean[, setdiff(intersect(colnames(data), nom_c), c('id', 'age.at.onset'))], 2, function(x) as.numeric(gsub(' ', '', x)))
        # data_clean <- data_clean[complete.cases(data_clean), ]
        
        if(nrow(data_clean) < nrow(data)){ erreurs <- rbind(erreurs, data.frame(x = paste0(length(setdiff(data$id, data_clean$id)), ' subjects are deleted due to missing data / id : ', paste(setdiff(data$id, data_clean$id), collapse = ' ; '))))}
        
        erreurs
    })    
        
    # Res proba 
    data_proba <- reactive({
        erreurs <- dm()
        if(length(erreurs) == 0){
            data_proba <- cleaned()
            data_proba$probability <-  attr(predict(algo, newdata = data_proba[, noms_colnames], probability = TRUE), "probabilities")[, "1"]
            data_proba$group <- ifelse(data_proba$probability >= cutoff, "Medically seizure-free MTLE-HS", "Pharmaco-resistant MTLE-HS")
            data_proba[, c('id', 'group', 'probability', setdiff(colnames(data_proba), c('id', 'group', 'probability')))]
        }
    })
    
    # Cleanning data 
    output$contents <- renderDT({
        data <- raw()
        data_clean <- cleaned()
        erreurs <- dm()
        
        if(length(erreurs) == 0){
            # data$data_clean <- data_clean
            datatable(data.frame(Remarks = paste0('File uploaded without any errors. ', nrow(data_clean), ' subjects will be evaluated.')), rownames = F) %>% 
                formatStyle('Remarks', backgroundColor = styleEqual(c(paste0('File uploaded without any errors. ', nrow(data_clean), ' subjects will be evaluated.')), c('#99FF99')))
        }else{
            # rm(input$file1)
            datatable(setNames(rbind(erreurs, data.frame(x = c("", "Please change the datafile in order to meet the requested structure of the database and reload it."))), 'Remarks'), rownames = F)  %>% 
                formatStyle('Remarks', backgroundColor = styleEqual(c("Please change the datafile in order to meet the requested structure of the database and reload it."), c('#FF6633')))
            # req(input$file1)
        }
    })
    
    # proba data presentation
    output$probaData <- renderDT({
        erreurs <- dm()
        
        if(length(erreurs) == 0){
          datatable(data_proba(), rownames = F)
        }
    })
    
    # Download 
    output$downloadData_ImportFile <- renderUI({
        erreurs <- dm()
        if(length(erreurs) == 0){
            req(data_proba())
            downloadButton("dowload_allFile", "Click here to download data", class = "btn-primary")
        }
    })
    output$dowload_allFile <- downloadHandler(
        filename = function() {
            paste0("data_withResultsAlgo.csv")
        },
        content = function(file) {
            write.table(data_proba(), file, row.names = F, sep = ';')
        }
    )
    

    
    # plot proba
    myHeightAlgorithm <- reactive({
        erreurs <- dm()
        if(length(erreurs) == 0){
            data_proba <- data_proba()
            15*nrow(data_proba)
        }
    })

    output$Plot_proba_File <- renderPlot({
        erreurs <- dm()
        if(length(erreurs) == 0){
            data_proba <- data_proba()
            data_proba$id <- factor(data_proba$id, levels = data_proba$id[order(data_proba$probability)])
            sequence <- rev(seq(0, 1, by = 0.001))
            p <- ggplot(data.frame(value = rep(sequence, nrow(data_proba)), 
                              score = rep(data_proba$probability, each = length(sequence)), 
                              id = rep(data_proba$id, each = length(sequence))),
                   aes(y = id)) + 
                geom_bar(aes(x = value, col = value, fill = value), stat="identity", position ="identity") + 
                scale_fill_gradientn(colors = c(brewer.pal(3, "Set1")[1], "lightgrey", brewer.pal(3, "Set1")[3]), name = '',
                                     limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) +
                scale_colour_gradientn(colors = c(brewer.pal(3, "Set1")[1], "lightgrey", brewer.pal(3, "Set1")[3]), name = '',
                                       limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) +
                xlab('') + ylab('') + geom_vline(xintercept = cutoff, col = 'black') +
                geom_hline(aes(yintercept = id), linetype = 'dashed', col = '#666666') + 
                scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
                ggtitle("Cutoff") + guides(colour = 'none', fill = 'none') +
                geom_point(data = data_proba, aes(x = probability), col = 'blue', shape = 18, size = 10) +
                geom_point(data = data_proba, aes(x = probability), col = 'white', shape = 18, size = 5) +
                geom_point(data = data_proba, aes(x = probability), col = 'blue', size = 1) +
                theme(plot.title = element_text(hjust = cutoff, vjust = -1, size = 15, color = '#666666'), 
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=15)) + 
                geom_text(data = data_proba, aes(x = probability, label = round(probability, 2), hjust = ifelse(probability > 0.91, 1.6, -0.6)), color="blue", vjust = 0.5, size = 5, fontface=2)
            print(p)
        }
        
    }, height = myHeightAlgorithm, width = 600)
    
    
    # -----------------------------------------------------------------------
    # Individual data submit ---------------------------------------------------------
    
    observeEvent(input$aura, {
      if (input$aura == 0) {
        updateRadioButtons(inputId = "abdominal.aura", selected = 0)
        updateRadioButtons(inputId = "psycho.affective.aura", selected = 0)
        updateRadioButtons(inputId = "autonomic.aura", selected = 0)
        updateRadioButtons(inputId = "experiential.aura", selected = 0)
        updateRadioButtons(inputId = "visual.aura", selected = 0)
        updateRadioButtons(inputId = "sensory.aura", selected = 0)
        updateRadioButtons(inputId = "non.specific.aura", selected = 0)
                             
      }
      })
    
    # reactive expression
    text_reactive <- eventReactive(input$submit, {
        empty <- NULL
        if(is.null(input$age.at.onset)) empty <- c(empty, "age.at.onset")
        if(is.null(input$sex)) empty <- c(empty, "sex")
        if(is.null(input$prematurity)) empty <- c(empty, "prematurity")
        if(is.null(input$family.history.of.epilepsy)) empty <- c(empty, "family.history.of.epilepsy")
        if(is.null(input$febrile.convulsion)) empty <- c(empty, "febrile.convulsion")
        if(is.null(input$head.trauma)) empty <- c(empty, "head.trauma")
        if(is.null(input$meningitis.or.encephalitis)) empty <- c(empty, "meningitis.or.encephalitis")
        if(is.null(input$aura)) empty <- c(empty, "aura")
        if(is.null(input$abdominal.aura)) empty <- c(empty, "abdominal.aura")
        if(is.null(input$psycho.affective.aura)) empty <- c(empty, "psycho.affective.aura")
        if(is.null(input$autonomic.aura)) empty <- c(empty, "autonomic.aura")
        if(is.null(input$experiential.aura)) empty <- c(empty, "experiential.aura")
        if(is.null(input$visual.aura)) empty <- c(empty, "visual.aura")
        if(is.null(input$sensory.aura)) empty <- c(empty, "sensory.aura")
        if(is.null(input$non.specific.aura)) empty <- c(empty, "non.specific.aura")
        if(is.null(input$gestural.automatisms)) empty <- c(empty, "gestural.automatisms")
        if(is.null(input$oro.alimentary.automatisms)) empty <- c(empty, "oro.alimentary.automatisms")
        if(is.null(input$verbal.automatisms)) empty <- c(empty, "verbal.automatisms")
        if(is.null(input$dystonia.of.a.limb)) empty <- c(empty, "dystonia.of.a.limb")
        if(is.null(input$at.least.one.focal.to.bilateral.tonic.clonic.seizure)) empty <- c(empty, "at.least.one.focal.to.bilateral.tonic.clonic.seizure")
        empty
    })
    
    # create data at individual level
    data_indiv <- reactive({
        data_indiv <- data.frame(
            id = 1, 
            age.at.onset = as.numeric(input$age.at.onset), 
            sex = as.numeric(input$sex),
            prematurity = as.numeric(input$prematurity),
            family.history.of.epilepsy = as.numeric(input$family.history.of.epilepsy),
            febrile.convulsion = as.numeric(input$febrile.convulsion),
            head.trauma = as.numeric(input$head.trauma),
            meningitis.or.encephalitis = as.numeric(input$meningitis.or.encephalitis),
            aura =  as.numeric(input$aura),
            abdominal.aura =  as.numeric(input$abdominal.aura),
            psycho.affective.aura =  as.numeric(input$psycho.affective.aura),
            autonomic.aura =  as.numeric(input$autonomic.aura),
            experiential.aura =  as.numeric(input$experiential.aura),
            visual.aura =  as.numeric(input$visual.aura),
            sensory.aura =  as.numeric(input$sensory.aura),
            non.specific.aura =  as.numeric(input$non.specific.aura),
            gestural.automatisms =  as.numeric(input$gestural.automatisms),
            oro.alimentary.automatisms =  as.numeric(input$oro.alimentary.automatisms),
            verbal.automatisms =  as.numeric(input$verbal.automatisms),
            dystonia.of.a.limb =  as.numeric(input$dystonia.of.a.limb),
            at.least.one.focal.to.bilateral.tonic.clonic.seizure =  as.numeric(input$at.least.one.focal.to.bilateral.tonic.clonic.seizure))
        # data_indiv <- apply(data_indiv[,, drop = F], 2, as.numeric)
        data_indiv$probability <-  attr(predict(algo, newdata = data_indiv[, noms_colnames], probability = TRUE), "probabilities")[, "1"]
        data_indiv$group <- ifelse(data_indiv$probability >= cutoff, "Medically seizure-free MTLE-HS", "Pharmaco-resistant MTLE-HS")
        data_indiv
    })
    
    # text output
    output$text_submit <- renderText({
        empty <- text_reactive()
        if(length(empty) > 0){
            paste0("You forgot to select information for:\n\t", paste(dplyr::recode(empty, "age.at.onset" = "Age at onset of epilepsy", 
                                                                             "sex" = "Sex", 
                                                                             "prematurity" = "Prematurity or neonatal anoxia",  
                                                                             "family.history.of.epilepsy" = "Family history of epilepsy (febrile convulsion or epilepsy)", 
                                                                             "febrile.convulsion" = "Febrile convulsion (complicated or simple)", 
                                                                             "head.trauma" = "Head Trauma", 
                                                                             "meningitis.or.encephalitis" = "Meningitis or encephalitis", 
                                                                             "aura" = "Aura (all types)", 
                                                                             "abdominal.aura" = "Abdominal aura (rising epigastric sensation, nausea, abdominal pain, dysphagia)", 
                                                                             "psycho.affective.aura" = "Psycho affective aura (fear/anxiety, happiness/well-being, sadness, near death sensation)", 
                                                                             "autonomic.aura" = "Autonomic aura (rising feeling of warmth, thoracic constriction, tachycardia, thrill-feeling of cold, breathless, pallor/rubor, urge to urinate, sweating, thirst/hunger)", 
                                                                             "experiential.aura" = "Experiential aura (Deja vu/deja vecu, reviviscence, dreamy state, premonition)", 
                                                                             "visual.aura" = "Visual aura (light variation, blurring, illusion)", 
                                                                             "sensory.aura" = "Sensory aura (tingling, pain)", 
                                                                             "non.specific.aura" = "Non specific aura (indescribable, numbness, faintness, headache, feeling of shaking, stiffness)", 
                                                                             "gestural.automatisms" = "Gestural automatisms", 
                                                                             "oro.alimentary.automatisms" = "Oro alimentary automatisms", 
                                                                             "verbal.automatisms" = "Verbal automatisms", 
                                                                             "dystonia.of.a.limb" = "Dystonia of a limb", 
                                                                             "at.least.one.focal.to.bilateral.tonic.clonic.seizure" = "At least one focal to bilateral tonic clonic seizure"), 
                                                                      collapse = '  \n\t'), "\n\nPlease complete and re-submit.")
        }else{
            "Thank you, you selected all the information needed for computing."
        }
    })

    # text results proba
    output$text_resProba <- renderText({
        empty <- text_reactive()
        if(length(empty) == 0){
            data_indiv <- data_indiv()
            paste0("Probability of the patient to be Medically seizure-free MTLE-HS is: ", round(data_indiv$probability[1], 4), 
                   "\nCutoff is: ", round(cutoff, 4), 
                   "\n\n\tThe patient is: ", data_indiv$group[1]) 
      
        }
    })   
    
    # plot proba
    output$Plot_proba <- renderPlot({
        empty <- text_reactive()
        if(length(empty) == 0){
            data_indiv <- data_indiv()
            p <- ggplot(data.frame(value = rev(seq(0, 1, by = 0.001)), score = data_indiv$probability[1], sujet = data_indiv$id[1]),
               aes(x = 1)) + 
            geom_bar(aes(y = value, col = value, fill = value), stat="identity", position ="identity") + 
            scale_fill_gradientn(colors = c(brewer.pal(3, "Set1")[1], "lightgrey", brewer.pal(3, "Set1")[3]), name = '',
                                 limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) +
            scale_colour_gradientn(colors = c(brewer.pal(3, "Set1")[1], "lightgrey", brewer.pal(3, "Set1")[3]), name = '',
                                   limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) +
            xlab('') + ylab('') + geom_hline(yintercept = cutoff, linetype = 'dashed', col = '#666666') +
            theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), plot.title = element_text(hjust = cutoff, vjust = -1, size = 15, color = '#666666'), 
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=15)) +
            scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
            ggtitle("Cutoff") + guides(colour = 'none', fill = 'none') +
            geom_point(data = data.frame(score = data_indiv$probability[1], sujet = data_indiv$id[1]), aes(x = 1, y = score), col = 'blue', shape = 18, size = 10) +
            geom_point(data = data.frame(score = data_indiv$probability[1], sujet = data_indiv$id[1]), aes(x = 1, y = score), col = 'white', shape = 18, size = 5) +
            geom_point(data = data.frame(score = data_indiv$probability[1], sujet = data_indiv$id[1]), aes(x = 1, y = score), col = 'blue', size = 1) +
            annotate(geom="text", x=1, y=data_indiv$probability[1], label=round(data_indiv$probability[1], 2), color="blue", vjust = 0.5, size = 5, fontface=2,
                     hjust=ifelse(data_indiv$probability[1] > 0.91, 1.6, -0.6)) +
            coord_flip()
            print(p)
            
        }
        
        }, height = 200, width = 600)
        
    # Download 
    output$downloadData <- renderUI({
        empty <- text_reactive()
        if(length(empty) == 0){
            req(input$submit, data_indiv())
            downloadButton("dowload_individual", "Click here to download data", class = "btn-primary")
        }
    })
    output$dowload_individual <- downloadHandler(
            filename = function() {
                paste0("data_withResultsAlgo.csv")
            },
            content = function(file) {
                write.table(data_indiv(), file, row.names = F, sep = ';')
            }
    )
    
    # output$download <- downloadHandler(
    #     filename = function() {
    #         paste0(input$dataset, ".csv")
    #     },
    #     content = function(file) {
    #         write.csv(data(), file)
    #     }
    # )
    
    # algo <- readRDS(paste('data', 'algorithm.RDS', sep = '/'))
    # 
    # data_clean$prob <- attr(predict(algo, newdata = data_clean[, setdiff(nom_c, "id")], probability = TRUE), "probabilities")[, "1"]
    # data_clean$group <- ifelse(data_clean$prob >= 0.7617891, 'Medically seizure-free MTLE-HS', 'Pharmaco-resistant MTLE-HS')
    # 
    # cutoff <- 0.7617891
    # 
    # 
    # output$data_clean <- renderDT({
    #     data_clean
    # })
    
    # output$Plot_proba <- renderPlot({
    #     ggplot(data.frame(id = rep(data_clean$id, each = length(rev(seq(0, 1, by = 0.01)))), 
    #                       prob = rep(data_clean$prob, each = length(rev(seq(0, 1, by = 0.01)))), 
    #                       value = rev(seq(0, 1, by = 0.01))), 
    #            aes(x = 1)) + 
    #         geom_bar(aes(y = value, col = value, fill = value), stat="identity", position ="identity") + 
    #         facet_wrap(~id, ncol = 2) + 
    #         scale_fill_gradientn(colors = c(brewer.pal(3, "Set1")[3], "lightgrey", brewer.pal(3, "Set1")[1]), name = '', 
    #                              limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) + 
    #         scale_colour_gradientn(colors = c(brewer.pal(3, "Set1")[3], "lightgrey", brewer.pal(3, "Set1")[1]), name = '', 
    #                                limits = c(0,1), values = rescale(c(0, cutoff, 1)), breaks = c(0, cutoff, 1), labels=c("Minimum", paste0("Seuil=", cutoff), "Maximum")) + 
    #         xlab('') + ylab(id) + geom_hline(yintercept = cutoff, linetype = 'dashed', col = '#666666') + 
    #         theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), plot.title = element_text(hjust = cutoff+0.01, vjust = -1, size = 10, color = '#666666'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    #         scale_y_continuous(limits = c(0,1 ), expand = c(0, 0)) + 
    #         ggtitle("cutoff") + guides(colour = 'none', fill = 'none') + 
    #         geom_point(data = data.frame(score = data_clean$prob[id], sujet = data_clean$id[id]), aes(x = 1, y = score), col = 'blue', shape = 18, size = 10) + 
    #         geom_point(data = data.frame(score = data_clean$prob[id], sujet = data_clean$id[id]), aes(x = 1, y = score), col = 'white', shape = 18, size = 5) + 
    #         geom_point(data = data.frame(score = data_clean$prob[id], sujet = data_clean$id[id]), aes(x = 1, y = score), col = 'blue', size = 1) + 
    #         annotate(geom="text", x=1, y=data_clean$prob[id], label=round(data_clean$prob[id], 2), color="blue", vjust = 0.5, size = 5, fontface=2, 
    #                  hjust=ifelse(data_clean$prob[id] > 0.91, 1.6, -0.6)) + 
    #         coord_flip() 
    # })
})
    




