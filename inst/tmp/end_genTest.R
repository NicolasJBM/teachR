


withProgress(message = "Making exam", value = 0, {
  
  choices <- tables$contentexam
  if (nrow(choices) > 45 & input$withscan) choices <- choices[1:45, ]
  
  # Save parameters accessible for question generation
  if (!is.null(input$typequest))
    type_quest <- input$typequest else type_quest <- "mcq"
    if (type_quest == "mcq") {
      choices <- dplyr::filter(choices, KD == "mcq" | KD == "both")
    } else if (type_quest == "open") {
      choices <- dplyr::filter(choices, KD == "open" | KD == "both")
    }
    
    if (!is.null(input$showquestid))
      show_question_id <- input$showquestid else show_question_id <- "Main"
      if (!is.null(input$showquestpoint))
        show_question_pt <- input$showquestpoint else show_question_pt <- FALSE
        
        
        
        
        
        
        
        
        
        
        
        
        dir.create("3_questions/web")
        
        # Define the kind of table
        if (input$platform %in% c("Web", "Blackboard", "Moodle"))
          type_table <- "html" else type_table <- "latex"
        currency <- input$currency
        alternatives <- input$alternatives
        
        save(type_quest, show_question_id, show_question_pt, choices,
             type_table, currency, alternatives,
             file = paste0(wd, "/parameters/exam_parameters.RData"))
        
        # Set paths to exercises and templates
        exercises <- paste0(find.package("questR"), "/rmd")
        templates <- paste0(find.package("questR"), "/tex")
        
        # Create the paths for selected questions
        suffix <- ".Rmd"
        choices <- choices %>%
          dplyr::mutate(paths = purrr::map(
            QN, function(x, suffix) paste0(x, suffix), suffix = suffix)
          )
        myexam <- choices$paths
        
        # Create the directory in which all files are generated
        ifelse(
          !dir.exists(file.path(wd, "tmp")),
          dir.create(file.path(wd, "tmp")), FALSE
        )
        tmpdir <- paste0(wd, "/tmp")
        unlink(paste0(tmpdir, "/*"))
        
        if (input$typequest == "mcq")
          stypequest <- "mcq" else stypequest <- "open"
        
        examid <- substring(gsub("-", "", input$datexam),3)
        
        
        # Generate the exam
        if (input$platform == "Web") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(
            test_or_solution,
            file = paste0(wd, "/parameters/test_or_solution.RData")
          )
          web <- exams::exams2html(
            myexam,
            n = 1,
            dir = "web",
            edir = exercises,
            tdir = tmpdir,
            name = paste0(input$name, "_", examid),
            mathjax = TRUE
          )
          
        } else if (input$platform == "Blackboard") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(
            test_or_solution,
            file = paste0(wd, "/parameters/test_or_solution.RData")
          )
          moodle <- exams::exams2blackboard(
            myexam,
            n = 1,
            dir = "blackboard",
            edir = exercises,
            tdir = tmpdir,
            zip = ifelse(Sys.info()[[1]] == "Windows", FALSE, TRUE),
            name = paste0(input$name, "_", examid)
          )
          
        } else if (input$platform == "Moodle") {
          incProgress(1 / 2, detail = "Generating...")
          test_or_solution = "test"
          save(
            test_or_solution,
            file = paste0(wd, "/parameters/test_or_solution.RData")
          )
          moodle <- exams2moodle(
            myexam,
            n = 1,
            dir = "moodle",
            edir = exercises,
            tdir = tmpdir,
            name = paste0(input$name, "_", examid)
          )
          
        } else {
          
          seed <- sample(c(1:9999), 1)
          steps <- 1 + input$versions
          
          incProgress(1 / steps, detail = "Preparing versions...")
          
          if (input$versions > 1) {
            numberblocs <- nrow(choices) / input$blocsize
            prepversions <- choices
            prepversions$bloc <- sort(rep(c(1:input$versions),input$blocsize))
            prepversions <- split(prepversions, prepversions$bloc)
            orderversions <- as.data.frame(t(gtools::permutations(numberblocs,numberblocs)))
            orderversions <- dplyr::select(orderversions, c("V1", sample(names(orderversions)[-1], (input$versions-1))))
            myexams <- list()
            for (i in 1:length(orderversions)){
              tmp <- prepversions[orderversions[,i]] %>%
                dplyr::bind_rows() %>%
                dplyr::mutate(version = i)
              myexams[[i]] <- tmp$paths
            }
            
          } else {
            myexams <- list(choices$paths)
          }
          
          for (i in 1:length(myexams)){
            
            incProgress((i+1) / steps, detail = paste0("Generating version ", i))
            
            # Set the exam id
            versionid <- paste0(
              examid,
              paste0(rep(0, (3 - nchar(as.character(i)))), collapse = ""),
              i
            )
            
            header <- list(Date = input$datexam, ID = versionid)
            
            test_or_solution = "test"
            save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
            
            set.seed(seed)
            exam <- exams::exams2pdf(
              myexams[[i]],
              n = 1,
              dir = "print",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("questions_", input$name, "_", versionid),
              header = header,
              points = choices$PT,
              template = paste0(templates, "/exam_", input$language, "_", input$typequest, "_", input$format, ".tex")
            )
            
            unlink(paste0(tmpdir, "/*"))
            
            test_or_solution = "solution"
            save(test_or_solution, file = paste0(wd, "/parameters/test_or_solution.RData"))
            
            set.seed(seed)
            solu <- exams::exams2pdf(
              myexams[[i]],
              n = 1,
              dir = "print",
              edir = exercises,
              tdir = tmpdir,
              texdir = tmpdir,
              name = paste0("solutions_", input$name, "_", versionid),
              header = header,
              template = paste0(templates, "/solution_", input$language, "_", input$typequest, "_", input$format, ".tex")
            )
            
            if (stypequest == "mcq" & input$withscan) {
              if (input$format == "A4") {
                teachR::make_scan_A4(
                  exam = exam,
                  name = versionid,
                  language = input$language,
                  title = input$title,
                  institution = input$institution,
                  date = input$datexam,
                  alternatives = input$alternatives,
                  reglength = input$reglength,
                  encoding = ""
                )
              }
            }
          }
          
        }
        
        exportexam <- choices %>%
          dplyr::select(-paths) %>%
          as.data.frame(stringsAsFactors = FALSE)
        
        utils::write.csv(exportexam, paste0("parameters/questions_", input$name, "_", examid, ".csv"), row.names = FALSE)
        
        if (input$versions > 1){
          exportblocs <- prepversions %>%
            dplyr::bind_rows() %>%
            dplyr::select(bloc, question = QN) %>%
            unique()
          
          utils::write.csv(exportblocs, paste0("parameters/blocs_", input$name, examid,".csv"), row.names = FALSE)
          utils::write.csv(orderversions, paste0("parameters/versions_", input$name, examid,".csv"), row.names = FALSE)
        }
        
})