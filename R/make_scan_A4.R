#' Simplification of the function exams2nops designed by Zeileis et al. to produce only the answer sheet and database.
#' @param exam Output of the function exams2pdf.
#' @param name character. A name prefix for resulting exams and RDS file.
#' @param language character. Path to a DCF file with a language specification. Currently, the package ships: English ("en"), Dutch ("nl"), French ("fr"), German ("de"), Hungarian ("hu"), Italian ("it"), Romanian ("ro"), Portuguese ("pt"), Spanish ("es").
#' @param title character. Title of the exam, e.g., "Introduction to Statistics".
#' @param institution character. Name of the institution at which the exam is conducted.
#' @param date character or "Date" object specifying the date of the exam.
#' @param startid integer. Starting ID for the exam numbers (defaults to 1).
#' @param alternatives integer. Number of boxes.
#' @param reglength integer. Number of digits in the registration ID. The default is 7 and it can be increased up to 10.
#' @param encoding character. encoding
#' @return Answer sheet for the exam and database of solutions.
#' @importFrom tools texi2dvi
#' @importFrom stats na.omit
#' @importFrom utils data
#' @references Zeileis, A., N. Umlauf, and F. Leisch. 2014. Flexible Generation of E-Learning Exams in R: Moodle Quizzes, OLAT Assessments, and Beyond. Journal of Statistical Software 58 (1).
#' @export

make_scan_A4 <- function(exam,
                                 name = "scan",
                                 language = "en",
                                 title = "Title of the exam",
                                 institution = "Academic Institution",
                                 date = "2018-05-10",
                                 startid = 1,
                                 alternatives = 5,
                                 reglength = 7L,
                                 encoding = "") {
  file <- paste0("tmp/", name, ".tex")
  saveRDS(exam, file = file.path(paste0("answers/", name, ".rds")))

  ## header: date, id, usepackage
  Date2ID <- function(date) function(i) paste(format(date, "%y%m%d"),
        formatC(i + startid - 1L,
          width = 5,
          flag = 0,
          format = "f",
          digits = 0
        ),
        sep = ""
      )
  if (!inherits(date, "Date")) date <- as.Date(date)
  d2id <- Date2ID(date)

  ## header: localization (titles, logos, etc.)
  loc <- list(
    nopsinstitution = institution,
    nopstitle = title
  )
  ## header: internationalization
  language <- system.file(file.path("nops", paste0(language, ".dcf")), package = "exams")
  lang <- nops_language(language, markup = "latex")[c(
    "PersonalData", "FamilyName", "GivenName", "Signature", "RegistrationNumber",
    "Checked", "NoChanges", "DocumentType", "DocumentID", "Scrambling",
    "Replacement", "MarkCarefully", "NotMarked", "Or",
    "MarkExampleA", "MarkExampleB", "MarkExampleC", "MarkExampleD", "MarkExampleE",
    "Warning", "Answers", "FillAnswers", "Point", "Points"
  )]
  ## header: collect everything
  header <- c(list(Date = date, ID = d2id), lang)

  page1 <- make_nops_page(
    n = length(names(exam[[1]])),
    alternatives = alternatives,
    reglength = reglength
  )

  ## number of additional units in registration ID
  addreg <- pmin(3L, pmax(0L, reglength - 7L))


  ## encoding
  enc <- gsub("-", "", tolower(encoding), fixed = TRUE)
  if (enc %in% c("iso8859", "iso88591")) enc <- "latin1"
  if (enc == "iso885915") enc <- "latin9"

  rval <- c(
    "
    \\documentclass[10pt,a4paper]{article}
    \\usepackage{graphicx,color}
    \\usepackage{amsmath,amssymb,latexsym}
    \\usepackage{verbatim,url,fancyvrb,ae}
    \\usepackage{multicol,a4wide,pdfpages}
    \\IfFileExists{sfmath.sty}{
    \\RequirePackage{sfmath}
    }{}
    
    \\DefineVerbatimEnvironment{Sinput}{Verbatim}{fontshape=sl}
    \\DefineVerbatimEnvironment{Soutput}{Verbatim}{}
    \\DefineVerbatimEnvironment{Scode}{Verbatim}{fontshape=sl}
    \\newenvironment{Schunk}{}{}
    
    \\usepackage[T1]{fontenc}",
    if (enc != "") sprintf("\\usepackage[%s]{inputenc}", enc) else NULL,
    "
    \\renewcommand{\\rmdefault}{phv}
    \\renewcommand{\\sfdefault}{phv}
    
    \\setlength{\\parskip}{0.7ex plus0.1ex minus0.1ex}
    \\setlength{\\parindent}{0em}
    \\setlength{\\textheight}{29.6cm} 
    \\setlength{\\oddsidemargin}{-2.54cm} 
    \\setlength{\\evensidemargin}{-2.54cm} 
    \\setlength{\\topmargin}{-2.54cm} 
    \\setlength{\\headheight}{0cm} 
    \\setlength{\\headsep}{0cm} 
    \\setlength{\\footskip}{0cm} 
    \\setlength{\\unitlength}{1mm} 
    \\usepackage{chngpage}
    
    %% to support different lengths of registration numbers
    \\newif\\ifregseven
    \\newif\\ifregeight
    \\newif\\ifregnine
    \\newif\\ifregten
    ",
    sprintf("\\reg%s%s", c("seven", "eight", "nine", "ten"), tolower(0L:3L == addreg)),
    "
    \\ifregseven
    \\def\\namecenter{72.5}
    \\def\\namewidth{105}
    \\def\\namechecked{123}
    \\def\\nameline{90}
    \\def\\regcenter{159}
    \\def\\regleft{131}
    \\def\\regleftt{139}
    \\def\\regleftb{133}
    \\def\\regleftn{129}
    \\def\\regwidth{56}
    \\def\\regwidthn{60}
    \\def\\regnum{7}
    \\def\\regnumt{6}
    \\fi
    
    \\ifregeight
    \\def\\namecenter{65.0}
    \\def\\namewidth{90}
    \\def\\namechecked{108}
    \\def\\nameline{90}
    \\def\\regcenter{155}
    \\def\\regleft{123}
    \\def\\regleftt{131}
    \\def\\regleftb{125}
    \\def\\regleftn{121}
    \\def\\regwidth{64}
    \\def\\regwidthn{68}
    \\def\\regnum{8}
    \\def\\regnumt{7}
    \\fi
    
    \\ifregnine
    \\def\\namecenter{62.5}
    \\def\\namewidth{85}
    \\def\\namechecked{103}
    \\def\\nameline{85}
    \\def\\regcenter{151}
    \\def\\regleft{115}
    \\def\\regleftt{123}
    \\def\\regleftb{117}
    \\def\\regleftn{113}
    \\def\\regwidth{72}
    \\def\\regwidthn{76}
    \\def\\regnum{9}
    \\def\\regnumt{8}
    \\fi
    
    \\ifregten
    \\def\\namecenter{60.0}
    \\def\\namewidth{80}
    \\def\\namechecked{98}
    \\def\\nameline{80}
    \\def\\regcenter{147}
    \\def\\regleft{107}
    \\def\\regleftt{115}
    \\def\\regleftb{109}
    \\def\\regleftn{105}
    \\def\\regwidth{80}
    \\def\\regwidthn{84}
    \\def\\regnum{10}
    \\def\\regnumt{9}
    \\fi
    
    %% for exams2pdf
    \\newenvironment{question}{\\item}{}
    \\newenvironment{solution}{\\comment}{\\endcomment}
    \\newenvironment{answerlist}{\\renewcommand{\\labelenumi}{(\\alph{enumi})}\\begin{samepage}\\begin{enumerate}}{\\end{enumerate}\\end{samepage}}
    %% additional header commands
    \\makeatletter
    \\newcommand{\\ID}[1]{\\def\\@ID{#1}}
    \\newcommand{\\Date}[1]{\\def\\@Date{#1}}
    %
    \\newcommand{\\nopsinstitution}[1]{\\def\\@nopsinstitution{#1}}
    \\newcommand{\\nopstitle}[1]{\\def\\@nopstitle{#1}}
    %
    \\newcommand{\\PersonalData}[1]{\\def\\@PersonalData{#1}}
    \\newcommand{\\FamilyName}[1]{\\def\\@FamilyName{#1}}
    \\newcommand{\\GivenName}[1]{\\def\\@GivenName{#1}}
    \\newcommand{\\Signature}[1]{\\def\\@Signature{#1}}
    \\newcommand{\\RegistrationNumber}[1]{\\def\\@RegistrationNumber{#1}}
    \\newcommand{\\Checked}[1]{\\def\\@Checked{#1}}
    \\newcommand{\\NoChanges}[1]{\\def\\@NoChanges{#1}}
    \\newcommand{\\DocumentType}[1]{\\def\\@DocumentType{#1}}
    \\newcommand{\\DocumentID}[1]{\\def\\@DocumentID{#1}}
    \\newcommand{\\Scrambling}[1]{\\def\\@Scrambling{#1}}
    \\newcommand{\\Replacement}[1]{\\def\\@Replacement{#1}}
    \\newcommand{\\MarkCarefully}[1]{\\def\\@MarkCarefully{#1}}
    \\newcommand{\\NotMarked}[1]{\\def\\@NotMarked{#1}}
    \\newcommand{\\Or}[1]{\\def\\@Or{#1}}
    \\newcommand{\\MarkExampleA}[1]{\\def\\@MarkExampleA{#1}}
    \\newcommand{\\MarkExampleB}[1]{\\def\\@MarkExampleB{#1}}
    \\newcommand{\\MarkExampleC}[1]{\\def\\@MarkExampleC{#1}}
    \\newcommand{\\MarkExampleD}[1]{\\def\\@MarkExampleD{#1}}
    \\newcommand{\\MarkExampleE}[1]{\\def\\@MarkExampleE{#1}}
    \\newcommand{\\Warning}[1]{\\def\\@Warning{#1}}
    \\newcommand{\\Answers}[1]{\\def\\@Answers{#1}}
    \\newcommand{\\FillAnswers}[1]{\\def\\@FillAnswers{#1}}
    \\newcommand{\\Point}[1]{\\def\\@Point{#1}}
    \\newcommand{\\Points}[1]{\\def\\@Points{#1}}",
    sprintf(paste0("\\ID{", name, "}")),
    sprintf(paste0("\\Date{", date, "}")),
    sprintf(paste0("\\nopsinstitution{", institution, "}")),
    sprintf(paste0("\\nopstitle{", title, "}")),
    "
    \\PersonalData{Personal Data}
    \\FamilyName{Family Name}
    \\GivenName{Given Name}
    \\Signature{Signature}
    \\RegistrationNumber{Registration Number}
    \\Checked{checked}
    \\NoChanges{In this section no modifications of the data must be made!}
    \\DocumentType{Type}
    \\DocumentID{Exam ID}
    \\Scrambling{Scrambling}
    \\Replacement{Replacement}
    \\MarkCarefully{Please mark the boxes carefully}
    \\NotMarked{Not marked}
    \\Or{or}
    \\MarkExampleA{72}
    \\MarkExampleB{80}
    \\MarkExampleC{102}
    \\MarkExampleD{109}
    \\MarkExampleE{115}
    \\Warning{This document is scanned automatically. Please keep clean and do not bend or fold. For filling in the document please use a \\textbf{blue or black pen}. \\\\ \\textbf{Only clearly marked and positionally accurate crosses will be processed!}}
    \\Answers{Answers}
    \\FillAnswers{In the following please fill in your answers.}
    \\Point{point}
    \\Points{points}
    
    %% \\exinput{header}
    
    \\newcommand{\\myID}{\\@ID}
    \\newcommand{\\myDate}{\\@Date}
    %
    \\newcommand{\\myinstitution}{\\@nopsinstitution}
    \\newcommand{\\mytitle}{\\@nopstitle}
    %
    \\newcommand{\\myPersonalData}{\\@PersonalData}
    \\newcommand{\\myFamilyName}{\\@FamilyName}
    \\newcommand{\\myGivenName}{\\@GivenName}
    \\newcommand{\\mySignature}{\\@Signature}
    \\newcommand{\\myRegistrationNumber}{\\@RegistrationNumber}
    \\newcommand{\\myChecked}{\\@Checked}
    \\newcommand{\\myNoChanges}{\\@NoChanges}
    \\newcommand{\\myDocumentType}{\\@DocumentType}
    \\newcommand{\\myDocumentID}{\\@DocumentID}
    \\newcommand{\\myScrambling}{\\@Scrambling}
    \\newcommand{\\myReplacement}{\\@Replacement}
    \\newcommand{\\myMarkCarefully}{\\@MarkCarefully}
    \\newcommand{\\myNotMarked}{\\@NotMarked}
    \\newcommand{\\myOr}{\\@Or}
    \\newcommand{\\myMarkExampleA}{\\@MarkExampleA}
    \\newcommand{\\myMarkExampleB}{\\@MarkExampleB}
    \\newcommand{\\myMarkExampleC}{\\@MarkExampleC}
    \\newcommand{\\myMarkExampleD}{\\@MarkExampleD}
    \\newcommand{\\myMarkExampleE}{\\@MarkExampleE}
    \\newcommand{\\myWarning}{\\@Warning}
    \\newcommand{\\myAnswers}{\\@Answers}
    \\newcommand{\\myFillAnswers}{\\@FillAnswers}
    \\newcommand{\\myPoint}{\\@Point}
    \\newcommand{\\myPoints}{\\@Points}
    
    \\makeatother
    
    \\markboth{\\textsf{{\\mytitle}: {\\myID}}}{\\textsf{{\\mytitle}: {\\myID}}}
    \\pagestyle{myheadings}
    \\begin{document} 
    ",
    page1,
    "

\\setlength{\\textheight}{24cm} 
\\newpage

\\setcounter{page}{1}

\\setlength{\\oddsidemargin}{0cm} 
\\setlength{\\evensidemargin}{0cm} 
\\setlength{\\topmargin}{0cm} 
\\setlength{\\headheight}{0cm} 
\\setlength{\\headsep}{1cm} 
\\setlength{\\footskip}{1cm}
\\end{document}
"
  )

  writeLines(rval, file)

  tmpwd <- getwd()
  setwd(paste0(tmpwd, "/questions"))

  texi2dvi(paste0("../", file),
    pdf = TRUE,
    clean = TRUE,
    quiet = TRUE,
    texi2dvi = getOption("texi2dvi")
  )

  invisible(rval)
  setwd(tmpwd)
}



make_nops_page <- function(n,
                           replacement = FALSE,
                           alternatives = 5,
                           reglength = 7L) {
  addreg <- pmin(3L, pmax(0L, reglength - 7L))
  mytype <- if (addreg < 1L) {
    ## the number of questions rounded up in steps of 5
    ## (needed for uibk scanning services)
    formatC(5 * ((n - 1) %/% 5 + 1), width = 3, flag = "0")
  } else {
    ## add prefix coding number of additional registration ID units plus replacement
    paste0(addreg + (replacement * 3L), formatC(5 * ((n - 1) %/% 5 + 1), width = 2, flag = "0"))
  }

  ## number of alternative choices
  alternatives <- rep(alternatives, length.out = n)

  ## helper function for abcde labels
  abcde <- function(i, above = FALSE, alternatives = alternatives) {
    ix <- (i - 1) %/% 15
    iy <- (i - 1) %% 15 + 1
    ix <- 19 + 64 * ix - as.numeric(ix >= 2) * 4
    iy <- 129 - 7 * iy - 3 * ((iy - 1) %/% 5) + above * 10
    alternatives <- max(alternatives)
    if (alternatives == 5) {
      sprintf(
        paste("\\put(%i,%i){\\makebox(0,0)[b]{\\textsf{", letters[1:5], "}}}", sep = "", collapse = "\n"),
        ix + 1 * 8, iy, ix + 2 * 8, iy, ix + 3 * 8, iy, ix + 4 * 8, iy, ix + 5 * 8, iy
      )
    } else if (alternatives == 4) {
      sprintf(
        paste("\\put(%i,%i){\\makebox(0,0)[b]{\\textsf{", letters[1:4], "}}}", sep = "", collapse = "\n"),
        ix + 1 * 8, iy, ix + 2 * 8, iy, ix + 3 * 8, iy, ix + 4 * 8, iy
      )
    } else if (alternatives == 3) {
      sprintf(
        paste("\\put(%i,%i){\\makebox(0,0)[b]{\\textsf{", letters[1:3], "}}}", sep = "", collapse = "\n"),
        ix + 1 * 8, iy, ix + 2 * 8, iy, ix + 3 * 8, iy
      )
    } else if (alternatives == 2) {
      sprintf(
        paste("\\put(%i,%i){\\makebox(0,0)[b]{\\textsf{", letters[1:2], "}}}", sep = "", collapse = "\n"),
        ix + 1 * 8, iy, ix + 2 * 8, iy
      )
    } else {
      stop("'alternatives' must be one of 5, 4, 3, 2")
    }
  }

  qbox <- function(i, alternatives = alternatives) {
    ix <- (i - 1) %/% 15
    iy <- (i - 1) %% 15 + 1
    ix <- 19 + 64 * ix - as.numeric(ix >= 2) * 4
    iy <- 129 - 7 * iy - 3 * ((iy - 1) %/% 5)

    if (alternatives > 0) {
      sprintf(
        "\\put(%i,%i){\\makebox(0,0){\\textsf{%i}}}\n\\multiput(%i,%i)(8,0){%i}{\\framebox(4,4){}}",
        ix + 2, iy + 6, i, ix + 6, iy + 4, alternatives
      )
    } else {
      sprintf("\\put(%i,%i){\\makebox(0,0){\\textsf{%i}}}", ix + 2, iy + 6, i)
    }
  }

  c(
    "
\\thispagestyle{empty}
    \\begin{picture}(210,290) 
    \\thicklines 
    
    % position marks for scanning
    \\put(17.5,13){\\line(1,0){5}} \\put(20,10.5){\\line(0,1){5}} 
    \\put(187.5,13){\\line(1,0){5}} \\put(190,10.5){\\line(0,1){5}} 
    \\put(157.5,270){\\line(1,0){5}} \\put(160,267.5){\\line(0,1){5}} 
    \\put(27.5,270){\\line(1,0){5}} \\put(30,267.5){\\line(0,1){5}} 
    
    % personal data box
    \\put(\\namecenter,244){\\makebox(0,0){\\textsf{\\myPersonalData}}} 
    \\put(20,198){\\framebox(\\namewidth,43){}} \\thinlines 
    \\multiput(20,217)(0,12){2}{\\line(1,0){\\nameline}} \\thicklines 
    \\put(21,236){\\makebox(0,5)[l]{\\textsf{\\myFamilyName:}}} 
    \\put(21,224){\\makebox(0,5)[l]{\\textsf{\\myGivenName:}}} 
    \\put(21,212){\\makebox(0,5)[l]{\\textsf{\\mySignature:}}} 
    \\put(\\namechecked,200){\\makebox(0,0)[rb]{\\scriptsize{\\textsf{\\myChecked}}}} 
    
    % registration number box
    \\put(\\regcenter,244){\\makebox(0,0){\\textsf{\\myRegistrationNumber}}} 
    \\put(\\regleft,233){\\framebox(\\regwidth,8){}} \\thinlines 
    \\multiput(\\regleftt,233)(8,0){\\regnumt}{\\line(0,1){1.5}} \\thicklines 
    \\multiput(\\regleftb,163)(8,0){\\regnum}{\\begin{picture}(0,0) 
    \\multiput(0,0)(0,7){10}{\\framebox(4,4){}}\\end{picture}}",
    if (replacement) "\\setcounter{nr3}{0}" else "\\newcounter{nr3}",
    "
    \\multiput(\\regleftn,228)(0,-7){10}{\\begin{picture}(0,0) 
    \\multiput(0,0)(\\regwidthn,0){2}{\\makebox(0,0){\\textsf{\\arabic{nr3}}}}
    \\end{picture} \\stepcounter{nr3}} 
    % general instructions and logo
    \\put(40,270){\\makebox(0,0)[bl]{\\textsf{\\textbf{\\LARGE{\\myinstitution}}}}}
    \\put(20,147){\\parbox{170mm}{\\textsf{\\myWarning}}} 
    
    % mark examples
    \\put(20,158){\\makebox(0,0)[l]{\\textsf{\\myMarkCarefully:}}}
    \\put(\\myMarkExampleB,158){\\makebox(0,0)[l]{\\textsf{\\myNotMarked:}}}
    \\put(\\myMarkExampleD,158){\\makebox(0,0)[l]{\\textsf{\\myOr}}}
    \\put(\\myMarkExampleA,157){\\framebox(4,4){}} 
    \\put(\\myMarkExampleA,157){\\line(1,1){4}} \\put(\\myMarkExampleA,161){\\line(1,-1){4}} 
    \\put(\\myMarkExampleA.2,157){\\line(1,1){3.8}} \\put(\\myMarkExampleA.2,161){\\line(1,-1){3.8}} 
    \\put(\\myMarkExampleA,157.2){\\line(1,1){3.8}} \\put(\\myMarkExampleA,160.8){\\line(1,-1){3.8}} 
    \\put(\\myMarkExampleC,157){\\framebox(4,4){}} 
    \\put(\\myMarkExampleE,158){\\colorbox{black}{\\framebox(2,2){}}} 
    
    
    % title and date
    \\put(40,262){\\parbox[t]{120mm}{\\large{\\textsf{\\textbf{{\\mytitle} {\\myDate}}}}}}
    
    % boxes for answers (inlcuding labels and separators)
    ",
    ## first column
    ## title
    sprintf("\\put(43,138){\\makebox(0,0){\\textsf{{\\myAnswers} 1 - %s}}}", min(15, n)),
    ## labels
    abcde(1, above = TRUE, alternatives = alternatives[1:min(15, n)]),
    abcde(min(15, n), above = FALSE, alternatives = alternatives[1:min(15, n)]),
    "",
    ## if second column
    if (n > 15) {
      c(
        ## separator
        "\\put(72,18){\\line(0,1){121}}",
        ## title
        sprintf("\\put(107,138){\\makebox(0,0){\\textsf{{\\myAnswers} 16 - %i}}}", min(30, n)),
        ## labels
        abcde(16, above = TRUE, alternatives = alternatives[16:min(30, n)]),
        abcde(min(30, n), above = FALSE, alternatives = alternatives[16:min(30, n)])
      )
    },
    ## if third column
    if (n > 30) {
      c(
        ## separator
        "\\put(134,18){\\line(0,1){121}}",
        ## title
        sprintf("\\put(167,138){\\makebox(0,0){\\textsf{{\\myAnswers} 31 - %i}}}", n),
        ## labels
        abcde(31, above = TRUE, alternatives = alternatives[31:n]),
        abcde(n, above = FALSE, alternatives = alternatives[31:n])
      )
    },
    ## box for each question
    sapply(1:n, function(i) qbox(i, alternatives = alternatives[i])),
    "
    % block with id, scrambling, type, replacement box
    \\linethickness{0.5mm} \\put(20,164){\\framebox(\\namewidth,28){}} \\thicklines  
    \\put(32,177){\\makebox(0,0)[t]{\\textsf{\\myDocumentType}}} 
    \\put(25,166){\\framebox(14,7){}} 
    \\put(67,177){\\makebox(0,0)[t]{\\textsf{\\myDocumentID}}}
    \\put(46,166){\\framebox(42,7){}} \\put(25,183.5){\\parbox{70mm}{%
    \\textsf{\\myNoChanges}}}
    \\ifregseven
    \\thinlines \\put(113,180){\\line(0,1){1.5}} \\thicklines 
    \\put(113,191){\\makebox(0,0)[t]{\\textsf{\\textbf{\\myScrambling}}}} 
    \\put(106,180){\\framebox(14,7){}}
    % scrambling is currently always zero
    \\put(109.5,183.5){\\makebox(0,0){\\Large{\\textsf{0}}}}
    \\put(116.5,183.5){\\makebox(0,0){\\Large{\\textsf{0}}}}
    \\fi
    \\put(67,169.5){\\makebox(0,0){\\Large{\\textsf{\\myID}}}}",
    sprintf("\\put(32,169.5){\\makebox(0,0){\\Large{\\textsf{%s}}}}", mytype),

    ## replacement?
    if (replacement & addreg == 0L) {
      "
      % replacement sheet
      \\put(116,170){\\framebox(4,4){}}
      \\put(114,172){\\makebox(0,0)[r]{\\textsf{\\myReplacement:}}}
      
      % cross in replacement box
      \\put(116,170){\\line(1,1){4}} \\put(116.1,174.15){\\line(1,-1){4}} 
      \\put(116.2,169.9){\\line(1,1){3.8}} \\put(116.2,174){\\line(1,-1){3.8}} 
      \\put(116,170.2){\\line(1,1){3.8}} \\put(116,173.8){\\line(1,-1){3.8}} 
      "
    },

    "
    \\end{picture}
    "
  )
}



nops_language <- function(file,
                          markup = c("latex", "html")) {
  ## read file
  lang <- drop(read.dcf(file))

  ## necessary fields for a correct lanuage specification
  langfields <- c(
    "PersonalData", "FamilyName", "GivenName", "Signature", "RegistrationNumber",
    "Checked", "NoChanges", "DocumentType", "DocumentID", "Scrambling",
    "Replacement", "MarkCarefully", "NotMarked", "Or",
    "MarkExampleA", "MarkExampleB", "MarkExampleC", "MarkExampleD", "MarkExampleE",
    "Warning", "Answers", "FillAnswers", "Point", "Points",
    "ExamResults", "Evaluation", "Mark", "Question", "GivenAnswer", "CorrectAnswer",
    "ExamSheet"
  )
  if (!all(langfields %in% names(lang))) stop("invalid language specification")

  ## desired output markup
  markup <- match.arg(tolower(markup), c("latex", "html"))
  if (markup == "html") lang <- structure(tth::tth(lang), .Names = names(lang))

  return(as.list(lang))
}
