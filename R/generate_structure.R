#' @name generate_structure
#' @title Generate the structure of a question bank
#' @author Nicolas Mangin
#' @description Recompose structural databases of a question bank.
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @export


generate_structure <- function() {

  # Bind variables for dplyr
  field_id <- NULL
  chapter_order <- NULL
  section_order <- NULL
  subsection_order <- NULL
  topic_order <- NULL
  question_id <- NULL
  chapter_id <- NULL
  section_id <- NULL
  subsection_id <- NULL
  topic_id <- NULL
  chapter_label <- NULL
  section_label <- NULL
  subsection_label <- NULL
  topic_label <- NULL
  question_nbr <- NULL
  question_language <- NULL
  objective <- NULL
  description <- NULL
  type <- NULL
  level <- NULL
  bloom <- NULL
  difficulty <- NULL
  stage <- NULL
  filter_variable <- NULL
  filter_value <- NULL
  topic_code <- NULL
  statement_id <- NULL
  statement_nbr <- NULL
  statement_language <- NULL
  proposition <- NULL
  value <- NULL
  explanation <- NULL
  language <- NULL
  topic_label <- NULL
  alternative_id <- NULL
  alternative_language <- NULL
  alternative_nbr <- NULL
  proposition_id <- NULL
  proposition_language <- NULL
  proposition_nbr <- NULL
  question <- NULL

  # Create general structure
  chapters <- readxl::read_excel("data-raw/structure/1a_chapters.xlsx")
  sections <- readxl::read_excel("data-raw/structure/2a_sections.xlsx")
  subsections <- readxl::read_excel("data-raw/structure/3a_subsections.xlsx")
  topics <- readxl::read_excel("data-raw/structure/4a_topics.xlsx")
  str_base <- readxl::read_excel("data-raw/structure/5_base.xlsx")
  str_statements <- readxl::read_excel("data-raw/structure/6_statements.xlsx")
  str_alt_labels <- readxl::read_excel("data-raw/structure/7a_alternatives_labels.xlsx")
  str_alt_choices <- readxl::read_excel("data-raw/structure/7b_alternatives_choices.xlsx")
  str_question_labels <- readxl::read_excel("data-raw/structure/8a_question_labels.xlsx")
  str_question_criteria <- readxl::read_excel("data-raw/structure/8b_question_criteria.xlsx")

  structure <- topics %>%
    dplyr::left_join(subsections, by = "subsection_id") %>%
    dplyr::left_join(sections, by = "section_id") %>%
    dplyr::left_join(chapters, by = "chapter_id") %>%
    dplyr::mutate(
      filter_variable = dplyr::case_when(
        topic_order > 0 ~ "topic_id",
        subsection_order > 0 ~ "subsection_id",
        section_order > 0 ~ "section_id",
        TRUE ~ "chapter_id"
      ),
      filter_value = dplyr::case_when(
        topic_order > 0 ~ topic_id,
        subsection_order > 0 ~ subsection_id,
        section_order > 0 ~ section_id,
        TRUE ~ chapter_id
      )
    ) %>%
    dplyr::mutate(field_id = substr(str_base$question_id[1], 1, 2)) %>%
    tidyr::unite(
      "topic_code",
      field_id, chapter_order,
      section_order, subsection_order,
      topic_order,
      sep = "",
      remove = FALSE
    )
  rm(chapters, sections, subsections, topics)

  # create test database
  str_base <- str_base %>%
    dplyr::left_join(structure, by = "topic_id") %>%
    dplyr::select(
      question_id,
      question_nbr,
      question_language,
      objective,
      description,
      type,
      level,
      bloom,
      difficulty,
      stage,
      topic_id,
      topic_order,
      subsection_id,
      subsection_order,
      section_id,
      section_order,
      chapter_id,
      chapter_order,
      filter_variable,
      filter_value,
      field_id,
      topic_code
    )
  save(str_base, file = "data/str_base.RData")
  rm(str_base)

  # Create statements database
  str_statements <- str_statements %>%
    dplyr::left_join(structure, by = "topic_id") %>%
    dplyr::select(
      statement_id,
      statement_nbr,
      statement_language,
      proposition,
      explanation,
      value,
      topic_id,
      topic_order,
      subsection_id,
      subsection_order,
      section_id,
      section_order,
      chapter_id,
      chapter_order,
      field_id,
      topic_code
    )
  save(str_statements, file = "data/str_statements.RData")
  rm(str_statements)


  # create alternatives database
  str_alternatives <- str_alt_labels %>%
    dplyr::left_join(str_alt_choices, by = "alternative_id") %>%
    dplyr::left_join(structure, by = "topic_id") %>%
    dplyr::select(
      alternative_id,
      alternative_nbr,
      alternative_language,
      question,
      explanation,
      proposition_id,
      proposition_nbr,
      proposition_language,
      proposition,
      value,
      topic_id,
      topic_order,
      subsection_id,
      subsection_order,
      section_id,
      section_order,
      chapter_id,
      chapter_order,
      filter_variable,
      filter_value,
      field_id,
      topic_code
    )
  save(str_alternatives, file = "data/str_alternatives.RData")
  rm(str_alternatives)

  save(str_question_labels, file = "data/str_question_labels.RData")
  save(str_question_criteria, file = "data/str_question_criteria.RData")

  # Create labels database
  languages <- c("EN", "FR", "DE", "ES", "IT", "NL")
  chapters_lab <- readxl::read_excel(
    "data-raw/structure/1b_chapters_labels.xlsx"
  ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(languages),
      names_to = "language",
      values_to = "chapter_label"
    )
  sections_lab <- readxl::read_excel(
    "data-raw/structure/2b_sections_labels.xlsx"
  ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(languages),
      names_to = "language",
      values_to = "section_label"
    )
  subsections_lab <- readxl::read_excel(
    "data-raw/structure/3b_subsections_labels.xlsx"
  ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(languages),
      names_to = "language",
      values_to = "subsection_label"
    )
  topics_lab <- readxl::read_excel(
    "data-raw/structure/4b_topics_labels.xlsx"
  ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(languages),
      names_to = "language",
      values_to = "topic_label"
    )
  str_labels <- structure %>%
    dplyr::left_join(topics_lab, by = "topic_id") %>%
    dplyr::left_join(subsections_lab, by = c("subsection_id", "language")) %>%
    dplyr::left_join(sections_lab, by = c("section_id", "language")) %>%
    dplyr::left_join(chapters_lab, by = c("chapter_id", "language")) %>%
    dplyr::select(
      topic_id,
      topic_order,
      subsection_id,
      subsection_order,
      section_id,
      section_order,
      chapter_id,
      chapter_order,
      field_id,
      topic_code,
      language,
      topic_label,
      subsection_label,
      section_label,
      chapter_label
    )
  save(str_labels, file = c("data/str_labels.RData"))
  rm(str_labels)
}
