#' @name course_set_paths
#' @title Complete course folder paths
#' @author Nicolas Mangin
#' @description Create a list of addresses on local computer based on the selected course folder.
#' @param course_folder Character. Course folder as selected by the user.
#' @return A list of folder paths based on a standard course structure and used by the application.
#' @importFrom shiny addResourcePath
#' @export


course_set_paths <- function(course_folder){
  
  if (!base::is.null(course_folder) & !base::is.na(course_folder) & base::nchar(course_folder)){
    
    subfolders <- base::list(
      course = course_folder,
      original = base::paste0(course_folder, "/basis/1_documents/1_original"),
      translated = base::paste0(course_folder, "/basis/1_documents/2_translated"),
      trees = base::paste0(course_folder, "/basis/3_trees/table"),
      jstrees = base::paste0(course_folder, "/basis/3_trees/jstree"),
      ratings = base::paste0(course_folder, "/basis/4_analytics/1_raw/ratings"),
      comments = base::paste0(course_folder, "/basis/4_analytics/1_raw/comments"),
      views = base::paste0(course_folder, "/basis/4_analytics/1_raw/views"),
      functions = base::paste0(course_folder, "/basis/5_functions"),
      templates_note = base::paste0(course_folder, "/basis/6_templates/1_note"),
      templates_page = base::paste0(course_folder, "/basis/6_templates/2_page"),
      templates_slide = base::paste0(course_folder, "/basis/6_templates/3_slide"),
      templates_video = base::paste0(course_folder, "/basis/6_templates/4_video"),
      templates_game = base::paste0(course_folder, "/basis/6_templates/5_game"),
      templates_case = base::paste0(course_folder, "/basis/6_templates/6_case"),
      templates_question = base::paste0(course_folder, "/basis/6_templates/7_question"),
      templates_feedback = base::paste0(course_folder, "/basis/6_templates/8_feedback"),
      templates_analysis = base::paste0(course_folder, "/basis/6_templates/9_analysis"),
      credentials = base::paste0(course_folder, "/basis/7_credentials"),
      databases = base::paste0(course_folder, "/basis/8_databases"),
      blog = base::paste0(course_folder, "/materials/1_blog"),
      textbooks = base::paste0(course_folder, "/materials/2_textbooks"),
      presentations = base::paste0(course_folder, "/materials/3_presentations"),
      scripts = base::paste0(course_folder, "/materials/4_scripts"),
      games = base::paste0(course_folder, "/materials/5_games"),
      cases = base::paste0(course_folder, "/materials/6_cases"),
      tests = base::paste0(course_folder, "/materials/7_tests"),
      analyses = base::paste0(course_folder, "/materials/8_analyses"),
      temp = base::paste0(course_folder, "/temporary"),
      data = base::paste0(course_folder, "/temporary/data"),
      csl = base::paste0(course_folder, "/temporary/format/csl"),
      tex = base::paste0(course_folder, "/temporary/format/tex"),
      css = base::paste0(course_folder, "/temporary/format/css")
    )
    
    databases <- base::list(
      documents = base::paste0(course_folder, "/basis/1_documents/documents.RData"),
      propositions = base::paste0(course_folder, "/basis/1_documents/propositions.RData"),
      translations = base::paste0(course_folder, "/basis/1_documents/translations.RData"),
      doctypes = base::paste0(course_folder, "/basis/2_tags/document_types.RData"),
      tags = base::paste0(course_folder, "/basis/2_tags/tags.RData"),
      courses = base::paste0(course_folder, "/basis/3_trees/courses.RData"),
      ratings = base::paste0(course_folder, "/basis/4_analytics/2_data/ratings.RData"),
      comments = base::paste0(course_folder, "/basis/4_analytics/2_data/comments.RData"),
      views = base::paste0(course_folder, "/basis/4_analytics/2_data/views.RData"),
      tests = base::paste0(course_folder, "/basis/4_analytics/2_data/tests.RData"),
      students = base::paste0(course_folder, "/basis/4_analytics/2_data/students.RData"),
      results = base::paste0(course_folder, "/basis/4_analytics/2_data/results.RData"),
      page_ratings = base::paste0(course_folder, "/basis/4_analytics/3_statistics/page_ratings.RData"),
      page_comments = base::paste0(course_folder, "/basis/4_analytics/3_statistics/page_comments.RData"),
      video_views = base::paste0(course_folder, "/basis/4_analytics/3_statistics/video_views.RData"),
      document_parameters = base::paste0(course_folder, "/basis/4_analytics/3_statistics/document_parameters.RData"),
      document_models = base::paste0(course_folder, "/basis/4_analytics/3_statistics/document_models.RData"),
      item_parameters = base::paste0(course_folder, "/basis/4_analytics/3_statistics/item_parameters.RData"),
      item_models = base::paste0(course_folder, "/basis/4_analytics/3_statistics/item_models.RData")
    )
    
    shiny::addResourcePath("temporary", base::paste0(subfolders$temp))
    
    course_paths <- base::list(
      subfolders = subfolders,
      databases = databases
    )
    
  } else {
    
    course_paths <- NA
    
  }
  
  return(course_paths)
}
