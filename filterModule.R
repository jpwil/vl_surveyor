###################
## FILTER MODULE ##
###################

# inspired, in part, by jcheng5/filters.R
# https://gist.github.com/jcheng5/76cffbbdd9db0b0e971fd34f575fa45b



source("uiVars.R") # load filter dictionary

# instead of NS() in all the functions, I pass on the module inputIds as filter_id
selectInputVL <- function(id, filter_id, label, ...) {
  pickerInput(inputId = filter_id,
              label = label,
              choices = sort(unique(vl_data[[id]])),
              multiple = TRUE,
              selected = NULL,
              options = list(`actions-box` = TRUE,
                             `none-Selected-Text` = "No filter applied",
                             `deselect-All-Text` = "Reset"),
              ...)
}

sliderInputVL <- function(id, filter_id, label, sep = ",", ticks = FALSE, step, ...) {
  min = min(vl_data[[id]], na.rm = TRUE)
  max = max(vl_data[[id]], na.rm = TRUE)
  sliderInput(inputId = filter_id,
              label = label,
              min = min,
              max = max,
              value = c(min, max),
              sep = sep,
              ticks = ticks,
              step = step,
              ...)
}

checkInputVL <- function(id, filter_id, label, ...) {
  checkboxInput(inputId = filter_id,
                label = label,
                value = TRUE,
                ...)
}

# create a wrapper function for uiVars to create input widgets
wrapVLUI <- function(type, id, filter_id, label, sep, ticks, step, ...) {
  if (type == "select") {
    selectInputVL(id, filter_id, label, ...)
  } else if (type == "slider") {
    sliderInputVL(id, filter_id, label, sep, ticks, step, ...) 
  } else if (type == "check_na") {
    filter_id <- paste0(filter_id, "_na")  
    checkInputVL(id, filter_id, label, ...)
  } else {
    stop("UI type not supported")
  }
}

filterUI <- function(id_module) {
  uiVars_mod <- uiVars %>% 
    mutate(filter_id = NS(id_module, id))
  tagList(
    pmap(uiVars_mod, wrapVLUI), # see definitions above
    div(align="center", actionButton(NS(id_module, "reset_filter"), "Reset all filters", width = "100%")))
}

filterServer <- function(id_module) {
  moduleServer(id_module, function(input, output, session) {
    
    selectFilter <- function(id) {
      if (is.null(input[[id]])) {
        TRUE
      } else {
        vl_data[[id]] %in% input[[id]]
      }
    }
    
    sliderFilter <- function(id) {
      is.na(vl_data[[id]]) |
        (vl_data[[id]] >= input[[id]][1] & 
           vl_data[[id]] <= input[[id]][2])
    }
    
    checkFilter <- function(id) {
      id_input <- paste0(id, "_na")
      if (input[[id_input]]) {
        TRUE
      } else {
        !is.na(vl_data[[id]])
      }
    }    
    
    # list of reactives (each element is the filter for the nth filter as per uiVars)
    filter <- 
      pmap(uiVars[,c("type", "id")], function(type, id) reactive({  
        if (type == "select") {
          selectFilter(id)
        } else if (type == "slider") {
          sliderFilter(id)
        } else if (type == "check_na") {
          checkFilter(id)
        } else {
          stop("FILTER ERROR")
        }
      })
      )
    

    # list of combined filter reactives for choice filter 
    filter_choice <- 
      lapply((1:nrow(uiVars))[uiVars$type=="select"],
             function(i) reactive({ 
               filter_values <- lapply((1:nrow(uiVars))[-i], 
                                       function(x) filter[[x]]())
               Reduce(`&`, filter_values)
             }))
    
    # list of choices to update selectInput 
    choice <- lapply(1:length(uiVars[uiVars$type == "select", 2, drop = TRUE]),
                     function(x) reactive({
                       sort(unique((vl_data[uiVars[uiVars$type == "select", 2, drop = TRUE]][,x, drop = TRUE])[filter_choice[[x]]()]))
                     })
    )
    
    # update selectInput choices based on new inputs
    lapply((1:nrow(uiVars))[uiVars$type=="select"],
           function(x) {
             observeEvent(choice[[x]](), {
               current_values <- input[[uiVars[x,2,drop = TRUE]]]
               updatePickerInput(
                 session,
                 inputId = uiVars[x,2,drop = TRUE],
                 choices = unique(c(current_values, choice[[x]]())),
                 selected = current_values
               )
             })
           })
    
    # reset all inputs
    lapply((1:nrow(uiVars))[uiVars$type=="select"],
           function(x) {
             observeEvent(input$reset_filter, {
               updatePickerInput(
                 session,
                 inputId = uiVars[x,2,drop = TRUE],
                 choices = sort(unique(vl_data[[uiVars[x,2, drop = TRUE]]])),
                 selected = NULL
               )
             })
           })
    
    lapply((1:nrow(uiVars))[uiVars$type=="slider"],
           function(x) {
             observeEvent(input$reset_filter, {
               min = min(vl_data[[uiVars[x,2, drop = TRUE]]], na.rm = TRUE)
               max = max(vl_data[[uiVars[x,2, drop = TRUE]]], na.rm = TRUE)
               updateSliderInput(
                 session,
                 inputId = uiVars[x,2,drop = TRUE],
                 value = c(min, max)
               )
             })
           })
    
    lapply((1:nrow(uiVars))[uiVars$type=="check_na"],
           function(x) {
             observeEvent(input$reset_filter, {
               updateCheckboxInput(
                 session,
                 inputId = paste0(uiVars[x,2,drop = TRUE], "_na"),
                 value = TRUE
               )
             })
           })
    
    # single reactive vector (logical and of the list of reactives)
    filter_combined <- reactive({
      filter_values <- lapply(1:nrow(uiVars), function(x) filter[[x]]())
      Reduce(`&`, filter_values)
    }) 
    
    reactive({ filter_combined() }) # this is the module server output
     
    
  })
}
