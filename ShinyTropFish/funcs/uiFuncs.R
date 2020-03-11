## Additional ui functions for spictapp
## Tobias K. Mildenberger
## March 2020

## Javascript to close browser window (doesn't work on firefox > 46.0)
## ------------------------------------------------------------------
jscode <- "shinyjs.closeWindow = function() { window.close(); }"


textInputRow<-function (inputId, label, value = "")
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

textInput2<-function (inputId, label, value = "",...)
{
    tagList(tags$label(label, `for` = inputId), tags$input(id = inputId,
                                                           type = "text", value = value,...))
}

textInput3<-function (inputId, label, value = "",...)
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value,...))
}
