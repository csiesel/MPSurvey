#' UI for the recode_var Shiny app
#'
#' @return A Shiny UI object.
#' @keywords internal
recode_var_app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("MPSurvey Variable Recoding Tool"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "vars",
          "Variables to group/recode",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::textInput("new_var", "New variable name", value = "new_var"),
        shiny::checkboxInput(
          "keep_unmatched",
          "Keep unmatched original values (single-variable only)",
          value = TRUE
        ),
        shiny::actionButton("build_map", "Build recode table"),
        shiny::hr(),
        shiny::tags$button(
          "Copy Code",
          id = "copy_code",
          type = "button",
          class = "btn btn-default",
          onclick = "copyGeneratedCode();"
        ),
        shiny::hr(),
        shiny::h3("Close this window after you have copied your code")
      ),
      shiny::mainPanel(
        shiny::tags$head(
          shiny::tags$style(shiny::HTML("
            .recode-table .dataTables_wrapper {
              width: 100%;
            }
            .recode-table table.dataTable {
              width: 100% !important;
            }
            .code-box {
              white-space: pre-wrap;
              background-color: #f8f9fa;
              border: 1px solid #ddd;
              border-radius: 4px;
              padding: 12px;
              min-height: 100px;
              font-family: monospace;
            }
          ")),
          shiny::tags$script(shiny::HTML("
            function copyGeneratedCode() {
              var codeEl = document.getElementById('code_out');
              var buttonEl = document.getElementById('copy_code');

              if (!codeEl) {
                alert('No code found to copy.');
                return;
              }

              var text = codeEl.innerText || codeEl.textContent;

              if (!text || text.trim() === '') {
                alert('No code found to copy.');
                return;
              }

              function notifySuccess() {
                if (buttonEl) {
                  var originalText = buttonEl.innerText;
                  buttonEl.innerText = 'Copied!';
                  buttonEl.classList.remove('btn-default');
                  buttonEl.classList.add('btn-success');

                  setTimeout(function() {
                    buttonEl.innerText = originalText;
                    buttonEl.classList.remove('btn-success');
                    buttonEl.classList.add('btn-default');
                  }, 1500);
                }

                if (window.Shiny) {
                  Shiny.setInputValue('copy_success', Date.now(), {priority: 'event'});
                }
              }

              if (navigator.clipboard && window.isSecureContext) {
                navigator.clipboard.writeText(text).then(function() {
                  notifySuccess();
                }).catch(function(err) {
                  fallbackCopyText(text, notifySuccess);
                });
              } else {
                fallbackCopyText(text, notifySuccess);
              }
            }

            function fallbackCopyText(text, onSuccess) {
              var ta = document.createElement('textarea');
              ta.value = text;
              ta.setAttribute('readonly', '');
              ta.style.position = 'fixed';
              ta.style.left = '-9999px';
              ta.style.top = '0';
              document.body.appendChild(ta);
              ta.focus();
              ta.select();

              try {
                document.execCommand('copy');
                if (onSuccess) onSuccess();
              } catch (err) {
                alert('Copy failed. Please copy manually.');
              }

              document.body.removeChild(ta);
            }
          "))
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Recode table"),
            shiny::p("Edit the new_value column to define the recode."),
            shiny::div(
              class = "recode-table",
              DT::DTOutput("map_table")
            )
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h4("Generated code"),
            shiny::p("Copy this code back into your R script."),
            shiny::div(
              class = "code-box",
              shiny::textOutput("code_out")
            )
          )
        )
      )
    )
  )
}

