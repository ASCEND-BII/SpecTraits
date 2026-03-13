################################################################################
### About panel

about_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 8, offset = 2,
      br(),
      HTML("<h1 align='center' style='color:#005F5F; font-weight:bold;'>About SpecTraits</h1>"),
      br(),

      wellPanel(
        style = "background-color: #005F5F; border: 2px solid #004040; color: white;",

        # Version and Links
        HTML("<p align='center' style='font-size: 16px; color: white;'>
              <strong>Version 0.1</strong> |
              <img src='github.png' width='20px' height='auto' style='vertical-align: middle;'>
              <a target='_blank' rel='noopener noreferrer' href='https://github.com/ASCEND-BII/SpecTraits' style='color: #c0c0c0; text-decoration: none;'>GitHub</a>
             </p>"),
        br(),

        # Motivation Section
        HTML("<p style='text-align: justify; color: white;'>
              Leaf spectroscopy has emerged as a powerful tool for the rapid, non-destructive
              estimation of leaf traits. However, deriving trait information from spectral
              data is often not standardized across research studies and can be technically
              demanding, creating substantial barriers for users without expertise.
              </p>"),
        HTML("<p style='text-align: justify; color: white;'>
              <strong>SpecTraits</strong> was developed to bridge this gap by providing an
              integrated, user-friendly platform that consolidates tools for deriving leaf
              traits from spectral data. Our goal is to make leaf spectroscopy more accessible
              to researchers and plant scientists who use leaves for phenotyping (e.g.,
              ecologists, agronomists, and evolutionary biologists), enabling them to fully
              leverage the potential of spectroscopy to advance the understanding of plant
              evolution and function.
              </p>"),
        br(),

        # Citation Section
        HTML("<p style='text-align: justify; color: white;'>
              If you use SpecTraits in your research, please cite:
              </p>"),
        HTML("<div style='background-color: #004040; padding: 15px; border-left: 4px solid #c0c0c0; margin: 10px 0; position: relative; border-radius: 5px;'>
              <pre id='citation-text' style='margin: 0; font-family: monospace; padding-right: 50px; font-size: 12px; white-space: pre-wrap; overflow-x: auto; color: white;'>@software{SpecTraits,
  author = {Guzmán, J. Antonio and Cavender-Bares, Jeannine},
  title = {SpecTraits: A Shiny Application for Leaf Trait Prediction Using Spectroscopy},
  year = {2026},
  version = {0.1},
  url = {https://github.com/ASCEND-BII/SpecTraits}
}</pre>
              <button id='copy-citation-btn' onclick='copyCitation()'
                      style='position: absolute; top: 10px; right: 10px;
                             background-color: #6c757d; color: white; border: none;
                             border-radius: 5px; padding: 8px 12px; cursor: pointer;
                             font-size: 14px; font-weight: bold;'
                      title='Copy BibTeX citation to clipboard'>
                📋 Copy BibTeX
              </button>
              <span id='copy-feedback' style='position: absolute; top: 10px; right: 10px;
                                             background-color: #28a745; color: white;
                                             padding: 8px 12px; border-radius: 5px;
                                             font-size: 14px; display: none;'>
                ✓ Copied!
              </span>
              </div>"),
        HTML("<script>
              function copyCitation() {
                // BibTeX software citation text
                var citationText = '@software{SpecTraits,\\n  author = {Guzmán, J. Antonio and Cavender-Bares, Jeannine},\\n  title = {SpecTraits: A Shiny Application for Leaf Trait Prediction Using Spectroscopy},\\n  year = {2026},\\n  version = {0.1},\\n  url = {https://github.com/ASCEND-BII/SpecTraits}\\n}';

                // Copy to clipboard
                navigator.clipboard.writeText(citationText).then(function() {
                  // Show feedback
                  var btn = document.getElementById('copy-citation-btn');
                  var feedback = document.getElementById('copy-feedback');
                  btn.style.display = 'none';
                  feedback.style.display = 'inline-block';

                  // Reset after 2 seconds
                  setTimeout(function() {
                    btn.style.display = 'inline-block';
                    feedback.style.display = 'none';
                  }, 2000);
                }).catch(function(err) {
                  console.error('Could not copy text: ', err);
                  alert('Failed to copy citation. Please copy manually.');
                });
              }
              </script>"),
        br(),

        # Funding Section
        HTML("<p style='text-align: justify; color: white;'>
              The development of SpecTraits was supported by:
              </p>"),
        HTML("<div style='display: flex; justify-content: center; align-items: flex-start; margin: 20px 0; gap: 30px;'>
                <div style='text-align: center;'>
                  <img src='ACEND.png' height='150px' style='display: block; margin: 0 auto;'>
                  <p style='margin-top: 10px; color: white;'>NSF DBI: 2021898</p>
                </div>
                <div style='text-align: center;'>
                  <img src='NSF.png' height='150px' style='display: block; margin: 0 auto;'>
                </div>
              </div>"),
        br(),

        # Contact Section
        HTML("<p style='text-align: justify; color: white;'>
              For questions, bug reports, or feature requests, please visit our
              <a href='https://github.com/ASCEND-BII/SpecTraits/issues' target='_blank' style='color: #c0c0c0; text-decoration: none;'>GitHub Issues page</a>
              or contact the development team.
              </p>"),
        br(),

        HTML("<p align='center' style='color: #d0d0d0; font-size: 14px;'>
              Last updated: 2026-04-01
              </p>")
      )
    )
  )
}
