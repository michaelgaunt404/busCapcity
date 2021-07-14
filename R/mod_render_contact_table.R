#' render_contact_table
#' 
#' Render contact table.
#' 
#' @return HTML

#template
# render_contact_table = function() {
#     contact = data.frame('Name' = c('<b>Michael Gaunt</b>', '<b>Bethany Yollin</b>', '<b>Yihong Zou</b>'),
#                          'Title' = c('Senior Systems Analyst', 'Data Scientist', 'Data Scientist'),
#                          'Email' = c(paste(tags$a(href = 'mailto:nicholas.richter@wsp.com', target = '_blank',
#                                                   icon('envelope'))),
#                                      paste(tags$a(href = 'mailto:bethany.yollin@wsp.com', target = '_blank',
#                                                   icon('envelope'))),
#                                      paste(tags$a(href = 'mailto:yihong.zou@wsp.com', target = '_blank',
#                                                   icon('envelope'))))
#     )
#     renderDataTable(contact, escape = F, rownames = FALSE, selection = 'none', style = 'bootstrap',
#                     options = list(paging = FALSE, searching = FALSE, dom = 't',
#                                    # remove table header
#                                    initComplete = JS('function(settings, json) {',
#                                                          '$(this.api().table().header()).css({"display": "none"});',
#                                                      '}'))
#     )
# }

render_contact_table = function() {
  contact = data.frame('Name' = c('<b>Michael Gaunt</b>'),
                       'Title' = c('Data Scientist'),
                       'Email' = c(paste(tags$a(href = 'mailto:michael.gaunt@wsp.com', target = '_blank',
                                                icon('envelope'))))
  )
  DT::renderDataTable(contact, escape = F, rownames = FALSE, selection = 'none', style = 'bootstrap',
                      options = list(paging = FALSE, searching = FALSE, dom = 't',
                                     # remove table header
                                     initComplete = JS('function(settings, json) {',
                                                       '$(this.api().table().header()).css({"display": "none"});',
                                                       '}'))
  )
}

