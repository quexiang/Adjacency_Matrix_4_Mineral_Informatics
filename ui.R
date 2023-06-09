library(shiny)
library(markdown)
library(shinydashboard)
library(shinyjs)

title <- "Mineral Informatics"

about <- includeMarkdown("about.md")

cases_list <-{c(
  "element-mineral" = "elm_mineral",
  "element-mineral-ab" = "elm_mineral_ab",
  "mineral-locality" = "mineral_loc",
  "igneous-minerals" = "igneous_minerals",
  "oxygen-spinel" = "oxygen_spinel"
  
)}

data_source_sel <-selectInput(
  "cases_source",
  "Ues Cases Selection",
  choices = cases_list,
  selected = "elm_mineral_ab"
)

data_source_md <-uiOutput("dataset_description")

dataset_sel <-uiOutput("graph_set")

ordering <- uiOutput("ordering_choices")

community <- uiOutput("comm_choices")

community_elm <-uiOutput("comm_choices_elm")


alpha <- conditionalPanel(
  condition = "output.weighted",
  checkboxInput("alpha_weight", "Set alpha by edge weight", FALSE)
)

annotate <- conditionalPanel(
  condition = "output.annotate_vars",
  checkboxInput("ann_var", "Annotate plot by node attribute sorting", FALSE)
)

#for select by elm
ordering_elm <- uiOutput("ordering_choices_elm")

annotate_elm <- conditionalPanel(
  condition = "output.annotate_vars",
  checkboxInput("ann_var_elm", "Annotate plot by node attribute sorting", FALSE)
)

alpha_elm <- conditionalPanel(
  condition = "output.weighted_elm",
  checkboxInput("alpha_weight_elm", "Set alpha by edge weight", FALSE)
)


plot <- plotOutput("adj_plot", height = "1300px", width = "100%")

pt_by_selm <- plotOutput("ptable_plot", height = "1300px", width = "100%")

# Layout every page section
header <- dashboardHeader(title = title)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Periodic Table", tabName = "ptable", icon = icon("users")),
    menuItem("Plot", tabName = "plot", icon = icon("bar-chart")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  ),
  data_source_md,
  data_source_sel,
  dataset_sel,
  community
)

body <- dashboardBody(
       tabItems(
       tabItem("about", about),
       tabItem("plot", box(title = "Plot Properties", solidHeader = TRUE, 
                           status = "info", width = 12, fluidRow(column(4, ordering), column(4, annotate), column(4, alpha)), collapsible = TRUE),
       box(width = 12, plot)),
       tabItem("ptable",box(title="Periodic Table",width="100%",height = "620px",status='primary',#height="100%",
                            tags$hr(),
                            tags$link(rel = "stylesheet",type = "text/css",href = "css/projects.css"),
                            tags$div(class = "maindiv",
                                     tags$div(class = "roundcorners leftdiv",HTML('  <table style="width:100%;">
                              	<tr><td><div class="label">Name</div></td></tr>
                              	<tr><td><input type="text" id="namefilter" style="width:124px" /></td></tr>
                  
                              	<tr><td><div class="label">Atomic Number</div></td></tr>
                              	<tr><td><input type="number" id="atomicnumberfilter" style="width:124px" min=1 max=118 /></td></tr>
                  
                              	<tr><td><div class="label">Symbol</div></td></tr>
                              	<tr><td><input type="text" id="symbolfilter" style="width:124px" /></td></tr>
                  
                        				<tr><td><div class="label">Category</div></td></tr>
                        				<tr><td>
                        					<select id="categoryfilter">
                        						<option value=""></option>
                        						<option value="Alkali metal">Alkali metal</option>
                        						<option value="Alkaline earth metal">Alkaline earth metal</option>
                        						<option value="Lanthanide">Lanthanide</option>
                        						<option value="Actinide">Actinide</option>
                        						<option value="Transition metal">Transition metal</option>
                        						<option value="Post-transition metal">Post-transition metal</option>
                        						<option value="Metalloid">Metalloid</option>
                        						<option value="Reactive nonmetal">Reactive nonmetal</option>
                        						<option value="Noble gas">Noble gas</option>
                        						<option value="Unknown">Unknown</option>
                        					</select>
                        				</td></tr>
                              	<tr><td><div class="label">Group</div></td></tr>
                              	<tr><td>
                      					<select id="groupfilter">
                        						<option value=""></option>
                        						<option value="0">none</option>
                        						<option value="1">1</option>
                        						<option value="2">2</option>
                        						<option value="3">3</option>
                        						<option value="4">4</option>
                        						<option value="5">5</option>
                        						<option value="6">6</option>
                        						<option value="7">7</option>
                        						<option value="8">8</option>
                        						<option value="9">9</option>
                        						<option value="10">10</option>
                        						<option value="11">11</option>
                        						<option value="12">12</option>
                        						<option value="13">13</option>
                        						<option value="14">14</option>
                        						<option value="15">15</option>
                        						<option value="16">16</option>
                        						<option value="17">17</option>
                        						<option value="18">18</option>
                      					</select>
                  				      </td></tr>
                  
                              	<tr><td><div class="label">Period</div></td></tr>
                              	<tr><td>
                      					<select id="periodfilter">
                        						<option value=""></option>
                        						<option value="1">1</option>
                        						<option value="2">2</option>
                        						<option value="3">3</option>
                        						<option value="4">4</option>
                        						<option value="5">5</option>
                        						<option value="6">6</option>
                        						<option value="7">7</option>
                      					</select>
                  				      </td></tr>
                  
                              	<tr><td><div class="label">Block</div></td></tr>
                              	<tr><td>
                      					<select id="blockfilter">
                        						<option value=""></option>
                        						<option value="d">d</option>
                        						<option value="f">f</option>
                        						<option value="p">p</option>
                        						<option value="s">s</option>
                      					</select>
                      				</td></tr>
                              <tr>
                                  <td colspan="2"><div id="btnApplyFilter" class="button">Apply Filter</div></td>
                              </tr>
                  
                              <tr>
                                  <td colspan="2"><div id="btnClearFilter" class="button">Clear Filter</div></td>
                              </tr>
                              </table>
                      	</div>'),br()),
                                     HTML(' 	<div id="periodictablediv" class="roundcorners tablediv">
                    			<table id="periodictable" style="width: 100%; height: 100%;"></table>
                    		  </div>
                        	<div class="roundcorners rightdiv">
                    			<table style="width:100%;">
                                	<tr>
                    					<td><div class="label">Color by Category</div></td>
                    				</tr>
                                	<tr>
                    					<td class="radiobutton"><div class="radiobutton"><input type="radio" name="colorby" value="colorbycategory" id="colorcategory" checked /></div></td>
                    				</tr>
                    
                                	<tr>
                    					<td><div class="label">Color by Block</div></td>
                    				</tr>
                                	<tr>
                    					<td class="radiobutton"><div class="radiobutton"><input type="radio" name="colorby" value="colorbyblock" id="colorblock" /></div></td>
                    				</tr>
                    
                                </table>
                    
                    			<table id="categorykey" style="width:100%; display: inline;">
                                	<tr><td><div class="alkalimetal colorkey roundcorners">Alkali metal</div></td></tr>
                                	<tr><td><div class="alkalineearthmetal colorkey roundcorners">Alkaline earth metal</div></td></tr>
                                	<tr><td><div class="lanthanide colorkey roundcorners">Lanthanide</div></td></tr>
                                	<tr><td><div class="actinide colorkey roundcorners">Actinide</div></td></tr>
                                	<tr><td><div class="transitionmetal colorkey roundcorners">Transition metal</div></td></tr>
                                	<tr><td><div class="posttransitionmetal colorkey roundcorners">Post-transition metal</div></td></tr>
                                	<tr><td><div class="metalloid colorkey roundcorners">Metalloid</div></td></tr>
                                	<tr><td><div class="reactivenonmetal colorkey roundcorners">Reactive nonmetal</div></td></tr>
                                	<tr><td><div class="noblegas colorkey roundcorners">Noble gas</div></td></tr>
                                	<tr><td><div class="unknown colorkey roundcorners">Unknown</div></td></tr>
                                </table>
                    
                    			<table id="blockkey" style="width:100%; display: none;">
                                	<tr><td><div class="sblock colorkey roundcorners">s</div></td></tr>
                                	<tr><td><div class="dblock colorkey roundcorners">d</div></td></tr>
                                	<tr><td><div class="fblock colorkey roundcorners">f</div></td></tr>
                                	<tr><td><div class="pblock colorkey roundcorners">p</div></td></tr>
                                </table>
                    
                    		</div>')
                            ),
                            tags$div(id="infoboxbackground",style="background-color: #000000; position: absolute;left: 0px; right: 0px; top: 0px; bottom: 0px; opacity: 0.8; visibility: hidden;"
                            ),
                            tags$div(id ="infobox",class ="border roundcorners",style="position:absolute;  width: 500px; top: 50%; left: 50%; transform: translate(-50%, -50%); background-color: #FFFFFF; visibility: hidden;",
                                     HTML('<table style="width:100%;">
                          		<tr><th colspan="2" id="infoName" class="thelementname"></th>
                          		<tr><th>Atomic number</th><td id="infoAtomicNumber"></th>
                              	<tr><th>Chemical Symbol</th><td id="infoChemicalSymbol"></td>
                              	<tr><th>Category</th><td id="infoCategory"></td>
                              	<tr><th>Atomic weight - conventional</th><td id="infoAtomicWeightConventional"></td>
                              	<tr><th>Atomic weight - standard</th><td id="infoAtomicWeightStandard"></td>
                              	<tr><th>Occurrence</th><td id="infoOccurrence"></td>
                              	<tr><th>State of matter</th><td id="infoStateOfMatter"></td>
                              	<tr><th>Group</th><td id="infoGroup"></td>
                              	<tr><th>Period</th><td id="infoPeriod"></td>
                              	<tr><th>Block</th><td id="infoBlock"></td>
                              	<tr><td colspan="2"><div id="btnCloseInfoBox" class="button">Close</div></td></tr>
                      		</table>')
                            ),
                            tags$script(src = "js/data.js"),
                            tags$script(src = "js/periodictable.js"),
                            tags$script(src = "js/periodictableinfobox.js"),
                            tags$script(src = "js/periodictabledisplay.js"),
                            tags$script(src = "js/periodictablepage.js"),
                            useShinyjs()
       ),
       tags$div(id = "myelmBox",
                box(title = "Plot adjacency matrix by selected element",  status = "info",solidHeader = TRUE, width = 12, fluidRow(column(3, community_elm),column(3, ordering_elm), column(3, annotate_elm), column(3, alpha_elm)), collapsible = TRUE),
                box(width = 12, pt_by_selm))
       
       )
       
))

# Render and display
dashboardPage(header, sidebar, body, title = title)
