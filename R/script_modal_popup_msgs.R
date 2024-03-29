#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Holds scripts
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: this holds all the long text strings that are used in the application
#-------- should be called by the app.R file
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#map_intro====
modal_intro =
'
<h1 style="text-align: center;">Bus Bay Simulation Tool</h1>
<h4 style="text-align: center;">An interactive, web-based tool for bus bay operation simulation.</h4>

<hr />
<p style="text-align: left;">
This dashboard provides a means to simulate bus bay operations for planning practitioners.  
</p>

<hr/>
<img src="cube_cute.png" alt="cute_transit" style="float:right;height:200px;>

<p style="text-align: left;">&nbsp;<strong>What you can do with this dashboard:</strong></p>
<ul>
<li style="text-align: left;">Explore the Cascadia corridor given a variety of map layers relevant to UHSGT.&nbsp;</li>
<li style="text-align: left;">Inspect aggregated census metrics for the corridor and by user defined by spatial subsets.&nbsp;</li>
<li style="text-align: left;">Download data and metrics to share with colleagues offline.&nbsp;</li>
<li style="text-align: left;">Understand important regional planning deadlines and publications.&nbsp;</li>
</ul>

<p style="text-align: left;">&nbsp;<strong>How to navigate this dashboard:</strong></p>
<ul>
<li style="text-align: left;">This dashboard has {{CHECK NUMBER OF TABS}} tabs which are navigable via the sidebar (left).&nbsp;</li>
<li style="text-align: left;">The <strong>NAME#1</strong> tab is the landing page for this application. 
It contains two sub-tabs <strong>NAME#1.1</strong> and <strong>NAME#1.2</strong>. 
NAME#1.1 is where you initialize simulation parameters and provide bus and passenger simulation characteristics. 
NAME#1.2 provides tables and visulizations of the user inputs to inspect and confirm them.&nbsp;</li>
<li style="text-align: left;">The <strong>NAME#2</strong> tab displayes the results of the simulation run. It provides interactive tables and visulaitions that describe the results.&nbsp;</li>
<li style="text-align: left;">The <strong>NAME#1</strong> tab contains all the data resulting from the simulation. The raw data can be downloaded. &nbsp;</li>
<ul>
<li>The tables on this page contain direct links to the data sources so you may perform further investigation at you convenience.</li>
</ul>
<li style="text-align: left;">Understand important regional planning deadlines and publications.&nbsp;</li>
</ul>
<p style="text-align: left;">
<strong>Happy Simulating!</strong> &#128652 &#128652 &#128694 &#128694 &#128694
</p>
<hr/>
<p style="text-align: center;"><span style="color: #000000;"> For support, issue reporting, or for features you\'d like to see - contact: michael.gaunt@wsp.com or vist the project <a href="https://github.com/michaelgaunt404/GEOCDR">GIT repo</a></span></p>
  <p style="text-align: center;"><strong>Need more help?</strong> <a href="https://wsponline-my.sharepoint.com/:w:/g/personal/nicholas_richter_wsp_com/EXFdg_4V-LRDqr-lNXurfwgBHXnMj9cbJyKWGTn0fHu-fw">Download our instruction manual here.</a></p>
    <p style="text-align: center;"><strong><span style="color: #ff0000;">IMPORTANT: This is for internal use only. </span></strong></p>
' %>% 
  HTML()

getwd()
get_golem_wd()

