# Opal/DataSHIELD installation with dsSwissKnife and dsSSCP

1. follow https://opaldoc.obiba.org/en/latest/admin/installation.html

2. install R and 
   * $ install.packages("Rserve", , "http://www.rforge.net/")

3. open R, change .libPaths()
   * $ source("<rock-home>/conf/Rprofile.R")
   * $ install.packages("jsonlite", repos = 'https://cran.r-project.org')

4. follow https://rockdoc.obiba.org/en/latest/admin/installation.html

5. run rock and opal

6. login from the web interface (defaults to http://localhost:8080 )

7. click Administration -> DataHIELD , 
   * in *packages* click "Add Packages" select "Install base DataSHIELD packages"
   * in *packages* click "Add Packages" select "Repository: CRAN", "Name: tensorflow"
   * in *packages* click "Add Packages" select "Repository: CRAN", "Name: geigen"
   * in *packages* click "Add Packages" select "Repository: GitHub", "User or organization name: sib-swiss", "Name: dsSwissKnife"
   * in *packages* click "Add Packages" select "Repository: GitHub", "User or organization name: vanduttran", "Name: dsSSCP"
   * Select a profile in the Profiles section
   * Press Initialize in the profile's Settings section
   * Press Enable in the profile's Status section
8. configure a database access 
   https://opaldoc.obiba.org/en/latest/web-user-guide/administration/databases.html

9. now you can point your VisualSHIELD instance to the newly configured OPAL interface (defaults to http://localhost:8080 )
