#install.packages("devtools")
#library(devtools)
#install_github("andrewheiss/limer")
library(limer)
options(lime_api='http://104.131.95.42/index.php/admin/remotecontrol')
options(lime_username = 'geovannys')
options(lime_password = 'geovannys')
get_session_key()
try(data1 <-  get_responses(iSurveyID=876614, 
                            sLanguageCode='es', 
                            sCompletionStatus='complete',
                            sResponseType='short'),silent=TRUE)

release_session_key()
##########################################################################################
