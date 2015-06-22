speed-tf-VSS14
==============

Speed limits, and relation to temporal freq limits on attentional tracking. 

**doAllAnalyses.R** in **analyseExps** folder is the main analysis, which calls various other files.

Before that,
**loadAnonymiseSaveData.R** scripts in subdirs of **analyseExps** folder takes the raw data with identities and anonymises it, spits it into 
top-level **data** subdirectory. It is these anonymised files that doAllAnalyses etc analyse.

Seems to all be working, except maybe model limits.