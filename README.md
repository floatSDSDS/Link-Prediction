# Link-Prediction
> R Scripts for network, graph analysis features. compute all those graph features like AA, JAccard, etc.

- dependency:
  - igraph
  - dplyr
  - data.table

- including the following features (by 2017/1/20)
  - CN
  - Salton
  - Jaccard
  - Sorenson
  - HPI
  - HDI
  - LHN-I
  - AA
  - RA
  - PA
  - LocalPath
  - Katz (nope, that is not a real Katz)
  
- and some degree featrues depends on {igraph}

- I build a structure list, you have to first load a network (mx3 dataframe) with the cols "from, to, weight", and pass it to f_preprocess at first. otherwise it will report errors when using f_addxx.

- well actually nothing is ready for any other people to use it yet. not bug free and no docs, if there is any problem please contact.
