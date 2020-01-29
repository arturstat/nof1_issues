# IDEAL project
# http://www.ideal.rwth-aachen.de/
#
# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Defines 'lmeModel' class.
#
# Remarks:
#  None.

# R program C.8

# lmList class
setOldClass(
  Classes=c(
    "lmList"
  )
);

# groupedData classes
setOldClass(
  Classes=c(
    "nfnGroupedData",
    "nfGroupedData",
    "nffGroupedData",
    "nmGroupedData",
    "groupedData"
  )
);

# pdMat classes
setOldClass(
  Classes=c(
    "pdBlocked",
    "pdCompSymm",
    "pdDiag",
    "pdIdent",
    "pdSymm"
  )
);

# corStruct classes
setOldClass(
  Classes=c(
    "corAR1",
    "corARMA",
    "corCAR1",
    "corCompSymm",
    "corExp",
    "corGaus",
    "corLin",
    "corRatio",
    "corSpher",
    "corSymm"
  )
);

# varFunc classes
setOldClass(
  Classes=c(
    "varExp",
    "varPower",
    "varConstPower",
    "varIdent",
    "varFixed",
    "varComb"
  )
);

setClassUnion(
  name="lmeModel.data",
  members=c(
    "data.frame",
    "nfnGroupedData",
    "nffGroupedData",
    "nfGroupedData",
    "nmGroupedData",
    "groupedData"
  )
);

setClassUnion(
  name="lmeModel.fixed",
  members=c(
    "formula",
    "lmList",
    "nfnGroupedData",
    "nffGroupedData",
    "nfGroupedData",
    "nmGroupedData",
    "groupedData"
  )
);

setClassUnion(
  name="lmeModel.random",
  members=c(
    "formula",
    "list",
    "pdBlocked",
    "pdCompSymm",
    "pdDiag",
    "pdIdent",
    "pdSymm"
  )
);

setClassUnion(
  name="lmeModel.groups",
  members=c(
    "formula",
    "NULL"
  )
);

setClassUnion(
  name="lmeModel.corStruct",
  members=c(
    "corAR1",
    "corARMA",
    "corCAR1",
    "corCompSymm",
    "corExp",
    "corGaus",
    "corLin",
    "corRatio",
    "corSpher",
    "corSymm",
    "NULL"
  )
);

setClassUnion(
  name="lmeModel.varFunc",
  members=c(
    "varExp",
    "varPower",
    "varConstPower",
    "varIdent",
    "varFixed",
    "varComb",
    "NULL"
  )
);

setClass(
  Class="lmeModel",
  slots=c(
    data="lmeModel.data",
    fixed="lmeModel.fixed",
    random="lmeModel.random",
    groups="lmeModel.groups",
    correlation="lmeModel.corStruct",
    weights="lmeModel.varFunc",
    method="character"
  )
);

valid.lmeModel <- function(object) {
  if ( inherits(x=object@fixed, what="formula") ) {
    if (length(object@fixed) != 3) {
      return("'fixed' must be a formula of the form 'resp ~ pred'");
    }
    if ( !all( all.vars(object@fixed) %in% names(object@data) ) ) {
      return("some variables in 'fixed' are not available in 'data'");
    }
  }
  if ( inherits(x=object@random, what="formula") ) {
    if (length(object@random) != 2) {
      return("'random' must be a formula of the form '~ pred'");
    }
    if ( !all( all.vars(object@random) %in% names(object@data) ) ) {
      return("some variables in 'random' are not available in 'data'");
    }
  } else if ( is.list(object@random) ) {
    for ( i in 1:length(object@random) ) {
      if ( inherits(x=object@random[[i]], what="formula") ) {
        if (length(object@random[[i]]) != 2) {
          return("'random' must contain a formula of the form '~ pred'");
        }
        if ( !all( all.vars(object@random[[i]]) %in% names(object@data) ) ) {
          return("some variables in 'random' are not available in 'data'");
        }
      } else if ( !inherits(x=object@random[[i]], what="pdMat") )
        return("'random' must be a 'formula', a 'pdMat' object, or a list of these");
    }
  }
  if ( !is.null(object@groups) ) {
    if (length(object@groups) != 2) {
      return("'groups' must be a formula of the form '~ pred'");
    }
    if ( !all( all.vars(object@groups) %in% names(object@data) ) ) {
      return("some variables in 'groups' are not available in 'data'");
    }
  }
  if ( !(object@method == "REML" | object@method == "ML") ) {
    return("'method' is not 'REML' nor 'ML'");
  }
  return(TRUE);
} # valid.lmeModel

invisible(
  setValidity(
    Class="lmeModel",
    method=valid.lmeModel
  )
);
