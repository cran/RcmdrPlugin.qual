# Some Rcmdr dialogs for the epack package

# last modified: October 28 2009 by E. Hodgess

# Note: the following function (with contributions from Richard Heiberger) 
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

    
   
xbara <- function() {
	initializeDialog(title=gettextRcmdr("Xbar Chart"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=xbara, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
 
        
 	command <- paste('qcc(',dsnameValue,',"xbar")',sep="")
#	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }


rchart <- function() {
	initializeDialog(title=gettextRcmdr("r Chart"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=rchart, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
 
        
 	command <- paste('qcc(',dsnameValue,',"R")',sep="")
#	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }


schart <- function() {
	initializeDialog(title=gettextRcmdr("s Chart"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=schart, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
 
        
 	command <- paste('qcc(',dsnameValue,',"S")',sep="")
#	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }

npchart <- function() {
	initializeDialog(title=gettextRcmdr("np Chart"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
 nVar <- tclVar("1")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
  
  
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=npchart, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
    n <- round(as.numeric(tclvalue(nVar)))
        if (is.na(n) || n <= 0){
            errorCondition(recall=pchart, message="Length must be a 
			positive number.")
            return()
            }
     
        command <- paste('qcc(',dsnameValue,',"np",size=',n,')',sep="")
 #	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(tklabel(top, text="How many?"), nEntry, sticky="e")
  
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
tkgrid.configure(nEntry, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }


pchart <- function() {
	initializeDialog(title=gettextRcmdr("p Chart"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
 nVar <- tclVar("1")
    nEntry <- tkentry(top, width="6", textvariable=nVar)
  
  
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=pchart, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
    n <- round(as.numeric(tclvalue(nVar)))
        if (is.na(n) || n <= 0){
            errorCondition(recall=pchart, message="Length must be a 
			positive number.")
            return()
            }
     
        command <- paste('qcc(',dsnameValue,',"p",size=',n,')',sep="")
 #	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(tklabel(top, text="How many?"), nEntry, sticky="e")
  
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
tkgrid.configure(nEntry, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }
   


shapeMod <- function(){
    initializeDialog(title=gettextRcmdr("Shape a data frame"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
 
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=shapeMod, message=gettextRcmdr("You must select a variable."))
            return()
            }

   
        freq1 <- tclvalue(freqVariable)
	
          closeDialog()
#        doItAndPrint(paste("shape1(", ActiveDataSet(), "$", x,
#            ",freq=",freq1,
#            ")", sep=""))
	command <- paste("shape1(", ActiveDataSet(), "$", x,
         ",freq=",freq1,
            ")", sep="")
	justDoIt(command)
        logger(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="data.frame")
    freqFrame <- tkframe(top)
    freqVariable <- tclVar("1")
    freqField <- tkentry(freqFrame, width="6", textvariable=freqVariable)
      tkgrid(getFrame(xBox), sticky="nw") 
   tkgrid(tklabel(freqFrame, text=gettextRcmdr("Number of rows: ")), freqField, sticky="w")
    tkgrid(freqFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(freqField, sticky="e")
    dialogSuffix(rows=4, columns=2)
    }

shape1 <- function(x,freq) {
	reshape = function(df,nrow,ncol,byrow=TRUE)data.frame(matrix(as.matrix(df),nrow,ncol,byrow=byrow))
	n1 <- length(x)
	nc1 <- floor(n1/freq)
	y <- reshape(df=x,nrow=freq,ncol=nc1)
	assign("new.df",y,envir=.GlobalEnv)
	}

paretoMod <- function(){
    initializeDialog(title=gettextRcmdr("Pareto Chart"))
    xBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
 
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=shapeMod, message=gettextRcmdr("You must select a variable."))
            return()
            }

   
	
          closeDialog()
#        doItAndPrint(paste("shape1(", ActiveDataSet(), "$", x,
#            ",freq=",freq1,
#            ")", sep=""))
	command <- paste("pareto1(", ActiveDataSet(), "$", x,
                   ")", sep="")
	justDoIt(command)
        logger(command)
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="barplot")
      tkgrid(getFrame(xBox), sticky="nw") 

   tkgrid(buttonsFrame, columnspan=2, sticky="w")
       dialogSuffix(rows=4, columns=2)
    }


pareto1 <- function(x) {
	y1 <- table(x)
	barplot(rev(y1[order(y1)]))
	abline(h=0)
}


 
xbaro <- function() {
	initializeDialog(title=gettextRcmdr("Xbar Chart for Individuals"))
    dsname <- tclVar(gettextRcmdr("Dataset"))
   
    entryDsname <- tkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=xbaro, 
                message=gettextRcmdr("You must enter the name of a data set."))  
            return()
            }  
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
            }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
                }
            }
 
        
 	command <- paste('qcc(',dsnameValue,',"xbar.one")',sep="")
#	assign(justDoIt(command), envir=.GlobalEnv)
	justDoIt(command)
        logger(command)

 if (eval(parse(text=paste("nrow(", dsnameValue, ")"))) == 0){
            errorCondition(recall=newHistPrice, message=gettextRcmdr("empty data set."))
            return()
            }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())

            }
    OKCancelHelp(helpSubject="qcc")
  
  rightFrame <- tkframe(top)
    tkgrid(tklabel(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
  
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(rows=10, columns=2, focus=entryDsname)
    }

ewMod <- function(){
  initializeDialog(title=gettextRcmdr("EWMA"))
  require(qcc)
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
	print(x)
        if (length(x) == 0){
            errorCondition(recall=ewMod, message=gettextRcmdr("You must select a variable."))
            return()
            }
        closeDialog()
    doItAndPrint(paste("ewma(", ActiveDataSet(), "$", x,")",sep=""))     
	
             tkdestroy(top)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="ewma")
   tkgrid(getFrame(xBox), sticky="nw") 
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
     dialogSuffix(rows=4, columns=2)
    }
