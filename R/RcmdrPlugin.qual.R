# Some Rcmdr dialogs for the epack package

# last modified: April 15 2009 by E. Hodgess

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
   

