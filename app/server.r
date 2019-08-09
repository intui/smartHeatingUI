
library(ggplot2)
library(openxlsx)
library(jsonlite)
library(lubridate)
library(tidyr)
library(stringr)
library(data.table)
library(DT)
library(httr)

library(shiny)
library(shinydashboard)
#library(shinyTime)

minTemp = 5
maxTemp = 50
verbose = TRUE
configFilename = "config.txt"
frequency = 60 #Updates in sec
#debug mode
#setwd("app")


## DB-Funktionen ##############################################################
readConfig <- function(configFilename) {
    config <- readLines(configFilename)
    temp <- str_extract(config,"AzureAPIKey=.*")
    temp <- temp[!is.na(temp)]
    temp <- str_replace(temp,"AzureAPIKey=","")
    Sys.setenv(APIKEY = temp)
}

#FlatID = 1
fetchData <- function(FlatID) {
    if(verbose) cat("fetchData\n")
    Areas <- read.xlsx("Bsp-DB.xlsx",sheet="Areas")
    Areas <- Areas[Areas$FlatID %in% FlatID,]
    Devices <- read.xlsx("Bsp-DB.xlsx",sheet="Devices")
    Devices <- Devices[Devices$Area %in% Areas$AreaID,]
    Sensors <- read.xlsx("Bsp-DB.xlsx",sheet="Sensors")
    Sensors <- Sensors[Sensors$AreaID %in% Areas$AreaID,]
    UserPrograms <- read.xlsx("Bsp-DB.xlsx",sheet="UserPrograms")
    UserPrograms <- UserPrograms[UserPrograms$DeviceID %in% Devices$DeviceID,]
    #SensorData <- read.xlsx("Bsp-DB.xlsx",sheet="SensorData")
    #ts <- as.POSIXct(as.POSIXct(paste(Sys.Date(),"00:00:00")):Sys.time(),origin=as.Date("1970-01-01"))
    #SensorData <- data.frame(
    #    SensorID = rep(1:6,each=length(ts)),
    #    TS = ts,
    #    Temp = rnorm(6*length(ts),16+rep(1:6,each=length(ts)),5)
    #)
    SensorID="00000000-0000-0000-0000-000000000007"
    fromTS <- as.POSIXct("2019-07-20 20:00:00")
    SensorData <- fetchSensorData(SensorID,fromTS)
    
    return(list(Areas=Areas,
                Devices=Devices,
                Sensors=Sensors,
                UserPrograms=UserPrograms,
                SensorData=SensorData))
}

SensorDataToDataframe <- function(RawData) {
    ### Achtung Sommerzeit
    
    return(data.frame(sensorDataID = sapply(RawData,FUN=function(x) {x$sensorDataID}),
                      SensorID = sapply(RawData,FUN=function(x) {x$sensorID}),
                      #tsCreated = 3600+as.POSIXct(sapply(RawData,FUN=function(x) {x$tsCreated}),format="%Y-%m-%dT%H:%M:%S"),
                      tsCreated = as.POSIXct(sapply(RawData,FUN=function(x) {x$tsCreated}),format="%Y-%m-%dT%H:%M:%S"),
                      sensorDataType = sapply(RawData,FUN=function(x) {x$sensorDataType}),
                      unit = sapply(RawData,FUN=function(x) {x$unit}),
                      value = sapply(RawData,FUN=function(x) {x$value}),
                      valueText = sapply(RawData,FUN=function(x) {x$valueText}),
                      stringsAsFactors = FALSE
    ))
}
fetchSensorData <- function(SensorID,fromTS=(Sys.time()-299)) {
    print("fetchSensorData")
    url = "https://smartheatingapi.azurewebsites.net/api/GetSensorData?"
    #SensorID="00000000-0000-0000-0000-000000000007"
    #fromTS <- as.POSIXct("2019-07-15 20:00:00")
    #fromTS <- max(TodayTemp$tsCreated)
    #fromTS <- Sys.time()
    apicall = paste0(url,
                     "sensorID=",SensorID,
                     "&dateFrom=",format(fromTS,"%Y-%m-%dT%H:%M:%S.000Z"),
                     "&code=",Sys.getenv("APIKEY"))
    raw.result <- GET(url = apicall)
    if(raw.result$status_code!=200) {
        print(paste0("Leider nicht erfolgreich, Statuscode: ",raw.result$status_code))
        return()
    } 
    if(length(content(raw.result))>0) {
        temp <- SensorDataToDataframe(content(raw.result))
        return(temp)
    }
    return()
}
#fetchDeviceTemp(1,max(SensorData$TS),Sys.time())
uploadUserProgramsToDB <- function(UserPrograms) {
    #Todo: upload to DB
    if(verbose) cat("uploadUserProgramsToDB\n")
    wb <- loadWorkbook("Bsp-DB.xlsx")
    writeData(wb, sheet = "UserPrograms", UserPrograms)
    saveWorkbook(wb, "Bsp-DB.xlsx", overwrite = TRUE)
}


## convert User Programs ######################################################
secondsFromHHMM <- function(timeStr) {
    h <- as.numeric(gsub(":[0-5][0-9]$","",timeStr))
    m <- as.numeric(gsub("^(0[0-9]|1[0-9]|2[0-3]|[0-9]):","",timeStr))
    return(3600*h+60*m)
}
convertJSONtoUP <- function(jsonString) {
    if(verbose) cat("convertJSONtoUP\n")
    #jsonString <- UserPrograms$UserProgram[1]
    up <- fromJSON(jsonString)$programItems
    h <- str_pad(floor(up$Seconds/3600),width=2,pad="0")
    m <- str_pad(floor((up$Seconds %% 3600)/60),width=2,pad="0")
    up$From <- paste0(h,":",m)
    up$To <- c(up$From[2:nrow(up)],"24:00")
    return(up)
}
convertUPToJSON <- function(UserProgram) {
    if(verbose) cat("convertUPToJSON\n")
    up <- data.frame(
        ProgramItemID = seq(0,nrow(UserProgram)-1),
        Seconds = secondsFromHHMM(UserProgram$From),
        TargetValue = UserProgram$TargetValue
        )
    return(as.character(toJSON(up)))
}


convertUPtoDF <- function(UserProgram, freq = 300) {
    if(verbose) cat("convertUPtoDF\n")
    secDay <- 24*60*60 - 1
    #up <- fromJSON(UserProgram)
    df <- data.frame(Seconds = seq(0,secDay,by=freq))
    df$TS <- force_tz(as.POSIXct(df$Seconds,origin=Sys.Date(), tz="UTC"), tzone = Sys.timezone())
    df$Time <- format(df$TS,"%H:%M")
    df <- merge(df, UserProgram[,c("Seconds","TargetValue")],all.x=TRUE) %>% fill(TargetValue)
    return(df)
}
frequencyFilter <- function(df, freq = 300, len = NA) {
    if(verbose) cat("frequencyFilter\n")
    if(!is.na(freq) & is.na(len)) {
        index <- round(seq(as.numeric(first(df$TS)),
                     as.numeric(last(df$TS)),
                     by = freq))
        return(df[df$TS %in% index,])
    } else if (!is.na(len)) {
        print(nrow(df))
        index <- round(seq(1,nrow(df),length.out = len) )
        return(df[index,])
    }
    return(NA)
}

# helper function to create delete button in DT
buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
}


# Define server logic
server <- function(input, output, session) {
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(
        UserName = NULL, 
        UserID = NULL, 
        FlatName = NULL,
        FlatID = NULL, 
        AreaID = NULL,
        DeviceID = NULL,
        SensorID = NULL,
        UserProgramID = NULL,
        data = NULL,
        status = NULL
        )
    
    # first row of new user program
    vals$newUP=data.frame(
        From = "00:00",
        To = "24:00",
        TargetValue = 21,
        stringsAsFactors = FALSE
    )
    
    readConfig(configFilename)
    
    
    # Modal Window with User- and Flat-Selection
    Users <- read.xlsx("Bsp-DB.xlsx",sheet="Users")
    Flats <- read.xlsx("Bsp-DB.xlsx",sheet="Flats")
    selectUserModal <- function(failed = FALSE) {
        modalDialog(title="Please select the user",
                    "Hier kommt der LogIn-Prozess hin",
                    selectInput('selectedUser', 'User', Users$UserText),
                    selectInput('selectedFlat', 'Wohnung', 
                                #Flats$FlatText[Flats$UserID==Users$UserID[Users$UserText==input$selectedUser]]),
                                Flats$FlatText),
                    footer = tagList(
                        actionButton("ok", "OK")
                    )
                    )}
    showModal(selectUserModal())
    
    observeEvent(input$ok, {
        vals$UserName <- input$selectedUser
        vals$UserID <- Users$UserID[Users$UserText==input$selectedUser]
        vals$FlatName <- input$selectedFlat
        vals$FlatID <- Flats$FlatID[Flats$FlatText==input$selectedFlat]
        vals$data <- fetchData(vals$FlatID)
        removeModal()
        
    })
    # Selection of Area
    observeEvent(input$selectedArea, {
        vals$AreaID <- vals$data$Area$AreaID[vals$data$Area$AreaText == input$selectedArea]
    })
    # Selection of Device
    # set also active user program
    observeEvent(input$selectedDevice, {
        vals$DeviceID <- vals$data$Device$DeviceID[vals$data$Device$DeviceText == input$selectedDevice]
        print(vals$DeviceID)
        if (!is.null(vals$DeviceID)) {
            temp <- vals$data$UserPrograms$UserProgram[vals$data$UserPrograms$DeviceID==vals$DeviceID &
                                                       vals$data$UserPrograms$Active==1]
            #print(temp)
            vals$activeUP <- convertJSONtoUP(temp)
            #print(vals$activeUP)
            vals$activeUPDF <- convertUPtoDF(vals$activeUP)
        }
    })
    observeEvent(input$selectedSensor, {
        #print("event selectedSensor")
        vals$SensorID <- vals$data$Sensors$SensorID[vals$data$Sensors$SensorText == input$selectedSensor]
        #print(vals$SensorID)
        if (!is.null(vals$SensorID)) {
            vals$status$TodayTemp <- vals$data$SensorData[
                vals$data$SensorData$SensorID==vals$SensorID & 
                    vals$data$SensorData$tsCreated>=as.POSIXct(paste(Sys.Date(),"00:00:00")),]
        #TodayTemp <- SensorData[
        #    SensorData$SensorID==SensorID & 
        #        SensorData$tsCreated>=as.POSIXct(paste(Sys.Date(),"00:00:00")),]
            #vals$status$TodayTemp <- frequencyFilter(
        #   vals$data$SensorData[vals$data$SensorData$SensorID==vals$SensorID
        #                                              & vals$data$SensorData$TS>=as.POSIXct(paste(Sys.Date(),"00:00:00")),]
        #
        #,len=50)
        }
    })
    observeEvent(input$selectedUP, {
        vals$UserProgramID <- vals$data$UserPrograms$UserProgramID[vals$data$UserProgram$UserProgramText == input$selectedUP]
    })
    
## new user program ###########################################################
    # the data table
    output$newUPTable=renderDataTable({
        DT=vals$newUP
        DT$Action = c(NA,
                      buttonInput(
                          FUN = actionButton,
                          len = nrow(DT)-1,
                          id = 'newUP_deleteButton_',
                          label = "Delete",
                          onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)'
                      ))
        datatable(DT,
                  escape=c("From","To","TargetValue"),editable=TRUE,selection="none",
                  options = list(dom = 't',ordering=F))
    })
    #add row
    observeEvent(input$newUPAddRow,{
        if(verbose) cat("newUPAddRow\n")
        newRow=data.table(
            From = vals$newUP$To[nrow(vals$newUP)],
            To = "24:00",
            TargetValue = vals$newUP$TargetValue[nrow(vals$newUP)]
        )
        vals$newUP=rbind(vals$newUP,newRow)
    })
    
    #delete row
    observeEvent(input$lastClick,{
        if(verbose) cat("lastClick:",input$lastClick,"\n")
        if (grepl("newUP_deleteButton_",input$lastClick)) {
            rowToDel <- as.numeric(gsub("newUP_deleteButton_","",input$lastClick))+1
            vals$newUP <-  vals$newUP[-rowToDel,]
        }
        
    })
    
    # cells are modified
    observeEvent(input$newUPTable_cell_edit, {
        info = input$newUPTable_cell_edit
        if(verbose) print(paste("newUPTable_cell_edit:",
                                paste0(info,collapse=";")))
        if(info$col<=2 & grepl("^(0[0-9]|1[0-9]|2[0-3]|[0-9]):[0-5][0-9]$|^24:00$",info$value)) {
            # first from value must be 00:00, last from value must be 24:00
            if( (info$row>1 | info$col!=1) & (info$row<nrow(vals$newUP) | info$col!=2) ) {
                vals$newUP[info$row,info$col] = info$value
                # if from is modified, change to of last row
                if (info$col==1 & info$row>1) vals$newUP$To[info$row-1] <- info$value
                # if to is modified, change from of next row 
                if (info$col==2 & info$row<nrow(vals$newUP)) vals$newUP$From[info$row+1] <- info$value
            }
        } else if(info$col==3) {
            if (is.numeric(info$value) & info$value>=minTemp & info$value<=maxTemp) {
                vals$newUP$TargetValue = info$value
            }
        }
    })
    # on new user program button open modal window
    observeEvent(input$createNewUP, {
        if(verbose) cat("createNewUP\n")
        createNewUPModal <- function(failed = FALSE) {
            modalDialog(size = "l",easyClose=TRUE,
                title="New User Program",
                textInput("newUPName","Name"),
                dataTableOutput("newUPTable"),
                actionButton("newUPAddRow","Add Row"),
                footer = tagList(
                        actionButton("NewUPModalOk", "OK"),actionButton("NewUPModalCancel", "Cancel")
                )
        )}
        showModal(createNewUPModal())
    })
    observeEvent(input$NewUPModalOk, {
        if(verbose) cat("NewUPModalOk\n")
        #create new user program
        if(verbose) print(vals$newUP)
        newRow <- data.frame(
            UserProgramID = nrow(vals$data$UserPrograms) + 1,
            UserProgramText = input$newUPName,
            DeviceID = vals$DeviceID,
            UserProgramType = "D",
            UserProgram = convertUPToJSON(vals$newUP),
            Active = 1,
            ValidFrom = NA,
            ValidTo = NA,
            stringsAsFactors = FALSE
        )
        if(verbose) print(newRow)
        vals$data$UserPrograms <- rbind(vals$data$UserPrograms,newRow)
        uploadUserProgramsToDB(vals$data$UserPrograms)
        removeModal()
    })
    observeEvent(input$NewUPModalCancel, {
        removeModal()
    })    
        
    
## Modify existing user program ###############################################  
    observeEvent(input$modifyUP, {
        createModifyUPModal <- function(failed = FALSE) {
            modalDialog(size = "l",easyClose=TRUE,
                        title="Modify User Program",
                        textInput("UPName","Name"),
                        dataTableOutput("newUPTable"),
                        actionButton("newUPAddRow","Add Row"),
                        footer = tagList(
                            actionButton("NewUPModalOk", "OK"),actionButton("NewUPModalCancel", "Cancel")
                        )
            )}
        showModal(createNewUPModal())
    })

## User Program Plot
    output$UPSelector <- renderUI({
        selectInput('selectedUP', 'User Program', 
                    vals$data$UserPrograms$UserProgramText[vals$data$UserPrograms$DeviceID %in% vals$DeviceID])
    })
    
    
    output$UPPlot <- renderPlot({
        #up <- convertJSONtoUP(UserPrograms$UserProgram[1])
        up <- convertJSONtoUP(vals$data$UserPrograms$UserProgram[vals$UserProgramID])
        
        df <- convertUPtoDF(up)
        ggplot(df,aes(x=TS,y=TargetValue)) + geom_line() +
            geom_vline(xintercept=Sys.time(),size=1,color="red") +
            coord_cartesian( 
                            ylim = c(5,max(df$TargetValue)+2))
    })
    
        
    
#   observe({
#        invalidateLater(2000, session)
#   })

## Status ####################################################################   
    output$currentTime <- renderText({
        invalidateLater(1000)
        format(Sys.time(),"%d.%m.%Y %H:%M:%S")
    })
    
    # Sensor selector
    output$SensorSelector <- renderUI({
        selectInput('selectedSensor', 'Sensor', 
                    vals$data$Sensors$SensorText[vals$data$Sensors$AreaID %in% vals$AreaID])
    })
    # aktuelle Temperatur und TargetValue
    output$statusPlot <- renderPlot({
        if (is.null(vals$SensorID)) {return()}
        #Abfrage jede Minute
        invalidateLater(frequency*1000)
        #vals$SensorID <- vals$data$Sensors$SensorID[vals$data$Sensors$SensorText == input$selectedSensor]
        #vals$status$TodayTemp <- frequencyFilter(
        #    vals$data$SensorData[vals$data$SensorData$SensorID==vals$SensorID
        #                         & vals$data$SensorData$TS>=as.POSIXct(paste(Sys.Date(),"00:00:00")),]
        #    ,len=500)
        cat("SensorID",vals$SensorID,"\n")
        #TodayTemp <- SensorData[SensorData$tsCreated>=as.POSIXct(Sys.Date()),]
        #max(TodayTemp$tsCreated)+frequency-1
        if (Sys.time()>max(vals$status$TodayTemp$tsCreated)+frequency-1) {
            #newValues <- fetchSensorData(SensorID,max(TodayTemp$tsCreated)+frequency-1)
            #newValues <- fetchSensorData(SensorID,Sys.time())
            newValues <- fetchSensorData(vals$SensorID,max(vals$status$TodayTemp$tsCreated)+frequency-1)
            #TodayTemp <- unique(rbind(TodayTemp,newValues))
            if(!is.null(newValues)) vals$status$TodayTemp <- unique(rbind(vals$status$TodayTemp,newValues))
        }
         g <- ggplot(vals$status$TodayTemp,aes(x=tsCreated,y=value)) + geom_line() +
            geom_vline(xintercept=Sys.time(),size=1,color="red") +
            coord_cartesian(xlim = c(Sys.time()-5*3600,Sys.time()+1800), 
                            ylim = c(5,max(vals$status$TodayTemp$value)+2))
         #print(vals$activeUPDF)
         
         g + geom_line(data = vals$activeUPDF,aes(x=TS,y=TargetValue),col = "darkblue")
         #print(g)
         
         
         #activeUPDF <- convertUPtoDF(up)
         
         #g <- ggplot(TodayTemp,aes(x=tsCreated,y=value)) + geom_line() +
         #    geom_vline(xintercept=Sys.time(),size=1,color="red") +
         #    coord_cartesian(xlim = c(Sys.time()-5*3600,Sys.time()+1800),
         #                    ylim = c(5,max(TodayTemp$value)+2))
        # 
        # g + geom_line(data = activeUPDF,aes(x=TS,y=TargetValue),col = "darkblue")
        #     
        # print(g)  
        
    })
        
## Sidebar ####################################################################   
    output$UserText <- renderText({paste0("User: ",vals$UserID)})
    output$FlatText <- renderText({paste0("Flat: ",vals$FlatID)})
    
    output$AreaSelector <- renderUI({
        selectInput('selectedArea', 'Area', vals$data$Areas$AreaText)
    })
    output$DeviceSelector <- renderUI({
        selectInput('selectedDevice', 'Device', 
                    vals$data$Device$DeviceText[vals$data$Device$AreaID %in% vals$AreaID])
    })
    
    
    

}


