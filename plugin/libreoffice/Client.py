import sys
import urllib
import os
import asyncio
import websockets
from websockets.client import WebSocketClientProtocol
from websockets.protocol import WebSocketCommonProtocol
import traceback
import json
import ssl
import threading
import logging
import datetime
import copy
import webbrowser 
import requests
import urllib 
import tempfile

from urllib.parse import urlencode

logging.basicConfig(filename="./demoscriptoutput.log", level = 
        logging.DEBUG, filemode = "w", format="format=%(asctime)s %(name)-12s %(levelname)-8s %(threadName)s %(message)s")

logger = logging.getLogger(__name__)    
logger.debug("Loaded script file "  + os.getcwd())

## Display a dictionary on the spreadsheet.
## Clients update elements to the map
## Display an element if needed.
## Clients request for the next available row index.
class TableDisplay:
    def __init__(self):
        logger.debug ("Creating table display")
        self.startRow = 2
        # The next available row. Count the header as 
        # the first row.
        self.availableRowIndex = 2
        self.dataMap = {}
        self.indexMap = {}
    def add(self, anEntry):
        key = anEntry.key()
        if key in self.indexMap:
            pass
        else:
            self.indexMap[key] = self.availableRowIndex 
            self.availableRowIndex = self.availableRowIndex + 1
        self.dataMap[key] = anEntry

    def getComputedRow(self, anEntry):
        key = anEntry.key();
        if key in self.indexMap:
            return self.indexMap[key]
        else:
            return self.availableRowIndex
    def values(self):
        return self.dataMap.values();
# Design notes: 
# Use composition over inheritance.
class Util:
    # Indexes are zero based.
    @staticmethod
    def convertToBool(aString):    
        bool(aString)

class DemoClient:
    def __init__(self):
    #get the doc from the scripting context which is made available to all scripts
        desktop = XSCRIPTCONTEXT.getDesktop()
        model = desktop.getCurrentComponent()
    #check whether there's already an opened document. Otherwise, create a new one
        if not hasattr(model, "Sheets"):
            model = desktop.loadComponentFromURL(
                "private:factory/scalc","_blank", 0, () )
    #get the XText interface
        sheet = model.Sheets.getByIndex(0)

    def isEven(self, aCellAddress):
        return ((aCellAddress.Row % 2) == 0)

    def updateCellContent(self, worksheet, cell, value):
        try:
            if cell == None:
                logger.debug("No cell found for " + str(value))
            else:                
                logger.debug("Updating worksheet by name " + worksheet + " CELL " + str(cell) + ": Value " + str(value))
                sheet = self.getWorksheetByName(worksheet)
                if sheet == None:
                    logger.debug("No sheet found for " + worksheet);
                    return
                tRange = sheet.getCellRangeByName(cell)
                tAddress = tRange.getCellAddress()
                cell = sheet.getCellByPosition(tAddress.Column, tAddress.Row)
                #Const nCellBackColor = 15132415 REM # "Blue gray"
                if (self.isEven(tAddress)):
                    cell.CellBackColor = 15132415
                tRange.String = value
        except:
            logger.error(traceback.format_exc())

    @asyncio.coroutine
    def updateCellContentT(self, worksheet, cell,value):
        self.updateCellContent(worksheet, cell, value)

    def workSheetExists(self, aName):
        sheet = self.getWorksheetByName(aName);
        return (sheet != None)
    
    #Either create a new sheet or return an existing one.
    def upsertNewWorksheet(self, aName): 
        if aName == None:
            return None
        desktop = XSCRIPTCONTEXT.getDesktop()
        model = desktop.getCurrentComponent()
        logger.debug("Model " + str(model.Sheets))
        logger.debug("Creating new sheet " + aName)
        if self.workSheetExists(aName):
            return self.getWorksheetByName(aName)
        else:
            newSheet = model.Sheets.insertNewByName(aName, 1)
            row = 1
            displayName = aName
            logger.debug("Adding header rows for " + displayName)
            return newSheet

    def getCellContentForSheet(self, sheetName, aCell):
        sheet = self.getWorksheetByName(sheetName);
        tRange = sheet.getCellRangeByName(aCell);
        return tRange.String
    
    def getCellContent(self, aCell): 
        return self.getCellContentForSheet("user_login_sheet", aCell);

    def getWorksheetByIndex(self, worksheetIndex): 
        desktop = XSCRIPTCONTEXT.getDesktop()
        model = desktop.getCurrentComponent()
        sheet = model.Sheets.getByIndex(worksheetIndex)    
    def getWorksheetByName(self, worksheetName): 
        try:
            logger.info("Worksheet name " + worksheetName);
            desktop = XSCRIPTCONTEXT.getDesktop()
            if desktop == None:
                return None;
            model = desktop.getCurrentComponent()
            if model == None: 
                logger.fatal("This can never happen " + worksheetName)
            if model.Sheets != None:
                try:                
                    sheet = model.Sheets.getByName(worksheetName) 
                    return sheet   
                except:
                    logger.error("Couldnt find worksheet " + worksheetName);
                    return None;
            else:
                return None;
        except:
            logger.error(traceback.format_exc())

    def getMarketDataWorksheet(self):
        return self.getWorksheetByName(self.markeDataSheet);

    def getCellContent(self, aCell): 
        sheet = self.getWorksheet(0);
        tRange = sheet.getCellRangeByName(aCell)
        return tRange.String

    def getWorksheet(self, anIndex) :
        desktop = XSCRIPTCONTEXT.getDesktop()
        model = desktop.getCurrentComponent()
        sheet = model.Sheets.getByIndex(anIndex)
        return sheet

    def clearInfoWorksheet(self):
        sheet = self.getWorksheet(self.INFO_WORK_SHEET);
        count = self.INFO_ROW_COUNT ## TBD: Needs to be a constant.
        while count < self.INFO_ROW_COUNT:
            cellId = "A" + str(count)
            trange = sheet.getCellRangeByName(cellId)
            trange.String = ""
            count = count + 1

    def updateInfoWorksheet(self, aMessage) :
        logger.info("Processing message " + aMessage);

    def clearErrorWorksheet(self):
        sheet = self.getWorksheet(0);
        tRange = sheet.getCellRangeByName(self.ERROR_CELL)
        tRange.String = ""

    def updateErrorWorksheet(self, aMessage) :
        sheet = self.getWorksheet(0);
        tRange = sheet.getCellRangeByName(self.ERROR_CELL)
        tRange.String = tRange.String + "\n" + (str (aMessage))

    def clearCells(self):
        self.clearCompanySelectionListBox()
        self.clearInfoWorksheet();
        self.clearErrorWorksheet();
        self.INFO_ROW_COUNT = 30
        self.portfolioDetailCount = 0


    def commandDictionary (self) :
        return {
        u"Login"                 : LOGIN_COMMAND, 
        u"DemoUpload"             : DEMO_UPLOAD
        }

    def sendKeepAlive(self) :
        k = {
            "nickName" : self.getUserName()
            , "commandType" : "KeepAlive"
            , "keepAlive" : "Ping"
        }
        return k
        
    def handleKeepAlive(self, jsonRequest) :
        return None 

    def keepAliveInterval(self) :
        self.interval = self.getCellContent(self.KEEP_ALIVE_CELL)
        return self.interval

    ## Send a keep alive request every n seconds. 
    ## This is not entirely accurate: the client needs to send 
    ## a message only after n seconds of idle period. TODO
    @asyncio.coroutine
    def send(self, aJsonMessage):
        try:
            yield from self.websocket.send(json.dumps(aJsonMessage))
        except:
            logger.error("Unable to send " + str(aJsonMessage))
            # yield from self.websocket.close()
            logger.error(traceback.format_exc())

    @asyncio.coroutine
    def keepAlivePing(self):
        try:
            logger.debug("Starting the keep alive timer..")

            while True: 
                reply = self.sendKeepAlive();
                logger.debug("Reply " + str(reply))
                serverConnection = self.websocket                                
                logger.debug("Keep alive ping:" + str(reply) + " Sleeping " + self.keepAliveInterval())        
                self.sendAsTask(reply)
                yield from asyncio.sleep(int(self.keepAliveInterval()), loop = self.loop)
                
        except:
            logger.error(traceback.format_exc())
            return None

    def handleLoginResponse(self, data) :
        (self.loop.create_task(self.keepAlivePing()))
        logger.debug("Handling login response: " + str(data))
        result = self.sendUserLoggedIn(data);
        return result

    def getCommandType(self, incomingJson) :
        data = json.loads(incomingJson);
        if "commandType" in data: 
            return data["commandType"]
        elif "Right" in data:
            return (data["Right"])["commandType"]
        else:
            return "Undefined"

    def getCommandTypeValue(self, aCommandType) : 
        return self.commandDictionary()[aCommandType]
    
    """ 
        Send a json request by wrapping it inside a task 
    """
    def sendAsTask(self, aJsonRequest):
        logger.debug(">>>" + str(aJsonRequest))
        self.loop.create_task(self.send(aJsonRequest));

    def handleSendMessage(self, jsonResponse):
        try:    
            logger.debug("Not handling " + str(jsonResponse));
            return None
        except:
            self.updateErrorWorksheet(traceback.format_exc())
            return None

    def clientConnection (self) : 
        CONNECTION_CELL = "B1";
        return self.getCellContentForSheet("settings", CONNECTION_CELL);

    ### Right in the json response implies no errors.
    def processIncomingCommand(self, payloadI) :
        cType = self.getCommandType(payloadI);
        payloadJ = json.loads(payloadI)
        if "Right" in  payloadJ:
            payload = payloadJ["Right"]; 
        elif "Left" in payloadJ:
            self.updateErrorWorksheet(payloadJ);
            return;
        else:
            payload = payloadJ

        commandType = self.getCommandTypeValue(cType)
        if commandType == LOGIN_COMMAND: 
            reply = self.handleLoginResponse(payload);
        elif commandType == DEMO_UPLOAD:
        else:
            reply = None
        return reply

    #klass=WebSocketClientProtocol, timeout=10, max_size=2 ** 20, 
    #max_queue=2 ** 5, loop=None, origin=None, subprotocols=None, extra_headers=None, **kwds
    @asyncio.coroutine
    def demoLoop(self, userName, password):
        l = self.loop
        logger.debug("Before making connection")
        self.websocket = yield from websockets.client.connect(self.clientConnection()
                , loop = self.loop)
        logger.debug("CCAR loop %s, ***************", userName)
        try:
            while True:
                try: 
                    response = yield from  self.websocket.recv()
                    logger.debug("Reply --> " + str(reply));
                    if reply == None:
                        #logger.debug(" Not sending a response " + response);
                        pass
                    else:
                        yield from self.websocket.send(json.dumps(reply))
                except:
                    error = traceback.format_exc()
                    logger.error(error)
                    return "Loop exiting"
        except:
            logger.error(traceback.format_exc())
            logger.error("Closing connection. See exception above")
            yield from self.websocket.close()
        

    def LOGGER_CELL():
        return "A23"



    def login (self, loop, userName, password, ssl):
        try:
            self.loop = loop
            loop.run_until_complete(self.demoLoop(userName, password))
        except:
            error = traceback.format_exc() 
            logger.error(error);
            return "Error while logging in" 
        finally:
            logger.debug("Exiting main loop")
            return "Finished processing login"
### End class



# We find out all the portfolio changes
class PortfolioChanges:
    def __init__(self, demoClient, portfolioId):
        logger.debug("Collect a list of all changes for this portfolio")
        self.updates = {} 
        self.adds = {} 
        self.deletes = {}
        self.portfolioId = portfolioId
        self.demoClient = demoClient


    def getPortfolioSymbolDict(self, portfolioId):
        logger.debug("Return all the portfolios from the server");
        portfolioSymbolsServer = self.demoClient.portfolioGroup.getPortfolioSymbolTable(portfolioId).getPortfolioSymbols();
        serverDict = {}
        for s in portfolioSymbolsServer: 
            serverDict[s] = s
        return serverDict;        

    def collectNewrowsForPortfolio(self, portfolioId):
        # This is not going to work.
        # Look at the places where the updates happen.
        logger.debug("Compute changes for a " + portfolioId);
        maxRows = 200; # Approximate page size.
        portfolioSymbolsServer = self.demoClient.portfolioGroup.getPortfolioSymbolTable(portfolioId).getPortfolioSymbols();
        serverDict = {}
        for s in portfolioSymbolsServer: 
            serverDict[s] = s
        localSymbols = []
        localDict = self.demoClient.localDict
        logger.debug("Local dictionary " + str(localDict))
        nickName = self.demoClient.getNickName()
        for x in range(2, maxRows):
            p = self.createPortfolioSymbol(portfolioId, nickName, nickName, nickName, "", x)
            logger.debug("Portfolio created " + str(p));
            if p != None:
                localSymbols.append(p)

        for event in localSymbols :
            logger.debug("Setting s " + str(event))
            localDict[event] = event

        for l in localDict:
            # Create handles both insert and update. This is obviously not efficient.
            # We will manage updates correctly.
            e = localDict[l]            
            if (not e in serverDict):
                e.updateCrudType("Create")
                logger.debug("Adding a new symbol: current portfolio value" + str(l))
                self.demoClient.sendManagePortfolioSymbol(l.asJson())
        for s in serverDict:
            e = serverDict[s]
            if (not e in localDict):
                e.updateCrudType("Delete")
                logger.debug("Deleting an existing symbol: current portfolio value" + str(e))
                self.demoClient.sendManagePortfolioSymbol(e.asJson())



    def register(self, localVal, remoteVal):
        if hasChanged(localVal, remoteVal):
            saveLocal(localVal);
        else:
            pass;

    def saveLocal(self, localValue): 
        self.demoClient.localDict[localValue];

    def hasChanged(self, p1, p2) :
        # Has the value changed
        if p1 == None :
            return False
        if p2 == None : 
            return False;
        return p1.quantity != p2.quantity 

    def createLocalvalue(self, portfolioSymbol, creator, updator, nickName, crudType, row):
        assert (portfolioSymbol != None);
        portfolioId = portfolioSymbol.portfolioId;
        p = self.createPortfolioSymbol(portfolioId, creator, updator, nickName, "", row);
        # if p != None:
        #     p.updateCrudType("Create")
        #     self.demoClient.sendManagePortfolioSymbol(p.asJson());
        #     return p;


    def createPortfolioSymbol(self, portfolioId, creator, updator, nickName, crudType, row):

                symbol      = self.demoClient.getCellContentForSheet(portfolioId, "A" + str(row))
                quantity    = self.demoClient.getCellContentForSheet(portfolioId, "B" + str(row)) 
                side        = self.demoClient.getCellContentForSheet(portfolioId, "C" + str(row))
                symbolType  = self.demoClient.getCellContentForSheet(portfolioId, "D" + str(row))
                value       = self.demoClient.getCellContentForSheet(portfolioId, "E" + str(row))
                stressValue = self.demoClient.getCellContentForSheet(portfolioId, "F" + str(row))
                dateTime    = str(datetime.datetime.now())
                if symbol == None or symbol == "": 
                    return None;
                logger.debug("Creating portfolio symbol for id " + portfolioId + " for row " + str(row) + " symbol " + symbol + 
                                        " " + "Quantity " + quantity);
                jsonrecord = {
                      "commandType" : "ManagePortfolioSymbol"
                    , "crudType" : crudType
                    , "portfolioId" : portfolioId
                    , "symbol" : symbol 
                    , "quantity" : quantity
                    , "side" : side 
                    , "symbolType" : symbolType 
                    , "value" : value 
                    , "stressValue" : stressValue 
                    , "dateTime" : dateTime
                    , "creator" : creator 
                    , "updator" : updator 
                    , "nickName" : nickName
                }
                logger.debug("Portfolio json " + str(jsonrecord))
                return PortfolioSymbol(self.demoClient, jsonrecord)



### End Class
class ClientOAuth :
    def __init__(self, loginHint):
        logger.debug("Creating an oauth client")
        self.loginHint = loginHint
        # This url needs to change to the actual site.
        self.url = "http://localhost:3000/gmailOauthRequest"
    def getRequest(self):
        logger.debug("Creating a auth request")
        r = requests.get(self.url + "/" + self.loginHint)
        oauthJson = json.loads(r.text)
        authUri = oauthJson["authDetails"]["authorizationURI"]
        clientId = oauthJson["clientId"]["unCI"]
        responseType = "code"
        scope = "openid email"
        redirect_uri = oauthJson["redirectURLs"][0]
        login_hint = self.loginHint
        payload = {
                "client_id" : clientId, 
                "response_type" : responseType,
                "scope" : scope,
                "redirect_uri" : redirect_uri,  
                "login_hint" : login_hint}
        logger.debug("Auth uri " + authUri)
        logger.debug("payload " + urlencode(payload))
        authRequest = authUri + "?" + urlencode(payload)
        logger.debug ("Auth url " + authRequest)
        return authRequest

    def showBrowser(self):
        logger.debug ("Display web browser with the server params");
        webbrowser.open(self.getRequest())


def StartClient(*args):
    try:
        """Starts the Demo client."""
        asyncio.get_event_loop().set_debug(enabled=True);
        logger.debug("Starting the client..%s", str(args))
        client = DemoClient()
        client.clearCells()
        s = client.getSecuritySettings()
        l = client.getCellContent(client.LOGIN_CELL)
        p = client.getCellContent(client.PASSWORD_CELL)
        loop = asyncio.get_event_loop()
        import threading
        t = threading.Thread(target = client.login, args=(loop, l, p, Util.convertToBool(s)))
        t.start()
    except:
        logger.error(traceback.format_exc())

