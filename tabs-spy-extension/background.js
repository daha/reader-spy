// TODO: handle that the websocket goes down
//       * store events in LocalStorage?
//       * Notify the user?
// TODO: make the port configurable

(function () {
    var ws, connected = false, queue = [], backoff = 1000;

    function setupWebsocket() {
        ws = new WebSocket("ws://localhost:8765/tabs");
        ws.onopen = function () {
            console.log("Connected!");
            backoff = 1000;
            connected = true;
            ws.send(JSON.stringify({"hello": "world"}));
            queue.forEach(function (event) {
                ws.send(JSON.stringify(event));
            });
            queue = [];
        };
        ws.onclose = function () {
            connected = false;
            console.log("connection to WebSocket closed! " +
                        "trying again in " + backoff + " seconds.");
            setTimeout(setupWebsocket, backoff);
            if (backoff < 180000) {
                backoff *= 2;
            }
        };
        ws.onmessage = function (evt) {
            console.log(evt);
        };
    }
    function addEvent(event) {
        if (connected) {
            console.log("sending");
            console.log(event);
            ws.send(JSON.stringify(event));
        } else {
            console.log("storing");
            console.log(event);
            queue.push(event);
        }
    }

    function addEventForTab(eventType, tab) {
        var event = {"event": eventType,
                 "title": tab.title,
                 "url": tab.url,
                 "active": tab.active,
                 "time": new Date()};
        addEvent(event);
    }

    function addEventByTabId(eventType, tabId) {
        chrome.tabs.get(tabId, function (tab) {
            addEventForTab(eventType, tab);
        });
    }

    setupWebsocket();

    chrome.windows.getAll(null, function (windows) {
        windows.forEach(function (window) {
            chrome.tabs.getAllInWindow(window.id, function (tabs) {
                tabs.forEach(function (tab) {
                    console.log("all windows tab");
                    console.log(tab);
                    addEventForTab("startup", tab);
                });
            });
        });
    });

    chrome.tabs.onCreated.addListener(function (tab) {
        addEventForTab("created", tab);
    });

    chrome.tabs.onRemoved.addListener(function (tabId) {
        // TODO: the tab is no longer valid, need to cache the data or
        //       use the tabId in the events instead.
        addEventByTabId("removed", tabId);
    });

    chrome.tabs.onActiveChanged.addListener(function (tabId, selectInfo) {
        addEventByTabId("activeChanged", tabId);
    });

    chrome.tabs.onUpdated.addListener(function (tabId, changeInfo, tab) {
        if (changeInfo.url) {
            addEventForTab("updated", tab);
        }
    });

    chrome.windows.onFocusChanged.addListener(function (windowId) {
        var query, event;
        if (windowId !== -1) {
            query = {"active": true, "windowId": windowId};
            chrome.tabs.query(query, function (tabs) {
                tabs.forEach(function (tab) {
                    addEventForTab("activeChanged", tab);
                });
            });
        } else {
            event = {"event": "unfocused",
                     "time": new Date()};
            addEvent(event);
        }
    });
}());
