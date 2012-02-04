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
            queue.forEach(function (event) {
                ws.send(JSON.stringify(event));
            });
            queue = [];
        };
        ws.onclose = function () {
            connected = false;
            console.log("connection to WebSocket closed! " +
                        "trying again in " + Math.floor(backoff / 1000) +
                        " seconds.");
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
            ws.send(JSON.stringify(event));
        } else {
            queue.push(event);
        }
    }

    function addEventForTab(eventType, tab) {
        var event = {"event": eventType,
                     "tabId": tab.id,
                     "windowId": tab.windowId,
                     "title": tab.title,
                     "url": tab.url,
                     "active": tab.active,
                     "time": new Date()};
        addEvent(event);
    }

    function addEventByTabAndWindowId(eventType, tabId, windowId) {
        var event = {"event": eventType,
                     "tabId": tabId,
                     "time": new Date()
                    };
        if (windowId !== chrome.windows.WINDOW_ID_NONE) {
            event.windowId = windowId;
        }
        addEvent(event);
    }

    function addEventByTabId(eventType, tabId) {
        addEventByTabAndWindowId(eventType, tabId,
                                 chrome.windows.WINDOW_ID_NONE);
    }

    setupWebsocket();

    chrome.windows.getAll(null, function (windows) {
        windows.forEach(function (window) {
            chrome.tabs.getAllInWindow(window.id, function (tabs) {
                tabs.forEach(function (tab) {
                    addEventForTab("startup", tab);
                });
            });
        });
    });

    chrome.tabs.onCreated.addListener(function (tab) {
        addEventForTab("created", tab);
    });

    chrome.tabs.onRemoved.addListener(function (tabId) {
        addEventByTabId("removed", tabId);
    });

    chrome.tabs.onActiveChanged.addListener(function (tabId, selectInfo) {
        addEventByTabAndWindowId("activeChanged", tabId, selectInfo.windowId);
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
                     "windowId": windowId,
                     "time": new Date()};
            addEvent(event);
        }
    });

    chrome.browserAction.onClicked.addListener(function(tab) {
        addEventForTab("watched", tab);
    });
}());
