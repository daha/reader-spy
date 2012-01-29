var timedTabs = Array();
var totalCreated = 0;
var totalRemoved = 0;

console.log("load");
var ws = new WebSocket("ws://localhost:8765");
console.log(ws);
ws.onopen = function() {
    time = new Date();
    var event = {"time": time.toJSON(), "event": "start"};
    ws.send(JSON.stringify(event));
}

// ws.onmessage = function (evt) {}
// ws.onclose = function() {}

// Count currently open tabs when script starts.
chrome.windows.getAll(null, function (windows) {
    for (i in windows) {
        chrome.tabs.getAllInWindow(windows[i].id, function (tabs) {
            console.log("window: " + i + " tabs");
            console.log(tabs);
            totalCreated += tabs.length;
            timedTabs.push([getCurrentTime(), totalCreated - totalRemoved]);
        });
    }
});

// Listeners for created and removed tabs.
chrome.tabs.onCreated.addListener(function(tab) {
    console.log("created");
    console.log(tab);
    chrome.tabs.get(tab.id, function(t) { console.log(t); });
    totalCreated++;
    timedTabs.push([getCurrentTime(), totalCreated - totalRemoved]);
});
chrome.tabs.onRemoved.addListener(function(tabId) {
    console.log("removed" + tabId);
    totalRemoved++;
    timedTabs.push([getCurrentTime(), totalCreated - totalRemoved]);
});

chrome.tabs.onActiveChanged.addListener(function(tabId, selectInfo) {
    console.log("activeChanged tabId: " + tabId +
                ", windowId: " + selectInfo.windowId);
});

chrome.tabs.onUpdated.addListener(function(tabId, changeInfo, tab) {
    console.log("onUpdated: " + tabId);
    console.log(changeInfo);
    console.log(tab);
});

chrome.windows.onFocusChanged.addListener(function(windowId) {
    console.log("onFocusChanged: " + windowId);
    if (windowId != -1) { 
        chrome.tabs.query({"active": true,
                           "windowId": windowId}, function (tabs) {
                               console.log("onFocusChanged");
                               console.log(tabs);
                           });
        }
});


// Get current time with timezone offset for flot.
function getCurrentTime() {
    time = new Date();
    console.log("time" + time);
    return time.getTime() - (time.getTimezoneOffset() * 60 * 1000);
}
