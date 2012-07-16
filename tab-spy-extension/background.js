//====================================================================
// Copyright (c) 2011-2012, David Haglund
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//     * Redistributions of source code must retain the above
//       copyright notice, this list of conditions and the following
//       disclaimer.
//
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials
//       provided with the distribution.
//
//     * Neither the name of the copyright holder nor the names of its
//       contributors may be used to endorse or promote products
//       derived from this software without specific prior written
//       permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
//====================================================================
/*globals chrome,localStorage,WebSocket,console,setTimeout */

(function () {
    'use strict';
    var ws, connected = false, backoff = 1000;

    if (!localStorage.queue) {
        localStorage.queue = JSON.stringify([]);
    }

    if (!localStorage.client_id) {
        localStorage.client_id = (new Date()).getTime().toString();
    }

    function addToQueue(event) {
        var queue = JSON.parse(localStorage.queue);
        queue.push(event);
        chrome.browserAction.setBadgeText(
            {"text":
                queue.length < 10000 ? queue.length.toString() : "9999"}
        );
        localStorage.queue = JSON.stringify(queue);
    }

    // TODO: now it is possible to lose events, fixit
    function emptyQueue(callback) {
        var queue = JSON.parse(localStorage.queue);
        queue.forEach(callback);
        chrome.browserAction.setBadgeText({"text": ""});
        localStorage.queue = JSON.stringify([]);
    }

    function setupWebsocket() {
        ws = new WebSocket("ws://localhost:8765/tabs");
        ws.onopen = function () {
            console.log("Connected!");
            backoff = 1000;
            connected = true;
            emptyQueue(function (event) {
                ws.send(JSON.stringify(event));
            });
        };
        ws.onclose = function () {
            connected = false;
            console.log("connection to WebSocket closed! " +
                        "trying again in " + Math.floor(backoff / 1000) +
                        " seconds.");
            setTimeout(setupWebsocket, Math.random() * backoff + backoff / 2);
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
            addToQueue(event);
        }
    }

    function addEventForTab(eventType, tab) {
        var event = {"event": eventType,
                     "client_id": localStorage.client_id,
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
                     "client_id": localStorage.client_id,
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
                     "client_id": localStorage.client_id,
                     "windowId": windowId,
                     "time": new Date()};
            addEvent(event);
        }
    });

    chrome.browserAction.onClicked.addListener(function (tab) {
        addEventForTab("watched", tab);
    });
}());
