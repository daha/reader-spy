Reader spy
==========
Reader spy is a project I started in order to track the
blogs/feeds/webpages I read and watch. It is not intended to spy on
anyone else, just to record data on me for myself.

Some of my goals are to:

*  learn about my reading patterns.
*  rank all my RSS feeds, I have hundreds!
*  build a classifier to recommend new posts which might be of
   interest to me and give me that feedback in Google Reader.
*  make it possible to trace back how I got to a certain page.

Getting started
===============
* Compile and start sponge (the data collection server).

```
cd sponge
rebar get-deps compile
./sponge.sh start
# The events are logged to the data directory in sponge.
```
* Install the tab spy extension in Chrome
  1. Open extensions, **Tools > Extensions**.
  2. Activate **developer mode**, if not already activated.
  3. Click the **Load uppacked extension** button.
  4. Navigate to the tab-spy-extension directory and click **OK**.

Current status
==============
This project currently contain one chrome extension, tab spy
extension, which records, create and remove events of tabs as well as
when a tab is active. When clicking the tab spy icon a watched event
is generated for the tab currently in focus. It stores the events
locally when the server is not accessable. The number of stored events
are printed in the tab spy icon. When connected to the server it sends
all events, json formated, to the server over a WebSocket. Events may
be lost if the server is stopped or crash when the extension is
sending events to the server before receiving the close event.

The WebSocket server, sponge, is very simple and is using
[cowboy](https://github.com/extend/cowboy). It only receives the
events and log them to a file, one, json formated, event per line
without any processing. Before the server is started it gzip compress
all the old logs from the same host to save space.

Background
==========
When processing RSS feeds I have two modes, _searching mode_ and
_reading mode_. In _searching mode_ I browse a large number of RSS
feeds and blog posts in Google reader. When I see something that might
be interesting I open it in the background. When I get too many tabs
to show the icons on the tabs I usually leave Google Reader and enter
_reading mode_. In _reading mode_ I start looking at the pages I just
opened. Some tabs are closed directly, others are read and some are
left for later to read.

The browsing the feeds takes a lots of time, can I let a program do
the browsing?
