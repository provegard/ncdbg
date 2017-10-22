# Visual Studio Code

Since version 0.6.0, Visual Studio Code (VSCode) can act as a debugger frontend in addition to
Google Chrome.

To use VSCode, please follow the steps below.

## Start NCDbg

Start NCDbg as usual. No special configuration is needed.

## Install Debugger for Chrome

To be able to connect to NCDbg, a Microsoft-provided extension called Debugger for Chrome
is needed. Go to the _View_ menu in VSCode and select _Extensions_. Search for and install
Debugger for Chrome. Follow the instructions to restart.

## Open a workspace in VSCode

VSCode tries to map scripts exposed by NCDbg to workspace scripts, so if your scripts reside
in an existing workspace, it's a good idea to open that. In particular, you will need to add
an entry in _launch.json_.

## Modify launch.json

Open _launch.json_ and add the following configuration entry (in the `configurations` array):


  {
    "type": "chrome",
    "request": "attach",
    "name": "Attach to NCDbg",
    "address": "localhost",
    "port": 7778,
    "webRoot": "${workspaceRoot}"
  }

Adjust the host name and port accordingly. For VSCode to be able to map a script with a relative
path to a workspace script, the `webRoot` property should be set to the "base location" of
relative script paths.

## Start debugging

Finally switch to the Debug view, select "Attach to NCDbg" in the configurations dropdown and
click the green run arrow. If the configuration in _launch.json_ is correct, you should now be
connected.

All loaded scripts should appear under the "Loaded Scripts" pane.
