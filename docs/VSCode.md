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
an entry in _launch.json_ (see below).

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

### Where is launch.json?

It should be in the _.vscode_ folder in the workspace root. If it's not there, go to the Debug
view and click the settings gears towards the top. There should be a red indicator and a tooltip
that reads something like "Configure or Fix 'launch.json'". After clicking, in the environment
dropdown that appears, select Chrome. The _launch.json_ file should now open and you can modify
the added Chrome entry to look like the entry above.

## Start debugging

Finally switch to the Debug view, select "Attach to NCDbg" in the configurations dropdown and
click the green run arrow. If the configuration in _launch.json_ is correct, you should now be
connected.

All loaded scripts should appear under the "Loaded Scripts" pane.
