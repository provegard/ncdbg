# Source maps

NCDbg supports source maps, both inline and file-based. The following restrictions
apply:

* A source map path must be absolute. While NCDbg supports scripts with relative paths,
  it cannot reliably read the contents of a source map file with a relative path.
* A source map file as well as the corresponding original source file must be filesystem
  readable.

To enable source map support in DevTools, go to the Source tab in DevTools, click the
&#x22ee; symbol, open Settings and then check "Enable JavaScript source maps."

When debugging a file in "souce map mode", you may find that it's not possible to set
a breakpoint on certain lines, or that stepping over a line has no noticable effect.
These are common problems that most likely are a result of how the source map maps
between the original file and the transpiled file. If you feel that NCDbg is at fault,
please open an issue.

Source map files are published using an HTTP server on the same host and port that the
WebSocket server uses (use the `--listen` run argument to specify custom values). Please
note that NCDbg has no built-in access control at all (apart from rejecting access to
unpublished files).
