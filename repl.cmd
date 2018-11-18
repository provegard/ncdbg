@echo off

:: Prevent 'EXECUTING' status from being shown during REPL execution
set TERM=dumb

gradlew -q repl
