@ECHO OFF
rd /s /q packages
rd /s /q build
call build.cmd %*