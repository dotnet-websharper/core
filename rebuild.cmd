@ECHO OFF
rd /s /q packages
rd /s /q paket-files
rd /s /q build
call build.cmd %*