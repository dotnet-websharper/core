@echo off
setlocal

set DOTNETSOLUTION="WebSharper.sln"
call tools\WebSharper.Fake.cmd %*
