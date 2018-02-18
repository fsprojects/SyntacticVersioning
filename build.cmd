@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

IF NOT EXIST build.fsx (
  .paket\paket.exe update
  packages\build\FAKE\tools\FAKE.exe init.fsx
)

for /f "usebackq tokens=*" %%i in (`%~dp0\packages\vswhere\tools\vswhere -latest -products * -requires Microsoft.Component.MSBuild -property installationPath`) do (
  set VSInstallDir=%%i
)

IF NOT EXIST "%VSInstallDir%\MSBuild\15.0\Bin\MSBuild.exe" (
  exit /b -1
)

packages\build\FAKE\tools\FAKE.exe build.fsx %* "VSInstallDir=%VSInstallDir%"
