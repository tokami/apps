@echo off

call RexeLoc.bat

IF %Rexe%=="" (
	for /f "tokens=*" %%i in ('where /r ^"c:\Program Files^" Rscript.exe') do (
		set Rexe="%%i"
		goto output
	)
)
IF %Rexe%=="" (
	for /f "tokens=*" %%i in ('where /r ^"c:\Program Files (x86)^" Rscript.exe') do (
		set Rexe="%%i"
		goto output
	)
)
IF %Rexe%=="" (
	for /f "tokens=*" %%i in ('where /r %HOMEPATH% Rscript.exe') do (
		set Rexe="%%i"
		goto output
	)
)

:output
@echo set Rexe=%Rexe% > RexeLoc.bat

%Rexe% --vanilla runapp.R
