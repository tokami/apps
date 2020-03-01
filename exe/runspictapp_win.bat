cd ../spictapp
REM SET R=%%"where /R C:\ Rscript"
for /f %%i in ('where /R C:\ Rscript') do (
set Rexec=%%i
REM exit
)

%Rexec% --vanilla runapp.R
cd ../exe
