if %PROCESSOR_ARCHITECTURE%==ARM64 (
    SET ProgramPath=CuisVM.app\Contents\Windows-arm64\Squeak.exe
) else (
    SET ProgramPath=CuisVM.app\Contents\Windows-x86_64\Squeak.exe
)

FOR /F "delims=" %%I IN ('dir /b "CuisImage\Cuis*.image" 2^>nul') DO SET ImageFile=%%I
START /B %ProgramPath% "CuisImage/%ImageFile%" -u