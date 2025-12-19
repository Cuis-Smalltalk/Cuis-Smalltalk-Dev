if %PROCESSOR_ARCHITECTURE%==ARM64 (
    SET ProgramPath=CuisVM.app\Contents\Windows-arm64\Squeak.exe
) else (
    SET ProgramPath=CuisVM.app\Contents\Windows-x86_64\Squeak.exe
)

START /B %ProgramPath% "CuisImage/Cuis7.7-7777.image" -u