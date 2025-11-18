if %PROCESSOR_ARCHITECTURE%==ARM64 (
    SET ProgramPath=CuisVM.app\Contents\Windows-arm64\Squeak.exe
) else (
    SET ProgramPath=CuisVM.app\Contents\Windows-x86_64\Squeak.exe
)

START /B %ProgramPath% "CuisImage/Cuis7.5-7693.image" -u