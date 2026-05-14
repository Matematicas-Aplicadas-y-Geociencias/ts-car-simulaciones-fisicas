$ErrorActionPreference = "SilentlyContinue"

$repo = Split-Path -Parent $PSScriptRoot

Write-Host "Cleaning generated artifacts under $repo ..."

# Root-level generated artifacts
Remove-Item -Force "$repo\hola"
Remove-Item -Force "$repo\calor2d_utils.mod"

# LaTeX artifacts in docs
Get-ChildItem "$repo\docs" -File -Include *.aux,*.bbl,*.blg,*.log,*.out,*.synctex.gz,*.fdb_latexmk,*.fls,*.toc,*.pdf | ForEach-Object {
    Remove-Item -Force $_.FullName
}

# LaTeX build folder contents (keep directory itself)
if (Test-Path "$repo\docs\build") {
    Get-ChildItem "$repo\docs\build" -File | Where-Object { $_.Name -ne ".gitkeep" } | ForEach-Object {
        Remove-Item -Force $_.FullName
    }
}

# Fortran binaries and modules
if (Test-Path "$repo\paralelo\bin") {
    Get-ChildItem "$repo\paralelo\bin" -File | ForEach-Object {
        Remove-Item -Force $_.FullName
    }
}

if (Test-Path "$repo\paralelo\resultados\binarios") {
    Get-ChildItem "$repo\paralelo\resultados\binarios" -File | ForEach-Object {
        Remove-Item -Force $_.FullName
    }
}

Write-Host "Done."
