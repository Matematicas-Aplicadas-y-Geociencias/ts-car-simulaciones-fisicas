param(
    [int]$MaxThreads = 8,
    [int]$Runs = 10
)

$ErrorActionPreference = "Stop"
$repo = Split-Path -Parent $PSScriptRoot

Write-Host "Running benchmark with MaxThreads=$MaxThreads Runs=$Runs"
python "$repo\paralelo\scripts\benchmark_aceleracion.py" --max-threads $MaxThreads --runs $Runs
