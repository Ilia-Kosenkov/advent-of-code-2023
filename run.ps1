param([int]$TaskId, [string]$InputFilePath)

$content = Get-Content -Path $InputFilePath -Raw
dotnet run --project "Task-$TaskId/Task-$TaskId.fsproj" -- $content
