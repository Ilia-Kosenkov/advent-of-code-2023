param([int]$Task, [int]$Part,  [string]$InputFilePath)

$content = Get-Content -Path $InputFilePath -Raw
$idStr = $Task.ToString("00")
$partId = $Part.ToString("00")

dotnet run --project "Task-$idStr-$partId/Task-$idStr-$partId.fsproj" -- $content
