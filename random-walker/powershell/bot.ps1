$first = $true
$rng = [System.Random]::new(1)   # fixed seed = deterministic sequence

while ($line = [Console]::In.ReadLine()) {
  try { $data = $line | ConvertFrom-Json } catch { $data = $null }
  if ($first -and $data -and $data.config) {
    $w = $data.config.width
    $h = $data.config.height
    [Console]::Error.WriteLine("Random walker (PowerShell) launching on a ${w}x${h} map")
    $first = $false
  }
  $moves = @("N","S","E","W")
  $idx = $rng.Next(0,4)          # use our seeded generator
  Write-Output $moves[$idx]
}
