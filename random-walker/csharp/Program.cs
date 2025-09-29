using System;
using System.IO;
using System.Text.Json;

//
// Make both streams line-buffered
//
Console.SetOut(new StreamWriter(Console.OpenStandardOutput()) { AutoFlush = true });
Console.SetError(new StreamWriter(Console.OpenStandardError()) { AutoFlush = true });

var rng = new Random(1);

// Read first line separately so we can print the banner to stderr
string? line = Console.In.ReadLine();
if (line != null)
{
    using var doc = JsonDocument.Parse(line);
    var cfg = doc.RootElement.GetProperty("config");
    int w = cfg.GetProperty("width").GetInt32();
    int h = cfg.GetProperty("height").GetInt32();

    // Write banner to stderr first, flush to enforce ordering of *emission*
    Console.Error.WriteLine($"Random walker (C#) launching on a {w}x{h} map");
    Console.Error.Flush();

    // Now write the move for the first line to stdout
    Console.WriteLine(new[] { "N", "S", "E", "W" }[rng.Next(4)]);
}

// Process remaining lines
while ((line = Console.In.ReadLine()) != null)
{
    Console.WriteLine(new[] { "N", "S", "E", "W" }[rng.Next(4)]);
}
