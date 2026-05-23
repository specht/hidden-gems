#!/usr/bin/env ruby

require "json"
require "open3"
require "shellwords"
require "zlib"

SCRIPT_NAME = File.basename($PROGRAM_NAME)

DEFAULT_RENDERER_ARGS = [
  "--font-regular", "./fonts/IBM_Plex_Mono/IBMPlexMono-Regular.ttf",
  "--font-bold", "./fonts/IBM_Plex_Mono/IBMPlexMono-Bold.ttf",
  "--emoji-font", "./fonts/Noto_Color_Emoji/NotoColorEmoji-Regular.ttf",
  "--encoder", "x264",
  "--fontsize", "16"
].freeze

USAGE = <<~TEXT
  Usage:
    # Write MP4 only, using the default output path and default renderer options:
    ruby #{SCRIPT_NAME} input.json.gz

    # Override renderer defaults:
    ruby #{SCRIPT_NAME} input.json.gz --fontsize 24 --encoder nvenc

    # Use explicit output path:
    ruby #{SCRIPT_NAME} input.json.gz --mp4-path out.mp4

    # Use an explicit renderer script:
    ruby #{SCRIPT_NAME} input.json.gz --renderer ./ansi_render_to_video.js --fontsize 16

  Wrapper options:
    -h, --help
        Show this help.

    --mp4, --write-mp4
        Write the MP4 file.
        This is the default.

    --no-mp4, --no-write-mp4
        Do not write the MP4 file.

    --mp4-path PATH
        Override the MP4 output path.
        Default: input.mp4

    --renderer PATH
        Path to ansi_render_to_video.js.
        Default: ansi_render_to_video.js next to this script, if present;
                 otherwise ./ansi_render_to_video.js

  Default renderer options:
    #{DEFAULT_RENDERER_ARGS.shelljoin}

  Renderer options:
    All other options after input.json.gz are passed through to
    ansi_render_to_video.js. They are appended after the defaults, so they can
    override them, for example:

      --fontsize 24
      --encoder nvenc
      --crf 20
      --preset fast
TEXT

def default_renderer_path
  next_to_script = File.expand_path("ansi_render_to_video.js", __dir__)
  return next_to_script if File.exist?(next_to_script)

  "ansi_render_to_video.js"
end

def default_output_path(in_path, suffix)
  base = in_path.sub(/\.json\.gz\z/, "")
  base == in_path ? "#{in_path}#{suffix}" : "#{base}#{suffix}"
end

def take_value!(argv, i, option_name)
  value = argv[i + 1]

  if value.nil? || value.start_with?("-")
    abort "Missing value for #{option_name}\n\n#{USAGE}"
  end

  value
end

def parse_args(argv)
  config = {
    in_path: nil,
    mp4_path: nil,
    renderer_path: default_renderer_path,
    write_mp4: true,
    renderer_args: []
  }

  i = 0

  while i < argv.length
    arg = argv[i]

    case arg
    when "-h", "--help"
      puts USAGE
      exit 0

    when "--"
      config[:renderer_args].concat(argv[(i + 1)..] || [])
      break

    when "--mp4", "--write-mp4"
      config[:write_mp4] = true

    when "--no-mp4", "--no-write-mp4"
      config[:write_mp4] = false

    when "--mp4-path"
      config[:mp4_path] = take_value!(argv, i, arg)
      i += 1

    when /\A--mp4-path=(.+)\z/
      config[:mp4_path] = Regexp.last_match(1)

    when "--renderer"
      config[:renderer_path] = take_value!(argv, i, arg)
      i += 1

    when /\A--renderer=(.+)\z/
      config[:renderer_path] = Regexp.last_match(1)

    else
      if config[:in_path].nil? && !arg.start_with?("-")
        config[:in_path] = arg
      elsif config[:in_path]
        # Unknown args after input path are renderer args.
        config[:renderer_args] << arg
      else
        abort "Unknown wrapper option before input path: #{arg}\n\n#{USAGE}"
      end
    end

    i += 1
  end

  abort USAGE if config[:in_path].nil?

  unless File.exist?(config[:in_path])
    abort "Input file does not exist: #{config[:in_path]}"
  end

  unless config[:in_path].end_with?(".json.gz")
    abort "Input file should end with .json.gz: #{config[:in_path]}"
  end

  config[:mp4_path] ||= default_output_path(config[:in_path], ".mp4")

  unless config[:write_mp4]
    abort "Nothing to do: MP4 writing is disabled."
  end

  config
end

def shell_join(args)
  args.map(&:to_s).shelljoin
end

def render_mp4(in_path, mp4_path, renderer_path, renderer_args)
  unless File.exist?(renderer_path)
    abort "Renderer not found: #{renderer_path}"
  end

  effective_renderer_args = [
    *DEFAULT_RENDERER_ARGS,
    *renderer_args
  ]

  node_args = [
    "node",
    renderer_path,
    "/dev/stdin",
    mp4_path,
    *effective_renderer_args
  ]

  STDERR.puts "Rendering MP4:"
  STDERR.puts "  input:    #{in_path}"
  STDERR.puts "  output:   #{mp4_path}"
  STDERR.puts "  renderer: #{renderer_path}"
  STDERR.puts "  command:  #{shell_join(node_args)}"

  Open3.popen3(*node_args) do |stdin, stdout, stderr, wait_thr|
    stdout_thread = Thread.new do
      IO.copy_stream(stdout, STDOUT)
    rescue IOError
    end

    stderr_thread = Thread.new do
      IO.copy_stream(stderr, STDERR)
    rescue IOError
    end

    begin
      Zlib::GzipReader.open(in_path) do |gz|
        IO.copy_stream(gz, stdin)
      end
    rescue Errno::EPIPE
      # Renderer exited early; wait for its exit status below.
    ensure
      stdin.close unless stdin.closed?
    end

    status = wait_thr.value

    stdout_thread.join
    stderr_thread.join

    unless status.success?
      abort "Renderer failed with exit status #{status.exitstatus}"
    end
  end

  STDERR.puts "Done rendering #{mp4_path}."
end

config = parse_args(ARGV)

if config[:write_mp4]
  render_mp4(
    config[:in_path],
    config[:mp4_path],
    config[:renderer_path],
    config[:renderer_args]
  )
end
