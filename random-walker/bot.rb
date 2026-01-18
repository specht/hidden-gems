#!/usr/bin/env ruby
require 'json'

STDOUT.sync = STDERR.sync = true
srand(1)

first_tick = true

loop do
    a = []
    line = STDIN.readline
    data = JSON.parse(line)
    if first_tick
        config = data['config']
        width = config['width']
        height = config['height']
        STDERR.puts "Random walker (Ruby) launching on a #{width}x#{height} map"
    end
    dir = %w[N S E W].sample
    puts dir
    # STDERR.puts "Thinking...\nLet's go #{dir}!"
    first_tick = false
end
