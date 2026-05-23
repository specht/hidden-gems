#!/usr/bin/env ruby
# frozen_string_literal: true

require 'date'
require 'rbconfig'
require 'yaml'

ROOT = File.expand_path(__dir__)

ORIGINAL_RUNNER = File.join(ROOT, '_runner_original.rb')
BONUS_RUNNER    = File.join(ROOT, '_runner_bonus.rb')
STAGES_PATH     = File.join(ROOT, 'stages.yaml')

def runner_args_only(argv)
    dashdash = argv.index('--')
    dashdash ? argv[0...dashdash] : argv
end

def option_value(argv, name)
    argv.each_with_index do |arg, i|
        return argv[i + 1] if arg == name && i + 1 < argv.size

        prefix = "#{name}="
        return arg[prefix.length..] if arg.start_with?(prefix)
    end

    nil
end

def current_stage_key(stages)
    today = Date.today.strftime('%Y-%m-%d')

    stages.keys
    .select { |key| stages[key]['from'].to_s <= today }
    .sort_by { |key| [stages[key]['from'].to_s, key] }
    .last
end

def effective_stage_key(argv, stages)
    runner_argv = runner_args_only(argv)
    requested = option_value(runner_argv, '--stage') || 'current'

    if requested == 'current'
        current_stage_key(stages)
    else
        requested
    end
end

def bonus_stage?(stage_key)
    match = stage_key.to_s.match(/\Astage-(\d+)@/)
    return false unless match

    match[1].to_i >= 6
end

unless File.exist?(STAGES_PATH)
    warn "Could not find #{STAGES_PATH}"
    exit 1
end

unless File.exist?(ORIGINAL_RUNNER)
    warn "Could not find #{ORIGINAL_RUNNER}"
    exit 1
end

unless File.exist?(BONUS_RUNNER)
    warn "Could not find #{BONUS_RUNNER}"
    exit 1
end

stages = YAML.load_file(STAGES_PATH)
stage_key = effective_stage_key(ARGV, stages)

unless stage_key && stages.key?(stage_key)
    warn "Unknown stage: #{stage_key.inspect}"
    warn "Known stages: #{stages.keys.join(', ')}"
    exit 1
end

runner =
case ENV['HIDDEN_GEMS_RUNNER']
when 'original', 'old'
    ORIGINAL_RUNNER
when 'bonus', 'new'
    BONUS_RUNNER
else
    bonus_stage?(stage_key) ? BONUS_RUNNER : ORIGINAL_RUNNER
end

exec(RbConfig.ruby, runner, *ARGV)