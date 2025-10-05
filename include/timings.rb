class Timings
    def initialize
        @timings = Hash.new { |h, k| h[k] = [] }
        @total_start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    end

    def profile(label)
        start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        yield
    ensure
        elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - start
        @timings[label] << elapsed
    end

    def report
        total_time = Process.clock_gettime(Process::CLOCK_MONOTONIC) - @total_start
        puts "\n=== Profiling summary ==="
        puts "Total runtime: #{total_time.round(2)} s"

        sorted = @timings.sort_by { |_, times| -times.sum }

        sorted.each do |label, times|
        total = times.sum
        avg = total / times.size
        percent = (total / total_time * 100).round(2)
        puts "#{label.ljust(24)} #{times.size} calls, " \
            "total: #{(total * 1000).round(2)} ms, avg: #{(avg * 1000).round(2)} ms, " \
            "#{percent}% of total"
        end
    end
end
