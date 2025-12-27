# frozen_string_literal: true
require "set"

module FOVAngle
    module_function

    TAU = 2 * Math::PI
    EPS = 1e-12

    def visible_packed(w, h, maze_set, ox, oy, radius:)
        return [] unless ox.between?(0, w - 1) && oy.between?(0, h - 1)

        template = template_for_radius(radius)
        vis = []
        vis << ((oy << 16) | ox)

        blocked = AngleUnion.new
        pending = [] # holds [s,e] pairs for the current ring

        template.each do |ring|
            # ring is an Array of [dx, dy, spans]
            ring.each do |dx, dy, spans|
                x = ox + dx
                y = oy + dy
                next unless x.between?(0, w - 1) && y.between?(0, h - 1)

                # spans is either [[s,e]] or [[s,e],[s,e]]
                any_uncovered = false
                spans.each do |s, e|
                    unless blocked.covered?(s, e)
                        any_uncovered = true
                        break
                    end
                end
                next unless any_uncovered

                packed = (y << 16) | x
                vis << packed

                # Opaque?
                if maze_set.include?(packed)
                    # queue spans to commit AFTER finishing this exact distance ring
                    spans.each { |se| pending << se }
                end
            end

            # Commit occluders from this ring (same as original) :contentReference[oaicite:3]{index=3}
            pending.each { |s, e| blocked.add(s, e) }
            pending.clear

            # Early exit: once fully blocked, nothing farther can ever be visible.
            break if blocked.full?
        end

        vis
    end

    def visible(w, h, grid, ox, oy, radius:, &opaque_block)
        raise ArgumentError, "need an opaque? block" unless block_given?
        # Use visible_packed then convert.
        # Note: uses the block rather than maze_set, so we cannot avoid block dispatch here.
        packed_list = visible_packed_via_block(w, h, ox, oy, radius: radius, &opaque_block)
        s = Set.new
        packed_list.each do |p|
            x = p & 0xFFFF
            y = (p >> 16) & 0xFFFF
            s << [x, y]
        end
        s
    end

    @template_cache = {}

    def template_for_radius(radius)
        r = Integer(radius)
        @template_cache[r] ||= build_template(r).freeze
    end

    def build_template(radius)
        r2 = radius * radius
        by_d2 = Hash.new { |hsh, k| hsh[k] = [] }

        (-radius..radius).each do |dy|
            (-radius..radius).each do |dx|
                next if dx.zero? && dy.zero?
                d2 = dx * dx + dy * dy
                next unless d2 < r2

                spans = tile_spans(dx, dy).freeze
                by_d2[d2] << [dx, dy, spans]
            end
        end

        # Sort rings by d2 ascending (same ring processing as original) :contentReference[oaicite:4]{index=4}
        by_d2.keys.sort.map { |d2| by_d2[d2].freeze }
    end

    def visible_packed_via_block(w, h, ox, oy, radius:, &opaque_block)
        return [] unless ox.between?(0, w - 1) && oy.between?(0, h - 1)

        template = template_for_radius(radius)
        vis = []
        vis << ((oy << 16) | ox)

        blocked = AngleUnion.new
        pending = []

        template.each do |ring|
            ring.each do |dx, dy, spans|
                x = ox + dx
                y = oy + dy
                next unless x.between?(0, w - 1) && y.between?(0, h - 1)

                any_uncovered = false
                spans.each do |s, e|
                    unless blocked.covered?(s, e)
                        any_uncovered = true
                        break
                    end
                end
                next unless any_uncovered

                vis << ((y << 16) | x)

                if opaque_block.call(x, y)
                    spans.each { |se| pending << se }
                end
            end

            pending.each { |s, e| blocked.add(s, e) }
            pending.clear
            break if blocked.full?
        end

        vis
    end

    def tile_spans(dx, dy)
        corners = [
            [dx - 0.5, dy - 0.5], [dx + 0.5, dy - 0.5],
            [dx + 0.5, dy + 0.5], [dx - 0.5, dy + 0.5]
        ]
        ang = corners.map { |x, y| norm_angle(Math.atan2(y, x)) }.sort

        gaps = 4.times.map do |i|
            a = ang[i]
            b = ang[(i + 1) % 4]
            gap = b - a
            gap += TAU if gap < 0
            [gap, i]
        end

        _, imax = gaps.max_by { |g, _| g }
        start = ang[(imax + 1) % 4]
        finish = ang[imax]
        finish += TAU if finish < start

        if finish >= TAU
            [[start, TAU], [0.0, finish - TAU]]
        else
            [[start, finish]]
        end
    end

    def norm_angle(a)
        a %= TAU
        a += TAU if a < 0
        a
    end

    class AngleUnion
        def initialize
            @iv = [] # [s,e] sorted, disjoint
        end

        def add(s, e)
            i = 0
            while i < @iv.length && @iv[i][1] < s - EPS
                i += 1
            end
            ns = s
            ne = e
            while i < @iv.length && @iv[i][0] <= ne + EPS
                ns = [ns, @iv[i][0]].min
                ne = [ne, @iv[i][1]].max
                @iv.delete_at(i)
            end
            @iv.insert(i, [ns, ne])
        end

        def covered?(s, e)
            # wrapping query
            return covered?(s, TAU) && covered?(0.0, e) if s > e

            i = 0
            i += 1 while i < @iv.length && @iv[i][1] < s - EPS
            return false if i >= @iv.length || @iv[i][0] > s + EPS

            cover = @iv[i][1]
            return true if cover >= e - EPS

            i += 1
            while i < @iv.length && @iv[i][0] <= cover + EPS
                cover = [cover, @iv[i][1]].max
                return true if cover >= e - EPS
                i += 1
            end
            false
        end

        # Full coverage check for early exit
        def full?
            return false unless @iv.length == 1
            s, e = @iv[0]
            s <= 0.0 + EPS && e >= TAU - EPS
        end
    end
end
