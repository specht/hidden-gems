module FOVAngle
    module_function

    TAU = 2 * Math::PI
    EPS = 1e-12

    # Public: compute visible cells within radius (Euclidean).
    # grid[y][x]; origin (ox,oy); pass a block { |x,y| opaque? }
    # Returns a Set of [x,y].
    def visible(w, h, grid, ox, oy, radius:)
        raise ArgumentError, "need an opaque? block" unless block_given?

        return Set.new unless ox.between?(0, w-1) && oy.between?(0, h-1)

        r2 = radius * radius

        # Build list of candidate offsets within radius and in-bounds (exclude [0,0])
        cells = []
        min_dx = -[ox, radius].min
        max_dx = [w - 1 - ox, radius].min
        min_dy = -[oy, radius].min
        max_dy = [h - 1 - oy, radius].min

        (min_dy..max_dy).each do |dy|
            (min_dx..max_dx).each do |dx|
                next if dx.zero? && dy.zero?
                next unless dx * dx + dy * dy < r2
                cells << [dx, dy, dx * dx + dy * dy]
            end
        end

        # Sort by squared distance; we’ll process equal-distance groups together
        cells.sort_by! { |_, _, d2| d2 }

        vis = Set.new
        vis << [ox, oy]

        blocked = AngleUnion.new
        pending = [] # intervals to add after current distance group

        i = 0
        while i < cells.length
            j = i
            d2 = cells[i][2]
            j += 1 while j < cells.length && cells[j][2] == d2

            # For all cells at this exact distance, decide visibility using 'blocked' (nearer occluders only)
            (i...j).each do |k|
                dx, dy, _ = cells[k]
                x = ox + dx
                y = oy + dy

                # Compute the angular span(s) of this tile as seen from origin
                spans = tile_spans(dx, dy) # array of [start,end] in [0,TAU), non-wrapping
                any_uncovered = spans.any? { |s, e| !blocked.covered?(s, e) }

                if any_uncovered
                    vis << [x, y]
                    # If the tile is opaque, it blocks farther tiles. Queue its spans to add after this group.
                    if yield(x, y)
                        pending.concat(spans)
                    end
                end
            end

            # Commit occluders from this ring
            pending.each { |s, e| blocked.add(s, e) }
            pending.clear

            i = j
        end

        vis
    end

    # ---- Angle math helpers ----

    # Angular coverage of a tile centered at (dx,dy), with corners at (dx±0.5, dy±0.5)
    # Returns 1 or 2 non-wrapping intervals [start,end] in [0,TAU)
    def tile_spans(dx, dy)
        corners = [
            [dx - 0.5, dy - 0.5], [dx + 0.5, dy - 0.5],
            [dx + 0.5, dy + 0.5], [dx - 0.5, dy + 0.5]
        ]
        ang = corners.map { |x, y| norm_angle(Math.atan2(y, x)) }.sort
        # Find the largest gap; the tile’s minimal covering interval is the complement
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
        # Split if it wraps
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

    # Maintains a union of disjoint angular intervals on [0,TAU)
    class AngleUnion
        def initialize
            @iv = [] # array of [s,e], with 0 <= s < e <= TAU, non-overlapping, sorted by s
        end

        # Add [s,e] (non-wrapping). Use add_span for arbitrary spans.
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

        # Convenience for arbitrary (possibly wrapping) span
        def add_span(s, e)
            if s <= e
                add(s, e)
            else
                add(s, TAU)
                add(0.0, e)
            end
        end

        # Return true if [s,e] is fully covered by the union
        def covered?(s, e)
            # Handle wrapping by splitting before query
            return covered?(s, TAU) && covered?(0.0, e) if s > e

            # Find first interval whose end reaches s
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
    end
end