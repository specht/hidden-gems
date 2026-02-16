# pcg32_single_seed.rb
# PCG32 (XSH RR) with a simple one-argument seeding API.
# If no seed is given, uses current time (nanoseconds).
#
# Includes a golden-vector self-test when run directly.

class PCG32
  MULT   = 6364136223846793005
  MASK64 = (1 << 64) - 1
  MASK32 = (1 << 32) - 1

  # Fixed stream selection (any integer works; oddness enforced internally)
  STREAM_INITSEQ = 54

  def initialize(seed = nil)
    seed ||= default_time_seed
    seed!(seed)
  end

  def seed!(seed)
    initstate = seed & MASK64
    initseq   = STREAM_INITSEQ & MASK64

    @state = 0
    @inc   = ((initseq << 1) | 1) & MASK64
    next_uint32
    @state = (@state + initstate) & MASK64
    next_uint32
    self
  end

  def snapshot
    { state: @state.to_s(36), inc: @inc }
  end

  def restore!(snap)
    @state = snap[:state].to_i(36) & MASK64
    @inc   = snap[:inc]   & MASK64
    self
  end

  def next_uint32
    oldstate = @state
    @state   = (oldstate * MULT + @inc) & MASK64

    xorshifted = (((oldstate >> 18) ^ oldstate) >> 27) & MASK32
    rot = (oldstate >> 59) & 31
    rotr32(xorshifted, rot)
  end

  def next_uint64
    ((next_uint32 << 32) | next_uint32) & MASK64
  end

  def next_float
    next_uint32.to_f / (1 << 32)
  end

  def randrange(n)
    raise ArgumentError, "n must be positive" if n <= 0
    if n <= (1 << 32)
      threshold = (1 << 32) % n
      loop do
        r = next_uint32
        return r % n if r >= threshold
      end
    else
      limit = ((1 << 64) / n) * n
      loop do
        r = next_uint64
        return r % n if r < limit
      end
    end
  end

  def rand_normal(mean = 0.0, stddev = 1.0)
    @normal_spare ||= nil

    if @normal_spare
      z = @normal_spare
      @normal_spare = nil
      return mean + stddev * z
    end

    u1 = 1.0 - next_float
    u2 = next_float

    r = Math.sqrt(-2.0 * Math.log(u1))
    theta = 2.0 * Math::PI * u2

    z0 = r * Math.cos(theta)
    z1 = r * Math.sin(theta)

    @normal_spare = z1
    mean + stddev * z0
  end

  def sample(arr)
    raise ArgumentError, "empty array" if arr.empty?
    arr[randrange(arr.length)]
  end

  def shuffle!(arr)
    (arr.length - 1).downto(1) do |i|
      j = randrange(i + 1)
      arr[i], arr[j] = arr[j], arr[i]
    end
    arr
  end

  private

  def rotr32(x, r)
    ((x >> r) | ((x << ((-r) & 31)) & MASK32)) & MASK32
  end

  def default_time_seed
    ns = Process.clock_gettime(Process::CLOCK_REALTIME, :nanosecond) rescue (Time.now.to_r * 1_000_000_000).to_i
    ((Time.now.to_i << 32) ^ ns) & MASK64
  end
end

rng = PCG32.allocate
rng.send(:seed!, 42)  # force initstate=42 with STREAM_INITSEQ=54
expected_hex = %w[
    a15c02b7 7b47f409 ba1d3330 83d2f293 bfa4784b
    cbed606e bfc6a3ad 812fff6d e61f305a f9384b90
]
got_hex = 10.times.map { rng.next_uint32.to_s(16).rjust(8, '0') }

if got_hex != expected_hex
    raise "PCG32 self-test failed!"
end
