package io.github.edadma.kutter

// Aligning multicam angles by their sound. Two cameras pointed at the same event record the same audio
// from slightly different starts; lining their audio up is how a multicam edit stays in sync when the
// program cuts between them. kutter already computes a per-frame peak envelope for every audio source
// (see [[Waveform]]), so the raw material is on hand: sliding one envelope past the other and taking the
// lag where they correlate best recovers the delay between the two recordings, in timeline frames.
//
// This is a pure signal-processing helper — no MLT, no threads — so it is exercised headlessly. The
// correlation is zero-mean and normalised, so a loud passage does not dominate a quiet one and the score
// is a true similarity in [-1, 1] rather than an energy sum; the best lag is the one whose overlap looks
// most alike. Manual nudge remains the fallback the UI offers when a clap track is ambiguous.
object AudioSync:

  /** The normalised zero-mean cross-correlation of `a` against `b` shifted by `lag`: how alike `a(i)` and
    * `b(i + lag)` are over the frames where both exist. Returns a score in [-1, 1] (1 = identical shape),
    * or `Double.NegativeInfinity` when the overlap is shorter than `minOverlap` (too little evidence to
    * trust) or either side is flat over it (no variation to correlate). */
  def correlationAt(a: Array[Float], b: Array[Float], lag: Int, minOverlap: Int): Double =
    val lo = math.max(0, -lag)
    val hi = math.min(a.length, b.length - lag)
    val n  = hi - lo
    if n < minOverlap then return Double.NegativeInfinity

    var sa = 0.0
    var sb = 0.0
    var i  = lo
    while i < hi do
      sa += a(i)
      sb += b(i + lag)
      i += 1
    val ma = sa / n
    val mb = sb / n

    var num = 0.0
    var da  = 0.0
    var db  = 0.0
    i = lo
    while i < hi do
      val x = a(i) - ma
      val y = b(i + lag) - mb
      num += x * y
      da += x * x
      db += y * y
      i += 1
    if da <= 0.0 || db <= 0.0 then Double.NegativeInfinity
    else num / math.sqrt(da * db)

  /** The lag in `[-maxLag, maxLag]` at which `b`'s envelope best matches `a`'s — the number of frames
    * `b`'s content trails `a`'s (so `a(i)` lines up with `b(i + lag)`). A positive result means the same
    * instant appears later in `b` than in `a`. The scan requires a meaningful overlap at every candidate
    * so a sliver of high correlation at an extreme lag cannot win; with no usable lag it returns 0 (treat
    * the two as already aligned and let the user nudge). This is what turns a reference angle and another
    * recording into a synced pair. */
  def bestLag(a: Array[Float], b: Array[Float], maxLag: Int): Int =
    val minOverlap = math.max(8, math.min(a.length, b.length) / 4)
    var bestLag   = 0
    var bestScore = Double.NegativeInfinity
    var lag       = -maxLag
    while lag <= maxLag do
      val s = correlationAt(a, b, lag, minOverlap)
      if s > bestScore then
        bestScore = s
        bestLag = lag
      lag += 1
    if bestScore == Double.NegativeInfinity then 0 else bestLag

  /** The sync `offset` for a multicam clip angle whose envelope is `angle`, measured against the group's
    * reference (audio-bed) envelope `bed`: the source frame of the angle that plays at the group's frame
    * 0. It is exactly the best lag — at the bed's frame 0 the same instant is the angle's frame `lag` —
    * so an angle built with this offset reads the same moment as the bed at every program position. */
  def syncOffset(bed: Array[Float], angle: Array[Float], maxLag: Int): Int =
    bestLag(bed, angle, maxLag)
