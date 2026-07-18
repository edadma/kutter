package io.github.edadma.kutter

// The audio mixer's pure math — kept out of the UI so it can be exercised headlessly (KUTTER_PROBE_HIT).
// A fader carries a linear amplitude (1.0 = unity, the same scale as `Track.volume` and `Project.master`
// that the MLT `volume` filter and the audio-device gain read); the mixer shows it to the user in
// decibels, the unit a mixing desk is read in.
object Mixer:

  /** The gain in decibels of a linear amplitude `gain` (1.0 → 0 dB, 0.5 → about −6 dB). Silence has no
    * finite dB, so a `gain` of 0 (or below) returns negative infinity — `dbLabel` renders that as −∞. */
  def gainToDb(gain: Double): Double =
    if gain <= 0.0 then Double.NegativeInfinity else 20.0 * math.log10(gain)

  // The dB level fed to a fully-attenuating fader (linear gain 0) — well below audible, standing in for
  // the −∞ that a real gain of 0 would be, since the MLT `volume` filter's `level` takes a finite dB.
  private val SilenceDb = -1000.0

  /** The dB value to write to an MLT `volume` filter's `level` for a linear `gain`. Same as `gainToDb`,
    * but silence (gain 0) floors to a large finite negative rather than −∞, so the property is settable. */
  def levelDb(gain: Double): Double =
    if gain <= 0.0 then SilenceDb else 20.0 * math.log10(gain)

  /** A short decibel readout for a linear `gain`, for the fader's label: `-∞ dB` at silence, `0.0 dB` at
    * unity, and a signed value either side (`-6.0 dB`, `+6.0 dB`). Feeding a track's effective gain here
    * — 0 when muted — makes a muted fader read `-∞ dB`. */
  def dbLabel(gain: Double): String =
    gainToDb(gain) match
      case Double.NegativeInfinity     => "-∞ dB"
      case db if db > -0.05 && db < 0.05 => "0.0 dB"
      case db                          => f"$db%+.1f dB"
