package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The audio mixer pane: a channel strip per audio track over a master strip. Each strip is the
// track's name, a fader over its linear gain, a dB readout, and a mute toggle; the master strip
// mirrors the transport's volume control (same value, same handler) and drives the audio-device gain
// rather than a graph filter. Extracted from `App` so the editor's big component stays readable; it
// takes the tracks and the callbacks that ride the faders live.
private final case class MixerProps(
    audioTracks:   List[Track],
    master:        Double,
    onTrackVolume: (String, Double) => Unit,
    onToggleMute:  String => Unit,
    onMaster:      Double => Unit,
)

private val MixerPanel: Component[MixerProps] = component[MixerProps] { p =>
  val theme = useTheme()

  // A track's mute toggle: a small "M" that fills with the primary colour while the track is silenced,
  // matching the app's "active reads as primary" idiom.
  def muteButton(t: Track): VNode =
    box(onClick = _ => p.onToggleMute(t.id), cursor = Cursor.Pointer, radius = 6,
      bg = if t.muted then theme.primary else theme.background,
      padding = EdgeInsets.symmetric(horizontal = 10, vertical = 6))(
      text("M", size = 12, weight = FontWeight.Bold, color = if t.muted then theme.onPrimary else theme.border),
    )

  // One channel strip: the track's name, its fader (a horizontal Slider over the linear gain), the dB
  // readout of its effective gain (−∞ while muted), and the mute toggle. The fader edits `Track.volume`
  // live, so the change is heard on the next graph swap without a re-open.
  def faderRow(t: Track): VNode =
    row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
      sizedBox(width = 28)(text(t.name, size = 11, weight = FontWeight.Bold, color = theme.border)),
      box(flex = 1)(Slider(t.volume, v => p.onTrackVolume(t.id, v))),
      sizedBox(width = 58)(text(Mixer.dbLabel(t.gain), size = 11, color = theme.surfaceText, mono = true)),
      muteButton(t),
    )

  KutterUi.titledPanel(theme)("Audio Mixer")(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      if p.audioTracks.isEmpty then KutterUi.placeholder(theme)("No audio tracks")
      else col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 10)(
        p.audioTracks.map(faderRow)*,
      ),
      box(height = 1, bg = theme.border)(),
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
        sizedBox(width = 28)(text("Mix", size = 11, weight = FontWeight.Bold, color = theme.surfaceText)),
        box(flex = 1)(Slider(p.master, p.onMaster)),
        sizedBox(width = 58)(text(Mixer.dbLabel(p.master), size = 11, color = theme.surfaceText, mono = true)),
        sizedBox(width = 34)(box()()),
      ),
    ),
  )
}
