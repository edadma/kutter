package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The transport bar — the scrubber over play / timecode / frame index / name / master volume — as its
// own component so its position readout ticking during playback re-renders only this small subtree
// (riposte re-renders the dirty instance, not the root) rather than the whole editor. It polls the
// active monitor's position itself, into local state, a few times a second; `App` drives the timeline
// playhead separately through a repaint (see [[TimelinePanel]]'s poll), so the expensive track lanes do
// not reconcile as the playhead moves.

// Everything the transport needs from the editor, as a plain bundle. `player` reads the monitor the
// transport currently drives (the active one), so the transport can poll its position without the
// editor re-rendering; the callbacks reach back into the editor's playback state. `playing`, `total`,
// `label`, and `volume` are the values that change infrequently (a play/pause, a mode switch, a
// volume ride) — when they do, the editor re-renders and hands the transport a fresh bundle.
private final case class TransportProps(
    player:       () => Player | Null,
    total:        Int,
    fps:          Double,
    playing:      Boolean,
    label:        String,
    volume:       Double,
    onToggle:     () => Unit,
    onSeek:       Double => Unit,
    onScrubStart: () => Unit,
    onScrubEnd:   () => Unit,
    onVolume:     Double => Unit,
)

private val Transport: Component[TransportProps] = component[TransportProps] { p =>
  val theme = Theme.dark

  // The polled position of the active monitor — the readout and the scrubber thumb. Held here, not in
  // the editor, so advancing it each tick re-renders only the transport.
  val (frame, setFrame, _) = useState(0)

  // While this scrubber owns the drag, the Slider shows the cursor directly, so the poll must not fight
  // it. A local ref (the editor has its own for timeline scrubs); no re-render needed to read it.
  val scrubbing = useRef(false)

  // The poll's timer is armed once and never re-armed (its deps are constant), so its callback would
  // otherwise close over the accessor from the first render and always read whichever monitor was
  // active then. Mirror the latest accessor into a ref each render so the poll reads the monitor the
  // transport currently drives — switching to the clip monitor then tracks the clip, not the project.
  val activeMonitor = useRef(p.player)
  activeMonitor.current = p.player

  useInterval(
    () =>
      if !scrubbing.current then
        activeMonitor.current() match
          case pl: Player => setFrame(pl.position)
          case null       => (),
    100,
  )

  val progress = if p.total > 0 then math.min(1.0, frame.toDouble / p.total) else 0.0

  def iconButton(icon: SvgImage, onClick: () => Unit): VNode =
    box(onClick = _ => onClick(), cursor = Cursor.Pointer, padding = EdgeInsets.all(8), radius = 8)(
      svg(icon, width = 22, height = 22),
    )

  val scrubber = Slider(
    value         = progress,
    onChange      = p.onSeek,
    onChangeStart = _ => { scrubbing.current = true; p.onScrubStart() },
    onChangeEnd   = _ => { scrubbing.current = false; p.onScrubEnd() },
  )

  box(bg = theme.surface, padding = EdgeInsets.symmetric(horizontal = 16, vertical = 10))(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 10)(
      scrubber,
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 12)(
        iconButton(if p.playing then pauseIcon else playIcon, () => p.onToggle()),
        text(s"${KutterUi.timecode(frame, p.fps)} / ${KutterUi.timecode(p.total, p.fps)}", size = 14,
          color = theme.surfaceText, mono = true),
        text(s"frame $frame / ${p.total}", size = 12, color = theme.border, mono = true),
        spacer(),
        text(p.label, size = 13, color = theme.surfaceText),
        svg(volumeIcon, width = 18, height = 18),
        sizedBox(width = 90)(Slider(p.volume, p.onVolume)),
      ),
    ),
  )
}
