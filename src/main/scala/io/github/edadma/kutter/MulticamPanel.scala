package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The multicam switcher — a compact bar under the project monitor. Before a group exists it offers to
// build one from the bin's videos; once one does, it is a row of angle chips (the cameras and any title
// slides) that cut the program picture to that angle at the current playhead. Clicking an angle while
// playback runs is a live switch; scrubbing to a frame and clicking is a frame-precise one — the same
// action either way (see `Multicam.switchProgram`). The chip of the angle showing at the playhead reads
// as pressed. The audio bed is untouched by a switch, so the sound plays through the cut.
private final case class MulticamProps(
    group:       Option[Multicam],
    activeAngle: Int,
    canMake:     Boolean,
    onMake:      () => Unit,
    onSwitch:    Int => Unit,
    onAddTitle:  () => Unit,
)

private val MulticamPanel: Component[MulticamProps] = component[MulticamProps] { p =>
  val theme = useTheme()

  // One angle chip: its number and label, filled with the primary colour while it is the angle on air.
  // A title angle is outlined in the accent colour so a slide reads as distinct from a camera.
  def angleChip(i: Int, a: Angle, active: Boolean): VNode =
    val isTitle = a.source match { case _: AngleSource.Title => true; case _ => false }
    box(onClick = _ => p.onSwitch(i), cursor = Cursor.Pointer, radius = 6,
      bg = if active then theme.primary else theme.background,
      border = if isTitle then theme.primary else theme.border, borderWidth = 1,
      padding = EdgeInsets.symmetric(horizontal = 10, vertical = 6))(
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        text(s"${i + 1}", size = 12, weight = FontWeight.Bold, color = if active then theme.onPrimary else theme.border),
        text(a.label, size = 12, color = if active then theme.onPrimary else theme.surfaceText),
      ),
    )

  val body = p.group match
    case None =>
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 10)(
        KutterUi.textButton(theme)("Make Multicam", p.onMake, enabled = p.canMake),
        text(
          if p.canMake then "Sync the bin's videos into one switchable program"
          else "Import two or more videos to build a multicam",
          size = 11, color = theme.border,
        ),
      )
    case Some(g) =>
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
        text(g.name, size = 11, weight = FontWeight.Bold, color = theme.border),
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
          g.angles.zipWithIndex.map { case (a, i) => angleChip(i, a, i == p.activeAngle) }*,
        ),
        KutterUi.textButton(theme)("+ Title", p.onAddTitle),
      )

  box(bg = theme.background, padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(body)
}
