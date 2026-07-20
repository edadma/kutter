package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// Shared presentational helpers for kutter's panels — the small vnode builders that used to live as
// closure defs inside the one big `App` component. They are pure functions of a `Theme` (and their
// arguments), so both `App` and the extracted panel/dialog components build the same-looking chrome
// without duplicating it. `App` keeps thin same-named locals that delegate here, so its many call
// sites read unchanged.
object KutterUi:

  /** A frame count as an `m:ss.cc` timecode at `fps` — minutes, seconds, and hundredths (floored). The
    * hundredths make a scrub read precisely without a full SMPTE frame field. */
  def timecode(frames: Int, fps: Double): String =
    val cs   = math.max(0L, (frames / fps * 100).toLong) // centiseconds, floored
    val secs = cs / 100
    f"${secs / 60}:${secs % 60}%02d.${cs % 100}%02d"

  /** A bordered surface pane — the shell every panel sits in. */
  def panel(theme: Theme)(flexN: Int = 0, h: Double = Double.NaN)(child: VNode): VNode =
    box(bg = theme.surface, flex = flexN, height = h, radius = 10, border = theme.border, borderWidth = 1, clip = true)(
      child,
    )

  /** A titled panel: a small header bar over the panel's body — the shell the bin and the inspector
    * use, matching a Resolve-style editor's labelled panes. */
  def titledPanel(theme: Theme)(title: String)(body: VNode): VNode =
    panel(theme)(flexN = 1)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch)(
        box(bg = theme.background, padding = EdgeInsets.symmetric(horizontal = 12, vertical = 8))(
          text(title, size = 12, weight = FontWeight.Bold, color = theme.surfaceText),
        ),
        box(flex = 1, padding = EdgeInsets.all(12))(body),
      ),
    )

  /** A centred hint — a panel body with nothing in it yet, or an empty list. */
  def placeholder(theme: Theme)(hint: String): VNode =
    col(mainAxisAlignment = MainAxisAlignment.Center, crossAxisAlignment = CrossAxisAlignment.Center)(
      text(hint, size = 13, color = theme.border),
    )

  /** A small text button — a rounded label that runs `onClick`; dimmed and inert when `enabled` is
    * false. */
  def textButton(theme: Theme)(label: String, onClick: () => Unit, enabled: Boolean = true): VNode =
    box(onClick = if enabled then (_ => onClick()) else (_ => ()),
      cursor = if enabled then Cursor.Pointer else Cursor.Default, bg = theme.background, radius = 6,
      padding = EdgeInsets.symmetric(horizontal = 10, vertical = 6))(
      text(label, size = 12, color = if enabled then theme.surfaceText else theme.border),
    )

  /** A labelled control: a small caption over the field. */
  def labeled(theme: Theme)(label: String)(control: VNode): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 4)(
      text(label, size = 11, weight = FontWeight.Bold, color = theme.border),
      control,
    )

  /** A whole-number field: a text field that commits only a value that parses, so a partial or empty
    * entry leaves the model untouched rather than snapping it to zero. */
  def intField(value: Int, onChange: Int => Unit): VNode =
    TextField(value.toString, s => s.trim.toIntOption.foreach(onChange))
