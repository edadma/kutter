package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The inspector pane (right column): the selected clip's details, else the selected lower third's
// editor, else a hint. A clip and a lower third are never selected at once, so at most one editor
// shows. Extracted from `App`; the resolved selection is passed in, and edits funnel back through the
// callbacks (so a keystroke re-renders the editor and the player recompiles its graph).
private final case class InspectorProps(
    selectedClip:      Option[(PlacedClip, MediaClip)],
    selectedLt:        Option[LowerThird],
    styles:            List[Style],
    fps:               Double,
    onEditLt:          (String, LowerThird => LowerThird) => Unit,
    onRemovePlacement: String => Unit,
    onUnlink:          String => Unit,
)

private val InspectorPanel: Component[InspectorProps] = component[InspectorProps] { p =>
  val theme = useTheme()

  // The editor for the selected lower third: its words, its style, and its timing.
  def lowerThirdBody(lt: LowerThird): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      KutterUi.labeled(theme)("Name")(TextField(lt.name, v => p.onEditLt(lt.id, _.copy(name = v)))),
      KutterUi.labeled(theme)("Title")(TextField(lt.title, v => p.onEditLt(lt.id, _.copy(title = v)))),
      KutterUi.labeled(theme)("Style")(
        Select(
          options  = p.styles.map(s => (s.id, s.label)),
          selected = lt.styleId,
          onChange = v => p.onEditLt(lt.id, _.copy(styleId = v)),
          width    = 200,
        ),
      ),
      row(crossAxisAlignment = CrossAxisAlignment.Start, spacing = 8)(
        box(flex = 1)(KutterUi.labeled(theme)("In")(KutterUi.intField(lt.inFrame, v => p.onEditLt(lt.id, _.copy(inFrame = v))))),
        box(flex = 1)(KutterUi.labeled(theme)("Out")(KutterUi.intField(lt.outFrame, v => p.onEditLt(lt.id, _.copy(outFrame = v))))),
        box(flex = 1)(KutterUi.labeled(theme)("Fade")(KutterUi.intField(lt.fadeFrames, v => p.onEditLt(lt.id, _.copy(fadeFrames = v))))),
      ),
    )

  // The details for the selected clip: source, timeline position and length (read-only — trimming and
  // unlinking are the timeline's edge handles), whether it is a linked A/V half, and removal.
  def clipBody(pc: PlacedClip, clip: MediaClip): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      KutterUi.labeled(theme)("Clip")(text(clip.name, size = 13, color = theme.surfaceText, maxLines = 1)),
      row(crossAxisAlignment = CrossAxisAlignment.Start, spacing = 8)(
        box(flex = 1)(KutterUi.labeled(theme)("Start")(text(KutterUi.timecode(pc.timelineStart, p.fps), size = 13, color = theme.surfaceText, mono = true))),
        box(flex = 1)(KutterUi.labeled(theme)("Length")(text(KutterUi.timecode(pc.length, p.fps), size = 13, color = theme.surfaceText, mono = true))),
      ),
      if pc.link.isDefined then text("Linked A/V — picture and sound move together.", size = 11, color = theme.accent, maxLines = 0)
      else text("Unlinked.", size = 11, color = theme.border),
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
        KutterUi.textButton(theme)("Remove from timeline", () => p.onRemovePlacement(pc.id)),
        if pc.link.isDefined then KutterUi.textButton(theme)("Unlink", () => p.onUnlink(pc.id)) else spacer(),
      ),
    )

  KutterUi.titledPanel(theme)("Inspector")(
    p.selectedClip match
      case Some((pc, clip)) => clipBody(pc, clip)
      case None =>
        p.selectedLt match
          case Some(lt) => lowerThirdBody(lt)
          case None     => KutterUi.placeholder(theme)("Select a clip or lower third"),
  )
}
