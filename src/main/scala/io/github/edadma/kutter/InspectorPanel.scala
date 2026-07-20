package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The inspector pane (right column): the selected clip's details, else a selected title's editor (a
// title placed on a track, or an unplaced title in the bin), else a hint. A clip and a title are never
// selected at once, so at most one editor shows. Extracted from `App`; the resolved selection is passed
// in, and edits funnel back through the callbacks (so a keystroke re-renders the editor and the player
// recompiles its graph).
private final case class InspectorProps(
    selectedClip:      Option[(PlacedClip, MediaClip)],
    selectedTitle:     Option[(PlacedClip, LowerThird)], // a title placed on a video track
    selectedLt:        Option[LowerThird],               // a title selected in the bin (unplaced content)
    styles:            List[Style],
    fps:               Double,
    onEditLt:          (String, LowerThird => LowerThird) => Unit,
    onRemovePlacement: String => Unit,
    onUnlink:          String => Unit,
)

private val InspectorPanel: Component[InspectorProps] = component[InspectorProps] { p =>
  val theme = useTheme()

  // The content fields shared by both title editors: the words, the style, and the fade. The window
  // (start/length) is not here — it belongs to a placement and shows only when a placed title is selected.
  def titleFields(lt: LowerThird): Seq[VNode] = Seq(
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
    KutterUi.labeled(theme)("Fade")(KutterUi.intField(lt.fadeFrames, v => p.onEditLt(lt.id, _.copy(fadeFrames = v)))),
  )

  // The editor for a title selected in the bin: its content only, with a hint that it reaches the screen
  // by being dragged onto a video track.
  def binTitleBody(lt: LowerThird): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      (titleFields(lt) :+
        text("Drag onto a video track to place it on the timeline.", size = 11, color = theme.border, maxLines = 0))*,
    )

  // The editor for a title placed on a track: its content, its window on the timeline (read-only —
  // sliding and trimming are the timeline's handles), and removal.
  def titlePlacementBody(pc: PlacedClip, lt: LowerThird): VNode =
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 12)(
      (titleFields(lt) :+
        row(crossAxisAlignment = CrossAxisAlignment.Start, spacing = 8)(
          box(flex = 1)(KutterUi.labeled(theme)("Start")(text(KutterUi.timecode(pc.timelineStart, p.fps), size = 13, color = theme.surfaceText, mono = true))),
          box(flex = 1)(KutterUi.labeled(theme)("Length")(text(KutterUi.timecode(pc.length, p.fps), size = 13, color = theme.surfaceText, mono = true))),
        ) :+
        KutterUi.textButton(theme)("Remove from timeline", () => p.onRemovePlacement(pc.id)))*,
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
      case None => p.selectedTitle match
        case Some((pc, lt)) => titlePlacementBody(pc, lt)
        case None => p.selectedLt match
          case Some(lt) => binTitleBody(lt)
          case None     => KutterUi.placeholder(theme)("Select a clip or title"),
  )
}
