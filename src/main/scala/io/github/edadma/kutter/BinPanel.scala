package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// The bin panel (left column): the project's imported source clips and its lower thirds, each a titled
// section with actions on the right (import a video or audio clip / add a lower third), over the
// project-wide actions row (New/Open/Save/Settings/Export). Media and lower thirds can be built up
// independently — a project is its footage and its overlays, separately. Extracted from `App`; the
// data comes in as props and every action reaches back through a callback.
private final case class BinProps(
    bin:            List[MediaClip],
    lowerThirds:    List[LowerThird],
    selectedBinId:  Option[String],
    selectedLtId:   Option[String],
    onNew:          () => Unit,
    onOpen:         () => Unit,
    onSave:         () => Unit,
    onSettings:     () => Unit,
    onExport:       () => Unit,
    onImportVideo:  () => Unit,
    onImportAudio:  () => Unit,
    onImportLts:    () => Unit,
    onAddLt:        () => Unit,
    onPreviewClip:  MediaClip => Unit,
    onPlaceClip:    MediaClip => Unit,
    onRemoveClip:   MediaClip => Unit,
    onSelectLt:     String => Unit,
    onRemoveLt:     String => Unit,
)

private val BinPanel: Component[BinProps] = component[BinProps] { p =>
  val theme = useTheme()
  def button(label: String, onClick: () => Unit): VNode = KutterUi.textButton(theme)(label, onClick)

  // One lower-third row: a selectable body (name over title) and a remove button. The selected row is
  // filled with the primary colour so it reads as the inspector's subject.
  def ltRow(lt: LowerThird): VNode =
    val selected = p.selectedLtId.contains(lt.id)
    row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
      box(onClick = _ => p.onSelectLt(lt.id), cursor = Cursor.Pointer, flex = 1, radius = 6,
        bg = if selected then theme.primary else theme.background,
        padding = EdgeInsets.symmetric(horizontal = 8, vertical = 6))(
        col(mainAxisSize = MainAxisSize.Min, spacing = 1)(
          text(if lt.name.isEmpty then "(untitled)" else lt.name, size = 12,
            color = if selected then theme.onPrimary else theme.surfaceText),
          text(lt.title, size = 10, color = if selected then theme.onPrimary else theme.border),
        ),
      ),
      box(onClick = _ => p.onRemoveLt(lt.id), cursor = Cursor.Pointer, radius = 4, padding = EdgeInsets.all(4))(
        svg(closeIcon, width = 12, height = 12),
      ),
    )

  // One bin row: a kind icon, the file name (click to preview it in the clip monitor — the selected
  // row fills primary; drag it onto a timeline track to place it there), a Place button that drops it at
  // the playhead, and a remove button (which confirms, since removing a source takes its placements with
  // it). The name is the drag source: its payload is the clip id, which the timeline lanes accept as a drop.
  def binClipRow(clip: MediaClip): VNode =
    val selected = p.selectedBinId.contains(clip.id)
    row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 8)(
      svg(if clip.kind == MediaKind.Audio then volumeIcon else playIcon, width = 16, height = 16),
      box(onClick = _ => p.onPreviewClip(clip), cursor = Cursor.Pointer, flex = 1, radius = 6,
        dragPayload = clip.id,
        bg = if selected then theme.primary else Color.transparent,
        padding = EdgeInsets.symmetric(horizontal = 6, vertical = 4))(
        text(clip.name, size = 13, color = if selected then theme.onPrimary else theme.surfaceText, maxLines = 1),
      ),
      button("Place", () => p.onPlaceClip(clip)),
      box(onClick = _ => p.onRemoveClip(clip), cursor = Cursor.Pointer, radius = 4, padding = EdgeInsets.all(4))(
        svg(closeIcon, width = 12, height = 12),
      ),
    )

  KutterUi.titledPanel(theme)("Bin")(
    col(crossAxisAlignment = CrossAxisAlignment.Stretch, spacing = 10)(
      // Project actions: start over, open/save a `.kutter`, project settings, export a video.
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        button("New", p.onNew),
        button("Open", p.onOpen),
        button("Save", p.onSave),
        button("Settings", p.onSettings),
        spacer(),
        button("Export", p.onExport),
      ),
      // Media section: the imported clips (video and audio), each removable, with import actions.
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        text("Media", size = 11, weight = FontWeight.Bold, color = theme.border),
        spacer(),
        button("+ Video", p.onImportVideo),
        button("+ Audio", p.onImportAudio),
      ),
      if p.bin.isEmpty then KutterUi.placeholder(theme)("No media imported")
      else
        col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 4)(
          p.bin.map(binClipRow)*,
        ),
      // Lower thirds section: import a `.klt` list or add one by hand — both work with or without footage.
      row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 6)(
        text("Lower thirds", size = 11, weight = FontWeight.Bold, color = theme.border),
        spacer(),
        button("Import…", p.onImportLts),
        button("+ Add", p.onAddLt),
      ),
      box(flex = 1)(
        if p.lowerThirds.isEmpty then KutterUi.placeholder(theme)("No lower thirds yet")
        else
          scrollView(axis = Axis.Vertical, scrollbar = true, scrollbarThumb = theme.border)(
            col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 3)(
              p.lowerThirds.map(ltRow)*,
            ),
          ),
      ),
    ),
  )
}
