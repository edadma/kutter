package io.github.edadma.kutter

import io.github.edadma.suit.*
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

// kutter's modal dialogs, extracted from `App`: the generic confirmation prompt, the project-settings
// editor, and the export-progress bar. Each is presentational — it takes its data and a couple of
// callbacks and renders a suit `Dialog`; the editor state (a pending `ConfirmSpec`, the settings
// draft, the export progress) stays in `App`.

// The staged project settings the dialog edits — the name, the creation date, and the timeline format
// — held as one value in `App` so a fill-and-cancel never touches the project.
private[kutter] final case class SettingsDraft(name: String, created: String, spec: TimelineSpec)

/** The confirmation modal, shown while a `ConfirmSpec` is pending. Cancel or the scrim dismisses it;
  * the action button runs the pending action. One dialog serves every destructive prompt. */
private final case class ConfirmProps(spec: Option[ConfirmSpec], onDismiss: () => Unit)

private val ConfirmDialog: Component[ConfirmProps] = component[ConfirmProps] { p =>
  val theme = useTheme()
  val (title, msg, label, action) = p.spec match
    case Some(s) => (s.title, s.message, s.confirmLabel, s.action)
    case None    => ("", "", "OK", () => ())
  Dialog(open = p.spec.isDefined, onClose = p.onDismiss, width = 380)(
    // A fixed inner width so the message wraps (the dialog does not constrain its content on its own).
    sizedBox(width = 336)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 16)(
        text(title, size = 16, weight = FontWeight.Bold, color = theme.surfaceText),
        text(msg, size = 13, color = theme.border, maxLines = 0),
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 10)(
          spacer(),
          KutterUi.textButton(theme)("Cancel", p.onDismiss),
          KutterUi.textButton(theme)(label, () => { p.onDismiss(); action() }),
        ),
      ),
    ),
  )
}

/** The project-settings dialog. Resolution / frame-rate / audio-rate are dropdowns of the common
  * choices (the current value is added when it is not one of them, e.g. an auto-adopted odd size). The
  * frame rate is the edit's clock, so it is only editable while the timeline is empty (`fpsLocked`);
  * once there is content it shows read-only, because changing it would reinterpret every clip's
  * position in time. Edits flow through `onChange`; Apply commits the draft. */
private final case class SettingsProps(
    open:      Boolean,
    draft:     SettingsDraft,
    fpsLocked: Boolean,
    onChange:  (SettingsDraft => SettingsDraft) => Unit,
    onApply:   () => Unit,
    onClose:   () => Unit,
)

private val SettingsDialog: Component[SettingsProps] = component[SettingsProps] { p =>
  val theme = useTheme()
  val d     = p.draft
  val spec  = d.spec

  val dfFps     = spec.fps
  val dfFpsText = if math.abs(dfFps - math.rint(dfFps)) < 1e-6 then dfFps.toInt.toString else f"$dfFps%.2f"
  val sizeVal   = s"${spec.width}x${spec.height}"
  val sizeOpts =
    val base = TimelineSpec.sizeChoices.map((label, w, h) => (s"${w}x${h}", label))
    if base.exists(_._1 == sizeVal) then base else (sizeVal, s"${spec.width}×${spec.height} (current)") +: base
  val rateVal = s"${spec.fpsNum}/${spec.fpsDen}"
  val rateOpts =
    val base = TimelineSpec.rateChoices.map((label, n, dd) => (s"$n/$dd", s"$label fps"))
    if base.exists(_._1 == rateVal) then base else (rateVal, s"$dfFpsText fps (current)") +: base
  val audioOpts = TimelineSpec.audioRateChoices.map(r => (r.toString, s"$r Hz"))

  Dialog(open = p.open, onClose = p.onClose, width = 440)(
    sizedBox(width = 396)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 14)(
        text("Project Settings", size = 16, weight = FontWeight.Bold, color = theme.surfaceText),
        KutterUi.labeled(theme)("Name")(TextField(d.name, v => p.onChange(_.copy(name = v)))),
        KutterUi.labeled(theme)("Created")(TextField(d.created, v => p.onChange(_.copy(created = v)))),
        KutterUi.labeled(theme)("Resolution")(
          Select(sizeOpts, sizeVal, v => v.split("x") match
            case Array(w, h) => (w.toIntOption, h.toIntOption) match
                case (Some(ww), Some(hh)) => p.onChange(dr => dr.copy(spec = dr.spec.copy(width = ww, height = hh)))
                case _                    => ()
            case _ => (),
          width = 372),
        ),
        if p.fpsLocked then
          KutterUi.labeled(theme)("Frame rate")(
            col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 3)(
              text(s"$dfFpsText fps", size = 13, color = theme.surfaceText, mono = true),
              text("Locked — the frame rate is the edit's clock. Clear the timeline to change it.",
                size = 11, color = theme.border, maxLines = 0),
            ),
          )
        else
          KutterUi.labeled(theme)("Frame rate")(
            Select(rateOpts, rateVal, v => v.split("/") match
              case Array(n, dd) => (n.toIntOption, dd.toIntOption) match
                  case (Some(nn), Some(ddn)) => p.onChange(dr => dr.copy(spec = dr.spec.copy(fpsNum = nn, fpsDen = ddn)))
                  case _                     => ()
              case _ => (),
            width = 372),
          ),
        KutterUi.labeled(theme)("Audio rate")(
          Select(audioOpts, spec.audioRate.toString,
            v => v.toIntOption.foreach(r => p.onChange(dr => dr.copy(spec = dr.spec.copy(audioRate = r)))), width = 372),
        ),
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 10)(
          spacer(),
          KutterUi.textButton(theme)("Cancel", p.onClose),
          KutterUi.textButton(theme)("Apply", p.onApply),
        ),
      ),
    ),
  )
}

/** The export-progress dialog: a determinate bar over the encode, not dismissable by the scrim (the
  * export is running behind it), with a Cancel that stops it. */
private final case class ExportProps(open: Boolean, progress: Double, onCancel: () => Unit)

private val ExportDialog: Component[ExportProps] = component[ExportProps] { p =>
  val theme = useTheme()
  Dialog(open = p.open, onClose = () => (), maskClosable = false, width = 380)(
    sizedBox(width = 336)(
      col(crossAxisAlignment = CrossAxisAlignment.Stretch, mainAxisSize = MainAxisSize.Min, spacing = 16)(
        text("Exporting video…", size = 16, weight = FontWeight.Bold, color = theme.surfaceText),
        text(s"${math.round(p.progress * 100)}%", size = 13, color = theme.border, mono = true),
        ProgressBar(p.progress),
        row(crossAxisAlignment = CrossAxisAlignment.Center, spacing = 10)(
          spacer(),
          KutterUi.textButton(theme)("Cancel", p.onCancel),
        ),
      ),
    ),
  )
}
