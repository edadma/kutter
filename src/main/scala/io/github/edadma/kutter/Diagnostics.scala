package io.github.edadma.kutter

import io.github.edadma.mlt.Mlt
import io.github.edadma.suit.Color
import scala.io.Source
import zio.json.*

/** The windowless diagnostics — the `KUTTER_PROBE*` env-var checks that exercise kutter's pipeline
  * without opening a window. They are the standing regression guard for the parts that can be verified
  * off the GUI (the graph compile and render, the live-edit graph swap, the timeline's click mapping,
  * the batch importer, and the texish card renderer); the GUI itself is human-verified.
  *
  * `main` builds a project (a demo, or a named `.kutter`) and hands it here; `run` executes whichever
  * probe is selected and reports whether one ran, so the caller exits instead of opening the app. */
private[kutter] object Diagnostics:

  // The frame length a demo placement is sized to. The bundled clip resamples to 600 frames at the
  // 30fps profile; MLT clamps a placement to the source's real length, so this is a safe upper bound
  // for the probes without opening a producer to measure it (which would need MLT initialised first).
  private val DemoLength = 600

  /** A project holding a single video clip as a linked A/V pair — its picture on V1 and its audio on
    * A1, sharing a link id — for the probes and helpers that want a project built around one source.
    * Both placements run `len` frames from the clip's head. */
  def videoProject(path: String, len: Int = DemoLength): Project =
    val clip = MediaClip.make(path, MediaKind.Video, len)
    val link = Some("demo-link")
    Project.blank.copy(
      bin = List(clip),
      tracks = List(
        Track("v1", "V1", MediaKind.Video, List(PlacedClip.make(clip.id, 0, len, link = link))),
        Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(clip.id, 0, len, link = link))),
      ),
    )

  /** A demonstration project over `mediaPath`: the clip on V1 and three lower thirds in three different
    * styles, so the compositing and the style presets are all visible. A stand-in that gives the probes
    * overlays to exercise; the real app starts from a blank or remembered session instead. */
  def demoProject(mediaPath: String): Project =
    videoProject(mediaPath).copy(
      name = "Demo",
      lowerThirds = List(
        LowerThird("lt1", "Big Buck Bunny", "kutter · automated lower thirds", 45, 165, styleId = "broadcast-blue"),
        LowerThird("lt2", "Directed by", "the Blender Foundation", 230, 350, styleId = "bold-bar"),
        LowerThird("lt3", "Typeset by", "the texish engine", 420, 540, styleId = "texish-card"),
      ),
    )

  // Load a rendered card PNG and report its pixel size and whether it is an alpha-capable ARGB32 image —
  // enough to confirm off the GUI that a texish card came out at the frame size and kept the overlay
  // transparency the compositor relies on, using only libcairo's safe surface accessors.
  private def analyzeCard(path: String): (Int, Int, Boolean) =
    import io.github.edadma.libcairo.{Format, imageSurfaceCreateFromPNG}
    val s    = imageSurfaceCreateFromPNG(path)
    val dims = (s.getWidth, s.getHeight, s.getFormat.value == Format.ARGB32.value)
    s.destroy()
    dims

  /** Run whichever `KUTTER_PROBE*` diagnostic is selected, against `project`. Returns true if a probe
    * ran — the caller then exits without opening the GUI — or false if none was requested. */
  def run(project: Project): Boolean =
    // `KUTTER_PROBE=<frame>` renders that composited frame to probe.png and exits, for checking the
    // lower-third overlay without the GUI.
    sys.env.get("KUTTER_PROBE").flatMap(_.toIntOption) match
      case Some(frame) =>
        // KUTTER_PROBE_IMPORT=<file> imports that batch list onto the project first, so an imported
        // timeline (many overlays, cards clamped to the timeline end) can be rendered headlessly.
        val proj = sys.env.get("KUTTER_PROBE_IMPORT").fold(project) { f =>
          val ds  = project.styles.headOption.map(_.id).getOrElse("broadcast-blue")
          val src = Source.fromFile(f)
          val txt = try src.mkString finally src.close()
          BatchImport.parse(txt, 30.0, ds) match
            case Right(lts) => project.copy(lowerThirds = lts) // replace, so the render shows the imported set alone
            case Left(err)  => println(s"probe import error: $err"); project
        }
        Mlt.init()
        Player.probe(proj, frame, "probe.png")
        Mlt.close()
        return true
      case None => ()

    // `KUTTER_PROBE_EDIT=<frame>` exercises the live-edit graph swap headlessly: it renders that frame
    // after restyling the second lower third to "minimal", so the PNG shows the edit having taken hold
    // through a consumer reconnect — the same path an inspector edit drives.
    sys.env.get("KUTTER_PROBE_EDIT").flatMap(_.toIntOption) match
      case Some(frame) =>
        val edited = project.copy(lowerThirds =
          project.lowerThirds.map(lt => if lt.styleId == "bold-bar" then lt.copy(styleId = "minimal") else lt),
        )
        Mlt.init()
        Player.probeRebuild(project, edited, frame, "probe-edit.png")
        Mlt.close()
        return true
      case None => ()

    // `KUTTER_PROBE_AUDIO=<source.wav>` renders that source through the real playback graph to a WAV
    // (`KUTTER_AUDIO_OUT`, default /tmp/kutter-audio-out.wav) for offline analysis of the audio the graph
    // actually produces. `KUTTER_AUDIO_MODE` picks the graph shape: `graph` (default — one clip on an
    // audio track, the black base + mix, exactly as the app plays it), `twoclip` (two placements
    // sequenced, to expose a seam), or `lt` (with a lower third, to expose compositing overhead).
    sys.env.get("KUTTER_PROBE_AUDIO") match
      case Some(src) =>
        Mlt.init()
        val out  = sys.env.getOrElse("KUTTER_AUDIO_OUT", "/tmp/kutter-audio-out.wav")
        val mode = sys.env.getOrElse("KUTTER_AUDIO_MODE", "graph")
        val len  = Player.mediaLength(src)
        val clip = MediaClip.make(src, MediaKind.Audio, len)
        val proj = mode match
          case "twoclip" =>
            Project.blank.copy(bin = List(clip), tracks = List(
              Track("a1", "A1", MediaKind.Audio, List(
                PlacedClip.make(clip.id, 0, len / 2),
                PlacedClip.make(clip.id, len / 2, len - len / 2),
              )),
            ))
          case "lt" =>
            Project.blank.copy(bin = List(clip),
              tracks = List(Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(clip.id, 0, len)))),
              lowerThirds = List(LowerThird("t", "Name", "Title", 0, math.max(1, len - 1))))
          case "full" =>
            // Mimic the project monitor: a video clip composited on V1, the tone mixed on A1, and three
            // lower-third card composites — the graph shape the interference appears in.
            val vsrc  = sys.env.getOrElse("KUTTER_AUDIO_VIDEO", "big_buck_bunny_720p.mp4")
            val vlen  = Player.mediaLength(vsrc)
            val vclip = MediaClip.make(vsrc, MediaKind.Video, vlen)
            Project.blank.copy(
              bin = List(clip, vclip),
              tracks = List(
                Track("v1", "V1", MediaKind.Video, List(PlacedClip.make(vclip.id, 0, math.min(vlen, len)))),
                Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(clip.id, 0, len))),
              ),
              lowerThirds = List(
                LowerThird("t1", "One", "a", 0, len / 3),
                LowerThird("t2", "Two", "b", len / 3, 2 * len / 3),
                LowerThird("t3", "Three", "c", 2 * len / 3, math.max(1, len - 1)),
              ),
            )
          case _ =>
            Project.blank.copy(bin = List(clip),
              tracks = List(Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(clip.id, 0, len)))))
        Player.renderAudio(proj, out)
        Mlt.close()
        println(s"rendered audio ($mode, $len frames) -> $out")
        return true
      case None => ()

    // `KUTTER_PROBE_HIT` checks the timeline's pure click-mapping and drag math — `frameAt` (cursor x →
    // frame), `overlayAt`/`clipAt` (cursor x → the lower third or clip under it), `dragPlacement` (a
    // title's new window), and `clipStartBounds` (how far a placed clip may slide within its gap) —
    // without a window, since selecting and moving a block on the timeline hang off exactly these. It
    // prints PASS/FAIL per case and exits non-zero on any failure, a regression guard runnable off the GUI.
    if sys.env.contains("KUTTER_PROBE_HIT") then
      val total = 600
      val width = 600.0 // a 1:1 frame-to-pixel mapping keeps the expected values obvious
      val a     = Timeline.OverlayBlock("a", 45, 165, "A", Color.white, false)
      val b     = Timeline.OverlayBlock("b", 230, 350, "B", Color.white, false)
      val c     = Timeline.OverlayBlock("c", 100, 140, "C", Color.white, false) // overlaps a; drawn later
      var ok    = true
      def check(name: String, got: Any, want: Any): Unit =
        val pass = got == want
        if !pass then ok = false
        println(f"${if pass then "PASS" else "FAIL"}%-4s $name%-28s got=$got want=$want")

      check("frameAt mid", Timeline.frameAt(100.0, total, width), 100)
      check("frameAt clamp-low", Timeline.frameAt(-10.0, total, width), 0)
      check("frameAt clamp-high", Timeline.frameAt(9999.0, total, width), total - 1)
      check("overlayAt on a", Timeline.overlayAt(100.0, total, width, Seq(a, b)), Some("a"))
      check("overlayAt on b", Timeline.overlayAt(300.0, total, width, Seq(a, b)), Some("b"))
      check("overlayAt gap", Timeline.overlayAt(200.0, total, width, Seq(a, b)), None)
      check("overlayAt topmost", Timeline.overlayAt(120.0, total, width, Seq(a, c)), Some("c"))
      // dragPlacement: block in=45 out=165 (len 120), moved by a (snapped) delta and clamped.
      check("drag right", Timeline.dragPlacement(45, 120, 50, total), 95)
      check("drag left", Timeline.dragPlacement(45, 120, -40, total), 5)
      check("drag clamp-left", Timeline.dragPlacement(45, 120, -100, total), 0)
      check("drag clamp-right", Timeline.dragPlacement(45, 120, 9999, total), total - 1 - 120)

      // The drag magnetism. snapReach converts the fixed 8-pixel magnet to frames under the widget's
      // mapping (1:1 here → 8 frames, and never below 1 frame). snapDelta adjusts a sliding block's
      // delta so whichever edge lands nearest an edit point within reach sticks to it — a block at 100
      // of length 50 dragged +43 has its start at 143, 7 short of the target 150, so the delta grows
      // to 50; dragged +13 its end (163) is nearest 160, pulling the delta back to 10; out of reach or
      // with no targets the cursor's delta is untouched. snapEdgeDelta does the same for a trim's one
      // moving edge.
      check("snap reach 1:1", Timeline.snapReach(total, width), 8)
      check("snap reach floor", Timeline.snapReach(10, 9999.0), 1)
      check("snap start", Timeline.snapDelta(43, 100, 50, Seq(150), 8), 50)
      check("snap end", Timeline.snapDelta(13, 100, 50, Seq(160), 8), 10)
      check("snap out of reach", Timeline.snapDelta(20, 100, 50, Seq(150), 8), 20)
      check("snap no targets", Timeline.snapDelta(43, 100, 50, Nil, 8), 43)
      check("snap nearest wins", Timeline.snapDelta(43, 100, 50, Seq(150, 146), 8), 46)
      check("snap edge", Timeline.snapEdgeDelta(43, 100, Seq(150), 8), 50)
      check("snap edge out of reach", Timeline.snapEdgeDelta(20, 100, Seq(150), 8), 20)

      // clipAt: two clips sequenced on a lane — [0,100) and [150,250). A press inside one picks it; a
      // press in the gap between them picks neither. This is how a press on a media lane selects a clip.
      val cb0 = Timeline.ClipBlock("c0", 0, 100, "0", false, false)
      val cb1 = Timeline.ClipBlock("c1", 150, 100, "1", false, false)
      check("clipAt on 0", Timeline.clipAt(50.0, total, width, Seq(cb0, cb1)), Some("c0"))
      check("clipAt on 1", Timeline.clipAt(200.0, total, width, Seq(cb0, cb1)), Some("c1"))
      check("clipAt gap", Timeline.clipAt(125.0, total, width, Seq(cb0, cb1)), None)

      // clipStartBounds: a clip of length 50 at start 100 between neighbours [0,80) and [200,260) may
      // slide only within its gap — no earlier than the previous end (80), no later than the next start
      // less its length (200-50=150). With no neighbours it ranges the whole timeline up to total-length.
      check("clip bounds gap", Timeline.clipStartBounds(100, 50, total, Seq((0, 80), (200, 60))), (80, 150))
      check("clip bounds free", Timeline.clipStartBounds(100, 50, total, Nil), (0, total - 50))

      // freePlacement: dropping a clip at the playhead. On an empty track it lands where dropped; in a
      // gap smaller than the source it is trimmed to fit; dropped inside a clip it snaps to that clip's
      // end; and a linked pair snaps past clips on either track so picture and sound share one start.
      check("place empty", Timeline.freePlacement(100, 50, Nil, Nil), (100, 50))
      check("place fits gap", Timeline.freePlacement(100, 200, Seq((0, 80), (200, 60)), Nil), (100, 100))
      check("place snaps inside", Timeline.freePlacement(50, 100, Seq((0, 120)), Nil), (120, 100))
      check("place pair snaps both", Timeline.freePlacement(50, 30, Seq((0, 100)), Seq((90, 50))), (140, 30))

      // clipEdgeAt: a press within a few pixels of a block's left/right edge picks that edge to trim; a
      // press on the body picks neither (the caller then treats it as a move). With the 1:1 mapping,
      // cb0 spans px 0..100 and cb1 spans 150..250.
      check("edge left", Timeline.clipEdgeAt(2.0, total, width, Seq(cb0, cb1)), Some(("c0", Timeline.TrimEdge.Left)))
      check("edge right", Timeline.clipEdgeAt(100.0, total, width, Seq(cb0, cb1)), Some(("c0", Timeline.TrimEdge.Right)))
      check("edge body none", Timeline.clipEdgeAt(50.0, total, width, Seq(cb0, cb1)), None)
      check("edge next left", Timeline.clipEdgeAt(150.0, total, width, Seq(cb0, cb1)), Some(("c1", Timeline.TrimEdge.Left)))

      // clipTrimBounds: a placement at start 100 playing 80 frames from in-point 20 of a 200-frame
      // source, between neighbours [0,50) and [300,360). The right edge may grow to the source's end
      // (200-20-80=100) or the next clip (300-180=120), whichever is nearer, and shrink to a 1-frame
      // clip; the left edge may move back to the in-point limit (-20) or the previous clip (-50),
      // whichever is nearer, and forward to a 1-frame clip (79). A short source caps the right extension
      // at the source's own end.
      check("trim right", Timeline.clipTrimBounds(Timeline.TrimEdge.Right, 100, 20, 80, 200, total, Seq((0, 50), (300, 60))), (-79, 100))
      check("trim left", Timeline.clipTrimBounds(Timeline.TrimEdge.Left, 100, 20, 80, 200, total, Seq((0, 50), (300, 60))), (-20, 79))
      check("trim right at source end", Timeline.clipTrimBounds(Timeline.TrimEdge.Right, 100, 20, 80, 100, total, Nil), (-79, 0))

      // Batch import: the time parser (timecodes / seconds) and one whole HOCON list.
      check("time m:ss", BatchImport.parseTime("1:12"), Right(72.0))
      check("time h:mm:ss", BatchImport.parseTime("1:00:00"), Right(3600.0))
      check("time m:ss.cc", BatchImport.parseTime("0:03.50"), Right(3.5))
      check("time seconds", BatchImport.parseTime("3.5s"), Right(3.5))
      check("time bad", BatchImport.parseTime("abc").isLeft, true)
      val list =
        """lowerThirds = [
          |  { name = "A", title = "t", in = "0:01", out = "0:03" }
          |  { name = "B", in = 4, out = 6, fade = 5, style = minimal }
          |]""".stripMargin
      val imported = BatchImport.parse(list, 30.0, "broadcast-blue")
      check("import count", imported.map(_.size), Right(2))
      check("import in-frame", imported.map(_.head.inFrame), Right(30))    // 1s @ 30fps
      check("import out-frame", imported.map(_.head.outFrame), Right(90))  // 3s @ 30fps
      check("import default style", imported.map(_.head.styleId), Right("broadcast-blue"))
      check("import row-style", imported.map(_(1).styleId), Right("minimal"))
      check("import row-fade", imported.map(_(1).fadeFrames), Right(5))
      check("import bad-order", BatchImport.parse("""lowerThirds=[{name=A,in=5,out=3}]""", 30.0, "x").isLeft, true)

      // Texish card: render the preset template to a temp PNG and confirm it comes out at the frame size
      // as an alpha-capable ARGB32 image — the texish renderer end to end (document macros, font
      // selection, transparent page, PNG out). Its visual look is confirmed in the GUI smoke test.
      val cardPath = s"${System.getProperty("java.io.tmpdir")}/kutter-probe-texish.png"
      TexishCard.render(Style.texishCard.texish.get, "Jane Smith", "CEO, Acme", 640, 360, cardPath)
      val (cw, ch, argb) = analyzeCard(cardPath)
      check("texish card width", cw, 640)
      check("texish card height", ch, 360)
      check("texish card argb32", argb, true)

      // A batch row can carry a raw texish `body` (a HOCON triple-quoted string), which the importer
      // keeps verbatim on the lower third.
      val tq       = "\"\"\""
      val bodyList = s"lowerThirds = [ { name = X, in = 0, out = 2, body = ${tq}raw body${tq} } ]"
      check("import body", BatchImport.parse(bodyList, 30.0, "b").map(_.head.body), Right(Some("raw body")))

      // A card's own body overrides its style: even paired with a Cairo style (no template), renderCard
      // dispatches to texish and produces a valid transparent card.
      val bodyDoc  = """\use{document}\geometry margin:0\def footline {}\vfill\noindent{\color{white}\font lmroman 40 {sans bold}\the\ltname}"""
      val bodyLt   = LowerThird("probe-body", "Custom", "", 0, 60, body = Some(bodyDoc))
      val (bw, _, bargb) = analyzeCard(CardRenderer.renderCard(bodyLt, Style.broadcastBlue, 480, 270))
      check("body card width", bw, 480)
      check("body card argb32", bargb, true)

      // The fixed-track model: a blank project has the empty default tracks (V1, A1), no clips on them,
      // and no overlays; a placement lands a bin clip on a track. Cover the model's shape and that a
      // session with edits survives the JSON round-trip the cache store uses.
      check("blank project empty", !Project.blank.hasMedia && Project.blank.lowerThirds.isEmpty, true)
      check("blank default tracks", Project.blank.tracks.map(_.name), List("V1", "A1"))
      val vp = videoProject("clip.mp4", 120)
      check("video project bin", vp.bin.map(_.path), List("clip.mp4"))
      check("video project placed", vp.videoTracks.head.clips.map(_.length), List(120))
      check("video project hasMedia", vp.hasMedia, true)
      // The video is a linked A/V pair: its audio is placed on A1 with the same window and link id as
      // the picture on V1, so A1 shows the video's peaks and the two move together.
      check("video project audio linked", vp.audioTracks.head.clips.map(_.length), List(120))
      check("video project link shared",
        vp.videoTracks.head.clips.head.link == vp.audioTracks.head.clips.head.link && vp.videoTracks.head.clips.head.link.isDefined,
        true)
      val sess = Session(
        vp.copy(lowerThirds = List(LowerThird("x", "Name", "Title", 0, 60))),
        Some("/path/proj.kutter"),
      )
      check("session roundtrip", sess.toJsonPretty.fromJson[Session], Right(sess))

      // Two placements of the same bin clip sequence on one track: the second starts after the first, so
      // the track's content reaches the end of the second. This is the sequencing buildGraph lays out.
      val seqClip  = MediaClip.make("a.mp4", MediaKind.Video)
      val seqTrack = Track("v1", "V1", MediaKind.Video, List(
        PlacedClip.make(seqClip.id, 0, 100),
        PlacedClip.make(seqClip.id, 100, 100),
      ))
      check("track content end", seqTrack.contentEnd, 200)
      check("track gain muted", Track("a", "A", MediaKind.Audio, Nil, 0.8, muted = true).gain, 0.0)

      // The mixer's dB math: a linear fader gain converts to decibels for the readout — unity is 0 dB,
      // half amplitude about −6 dB, double about +6 dB, and silence −∞ (no finite dB). Feeding a muted
      // track's effective gain (0) here is what makes a muted fader read −∞.
      check("gainToDb unity", Mixer.gainToDb(1.0), 0.0)
      check("gainToDb silence", Mixer.gainToDb(0.0), Double.NegativeInfinity)
      check("dbLabel unity", Mixer.dbLabel(1.0), "0.0 dB")
      check("dbLabel silence", Mixer.dbLabel(0.0), "-∞ dB")
      check("dbLabel half", Mixer.dbLabel(0.5), "-6.0 dB")
      check("dbLabel double", Mixer.dbLabel(2.0), "+6.0 dB")
      // levelDb feeds the volume filter's `level`: unity is 0 dB, and silence floors to a large finite
      // negative (not −∞) so the property stays settable.
      check("levelDb unity", Mixer.levelDb(1.0), 0.0)
      check("levelDb silence finite", Mixer.levelDb(0.0) < -100.0, true)

      // Re-import replaces: lower thirds tagged with a source file are swapped out (not appended to) when
      // that same file is imported again; overlays from other files or added by hand are untouched.
      val existing = List(
        LowerThird("a", "A", "", 0, 60, source = Some("f.klt")),
        LowerThird("b", "B", "", 0, 60, source = Some("other.klt")),
        LowerThird("c", "C", "", 0, 60, source = None),
      )
      val reimported = List(LowerThird("a2", "A2", "", 0, 60, source = Some("f.klt")))
      val merged     = existing.filterNot(_.source.contains("f.klt")) ++ reimported
      check("reimport replaces source", merged.map(_.id), List("b", "c", "a2"))

      println(if ok then "ALL PASS" else "FAILURES")
      if !ok then sys.exit(1)
      return true

    // `KUTTER_PROBE_THREAD` renders a card on a spawned thread — the way the player's decode thread does
    // during a live rebuild — to check whether card rendering off the main thread is what crashes an
    // in-app import/add. Prints OK or the error; a hard crash (no output) confirms it too.
    if sys.env.contains("KUTTER_PROBE_THREAD") then
      @volatile var done = "no result"
      val t = new Thread(() =>
        try
          val cairo = CardRenderer.renderCard(LowerThird("t1", "Name", "Title", 0, 60), Style.broadcastBlue, 640, 360)
          val tex   = CardRenderer.renderCard(LowerThird("t2", "Name", "Title", 0, 60), Style.texishCard, 640, 360)
          done = s"OK cairo=$cairo texish=$tex"
        catch case e: Throwable => done = s"THREW ${e}",
      )
      t.start()
      t.join()
      println(s"thread render: $done")
      return true

    // `KUTTER_PROBE_SWAP` checks the crash fix: graphs are built on the main thread, and only the swap
    // (re-point the consumer, free the old graph) runs on a spawned thread — the shape of the live-edit
    // path after the fix. It must print OK (before the fix, building the graph off the main thread threw
    // an NSException from MLT's producer loader).
    if sys.env.contains("KUTTER_PROBE_SWAP") then
      Mlt.init()
      val base = videoProject("big_buck_bunny_720p.mp4")
      val src  = Source.fromFile("examples/lower-thirds.klt")
      val lts  = try BatchImport.parse(src.mkString, 30.0, "broadcast-blue").getOrElse(Nil) finally src.close()
      println(s"swap off thread: ${Player.probeSwapOffThread(base, base.copy(lowerThirds = lts))}")
      Mlt.close()
      return true

    // `KUTTER_PROBE_BLACK` renders a titles-only project (no clips on any track) to a PNG — checking the
    // black colour producer base composites the lower thirds so a texish card can be previewed before any
    // footage.
    if sys.env.contains("KUTTER_PROBE_BLACK") then
      Mlt.init()
      val proj = Project.blank.copy(lowerThirds = List(
        LowerThird("t", "Jane Smith", "CEO, Acme", 0, 90, styleId = "texish-card"),
      ))
      Player.probe(proj, 45, "probe-black.png")
      Mlt.close()
      println("black-base render wrote probe-black.png")
      return true

    // `KUTTER_PROBE_CARD` renders the texish-card style — whose template paints a translucent
    // `\colorbox` bar behind the name/title — first to its own transparent PNG, then composited over
    // real footage to probe-card.png. It checks that a translucent colour paints through kutter's ARGB
    // `CairoImageTypesetter` path (not just the CLI's PDF backend) and that the shipped template renders
    // without error, which is the point of a texish lower third with a background.
    if sys.env.contains("KUTTER_PROBE_CARD") then
      val card = LowerThird("card", "Jane Smith", "CEO, Acme", 0, 90, styleId = "texish-card")
      val out  = CardRenderer.renderCard(card, Style.texishCard, 1280, 720)
      println(s"texish-card wrote $out")
      // Composite the same card over real footage so the translucency reads over a picture, not gray.
      Mlt.init()
      Player.probe(videoProject("big_buck_bunny_720p.mp4").copy(lowerThirds = List(card)), 45, "probe-card.png")
      Mlt.close()
      println("texish-card over video wrote probe-card.png")
      return true

    // `KUTTER_PROBE_NLE` renders the fullest fixed-track shape the model compiles — a video track with
    // two clips sequenced along it (a gap between them shows the black base through), an audio track
    // mixed in, and a lower third on top — to a PNG. It checks that a track's playlist sequences its
    // placements with blanks for the gaps, that a second (audio) track mixes without putting its own
    // picture on screen (its video stream is disabled), and that the card stays on top. Frame 40 lands
    // inside the first clip. Sample clips live in examples/.
    if sys.env.contains("KUTTER_PROBE_NLE") then
      Mlt.init()
      val vid = MediaClip.make("examples/sample-silent.mp4", MediaKind.Video)
      val aud = MediaClip.make("examples/sample-audio.mp4", MediaKind.Audio)
      val proj = Project.blank.copy(
        bin = List(vid, aud),
        tracks = List(
          Track("v1", "V1", MediaKind.Video, List(
            PlacedClip.make(vid.id, 0, 120),
            PlacedClip.make(vid.id, 180, 120), // a 60-frame gap before this one shows the black base
          )),
          Track("a1", "A1", MediaKind.Audio, List(PlacedClip.make(aud.id, 0, 300))),
        ),
        lowerThirds = List(LowerThird("t", "Fixed tracks", "sequenced clips + audio", 10, 120, styleId = "broadcast-blue")),
      )
      val f = sys.env.get("KUTTER_PROBE_NLE").flatMap(_.toIntOption).getOrElse(40)
      Player.probe(proj, f, "probe-nle.png")
      Mlt.close()
      println(s"fixed-track render (frame $f) wrote probe-nle.png")
      return true

    false
