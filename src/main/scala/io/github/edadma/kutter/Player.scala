package io.github.edadma.kutter

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

import io.github.edadma.mlt.*
import io.github.edadma.suit.{UiThread, VideoTexture}
import io.github.edadma.sdl3.{AudioStream, initAudio, openAudioStream}
import io.github.edadma.logger.LoggerFactory

// The audio channel layout kutter asks MLT to resample every clip to: stereo float32, which is what
// SDL's audio stream takes. A byte-frame (one sample across both channels) is 8 bytes. The sample
// *rate* is per-project (the timeline spec's `audioRate`), not fixed here — resampling is transparent,
// so it is a setting rather than a constant.
private val AudioChannels       = 2
private val AudioBytesPerSample = AudioChannels * 4

private val MaxBufferedFrames = 24 // safety cap on decoded frames held ahead, so a stall can't grow it unbounded

// The filmstrip the timeline shows: a spread of small 16:9 thumbnails decoded once when a video track opens.
private val ThumbCount  = 24
private val ThumbWidth  = 160
private val ThumbHeight = 90

// The playback engine: an MLT graph decoded on a thread of its own, feeding a suit video layer and
// an SDL audio stream.
//
// The division of labour is the one the MLT binding was measured to want. A bare consumer renders
// synchronously on whatever thread pulls it and spawns no MLT threads, so the decode thread here is
// an ordinary JVM/Native thread the runtime's GC knows about. It pulls a frame, decodes its image
// (the expensive part) off the UI thread, hands the ready pixel planes to the UI thread through
// `UiThread.post` — the only cross-thread surface suit exposes — for the GPU upload, and pushes the
// frame's audio into the SDL stream.
//
// **Audio is the master clock.** Each MLT frame carries both its picture and its audio; the decode
// thread pushes the audio and then shows the picture only once the device has played up to that
// frame's audio, so video follows sound rather than a wall-clock timer that would drift. A timeline
// with no audio track falls back to pacing by the frame duration.
//
// **Every MLT call stays on the decode thread**, including closing the frame: an MLT graph shares a
// frame cache across the frames a producer yields, and closing a frame on the UI thread while this
// thread pulls the next one races that cache and corrupts it (a segfault in `mlt_cache_item_close`).
// The audio buffer and pixel planes are borrowed from the frame; the frame is closed only after both
// have been consumed. SDL's audio functions are thread-safe, so play/pause toggle the stream from
// the UI thread while the decode thread fills it.
final class Player(
    profile:  Profile,
    audioRate: Int,
    initialGraph:     Producer,
    consumer: Consumer,
    texture:  VideoTexture,
    audio:    AudioStream,
    thumbCache: Map[String, Thumbnails],
    waveCache:  Map[String, Waveform],
    initialVolumeFilters: Map[String, Filter],
    initialCloseGraph: () => Unit,
):
  private val log = LoggerFactory.getLogger

  // How much audio (per channel) to keep buffered ahead of the picture at the device — a quarter
  // second at the project's sample rate. The decoder runs ahead to maintain this reserve, and each
  // decoded picture is shown when its audio actually reaches the speakers, so the reserve deepens the
  // buffer without pushing sound out of sync with the picture. It is what lets a GC pause or a heavy
  // render frame pass without starving the device (the cause of clicks/pulsing on a shallow one-frame
  // buffer). Audio is the clock: a dropped or repeated picture goes unnoticed, but a gap is heard.
  private val audioLeadSamples = audioRate / 4

  // The graph the consumer pulls and the teardown that frees it. They are vars, not constructor vals,
  // because editing the timeline (a lower third, a clip's position) rebuilds the graph in place (see
  // `update` / `swapGraph`): the decode thread swaps in a freshly compiled tractor and frees the old
  // one. Only the decode thread touches them after construction. The generator caches, by contrast, are
  // fixed for a player's life — importing or removing a source re-opens the player, since each generator
  // owns a background thread; moving a clip does not, so its generators carry over the live swap.
  private var graph:        Producer   = initialGraph
  private var closeGraph:   () => Unit = initialCloseGraph

  // The audio tracks' `volume` filters, keyed by track id, so the mixer can ride a track's gain live —
  // setting the filter's `gain` property on the running graph rather than rebuilding it, which would
  // recreate every producer and reseek and so stutter playback. Every audio track carries one (even at
  // unity), so a fader always has a handle. Swapped in with the graph; touched only by the decode thread.
  private var volumeFilters: Map[String, Filter] = initialVolumeFilters

  // Track-gain changes parked by the UI thread for the decode thread to apply between frames — the same
  // hand-off shape as `pendingGraph`, but a live property tweak with no rebuild. A later change to a
  // track supersedes an earlier unapplied one; `gainLock` guards the map.
  @volatile private var pendingGains: Map[String, Double] = Map.empty
  private val gainLock = new AnyRef

  /** The filmstrip generator for the video source at `path`, or null when none was opened for it — the
    * timeline looks a clip block's strip up by source, so several placements of one clip share it. */
  def thumbsFor(path: String): Thumbnails | Null = thumbCache.getOrElse(path, null)

  /** The waveform generator for the audio source at `path`, or null when none was opened for it. */
  def waveFor(path: String): Waveform | Null = waveCache.getOrElse(path, null)

  // The flags the UI thread and the decode thread share. The UI thread sets `playing` (play/pause)
  // and `stopping` (teardown) and `seekTo` (scrub); the decode thread reads them and owns every MLT
  // call. `ended` records that the timeline played out — set by the decode thread at the end, cleared
  // by it on the next rewind.
  @volatile private var playing  = false
  @volatile private var stopping = false
  @volatile private var ended    = false
  @volatile private var seekTo: Int = -1

  // A project edited in the UI, waiting for the decode thread to recompile the graph. The UI thread
  // builds the whole new graph on the UI thread (in `update`) and parks it here; the decode thread only
  // swaps it in at a safe point between frames. MLT *frame* work belongs to the decode thread, but MLT
  // *producer creation* must run on the main thread — its loader reaches macOS APIs that throw an
  // NSException off the main thread — so the build cannot happen on the decode thread. A newer edit
  // supersedes an unswapped one, whose graph is freed so a burst of edits does not leak; `swapLock`
  // guards the hand-off.
  private case class PendingGraph(graph: Tractor, volumeFilters: Map[String, Filter], close: () => Unit)
  @volatile private var pendingGraph: PendingGraph | Null = null
  private val swapLock = new AnyRef

  // The playback position the UI reads for its scrubber; the decode thread writes it. Total length
  // and frame rate are captured on the UI thread before the decode thread exists.
  @volatile private var pos: Int = 0

  // Audio-clock bookkeeping, touched only by the decode thread: the running count of audio samples
  // (per channel) pushed to the stream since the last rewind, and whether the timeline has audio at all.
  private var pushedSamples: Long = 0L
  private var hasAudio            = false

  // Decoded frames waiting to be shown, each tagged with the sample position where its audio begins. The
  // decoder runs ahead of the picture to keep `AudioLeadSamples` buffered at the device; a frame is shown
  // once the device has played up to its audio, keeping picture and sound in step. `reachedEnd` records
  // that the timeline ran out while filling, so the queue is drained before playback truly stops. Touched
  // only by the decode thread.
  private final case class PendingFrame(frame: Frame, startSample: Long)
  private val displayQueue = scala.collection.mutable.Queue.empty[PendingFrame]
  private var reachedEnd   = false

  /** Total number of frames on the timeline. */
  val totalFrames: Int = math.max(1, graph.length)

  /** The timeline's frame rate — the profile's. */
  val fps: Double = profile.fps

  /** Apply an edited project. Must be called on the UI/main thread: the new graph is built here, so
    * MLT producer creation stays on the main thread; the decode thread then swaps it in between frames
    * and reseeks so the change shows at once, playing or paused. A burst of edits supersedes earlier
    * unswapped graphs, which are freed.
    *
    * This is the path for edits that leave the tracks' clips unchanged (the lower thirds, per-track
    * volume). Adding or removing a clip on a track changes the lanes, so the UI re-opens the player. */
  def update(project: Project): Unit =
    val built = Player.buildGraph(profile, project)
    swapLock.synchronized {
      pendingGraph match
        case p: PendingGraph => p.close()
        case null            => ()
      pendingGraph = PendingGraph(built.tractor, built.volumeFilters, built.close)
    }

  /** Ride an audio track's fader live: park the new linear gain for the decode thread to apply to the
    * track's `volume` filter between frames. No graph rebuild, so playback is not interrupted — the
    * change is heard on the next frame. Silently ignores a track with no volume filter (a video track).
    * Safe to call from the UI thread. */
  def setTrackGain(trackId: String, gain: Double): Unit =
    gainLock.synchronized { pendingGains = pendingGains + (trackId -> gain) }

  /** The playback position to show. Normally the frame most recently shown (0 .. totalFrames-1); it
    * snaps to the full length once the timeline has ended (so the readout lands on 0:20 / 0:20), and
    * reports a *pending* seek target the instant one is requested — before the decode thread has
    * serviced it — so a backward scrub doesn't bounce back for a frame. */
  def position: Int =
    if seekTo >= 0 then seekTo
    else if ended then totalFrames
    else pos

  /** Request a seek to `frame` (clamped). Only sets a flag — the decode thread performs the MLT seek
    * on its next iteration and refreshes the preview to that frame, even while paused. */
  def seek(frame: Int): Unit = seekTo = math.max(0, math.min(totalFrames - 1, frame))

  /** Called (on the UI thread) when the timeline reaches its end and playback stops on its own. */
  @volatile var onEnded: () => Unit = () => ()

  // For a timeline with no audio there is no clock to pace against, so fall back to the frame
  // duration — the coarse wall-clock pacing a silent preview used before audio existed.
  private val frameMillis: Long = math.max(1L, math.round(1000.0 / profile.fps))

  private var thread: Thread | Null = null

  /** Spawn the decode thread and begin paused. */
  def start(): Unit =
    val t = new Thread(() => decodeLoop(), "kutter-decoder")
    t.setDaemon(true)
    thread = t
    t.start()

  /** Resume (or restart) playback. Flips the flag and resumes the audio device; the decode thread
    * does the graph work, including rewinding a played-out timeline. */
  def play(): Unit =
    playing = true
    audio.resume()

  /** Pause playback — parks the decode thread and silences the audio device. */
  def pause(): Unit =
    playing = false
    audio.pause()

  def isPlaying: Boolean = playing

  /** Set the master playback volume: 0 is silent, 1 leaves the audio unchanged. It drives the audio
    * device's gain, so it takes effect at once and costs nothing per sample — the project's master
    * fader. Safe from the UI thread — SDL's audio functions are thread-safe. */
  def setVolume(v: Double): Unit = audio.gain = math.max(0.0, math.min(1.0, v)).toFloat

  /** Stop the decode thread, wait for it to finish, and tear the graph and audio device down. */
  def close(): Unit =
    stopping = true
    thread match
      case t: Thread => t.join()
      case null      => ()
    flushQueue() // close any decoded frames still queued for display
    thumbCache.values.foreach(_.close())
    waveCache.values.foreach(_.close())
    audio.pause()
    audio.destroy()
    consumer.stop()
    consumer.close()
    closeGraph()
    profile.close()

  private def decodeLoop(): Unit =
    log.debug("decode thread started", category = "player")
    while !stopping do
      val pending = swapLock.synchronized {
        val p = pendingGraph
        pendingGraph = null
        p
      }
      pending match
        case p: PendingGraph => swapGraph(p)
        case null            => ()

      // Apply any parked track-gain changes to the running graph's volume filters — a live property
      // tweak, no reseek, so riding a fader does not interrupt the picture or the sound.
      val gains = gainLock.synchronized {
        val g = pendingGains
        pendingGains = Map.empty
        g
      }
      if gains.nonEmpty then
        for (id, gain) <- gains do volumeFilters.get(id).foreach(f => Player.applyGain(f, gain))

      val target = seekTo
      if target >= 0 then
        // A scrub: seek here (all MLT stays on this thread) and refresh the preview to the target
        // even while paused. No audio is pushed while scrubbing.
        seekTo = -1
        rewindTo(target)
        renderOneFrame()
      else if !playing then Thread.sleep(15)
      else
        // Resuming after the timeline played out restarts from the beginning.
        if ended then
          log.debug("restarting from the beginning", category = "player")
          rewindTo(0)

        if !pumpPlayback() then
          // End of timeline: stop and let the UI flip its control to Play. The audio device is left
          // running so the last buffered samples drain rather than cut off.
          log.debug(s"reached end of timeline at pos $pos; stopping", category = "player")
          ended   = true
          playing = false
          UiThread.post { () => onEnded() }
    log.debug("decode thread exited", category = "player")

  /** Drop every decoded-but-unshown frame (closing each) — the queued frames are stale after a seek or a
    * graph swap. Decode-thread only. */
  private def flushQueue(): Unit =
    while displayQueue.nonEmpty do displayQueue.dequeue().frame.close()
    reachedEnd = false

  /** Seek the graph to `frame`, restore play speed, drop the buffered frames, flush the stale audio, and
    * reset the audio clock. Decode-thread only — every MLT call in the player lives here. */
  private def rewindTo(frame: Int): Unit =
    flushQueue()
    graph.seek(frame)
    graph.speed = 1.0
    consumer.purge()
    ended = false
    audio.clear()
    pushedSamples = 0L

  /** Swap in a graph already built on the UI thread (see `update` and the `pendingGraph` note) under
    * the running consumer. Decode-thread only, and it creates no MLT producers — the build happened on
    * the main thread. The consumer is re-pointed at the fresh tractor *before* the old one is freed, so
    * it never reads through a dangling graph; then the position is restored so the edit is visible on
    * the same frame. Freeing the old graph is a close, not a load, so it is safe off the main thread. */
  private def swapGraph(pending: PendingGraph): Unit =
    val savedPos = if seekTo >= 0 then seekTo else pos
    log.debug(s"swapping in edited graph at pos $savedPos", category = "player")

    consumer.connect(pending.graph) // attach the consumer to the new graph first
    val oldClose = closeGraph
    graph         = pending.graph
    volumeFilters = pending.volumeFilters // the fresh graph's filters become the live gain handles
    closeGraph    = pending.close
    oldClose() // now nothing points at the old graph, so free it

    // Restore the position and repaint that frame at once, so an edit lands whether playing or paused.
    rewindTo(savedPos)
    renderOneFrame()

  /** Upload a decoded frame's planes to the video texture on the UI thread, wait for the upload to
    * finish (the borrowed planes must outlive it), then close the frame. Decode-thread only. */
  private def uploadFrame(frame: Frame): Unit =
    val planes = frame.imagePlanes()
    val y      = planes.planes(0)
    val u      = planes.planes(1)
    val v      = planes.planes(2)
    val uploaded = new CountDownLatch(1)
    UiThread.post { () =>
      texture.update(y.data, y.stride, u.data, u.stride, v.data, v.stride)
      uploaded.countDown()
    }
    while !uploaded.await(200, TimeUnit.MILLISECONDS) && !stopping do ()
    frame.close()

  /** Pull one frame and show it immediately, with no audio — a scrub or post-swap refresh. Returns
    * false at end of stream. */
  private def renderOneFrame(): Boolean =
    consumer.rtFrame() match
      case None =>
        Thread.sleep(15) // MLT produced nothing at all; caller will loop
        true
      case Some(frame) =>
        if frame.speed == 0 then
          frame.close()
          false
        else
          pos = frame.position
          uploadFrame(frame)
          true

  /** One turn of playback. The decoder runs ahead to keep `audioLeadSamples` buffered at the device, then
    * every decoded frame whose audio has reached the speakers is shown. Audio is the clock: the picture
    * follows it, and the buffered reserve absorbs a GC pause or a heavy render frame that would otherwise
    * starve the device and click. Returns false once the timeline has ended and every buffered frame has
    * been shown. Decode-thread only. */
  private def pumpPlayback(): Boolean =
    // Fill: decode ahead until the reserve is deep enough (or the queue cap, the end, or an interruption).
    var filling = true
    while filling && !reachedEnd && !stopping && playing && seekTo < 0 &&
      displayQueue.size < MaxBufferedFrames && (pushedSamples - audioPlayed()) < audioLeadSamples do
      consumer.rtFrame() match
        case None => filling = false // MLT produced nothing right now; stop filling this pass
        case Some(f) =>
          if f.speed == 0 then
            f.close()
            reachedEnd = true // end of timeline; drain what is queued, then stop
          else
            val a           = f.audio(fps, audioRate, AudioChannels)
            val startSample = pushedSamples
            if a.samples > 0 then
              if !hasAudio then
                log.info(s"audio: ${a.frequency}Hz ${a.channels}ch, ${a.samples} samples/frame", category = "player")
              hasAudio = true
              audio.put(a.buffer, a.samples * AudioBytesPerSample)
              pushedSamples += a.samples
            displayQueue.enqueue(PendingFrame(f, startSample))

    // Show: with audio, every frame whose sound has started playing; silent, one frame paced by its
    // duration (there is no clock to follow).
    if hasAudio then
      while displayQueue.nonEmpty && seekTo < 0 && !stopping && audioPlayed() >= displayQueue.head.startSample do
        val pf = displayQueue.dequeue()
        pos = pf.frame.position
        uploadFrame(pf.frame)
      Thread.sleep(2)
    else if displayQueue.nonEmpty then
      val pf = displayQueue.dequeue()
      pos = pf.frame.position
      uploadFrame(pf.frame)
      Thread.sleep(frameMillis)
    else Thread.sleep(2)

    // Truly done only once the end was reached and every buffered frame has been shown.
    !(reachedEnd && displayQueue.isEmpty)

  /** Samples (per channel) the audio device has actually played since the last rewind: everything
    * pushed, less what is still queued ahead of the device. This is the master clock. */
  private def audioPlayed(): Long =
    pushedSamples - audio.queued.toLong / AudioBytesPerSample

object Player:

  // The timeline length (frames at the 30fps profile) an empty timeline's black base is sized to when
  // the lower thirds don't reach further — 10 seconds, matching the UI's empty-timeline default.
  private val DefaultLength = 300

  /** What `buildGraph` produces: the tractor to play or export, the timeline length in frames, and a
    * teardown that frees every piece in order. */
  private final case class BuildResult(
      tractor:       Tractor,
      length:        Int,
      volumeFilters: Map[String, Filter], // an audio track's `volume` filter, keyed by track id, for live gain
      close:         () => Unit,
  )

  /** The MLT profile a `spec` renders against — a custom, square-pixel, progressive profile at its
    * exact geometry and rational frame rate. Every place that builds a graph for a project (play,
    * export, a length probe) goes through this, so one project renders against one profile everywhere. */
  private[kutter] def profileFor(spec: TimelineSpec): Profile =
    Profile.custom(spec.width, spec.height, spec.fpsNum, spec.fpsDen)

  /** The native video format of the source at `path`, as a `TimelineSpec` — its own resolution and
    * frame rate, read by letting a throwaway profile adopt the producer's format. This is what a fresh
    * project auto-adopts from its first clip, so the timeline matches the footage without the user
    * choosing. Creates a producer, so it must run on the main thread and after `Mlt.init()`. The
    * audio rate is left at the default (resampling is transparent, so it is not adopted). */
  def probeSpec(path: String): TimelineSpec =
    val profile = Profile.default
    val p       = Producer(profile, path)
    profile.adoptFormatOf(p)
    val spec =
      TimelineSpec(profile.width, profile.height, profile.frameRateNum, math.max(1, profile.frameRateDen))
    p.close()
    profile.close()
    spec

  /** Open `project` and build a paused player, the video texture that shows it, and the audio stream
    * that voices it. Runs on the UI thread (the texture needs the runtime's renderer, and the audio
    * device is brought up here after SDL is initialised).
    *
    * The project renders against its own timeline spec (`profileFor`): MLT normalises every producer's
    * output to that resolution, frame rate, and — resampled — audio rate, so differently shaped sources
    * are conformed and still play.
    *
    * Playback runs a **tractor** — a multitrack timeline that is itself a producer, so the decode
    * loop, seeking, and A/V sync are unchanged. Each project track is a playlist sequencing its clips;
    * video tracks composite onto the base, audio tracks mix, and each lower third rides an overlay
    * track of its own, a full-frame card composited on top and faded in and out over its window. */
  def open(project: Project): (Player, VideoTexture) =
    val log     = LoggerFactory.getLogger
    val spec    = project.spec
    val profile = profileFor(spec)

    val built = buildGraph(profile, project)

    val consumer = Consumer.bare(profile)
    consumer.connect(built.tractor)
    consumer.start()

    log.info(
      s"opened project: ${built.length} frames @ ${profile.fps}fps, ${profile.width}x${profile.height} " +
        s"${profile.colorspace.name}, ${project.videoTracks.length} video / ${project.audioTracks.length} audio " +
        s"track(s), ${project.lowerThirds.length} lower third(s)",
      category = "player",
    )

    val texture = VideoTexture(
      profile.width,
      profile.height,
      io.github.edadma.suit.VideoFormat.I420,
      suitColorspace(profile.colorspace),
    )

    initAudio()
    val audio = openAudioStream(spec.audioRate, AudioChannels)
    log.info(
      s"audio device ${if audio.isNull then "FAILED to open" else "open"} @ ${spec.audioRate}Hz ${AudioChannels}ch",
      category = "player",
    )

    // A background generator per source clip, each on its own graph and thread so none touches the
    // playback decode thread: a filmstrip for every video source placed on a video track, a waveform for
    // every audio source placed on an audio track. Keyed by source path, so several placements of the
    // same clip share one generator; the timeline looks a clip block's strip or envelope up by source.
    // A source that is a linked A/V pair gets both — a filmstrip for its picture on V1 and a waveform for
    // its sound on A1. Generators fill in shortly after the window appears; the timeline reads them as
    // they are made.
    val thumbCache = scala.collection.mutable.LinkedHashMap.empty[String, Thumbnails]
    for track <- project.videoTracks; pc <- track.ordered; clip <- project.clipFor(pc.clipId) do
      if !thumbCache.contains(clip.path) then
        val t = new Thumbnails(ThumbCount, ThumbWidth, ThumbHeight)
        t.start(spec, clip.path)
        thumbCache(clip.path) = t

    // A waveform spans the whole source (like the filmstrip), so a trimmed placement can show exactly
    // the slice it plays. Its peak count is the source length when known, falling back to the placement's
    // length for a clip from a project saved before source lengths were measured.
    val waveCache = scala.collection.mutable.LinkedHashMap.empty[String, Waveform]
    for track <- project.audioTracks; pc <- track.ordered; clip <- project.clipFor(pc.clipId) do
      if !waveCache.contains(clip.path) then
        val w = new Waveform(math.max(1, if clip.frames > 0 then clip.frames else pc.length))
        w.start(spec, clip.path)
        waveCache(clip.path) = w

    val player =
      new Player(profile, spec.audioRate, built.tractor, consumer, texture, audio, thumbCache.toMap,
        waveCache.toMap, built.volumeFilters, built.close)
    player.setVolume(project.master) // the project's master fader drives the audio device gain
    (player, texture)

  /** Set an audio track's `volume` filter to a linear `gain`, the way kdenlive's mixer does: at unity the
    * filter is disabled (bypassed — MLT's volume filter is not bit-transparent even at 0 dB, so running it
    * as a no-op pulses steady content); otherwise it is enabled and its `level` set in dB. Used both when
    * building the graph and when riding a fader live between frames. */
  private[kutter] def applyGain(vol: Filter, gain: Double): Unit =
    if gain == 1.0 then vol.setInt("disable", 1)
    else
      vol.setInt("disable", 0)
      vol.set("level", Mixer.levelDb(gain).toString)

  /** Build the multitrack graph for `project`, and return the tractor to play, the timeline length,
    * and a teardown that closes every piece in order.
    *
    * Track 0 is a solid black background sized to the whole timeline: every layer folds onto it (a_track
    * 0, never chained — see the note by the transition-planting below), so an empty stretch of the
    * timeline shows black and the base is always a moving frame the still overlays can composite over.
    *
    *   - each **video track** is a playlist sequencing its clips (blanks fill the gaps), composited
    *     full-frame onto the base; video tracks composite in order, so a higher track shows over a lower
    *     one where they overlap and the lower shows through the higher's gaps;
    *   - each **audio track** is a playlist that mixes its sound into the base, its `volume` filter
    *     applying the track's fader (silenced when the track is muted);
    *   - each **lower third** composites its card onto the base, faded in and out over its window by the
    *     card's own animated alpha.
    *
    * A mix transition on every track carries its audio through to the tractor's output. The project's
    * master level is applied at the audio device (see `setVolume`), not in the graph.
    *
    * **Every `Producer` here is created on the calling thread, which must be the main thread** — MLT's
    * producer loader reaches macOS APIs that throw off the main thread (see `update`). */
  private def buildGraph(profile: Profile, project: Project): BuildResult =
    import scala.collection.mutable.ListBuffer

    val ltExtent = project.lowerThirds.map(_.outFrame + 1).maxOption.getOrElse(0)
    val extent   = project.contentEnd
    val length =
      math.max(1, if extent > 0 then math.max(extent, ltExtent) else math.max(DefaultLength, ltExtent))

    // Everything created here, kept for teardown in reverse order of dependence.
    val clipProds = ListBuffer.empty[Producer]
    val playlists = ListBuffer.empty[Playlist]
    val filters   = ListBuffer.empty[Filter]
    val cardProds = ListBuffer.empty[Producer]

    // The audio tracks' volume filters, keyed by track id, so the player can ride each track's gain live.
    val volFilters = scala.collection.mutable.Map.empty[String, Filter]

    // Track 0: the black base every layer folds onto. It is otherwise unbounded, so its out-point
    // bounds it to the timeline.
    val black = Producer(profile, "color:black")
    black.setInAndOut(0, length - 1)
    val baseTrack = Playlist(profile)
    baseTrack.append(black)

    val tractor = Tractor(profile)
    tractor.setTrack(baseTrack, 0)

    // Compile one project track into a playlist: its clips laid out in timeline order with blanks
    // filling the gaps between them, and the track's fader applied as a `volume` filter when it is not
    // unity (or the track is muted). A clip whose source is missing from the bin is skipped.
    //
    // A track's producers contribute only what the track is for: an **audio** track disables its
    // producers' video (`video_index = -1`), and a **video** track disables their audio
    // (`audio_index = -1`). So a video-with-sound clip, placed as a linked pair (picture on a video
    // track, sound on an audio track), plays its audio exactly once — from the audio track — while the
    // video track carries the picture. All sound comes from audio tracks; video tracks are silent.
    def compileTrack(track: Track): Playlist =
      val pl     = Playlist(profile)
      var cursor = 0
      for pc <- track.ordered do
        project.clipFor(pc.clipId).foreach { clip =>
          if pc.timelineStart > cursor then pl.blank(pc.timelineStart - cursor)
          val prod = Producer(profile, clip.path)
          if track.kind == MediaKind.Audio then prod.setInt("video_index", -1)
          else prod.setInt("audio_index", -1)
          prod.setInAndOut(pc.inPoint, pc.inPoint + pc.length - 1)
          pl.append(prod)
          clipProds += prod
          cursor = pc.timelineEnd
        }
      // Every audio track carries a `volume` filter so the mixer has a live handle to ride its gain
      // without rebuilding the graph (see `setTrackGain`); video tracks are silent, so none.
      //
      // The gain is driven the way kdenlive's mixer does it: the modern `level` property (in dB), and —
      // crucially — the filter is **disabled at unity**. MLT's normalize-based volume filter is not
      // bit-transparent even at 0 dB (its per-frame processing pulses steady content), so at unity the
      // filter is bypassed outright (`disable = 1`) rather than run as a no-op; a fader move re-enables it
      // live. The limiter (default -6dBFS) is pushed out of the way so an enabled filter is a clean gain.
      if track.kind == MediaKind.Audio then
        val vol = Filter(profile, "volume")
        vol.set("limiter", "60dBFS")
        applyGain(vol, track.gain)
        pl.attach(vol)
        filters += vol
        volFilters(track.id) = vol
      pl

    // Layers above the base, each on its own track. `comp`/`mix` are collected and planted after every
    // track is set, so the planting order is uniform (all composites, then all mixes).
    final case class Layer(track: Int, comp: Transition | Null, mix: Transition | Null)
    val layers    = ListBuffer.empty[Layer]
    var nextTrack = 1

    def composite(): Transition =
      val t = Transition(profile, "composite")
      t.alwaysActive = true
      t.set("geometry", "0%/0%:100%x100%")
      t

    def mixer(): Transition =
      val t = Transition(profile, "mix")
      t.alwaysActive = true
      t

    // Video tracks first (composited, bottom-to-top so a higher track wins), then audio tracks (mixed).
    // Video tracks are silent (their audio is disabled above), so they carry no mix — only a composite.
    for track <- project.videoTracks do
      val pl    = compileTrack(track)
      val slot  = nextTrack; nextTrack += 1
      playlists += pl
      tractor.setTrack(pl, slot)
      layers += Layer(slot, composite(), null)

    for track <- project.audioTracks do
      val pl    = compileTrack(track)
      val slot  = nextTrack; nextTrack += 1
      playlists += pl
      tractor.setTrack(pl, slot)
      layers += Layer(slot, null, mixer())

    // Lower thirds: a still card composited onto the base, faded by its own alpha. The fade rides the
    // card's alpha, not the composite's mix — a brightness filter ramps transparency in over the fade,
    // holds it opaque, ramps it out, and holds it fully transparent outside the window, so the same
    // animation both fades the card and defines when it shows. MLT interpolates the property animation
    // smoothly; the always-active composite's mix does not (the card would pop in at full opacity).
    for lt0 <- project.lowerThirds do
      val lt       = clampLt(lt0, length)
      val slot     = nextTrack; nextTrack += 1
      val cardPath = CardRenderer.renderCard(lt, project.styleFor(lt.styleId), profile.width, profile.height)
      val card     = Producer(profile, cardPath)
      card.setInAndOut(0, length - 1) // stretch the still card across the whole timeline
      val fade = Filter(profile, "brightness")
      fade.set("alpha", alphaFade(lt))
      card.attach(fade)
      tractor.setTrack(card, slot)
      filters += fade
      cardProds += card
      layers += Layer(slot, composite(), null)

    // Every layer composites/mixes directly onto the base (track 0), never chained through the track
    // beneath it. Chaining breaks for still-image overlays: a still's position never advances, so a
    // composite reading one as its lower input freezes the whole picture on a single frame. Folding
    // each layer onto track 0 keeps the moving base as every transition's lower input; the results
    // accumulate on track 0, which is what the tractor outputs.
    for l <- layers do
      l.comp match
        case c: Transition => tractor.plantTransition(c, 0, l.track)
        case null          => ()
    for l <- layers do
      l.mix match
        case m: Transition => tractor.plantTransition(m, 0, l.track)
        case null          => ()
    tractor.refresh()

    val closeGraph = () => {
      layers.foreach { l =>
        l.comp match { case c: Transition => c.close(); case null => () }
        l.mix match { case m: Transition => m.close(); case null => () }
      }
      filters.foreach(_.close())
      cardProds.foreach(_.close())
      clipProds.foreach(_.close())
      playlists.foreach(_.close())
      black.close()
      tractor.close()
      baseTrack.close()
    }

    BuildResult(tractor, length, volFilters.toMap, closeGraph)

  /** Render `project`'s audio through the real playback graph to a WAV at `out` — the exact samples the
    * decode thread would feed the audio device, captured to a file for offline analysis. Runs the graph
    * to its end with a non-realtime avformat consumer (float PCM, 48kHz stereo), so a steady tone in the
    * source comes out as whatever the tractor's mix actually produces. A debug tool, not a playback path. */
  def renderAudio(project: Project, out: String): Unit =
    val profile  = profileFor(project.spec)
    val built    = buildGraph(profile, project)
    val consumer = Consumer(profile, "avformat", Some(out))
    consumer.realTime        = 0    // process as fast as possible, drop nothing
    consumer.terminateOnPause = true // stop when the timeline ends (base track's speed hits 0)
    consumer.set("acodec", "pcm_f32le") // lossless float PCM so the envelope is exact
    consumer.set("frequency", project.spec.audioRate.toString)
    consumer.set("channels", AudioChannels.toString)
    consumer.set("vn", "1")             // audio only
    consumer.connect(built.tractor)
    consumer.start()
    while !consumer.isStopped do Thread.sleep(20)
    consumer.stop()
    consumer.close()
    built.close()
    profile.close()

  /** A running video export: an avformat consumer encoding a project's graph to a file. The consumer
    * spawns its own MLT worker threads, so once `startRender` has kicked it off nothing blocks — the
    * caller polls `position`/`isDone` and calls `finish` when it is done (or to cancel). Progress is a
    * frame count against `totalFrames`. */
  final class RenderJob private[Player] (profile: Profile, consumer: Consumer, built: BuildResult):
    /** The total number of frames the export writes — the timeline length. */
    val totalFrames: Int = built.length

    /** The number of frames written so far, clamped to the total. */
    def position: Int = math.max(0, math.min(totalFrames, consumer.position))

    /** Whether the encode has finished (the timeline reached its end) or otherwise stopped. */
    def isDone: Boolean = consumer.isStopped

    /** Stop encoding if it is still running and free the graph, consumer, and profile. Call once, when
      * `isDone` is true or to cancel a still-running export. */
    def finish(): Unit =
      consumer.stop()
      consumer.close()
      built.close()
      profile.close()

  /** Start exporting `project` to the video file `out`, encoded H.264/AAC in the container the path's
    * extension names, against the project's own timeline spec. Returns at once with a [[RenderJob]] to
    * poll — the encode runs on MLT's worker threads. **Must run on the main thread**: it builds the
    * graph, and MLT producer creation is main-thread-only. A constant-quality (CRF 20) encode, a
    * sensible default; explicit bitrate/codec choices can layer on later. */
  def startRender(project: Project, out: String): RenderJob =
    val profile  = profileFor(project.spec)
    val built    = buildGraph(profile, project)
    val consumer = Consumer(profile, "avformat", Some(out))
    consumer.realTime         = 0    // encode as fast as the machine allows, dropping nothing
    consumer.terminateOnPause = true // stop when the base track's speed hits 0 (the timeline's end)
    consumer.set("vcodec", "libx264")
    consumer.set("crf", "20")              // constant quality — visually near-lossless, sensible size
    consumer.set("acodec", "aac")
    consumer.set("ab", "192k")             // audio bitrate
    consumer.set("frequency", project.spec.audioRate.toString)
    consumer.set("channels", AudioChannels.toString)
    consumer.set("movflags", "+faststart") // web-friendly: index at the front so it streams before fully downloaded
    consumer.connect(built.tractor)
    consumer.start()
    new RenderJob(profile, consumer, built)

  /** The length in *timeline* frames a clip at `path` runs to, measured against `spec`'s profile — what
    * a full-length placement of a freshly imported clip is sized to. Because the measure is against the
    * timeline's own frame rate (not the source's), a 24 fps clip on a 30 fps timeline reports its length
    * in 30 fps frames, which is exactly what the model wants: every placement length is in timeline
    * frames. Creates a producer, so it must run on the main thread (the loader reaches macOS APIs that
    * throw off it) and after `Mlt.init()`. */
  def mediaLength(path: String, spec: TimelineSpec): Int =
    val profile = profileFor(spec)
    val p       = Producer(profile, path)
    val len     = math.max(1, p.length)
    p.close()
    profile.close()
    len

  /** Render one composited frame of `project`'s graph to a PNG at `out` — the sequenced tracks with
    * their lower thirds folded in at frame `frame`. A synchronous, windowless render used to check the
    * multitrack pipeline off the GUI, and the seed of a still-export path. */
  def probe(project: Project, frame: Int, out: String): Unit =
    import io.github.edadma.libcairo.{Format, imageSurfaceCreateForData}
    val profile = profileFor(project.spec)
    val built   = buildGraph(profile, project)
    val consumer = Consumer.bare(profile)
    consumer.connect(built.tractor)
    consumer.start()
    built.tractor.seek(math.max(0, math.min(built.length - 1, frame)))
    consumer.rtFrame() match
      case Some(f) =>
        val (buf, w, h, _) = f.imagePtr(ImageFormat.Rgba)
        rgbaToArgb32(buf, w * h)
        val surface = imageSurfaceCreateForData(buf, Format.ARGB32, w, h, w * 4)
        surface.writeToPNG(out)
        surface.destroy()
        f.close()
      case None => ()
    consumer.stop()
    consumer.close()
    built.close()
    profile.close()

  /** Exercise the live-edit graph swap off the GUI: build `before`'s graph under a bare consumer,
    * render a frame, then do exactly what `swapGraph` does — reconnect the consumer to `after`'s freshly
    * built graph and free the old one under it — and render frame `frame` of the edited graph to `out`.
    * A crash here would mean the consumer read through a freed graph; a correct PNG proves the swap is
    * safe. */
  def probeRebuild(before: Project, after: Project, frame: Int, out: String): Unit =
    import io.github.edadma.libcairo.{Format, imageSurfaceCreateForData}
    val profile = profileFor(before.spec)

    val builtA   = buildGraph(profile, before)
    val consumer = Consumer.bare(profile)
    consumer.connect(builtA.tractor)
    consumer.start()
    consumer.rtFrame().foreach(_.close()) // pull one frame through the original graph

    // The swap, as `swapGraph` does it: attach to the new graph first, then free the old.
    val builtB = buildGraph(profile, after)
    consumer.connect(builtB.tractor)
    builtA.close()

    builtB.tractor.seek(math.max(0, math.min(builtB.length - 1, frame)))
    consumer.purge()
    consumer.rtFrame() match
      case Some(f) =>
        val (buf, w, h, _) = f.imagePtr(ImageFormat.Rgba)
        rgbaToArgb32(buf, w * h)
        val surface = imageSurfaceCreateForData(buf, Format.ARGB32, w, h, w * 4)
        surface.writeToPNG(out)
        surface.destroy()
        f.close()
      case None => ()

    consumer.stop()
    consumer.close()
    builtB.close()
    profile.close()

  /** Test helper for the crash fix: build both graphs on the main thread (where MLT producer creation
    * is safe), then do only the swap — re-point the consumer at the second graph and free the first —
    * on a spawned thread, to confirm the swap does nothing that must stay on the main thread. Returns
    * "OK", or the error. A hard crash (NSException) would mean the swap still loads a producer. */
  def probeSwapOffThread(before: Project, after: Project): String =
    val profile  = profileFor(before.spec)
    val built1   = buildGraph(profile, before)
    val built2   = buildGraph(profile, after)
    val consumer = Consumer.bare(profile)
    consumer.connect(built1.tractor)
    consumer.start()
    consumer.rtFrame().foreach(_.close())
    @volatile var res = "no result"
    val t = new Thread(() =>
      try
        consumer.connect(built2.tractor)
        built1.close()
        consumer.rtFrame().foreach(_.close())
        res = "OK"
      catch case e: Throwable => res = s"THREW ${e}",
    )
    t.start()
    t.join()
    consumer.stop()
    consumer.close()
    built2.close()
    profile.close()
    res

  /** Clamp a lower third's window to the timeline: its in-point stays on the timeline with room for
    * the smallest possible card after it, and its out-point stays past the in-point and on the
    * timeline. A window a caller gave in profile frames that overruns a short timeline is pulled in
    * rather than rejected. */
  private def clampLt(lt: LowerThird, length: Int): LowerThird =
    val in  = math.max(0, math.min(lt.inFrame, length - 2))
    val out = math.max(in + 2, math.min(lt.outFrame, length - 1))
    lt.copy(inFrame = in, outFrame = out)

  /** The alpha animation that fades a card in and out and hides it outside its window. The values are
    * the card's alpha (0 fully transparent … 1 fully opaque) at absolute timeline frames — the
    * brightness filter carrying this rides the still card, whose position is the timeline frame. It
    * ramps 0→1 over the fade at the in-point, holds opaque, ramps back to 0 at the out-point, and stays
    * transparent before and after (MLT holds the nearest keyframe outside the animation's range). MLT's
    * property animation interpolates between the keyframes, which is what makes the fade smooth. */
  private def alphaFade(lt: LowerThird): String =
    val f = lt.fade
    Seq(
      s"${lt.inFrame}=0",
      s"${lt.inFrame + f}=1",
      s"${lt.outFrame - f}=1",
      s"${lt.outFrame}=0",
    ).mkString(";")

  /** Collapse MLT's colorspace tag to the three suit fixes a video texture is created with. The SD
    * family (601/470/170/240) shares coefficients, so it all maps to BT601; HD is BT709, which is
    * also the fallback for an unspecified source. */
  private def suitColorspace(cs: Colorspace): io.github.edadma.suit.VideoColorspace =
    import io.github.edadma.suit.VideoColorspace
    cs.value match
      case 709                   => VideoColorspace.BT709
      case 601 | 470 | 170 | 240 => VideoColorspace.BT601
      case _                     => VideoColorspace.BT709
