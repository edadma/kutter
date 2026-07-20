package io.github.edadma.kutter

import java.io.{File, DataInputStream, DataOutputStream, BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import scala.scalanative.unsafe.*

import io.github.edadma.mlt.*
import io.github.edadma.logger.LoggerFactory

// A waveform overview for one clip's audio — the peak amplitude per frame, for an audio track's
// lane. Like the video filmstrip (see [[Thumbnails]]) it runs its OWN MLT graph on a background
// thread, so it never touches the playback decode thread's graph, and it caches its result so a
// reopen loads without decoding.
//
// It pulls the clip frame by frame and, for each, reduces that frame's audio to a single peak (the
// largest absolute sample across both channels). Only the audio is decoded — the image is never
// asked for — so the pass is far cheaper than the thumbnail decode.
final class Waveform(count: Int):
  private val log = LoggerFactory.getLogger

  private val peaks = new Array[Float](math.max(1, count))

  // Leading peaks generated so far; volatile with the same publish rule as the filmstrip's slots.
  @volatile private var made = 0

  @volatile private var stopping = false
  private var thread: Thread | Null = null

  /** The peak (0..1) at `fraction` of the clip, or 0 where not yet generated. */
  def at(fraction: Double): Float =
    val i = math.max(0, math.min(count - 1, (fraction * count).toInt))
    if i < made then peaks(i) else 0f

  /** Spawn the background generator: its own graph rendering `path` against the timeline `spec`. */
  def start(spec: TimelineSpec, path: String): Unit =
    val t = new Thread(() => generate(spec, path), "kutter-waveform")
    t.setDaemon(true)
    thread = t
    t.start()

  def close(): Unit =
    stopping = true
    thread match
      case t: Thread => t.join()
      case null      => ()

  private def generate(spec: TimelineSpec, path: String): Unit =
    val cache = cacheFile(path)
    if loadCache(cache) then
      log.debug(s"waveform: loaded $made/$count from cache", category = "player")
      return

    val profile  = Profile.custom(spec.width, spec.height, spec.fpsNum, spec.fpsDen)
    val producer = Producer(profile, path)
    val consumer = Consumer.bare(profile)
    consumer.connect(producer)
    consumer.start()
    producer.seek(0)

    var i = 0
    while i < count && !stopping do
      consumer.rtFrame() match
        case Some(frame) =>
          if frame.speed == 0 then
            frame.close()
            i = count // end of stream — leave the rest at zero
          else
            peaks(i) = framePeak(frame, spec.fps, spec.audioRate)
            frame.close()
            made = i + 1
            i += 1
        case None => i = count

    consumer.stop()
    consumer.close()
    producer.close()
    profile.close()
    // Cache a pass that ran to completion (filled every slot or reached end of stream). A pass cut
    // short by close() is partial, so don't persist it.
    if !stopping then saveCache(cache)
    log.debug(s"waveform: generated $made/$count", category = "player")

  /** The largest absolute sample in a frame's audio, across all channels — its peak (clamped 0..1).
    * The samples are interleaved 32-bit floats borrowed from the frame (no copy). */
  private def framePeak(frame: Frame, fps: Double, audioRate: Int): Float =
    val a  = frame.audio(fps, audioRate, 2)
    val n  = a.samples * a.channels
    val fp = a.buffer.asInstanceOf[Ptr[Float]]
    var peak = 0.0f
    var k    = 0
    while k < n do
      val v = math.abs(fp(k))
      if v > peak then peak = v
      k += 1
    math.min(1.0f, peak)

  private def cacheFile(path: String): File =
    val src = new File(path)
    val key = f"${src.getAbsolutePath.hashCode & 0xffffffffL}%08x-${src.length}-${src.lastModified}-$count.f32"
    new File(new File(System.getProperty("user.home"), ".cache/kutter/waveforms"), key)

  private def loadCache(f: File): Boolean =
    if !f.exists() then false
    else
      val in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)))
      try
        val n = in.readInt()
        if n != count then false
        else
          var i = 0
          while i < count do
            peaks(i) = in.readFloat()
            i += 1
          made = count
          true
      catch case _: Throwable => false
      finally in.close()

  private def saveCache(f: File): Unit =
    f.getParentFile.mkdirs()
    val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(f)))
    try
      out.writeInt(count)
      var i = 0
      while i < count do
        out.writeFloat(peaks(i))
        i += 1
    finally out.close()
