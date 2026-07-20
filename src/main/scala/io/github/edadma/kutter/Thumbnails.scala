package io.github.edadma.kutter

import java.io.File

import io.github.edadma.mlt.*
import io.github.edadma.libcairo.{Format, imageSurfaceCreate, imageSurfaceCreateForData, imageSurfaceCreateFromPNG}
import io.github.edadma.suit.{RasterImage, CairoBitmap}
import io.github.edadma.logger.LoggerFactory

// A filmstrip of preview thumbnails for one clip, decoded off to the side. It runs its OWN MLT graph
// — a producer and bare consumer of its own, on a background thread — so it never touches the
// playback decode thread's graph: an MLT producer's frame cache is not shared-thread-safe, and two
// separate producers each with their own cache decode side by side without racing.
//
// Generation seeks to `count` positions evenly spread across the clip, pulls a frame at each, scales
// it into a small ARGB surface, and publishes it. The timeline reads the strip through `at` while it
// fills in; a slot not yet made reads back null, and the block shows a flat colour there until it is.
final class Thumbnails(count: Int, val width: Int, val height: Int):
  private val log = LoggerFactory.getLogger

  private val frames = new Array[RasterImage | Null](count)

  // The number of leading slots that have been generated. Volatile: the generator writes a slot then
  // bumps this, and a reader that sees `made == k` is guaranteed to see slots 0 until k, because
  // slots fill in order (the array writes happen-before this release, the read is its acquire).
  @volatile private var made = 0

  @volatile private var stopping = false
  private var thread: Thread | Null = null

  /** The thumbnail nearest `fraction` (0 until 1) of the clip, or null if that slot isn't ready. */
  def at(fraction: Double): RasterImage | Null =
    val i = math.max(0, math.min(count - 1, (fraction * count).toInt))
    if i < made then frames(i) else null

  /** Whether every thumbnail has been generated. */
  def complete: Boolean = made >= count

  /** Spawn the background generator: its own graph rendering `path` against the timeline `spec`. */
  def start(spec: TimelineSpec, path: String): Unit =
    val t = new Thread(() => generate(spec, path), "kutter-thumbs")
    t.setDaemon(true)
    thread = t
    t.start()

  /** Stop generating (if still running) and wait for the thread to unwind. */
  def close(): Unit =
    stopping = true
    thread match
      case t: Thread => t.join()
      case null      => ()

  private def generate(spec: TimelineSpec, path: String): Unit =
    val dir = cacheDir(path)
    dir.mkdirs()
    def cacheFile(i: Int): File = new File(dir, s"$i.png")

    // If every slot is already on disk, load them without opening MLT at all — the fast reopen path.
    // A cache directory is keyed by the source's size and mtime, so a changed file misses and
    // regenerates.
    val allCached = (0 until count).forall(i => cacheFile(i).exists())
    if allCached then
      var i = 0
      while i < count && !stopping do
        frames(i) = loadCached(cacheFile(i))
        made = i + 1
        i += 1
      log.debug(s"thumbnails: loaded $made/$count from cache", category = "player")
      return

    val profile  = Profile.custom(spec.width, spec.height, spec.fpsNum, spec.fpsDen)
    val producer = Producer(profile, path)
    val consumer = Consumer.bare(profile)
    consumer.connect(producer)
    consumer.start()
    val len = math.max(1, producer.length)

    var i = 0
    while i < count && !stopping do
      loadCached(cacheFile(i)) match
        case img: RasterImage => frames(i) = img
        case null =>
          // The frame at the centre of this slot's span, so a thumbnail represents its stretch of the
          // clip rather than its leading edge.
          val frameNo = math.min(len - 1, (((i + 0.5) / count) * len).toInt)
          producer.seek(frameNo)
          consumer.purge()
          consumer.rtFrame() match
            case Some(frame) =>
              if frame.speed != 0 then
                val bmp = thumbFromFrame(frame)
                frames(i) = bmp
                bmp.surface.writeToPNG(cacheFile(i).getAbsolutePath)
              frame.close()
            case None => ()
      made = i + 1
      i += 1

    consumer.stop()
    consumer.close()
    producer.close()
    profile.close()
    log.debug(s"thumbnails: generated $made/$count", category = "player")

  /** The cache directory for `path`'s thumbnails, keyed so a changed source or a changed thumbnail
    * size regenerates: `<home>/.cache/kutter/thumbnails/<pathHash>-<size>-<mtime>-<w>x<h>-<n>`. */
  private def cacheDir(path: String): File =
    val src = new File(path)
    val abs = src.getAbsolutePath
    val key = f"${abs.hashCode & 0xffffffffL}%08x-${src.length}-${src.lastModified}-${width}x${height}-$count"
    new File(new File(System.getProperty("user.home"), ".cache/kutter/thumbnails"), key)

  /** Load a cached thumbnail PNG, or null if it's missing or not the expected size. */
  private def loadCached(file: File): RasterImage | Null =
    if !file.exists() then null
    else
      val s = imageSurfaceCreateFromPNG(file.getAbsolutePath)
      if s.getWidth == width && s.getHeight == height then CairoBitmap.wrap(s)
      else
        s.destroy()
        null

  /** Scale a decoded frame into a small ARGB surface and wrap it as a drawable image. Requests the
    * frame in RGBA, swaps it to Cairo's ARGB32 byte order in place, wraps that buffer as a source
    * surface (no copy — it lives only for the scale), and paints it scaled into the thumbnail. */
  private def thumbFromFrame(frame: Frame): CairoBitmap =
    val (buf, w, h, _) = frame.imagePtr(ImageFormat.Rgba)
    rgbaToArgb32(buf, w * h)
    val src = imageSurfaceCreateForData(buf, Format.ARGB32, w, h, w * 4)
    val dst = imageSurfaceCreate(Format.ARGB32, width, height)
    val cr  = dst.create
    cr.scale(width.toDouble / w, height.toDouble / h)
    cr.setSourceSurface(src, 0, 0)
    cr.paint()
    cr.destroy()
    dst.flush()
    dst.markDirty()
    src.destroy()
    CairoBitmap.wrap(dst)
