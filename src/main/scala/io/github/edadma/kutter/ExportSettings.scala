package io.github.edadma.kutter

// The export format model — the container, codecs, and quality the render dialog drives, plus the two
// things a container/codec matrix needs: validation (which codecs a container can legally hold) and the
// avformat consumer properties an encode is configured from. It is deliberately above MLT: `Player`
// turns the properties here into consumer settings, but the rules — WebM takes only VP9 with Opus or
// Vorbis, MP4 rejects PCM, the ASF (WMV) container wants the Windows Media codecs — live here so they can
// be checked headlessly and surfaced in the dialog before an encode starts and fails mid-mux.

/** A file container (muxer). `ext` is the filename extension, `muxer` the ffmpeg/MLT format name — which
  * differs for WMV, whose extension is `.wmv` but whose muxer is `asf`, so the format is set explicitly
  * rather than left to the extension. */
enum Container(val ext: String, val label: String, val muxer: String):
  case Mp4  extends Container("mp4",  "MP4",                 "mp4")
  case Mov  extends Container("mov",  "QuickTime (MOV)",     "mov")
  case Mkv  extends Container("mkv",  "Matroska (MKV)",      "matroska")
  case WebM extends Container("webm", "WebM",                "webm")
  case Avi  extends Container("avi",  "AVI",                 "avi")
  case Wmv  extends Container("wmv",  "Windows Media (WMV)", "asf")

object Container:
  /** The container with `ext`, if any — how a chosen filename maps back to a container. */
  def fromExt(ext: String): Option[Container] = values.find(_.ext.equalsIgnoreCase(ext))

  /** The video codecs this container can legally hold, in the order the dialog offers them (the first is
    * the sensible default when a container is chosen). */
  def videoCodecs(c: Container): List[VideoCodec] =
    import VideoCodec.*
    c match
      case Mp4  => List(H264, H265, MPEG4)
      case Mov  => List(H264, H265, ProRes, MPEG4, MJPEG)
      case Mkv  => List(H264, H265, VP9, MPEG4, MJPEG)
      case WebM => List(VP9)
      case Avi  => List(MPEG4, MJPEG, H264)
      case Wmv  => List(WMV2)

  /** The audio codecs this container can legally hold, first being the default. */
  def audioCodecs(c: Container): List[AudioCodec] =
    import AudioCodec.*
    c match
      case Mp4  => List(AAC, MP3)
      case Mov  => List(AAC, PCM)
      case Mkv  => List(AAC, MP3, Opus, Vorbis, PCM)
      case WebM => List(Opus, Vorbis)
      case Avi  => List(MP3, PCM)
      case Wmv  => List(WMA, MP3)

/** A video codec. `id` is the ffmpeg encoder name; `crf` is whether it supports constant-quality (CRF)
  * encoding — H.264/H.265/VP9 do, the older intra codecs and ProRes do not (they take a bitrate or a
  * profile instead). */
enum VideoCodec(val id: String, val label: String, val crf: Boolean):
  case H264   extends VideoCodec("libx264",    "H.264",               true)
  case H265   extends VideoCodec("libx265",    "H.265 (HEVC)",        true)
  case VP9    extends VideoCodec("libvpx-vp9", "VP9",                 true)
  case ProRes extends VideoCodec("prores_ks",  "Apple ProRes",        false)
  case MPEG4  extends VideoCodec("mpeg4",      "MPEG-4",              false)
  case MJPEG  extends VideoCodec("mjpeg",      "Motion JPEG",         false)
  case WMV2   extends VideoCodec("wmv2",       "Windows Media Video", false)

object VideoCodec:
  def fromId(id: String): Option[VideoCodec] = values.find(_.id == id)

/** An audio codec. `lossless` marks the uncompressed/lossless codecs, which carry no bitrate control. */
enum AudioCodec(val id: String, val label: String, val lossless: Boolean):
  case AAC    extends AudioCodec("aac",        "AAC",                 false)
  case MP3    extends AudioCodec("libmp3lame", "MP3",                 false)
  case Opus   extends AudioCodec("libopus",    "Opus",                false)
  case Vorbis extends AudioCodec("libvorbis",  "Vorbis",              false)
  case PCM    extends AudioCodec("pcm_s16le",  "PCM (uncompressed)",  true)
  case WMA    extends AudioCodec("wmav2",      "Windows Media Audio", false)

object AudioCodec:
  def fromId(id: String): Option[AudioCodec] = values.find(_.id == id)

/** One export configuration: the container, the video and audio codecs, and the quality — a
  * constant-quality factor (`crf`, lower is better) for the CRF-capable codecs, or a target `videoBitrateK`
  * (kbps) otherwise, plus an `audioBitrateK` for the lossy audio codecs. `useCrf` chooses between the two
  * quality modes for a CRF-capable video codec; it is ignored for a codec that has no CRF (which always
  * uses the bitrate). */
final case class ExportSettings(
    container:     Container,
    video:         VideoCodec,
    audio:         AudioCodec,
    useCrf:        Boolean,
    crf:           Int,
    videoBitrateK: Int,
    audioBitrateK: Int,
):
  /** Whether the video codec's quality is set by CRF right now — CRF is both chosen and available. */
  def crfActive: Boolean = useCrf && video.crf

  /** A message naming why this combination cannot be muxed, or `None` when it is valid. The video and
    * audio codecs are each checked against the container's legal set; the message lists the alternatives,
    * so the dialog can tell the user exactly what to change. */
  def validate: Option[String] =
    if !Container.videoCodecs(container).contains(video) then
      Some(s"${video.label} video can't go in ${container.label}. Allowed: ${Container.videoCodecs(container).map(_.label).mkString(", ")}.")
    else if !Container.audioCodecs(container).contains(audio) then
      Some(s"${audio.label} audio can't go in ${container.label}. Allowed: ${Container.audioCodecs(container).map(_.label).mkString(", ")}.")
    else None

  /** Whether this configuration can be encoded (a valid container/codec combination). */
  def isValid: Boolean = validate.isEmpty

  // The video codec's quality properties: CRF when active (VP9 also needs `vb=0` so the rate control is
  // purely quality-driven), a fixed HQ profile for ProRes (which is profile-based), else a target bitrate.
  private def videoQualityProps: List[(String, String)] =
    if crfActive then
      if video == VideoCodec.VP9 then List("crf" -> crf.toString, "vb" -> "0")
      else List("crf" -> crf.toString)
    else if video == VideoCodec.ProRes then List("vprofile" -> "3") // HQ; ProRes ignores CRF and bitrate
    else List("vb" -> s"${videoBitrateK}k")

  // The audio codec's quality: a target bitrate for the lossy codecs, nothing for the uncompressed ones.
  private def audioQualityProps: List[(String, String)] =
    if audio.lossless then Nil else List("ab" -> s"${audioBitrateK}k")

  // Whether this is HEVC in an Apple container. libx265 tags its stream `hev1` by default, which Apple's
  // players (QuickTime, Photos, Safari, Finder) refuse to decode — the file opens playing audio only,
  // though the picture is intact. Forcing the equivalent `hvc1` tag makes the same bitstream play there.
  private def appleHevc: Boolean =
    video == VideoCodec.H265 && (container == Container.Mp4 || container == Container.Mov)

  /** The avformat consumer properties this configuration compiles to — the muxer format, the two codecs,
    * the quality settings, an `hvc1` tag for HEVC in an Apple container (so QuickTime plays the video, not
    * just the audio), and (for MP4/MOV) a web-friendly moov-at-front flag. Audio rate and channels are set
    * by the caller from the timeline spec, so they are not here. Pure, so it is unit-tested. */
  def consumerProps: List[(String, String)] =
    List("f" -> container.muxer, "vcodec" -> video.id, "acodec" -> audio.id) ++
      videoQualityProps ++
      (if appleHevc then List("vtag" -> "hvc1") else Nil) ++
      audioQualityProps ++
      (if container == Container.Mp4 || container == Container.Mov then List("movflags" -> "+faststart") else Nil)

object ExportSettings:
  /** The default export: an H.264/AAC MP4 at CRF 20 — visually near-lossless at a sensible size, the
    * broadcast-safe web default. */
  val default: ExportSettings =
    ExportSettings(Container.Mp4, VideoCodec.H264, AudioCodec.AAC, useCrf = true, crf = 20, videoBitrateK = 8000, audioBitrateK = 192)

  /** The settings after switching to `container`, keeping the current video/audio codec when the new
    * container still allows it, else snapping each to that container's default (its first legal codec).
    * So changing container never strands the user on an invalid combination, while a deliberate codec
    * choice that is still legal is preserved. */
  def withContainer(s: ExportSettings, container: Container): ExportSettings =
    val v = if Container.videoCodecs(container).contains(s.video) then s.video else Container.videoCodecs(container).head
    val a = if Container.audioCodecs(container).contains(s.audio) then s.audio else Container.audioCodecs(container).head
    s.copy(container = container, video = v, audio = a)
