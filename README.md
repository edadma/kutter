<p align="center">
  <img src="assets/logo.png" alt="Kutter" width="440">
</p>

# kutter

A desktop **video editor**, built in [Scala Native](https://scala-native.org) on the
[suit](https://github.com/edadma/suit) UI toolkit, decoding through the
[MLT](https://www.mltframework.org) multimedia framework (via the
[mlt](https://github.com/edadma/mlt) bindings) and playing audio through
[SDL3](https://github.com/edadma/sdl3).

kutter is a **fixed-track NLE**, in the shape of kdenlive or DaVinci Resolve: media is imported into a
**bin**, then placed onto **tracks** — `V1`/`V2` for video (composited top-down) and `A1`/`A2` for
audio (mixed) — which always exist whether or not they hold anything. Each track sequences its clips
along the timeline, and the whole stack compiles to an MLT tractor for playback and (later) export.

Its reason for being is **automated typeset lower thirds**: a batch of name/title cards, typeset by the
[texish](https://github.com/edadma/texish) engine, faded in and out over the footage. Lower thirds ride
on their own layer on top of the track stack — imported from a `.klt` list or added by hand, positioned
on the timeline, and previewed live.

## How playback works

Each MLT frame carries both its picture and its audio, decoded on a thread of its own — off the UI
thread, where the per-frame cost belongs.

- **Video**: the decoded YUV (I420) planes are handed to the UI thread through `UiThread.post`
  (suit's one thread-safe seam) and uploaded into a `VideoTexture`. A `video` widget paints a
  transparent hole the runtime blits the texture through — no colourspace conversion and no CPU
  blit; the renderer converts YUV→RGB in the blit's shader.
- **Audio**: the frame's samples are pushed into an SDL audio stream, and **audio is the master
  clock** — the decode thread holds each picture until the device has played up to that frame's
  audio, so video follows sound rather than drifting off a wall-clock timer. A timeline with no audio
  falls back to pacing by the frame duration.
- **The master volume** drives the SDL stream's gain. **Seeking** and **play/pause** only flip
  volatile flags; the decode thread owns every MLT call, so no graph call ever crosses onto the UI
  thread. Editing a lower third rebuilds the graph and swaps it under the running consumer.

See `src/main/scala/io/github/edadma/kutter/Player.scala` for the engine, `Project.scala` for the data
model, and `Main.scala` for the UI.

## Building

The whole app is a single Scala Native binary. It consumes **suit**, **sdl3**, **mlt**, **hocon** and
**texish** by source from sibling checkouts (a `ProjectRef` each), so changes to any of them — all under
co-development — flow straight in with no publish step. `logger` and `zio-json` come from Maven Central.

Requirements:

- Homebrew **MLT** (`libmlt-7`), **SDL3**, **cairo**, **freetype**, **librsvg** — the shared
  libraries the bindings link against.
- Sibling checkouts alongside this repo: `../suit`, `../sdl3`, `../mlt`, `../hocon`, `../texish`
  (source dependencies), and `../riposte` (suit's vdom core, referenced by suit's build).

## Running

```
sbt run                       # opens an empty project (import a video, or open a .kutter)
sbt "run /path/to/clip.mp4"   # opens a project with that clip placed on V1
sbt "run project.kutter"      # opens a saved project
```

Set `KUTTER_DEBUG=1` in the environment to raise the log level to `TRACE` (the frame path, audio
device, and timing) via the [logger](https://github.com/edadma/logger) library.

The project is rendered against a fixed 1280×720@30 profile with audio resampled to 48 kHz stereo, so
differently sized or timed sources still play (scaled/resampled) — deriving the profile from the media
is a later refinement.

## Diagnostics

The pipeline is verified off the GUI by `KUTTER_PROBE*` environment variables (see `Diagnostics.scala`):
`KUTTER_PROBE_HIT` runs the pure timeline/import/card checks (exits non-zero on any failure);
`KUTTER_PROBE_NLE=<frame>` renders the fixed-track sequencing to `probe-nle.png`; `KUTTER_PROBE=<frame>`
renders the demo project. Each exits without opening a window.

## Test media

`big_buck_bunny_720p.mp4` is a 20-second, 1280×720 excerpt of **Big Buck Bunny**, with stereo audio,
© 2008 [Blender Foundation](https://www.blender.org) (peach.blender.org), released under the
[Creative Commons Attribution 3.0](https://creativecommons.org/licenses/by/3.0/) licence. `examples/`
holds a silent and an audio sample clip and a `lower-thirds.klt` batch list. All are bundled purely as
sample media.
