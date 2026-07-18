package io.github.edadma.kutter

import io.github.edadma.suit.{Svg, SvgImage}

// The transport and chrome icons — conventional media-player glyphs as vector SVG, rendered through
// librsvg straight into suit's Cairo context so they stay crisp at any size. Built once at load and
// shared by the UI in `Main`.

private[kutter] val playIcon: SvgImage =
  Svg.fromString("""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M8 5v14l11-7z" fill="#e8e8ea"/></svg>""")

private[kutter] val pauseIcon: SvgImage =
  Svg.fromString(
    """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><rect x="6" y="5" width="4" height="14" fill="#e8e8ea"/><rect x="14" y="5" width="4" height="14" fill="#e8e8ea"/></svg>""",
  )

private[kutter] val volumeIcon: SvgImage =
  Svg.fromString(
    """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M3 9v6h4l5 5V4L7 9H3zm13.5 3A4.5 4.5 0 0 0 14 7.97v8.05A4.5 4.5 0 0 0 16.5 12z" fill="#e8e8ea"/></svg>""",
  )

// A close/remove "✕" drawn as a vector, since the UI font has no glyph for the multiplication-x
// character (it rendered as a tofu box). Muted to match the surrounding chrome.
private[kutter] val closeIcon: SvgImage =
  Svg.fromString(
    """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M6 6l12 12M18 6L6 18" stroke="#9aa0a6" stroke-width="2" stroke-linecap="round"/></svg>""",
  )
