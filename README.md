# wide-libretro

A "shim" around existing libretro cores that adds
[wideNES](https://prilik.com/blog/wideNES)-like widescreen to various platforms
in a (mostly) core-agnostic and frontend-agnostic manner.

...or at least that's the plan.

It's still very early days, and based on the project's _very_ sporadic commit
history, who knows if/when it'll actually start working.

For "v0" of wide-libretro, I'm targeting the GBA.

## Demo

TODO upload video via github...

Still very early days... but a few notable things present in the demo:

- `egui` + `skia` based rendering works within a `libretro` framebuffer
- frames from mGBA are rendered on the left
- "reconstructed" frames from `rustboyadvance-ppu-standalone` are rendered on
  the right (clearly, there's still some issues to iron out)
- video streams are being offset by the values of the BG3HOFS and BG3VOFS
  registers (fetched from GBA memory on each frame)

## Current Approach

The GBA contains several background layers, and so, for the widescreen effect to
work properly, we need to track each layer separately.

Unfortunately, GBA emulators don't magically include an API akin to
`fn get_layer_fb(idx: usize) -> &[u8]`, and in-fact, hacking-in such an API
isn't at all straightforward, as layers are composited on a _per pixel_ basis
during scanline rendering.

This leaves me with a few options:

1. fork a GBA emulator to hack-in such an API
2. fork a GBA emulator to add in render-pipeline instrumentation, and "re-run"
   parts of the render pipeline externally
3. give up and go home

At time time of writing (23/04/23), I'm going with option 2.

> NOTE: Option 1 may very well be easier and more maintainable long-term, but
> I *really* don't want to grok and then write a bunch of C code right now...

## Current Status

I've chosen to fork mGBA, as it is a fast, accurate, and well-maintained GBA
emulator with a working libretro core. Moreover, the libretro core supports the
`struct retro_memory_map` API out-of-the-box, which means I can directly access
relevant parts of the GBA's VRAM.

You can find the fork at [daniel5151/mgba](https://github.com/daniel5151/mgba).
The scope of the fork is very small - all I've done is added some
function-pointer based instrumentation to various GBA-specific video events
(i.e: vblank, hblank, PPU register access, etc...), which `wide-libretro` can
hook into as part of its init process.

Instead of using mGBA's PPU implementation, I've opted to lift the PPU
implementation from a Rust-based GBA emulator called
[RustBoyAdvance-NG](https://github.com/michelhe/rustboyadvance-ng), and extract
it into its own library, where it can be used outside the context of the
original emulator.

You can find that code over at [daniel5151/rustboyadvance-ppu-standalone](https://github.com/daniel5151/rustboyadvance-ppu-standalone).

## Usage

When launching the libretro frontend, make sure to set the `SHIM_CORE` env var
to point at the core you want to shim.

e.g: on Windows:

```powershell
# log spew from the shim.
# should probably look into hooking into libretro logging at some point...
$Env:RUST_LOG='trace'
# set the env var to mGBA
$Env:SHIM_CORE='C:\Users\Daniel Prilik\src\mbga\build\Release\mgba_libretro.dll'
# launch retroarch with the shim
.\retroarch.exe -v -L 'path\to\target\debug\wide_libretro.dll' 'path\to\your\rom.gba'
```

## Project motivation

It's been nearly 5 years since I first published
[wideNES](https://prilik.com/blog/wideNES), and it's about time that I revisited
the idea.

I've always wanted to apply the technique to the GBA, specifically so I could
play full-screen Advance Wars! Seeing the entire map at the same time would be
_so cool_, and since it's turn based, the fact that stuff outside the
"live-zone" isn't being updated is totally fine!

In any case, rather than relegating any new implementation to a specific
emulator, I realized that I could probably implement the wideNES technique as a
"shim" between an existing libretro core + frontend.

Notably, this means I don't have to integrate directly into any specific
emulator / frontend codebase, which means I can write the implementation
(primarily) in Rust!
