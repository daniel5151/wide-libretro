# wide-libretro

A "shim" around existing libretro cores that adds
[wideNES](https://prilik.com/blog/wideNES)-like widescreen to various platforms
in an emulator-agnostic and frontend-agnostic manner.

...or at least that's the plan.

It's still very early days, and idk if this project will actually go anywhere...

## Usage

When launching the libretro frontend, make sure to set the `SHIM_CORE` env var
to point at the core you want to shim.

e.g: on Windows:

```powershell
# log spew from the shim.
# should probably look into hooking into libretro logging at some point...
$Env:RUST_LOG='trace'
# set the env var to mGBA
$Env:SHIM_CORE='C:\Users\Daniel Prilik\src\libretro-shim\mgba_libretro.dll'
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

Unlike the NES, the GBA shouldn't have any major mid-frame shenanigans going on
(I hope), which means I should be able to use the `struct retro_memory_map` API
to inspect emulator state between calls to `retro_run`, and do any math I want
outside the emulator itself.

Notably, this means I don't have to integrate directly into any specific
emulator / frontend codebase, which means I can write the implementation in
Rust!
