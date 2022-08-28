#![allow(unused_variables)]

use self::env_ffi_helpers::*;
use egui::ScrollArea;
use egui_skia::EguiSkia;
use egui_skia::EguiSkiaPaintCallback;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use skia_safe::Color;
use skia_safe::Font;
use skia_safe::PathEffect;
use skia_safe::{Paint, Point, Surface};
use std::ffi::c_void;
use std::sync::mpsc;
use std::sync::Arc;
use sys::*;

#[allow(non_upper_case_globals, non_camel_case_types, dead_code)]
mod sys;

mod memory_map;

struct EmuFb {
    fb: Vec<u8>,
    width: usize,
    height: usize,
}

static EMU_FB: Lazy<Mutex<EmuFb>> = Lazy::new(|| {
    Mutex::new(EmuFb {
        fb: Vec::new(),
        width: 0,
        height: 0,
    })
});

/// Container for all things UI related.
struct UiState {
    ui_fb: Arc<Mutex<Vec<u8>>>,
    ui_send: Option<mpsc::Sender<UiMsg>>,
    ui_recv: Option<mpsc::Receiver<UiMsg>>,
}

static UI_STATE: Lazy<Mutex<UiState>> = Lazy::new(|| {
    let (ui_send, ui_recv) = mpsc::channel();
    Mutex::new(UiState {
        ui_fb: {
            let mut fb = Vec::new();
            fb.resize(SCREEN_WIDTH * SCREEN_HEIGHT * 4, 0);
            Arc::new(Mutex::new(fb))
        },
        ui_send: Some(ui_send),
        ui_recv: Some(ui_recv),
    })
});

static DYLIB: Lazy<Mutex<libloading::Library>> = Lazy::new(|| {
    // TODO: move this stuff to a better place?
    {
        // integrate with libretro logging?
        pretty_env_logger::init();
        {
            let mut ui_state = UI_STATE.lock();
            let fb = ui_state.ui_fb.clone();
            let ui_recv = ui_state.ui_recv.take().unwrap();
            std::thread::spawn(move || ui_thread(fb, ui_recv));
        }
    }

    let path_to_core = std::env::var("SHIM_CORE").expect("did not find SHIM_CORE");
    let library = unsafe {
        libloading::Library::new(path_to_core).expect("could not load core specified by SHIM_CORE")
    };
    Mutex::new(library)
});

// streamline the base-case of forwarding calls without modifying anything
macro_rules! fwd {
    (fn $name:ident($($arg:ident: $type:ty $(,)?)*);) => {
        fwd! { fn $name($($arg: $type,)*) -> (); }
    };

    (fn $name:ident($($arg:ident: $type:ty $(,)?)*) -> $ret:ty;) => {
        #[allow(clippy::missing_safety_doc)]
        #[no_mangle]
        pub unsafe extern "C" fn $name($($arg: $type,)*) -> $ret {
            log::trace!("forwarded {}", stringify!($name));

            let dylib = DYLIB.lock();
            let f: libloading::Symbol<'_, unsafe extern "C" fn($($arg: $type,)*) -> $ret> =
                unsafe { dylib.get(stringify!($name).as_bytes()).unwrap() };
            unsafe { (f)($($arg,)*) }
        }
    };
}

// fwd! { fn retro_set_video_refresh(arg1: retro_video_refresh_t); }
fwd! { fn retro_set_audio_sample(arg1: retro_audio_sample_t); }
fwd! { fn retro_set_audio_sample_batch(arg1: retro_audio_sample_batch_t); }
fwd! { fn retro_set_input_poll(arg1: retro_input_poll_t); }
// fwd! { fn retro_set_input_state(arg1: retro_input_state_t); }
fwd! { fn retro_init(); }
fwd! { fn retro_deinit(); }
fwd! { fn retro_api_version() -> u32; }
// fwd! { fn retro_get_system_info(arg1:  *mut retro_system_info); }
// fwd! { fn retro_get_system_av_info(info: *mut retro_system_av_info); }
fwd! { fn retro_set_controller_port_device(port: u32, device: u32); }
fwd! { fn retro_reset(); }
// fwd! { fn retro_run(); }
fwd! { fn retro_serialize_size() -> size_t; }
fwd! { fn retro_serialize(data: *mut ::std::os::raw::c_void, size: size_t) -> bool; }
fwd! { fn retro_unserialize(data: *const ::std::os::raw::c_void, size: size_t) -> bool; }
fwd! { fn retro_cheat_reset(); }
fwd! { fn retro_cheat_set(index: u32, enabled: bool, code: *const ::std::os::raw::c_char); }
fwd! { fn retro_load_game(game: *const retro_game_info) -> bool; }
fwd! { fn retro_load_game_special(game_type: u32, info: *const retro_game_info, num_info: size_t) -> bool; }
fwd! { fn retro_unload_game(); }
fwd! { fn retro_get_region() -> u32; }
fwd! { fn retro_get_memory_data(id: u32) -> *mut ::std::os::raw::c_void; }
fwd! { fn retro_get_memory_size(id: u32) -> size_t; }
// fwd! { fn retro_set_environment(cb: retro_environment_t); }

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn retro_set_input_state(cb: retro_input_state_t) {
    log::trace!("intercepted retro_set_input_state");

    // stash the callback we are given
    LIBRETRO_CALLBACKS.lock().input = cb;

    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(arg1: retro_input_state_t) -> ()> =
        unsafe { dylib.get("retro_set_input_state".as_bytes()).unwrap() };
    unsafe { (f)(cb) }
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
unsafe extern "C" fn retro_get_system_info(info: *mut retro_system_info) {
    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(*mut retro_system_info) -> ()> =
        unsafe { dylib.get("retro_get_system_info".as_bytes()).unwrap() };
    unsafe { (f)(info) };

    let system_info = unsafe { &mut *info };
    log::info!("intercepted retro_get_system_info: {:#?}", system_info);

    let wide_info = retro_system_info {
        library_name: b"wide-libretro\0".as_ptr() as _,
        library_version: b"0.0.1\0".as_ptr() as _,
        valid_extensions: system_info.valid_extensions,
        need_fullpath: system_info.need_fullpath,
        block_extract: system_info.block_extract,
    };
    log::info!("overriding retro_system_info.geometry with {:?}", wide_info);
    *system_info = wide_info;
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
unsafe extern "C" fn retro_get_system_av_info(info: *mut retro_system_av_info) {
    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(*mut retro_system_av_info) -> ()> =
        unsafe { dylib.get("retro_get_system_av_info".as_bytes()).unwrap() };
    unsafe { (f)(info) };

    let system_av_info = unsafe { &mut *info };
    log::info!(
        "intercepted retro_get_system_av_info: {:#?}",
        system_av_info
    );

    let wide_geometry = retro_game_geometry {
        base_width: SCREEN_WIDTH as u32,
        base_height: SCREEN_HEIGHT as u32,
        max_width: SCREEN_WIDTH as u32,
        max_height: SCREEN_HEIGHT as u32,
        aspect_ratio: 0.0,
    };
    log::info!(
        "overriding retro_system_av_info.geometry with {:?}",
        wide_geometry
    );
    system_av_info.geometry = wide_geometry;
}

/// Container for any callbacks the frontend hands the core.
struct LibretroCallbacks {
    env: retro_environment_t,
    video: retro_video_refresh_t,
    input: retro_input_state_t,
}

static LIBRETRO_CALLBACKS: Lazy<Mutex<LibretroCallbacks>> = Lazy::new(|| {
    Mutex::new(LibretroCallbacks {
        env: None,
        video: None,
        input: None,
    })
});

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
unsafe extern "C" fn retro_set_environment(cb: retro_environment_t) {
    log::trace!("intercepted + shim'd retro_set_environment");

    // stash the callback we are given
    LIBRETRO_CALLBACKS.lock().env = cb;

    // ...and hand off the shim'd callback to the underlying core
    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(retro_environment_t) -> ()> =
        unsafe { dylib.get("retro_set_environment".as_bytes()).unwrap() };
    unsafe { (f)(Some(retro_environment_shim)) }
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
unsafe extern "C" fn retro_set_video_refresh(cb: retro_video_refresh_t) {
    log::trace!("intercepted + shim'd retro_set_video_refresh");

    // stash the callback we are given
    LIBRETRO_CALLBACKS.lock().video = cb;

    // ...and hand off the shim'd callback to the underlying core
    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(retro_video_refresh_t) -> ()> =
        unsafe { dylib.get("retro_set_video_refresh".as_bytes()).unwrap() };
    unsafe { (f)(Some(retro_video_refresh_shim)) }
}

// here's where the real magic is gonna happen...
unsafe extern "C" fn retro_video_refresh_shim(
    orig_data: *const c_void,
    orig_width: u32,
    orig_height: u32,
    orig_pitch: usize,
) {
    assert!(matches!(
        CORE_STATE.lock().pixel_format,
        Some(retro_pixel_format::_RGB565)
    ));

    let orig_fb = unsafe {
        std::slice::from_raw_parts(
            orig_data as *const u16,
            (orig_pitch / std::mem::size_of::<u16>()) as usize * orig_height as usize,
        )
    };

    retro_video_refresh_shim_rs(
        orig_fb,
        orig_width as usize,
        orig_height as usize,
        orig_pitch as usize,
    );
}

fn retro_video_refresh_shim_rs(
    orig_fb: &[u16],
    orig_width: usize,
    orig_height: usize,
    orig_pitch: usize,
) {
    let mut emu_fb = EMU_FB.lock();
    emu_fb.width = orig_width;
    emu_fb.height = orig_height;

    // normalize original framebuffer into xrgb8888
    let orig_fb_xrgb_8888 = {
        let fb = &mut emu_fb.fb;
        if fb.len() != orig_width * orig_height * 4 {
            fb.resize(orig_width * orig_height * 4, 0);
        }

        let dst = fb.chunks_exact_mut(4);
        let src = orig_fb
            .chunks_exact(orig_pitch / std::mem::size_of::<u16>())
            .flat_map(|c| &c[..orig_width]);

        #[allow(clippy::unusual_byte_groupings)]
        for (dst, src) in dst.zip(src) {
            const R_MASK: u16 = 0b11111_000000_00000;
            const G_MASK: u16 = 0b00000_111111_00000;
            const B_MASK: u16 = 0b00000_000000_11111;

            let r = (((src & R_MASK) >> 11) as u8) << 3;
            let g = (((src & G_MASK) >> 5) as u8) << 2;
            let b = ((src & B_MASK) as u8) << 3;
            dst[3] = 0xff;
            dst[2] = r;
            dst[1] = g;
            dst[0] = b;
        }
        fb
    };

    drop(emu_fb);

    // do a little locking + option take dance
    let (done_tx, done_rx) = mpsc::channel();
    let ui_send = {
        let mut ui_state = UI_STATE.lock();
        ui_state.ui_send.take()
    };

    ui_send
        .as_ref()
        .unwrap()
        .send(UiMsg::UpdateUi(done_tx))
        .unwrap();
    {
        let mut ui_state = UI_STATE.lock();
        ui_state.ui_send = ui_send;
    }

    done_rx.recv().unwrap();

    // ok now dump this the final framebuffer
    let ui_state = UI_STATE.lock();
    let fb = ui_state.ui_fb.lock();
    unsafe {
        (LIBRETRO_CALLBACKS.lock().video.unwrap())(
            fb.as_ptr() as *const c_void,
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
            SCREEN_WIDTH * 4,
        )
    }
}

const SCREEN_WIDTH: usize = (1280. * 1.5) as _;
const SCREEN_HEIGHT: usize = (720. * 1.5) as _;

/// Container for bits of interesting info the core sends up to the frontend.
struct CoreState {
    memory_map: Option<memory_map::MemoryMap>,
    pixel_format: Option<retro_pixel_format>,
}

static CORE_STATE: Lazy<Mutex<CoreState>> = Lazy::new(|| {
    Mutex::new(CoreState {
        memory_map: None,
        pixel_format: None,
    })
});

mod env_ffi_helpers {
    use std::ffi::c_void;

    pub struct EnvCallbackArgs(u32, *mut c_void);

    impl EnvCallbackArgs {
        pub fn new(cmd: u32, ptr: *mut c_void) -> EnvCallbackArgs {
            EnvCallbackArgs(cmd, ptr)
        }

        pub fn cmd(&self) -> u32 {
            self.0
        }

        pub unsafe fn into_mut<'a, T>(self) -> &'a mut T {
            unsafe { self.1.cast::<T>().as_mut().unwrap() }
        }

        pub unsafe fn into_ref<'a, T>(self) -> &'a T {
            unsafe { self.1.cast::<T>().as_ref().unwrap() }
        }
    }

    pub struct UnsupportedEnvCmd;

    pub struct EnvCallback(unsafe extern "C" fn(u32, *mut c_void) -> bool);

    impl EnvCallback {
        pub fn new(f: unsafe extern "C" fn(u32, *mut c_void) -> bool) -> EnvCallback {
            EnvCallback(f)
        }

        pub unsafe fn get<T>(&self, data: EnvCallbackArgs) -> Result<&T, UnsupportedEnvCmd> {
            unsafe { self.get_nullable(data).map(Option::unwrap) }
        }

        pub unsafe fn get_nullable<T>(
            &self,
            data: EnvCallbackArgs,
        ) -> Result<Option<&T>, UnsupportedEnvCmd> {
            let is_supported = unsafe { (self.0)(data.0, data.1) };
            is_supported
                .then(|| unsafe { (data.1 as *mut T).as_ref() })
                .ok_or(UnsupportedEnvCmd)
        }

        pub unsafe fn set<T>(&self, cmd: u32, data: &T) -> Result<(), UnsupportedEnvCmd> {
            unsafe { self.set_nullable(cmd, Some(data)) }
        }

        pub unsafe fn set_nullable<T>(
            &self,
            cmd: u32,
            data: Option<&T>,
        ) -> Result<(), UnsupportedEnvCmd> {
            let ptr = match data {
                Some(data) => data as *const T as *mut c_void,
                None => std::ptr::null_mut(),
            };
            let is_supported = unsafe { (self.0)(cmd, ptr) };
            is_supported.then(|| ()).ok_or(UnsupportedEnvCmd)
        }

        pub fn passthrough(&self, data: EnvCallbackArgs) -> Result<(), UnsupportedEnvCmd> {
            // SAFETY: using original data + cmd provided by underlying core
            let is_supported = unsafe { (self.0)(data.0, data.1) };
            is_supported.then(|| ()).ok_or(UnsupportedEnvCmd)
        }
    }
}

unsafe extern "C" fn retro_environment_shim(cmd: u32, data: *mut c_void) -> bool {
    let env_cb = EnvCallback::new(LIBRETRO_CALLBACKS.lock().env.unwrap());
    retro_environment_shim_rs(env_cb, EnvCallbackArgs::new(cmd, data)).is_ok()
}

fn retro_environment_shim_rs(
    env_cb: EnvCallback,
    data: EnvCallbackArgs,
) -> Result<(), UnsupportedEnvCmd> {
    let cmd = data.cmd();
    match retro_environment(cmd) {
        // ==== Libretro harness ==== //
        retro_environment::GET_LOG_INTERFACE => {
            let _ = unsafe { env_cb.get::<retro_log_callback>(data)? };
            // TODO: hook into libretro logger
            log::info!("intercepted env:GET_LOG_INTERFACE");
            Ok(())
        }

        // ==== Memory Map //
        retro_environment::SET_MEMORY_MAPS => {
            let map = unsafe { data.into_mut::<retro_memory_map>() };

            let memory_map = memory_map::MemoryMap::from_retro_memory_map(map);
            log::info!("intercepted env:SET_MEMORY_MAPS: {:#x?}", memory_map);
            CORE_STATE.lock().memory_map = Some(memory_map);

            // pass things along...
            unsafe { env_cb.set(cmd, map) }
        }

        // ==== Audio ==== //
        retro_environment::SET_AUDIO_BUFFER_STATUS_CALLBACK => {
            log::info!("intercepted env:SET_AUDIO_BUFFER_STATUS_CALLBACK");
            env_cb.passthrough(data)
        }
        retro_environment::SET_MINIMUM_AUDIO_LATENCY => {
            log::info!("intercepted env:SET_MINIMUM_AUDIO_LATENCY");
            env_cb.passthrough(data)
        }

        // ==== Video ==== //
        retro_environment::SET_PIXEL_FORMAT => {
            let pixel_format = unsafe { data.into_mut::<i32>() };

            let pixel_format_orig = match pixel_format {
                0 => retro_pixel_format::_0RGB1555,
                1 => retro_pixel_format::_XRGB8888,
                2 => retro_pixel_format::_RGB565,
                _ => retro_pixel_format::_UNKNOWN,
            };
            log::info!("intercepted env:SET_PIXEL_FORMAT: {:?}", pixel_format);
            CORE_STATE.lock().pixel_format = Some(pixel_format_orig);

            // normalize underlying framebuffers to use XRGB8888
            *pixel_format = retro_pixel_format::_XRGB8888 as i32;

            unsafe { env_cb.set(cmd, pixel_format) }
        }

        // ==== Input ==== //
        retro_environment::GET_INPUT_BITMASKS => {
            let _ = unsafe { env_cb.get_nullable::<bool>(data)? };
            log::info!("intercepted env:GET_INPUT_BITMASKS");
            Ok(())
        }
        retro_environment::SET_INPUT_DESCRIPTORS => {
            log::info!("intercepted env:SET_INPUT_DESCRIPTORS");
            env_cb.passthrough(data)
        }

        // ==== Variables ==== //
        retro_environment::GET_CORE_OPTIONS_VERSION => {
            let version = unsafe { env_cb.get::<u32>(data)? };

            log::info!("intercepted env:GET_CORE_OPTIONS_VERSION: {}", version);
            Ok(())
        }
        retro_environment::GET_VARIABLE_UPDATE => {
            // _silently_ pass this right on through, to avoid logspam
            env_cb.passthrough(data)
        }
        retro_environment::GET_VARIABLE => {
            // call the callback to get the data, then inspect it
            let var = unsafe { env_cb.get::<retro_variable>(data)? };

            let key = {
                if !var.key.is_null() {
                    Some(unsafe { std::ffi::CStr::from_ptr(var.key) })
                } else {
                    None
                }
            };
            let value = {
                if !var.key.is_null() {
                    Some(unsafe { std::ffi::CStr::from_ptr(var.value) })
                } else {
                    None
                }
            };

            log::trace!("intercepted env:GET_VARIABLE: {:?}={:?}", key, value);
            Ok(())
        }
        retro_environment::SET_VARIABLES => {
            let mut vars: Vec<(String, String)> = Vec::new();

            let first = unsafe { data.into_ref::<retro_variable>() };

            let mut p = first as *const retro_variable;
            loop {
                let var = unsafe { *p };
                p = unsafe { p.add(1) };
                if var.key.is_null() && var.value.is_null() {
                    break;
                }

                assert!(!var.key.is_null());
                assert!(!var.value.is_null());
                let key = unsafe { std::ffi::CStr::from_ptr(var.key) };
                let value = unsafe { std::ffi::CStr::from_ptr(var.value) };
                vars.push((
                    key.to_string_lossy().to_string(),
                    value.to_string_lossy().to_string(),
                ));
            }

            log::info!("intercepted env:SET_VARIABLES: {:?}", vars);
            unsafe { env_cb.set(cmd, first) }
        }
        retro_environment::SET_CORE_OPTIONS_V2_INTL => {
            log::info!("intercepted env:SET_CORE_OPTIONS_V2_INTL");
            env_cb.passthrough(data)
        }

        // ==== Other ==== //
        retro_environment::GET_SYSTEM_DIRECTORY => {
            let dir = unsafe { env_cb.get::<*const i8>(data)? };

            let dir = unsafe { std::ffi::CStr::from_ptr(*dir) };
            log::info!("intercepted env:GET_SYSTEM_DIRECTORY: {:?}", dir);
            Ok(())
        }
        retro_environment::GET_LANGUAGE => {
            let _ = unsafe { env_cb.get::<u32>(data)? };
            log::info!("intercepted env:GET_LANGUAGE");
            Ok(())
        }
        retro_environment::SET_SUPPORT_ACHIEVEMENTS => {
            let support_achievements = unsafe { data.into_ref::<bool>() };

            log::info!(
                "intercepted env:SET_SUPPORT_ACHIEVEMENTS: {}",
                support_achievements
            );

            unsafe { env_cb.set(cmd, support_achievements) }
        }

        // pass the rest right on through
        _ => {
            log::warn!("forwarded unknown env:{:?}", retro_environment(cmd));
            env_cb.passthrough(data)
        }
    }
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn retro_run() {
    // run underlying emulator first
    {
        let dylib = DYLIB.lock();
        let f: libloading::Symbol<'_, unsafe extern "C" fn() -> ()> =
            unsafe { dylib.get("retro_run".as_bytes()).unwrap() };
        unsafe { (f)() };
    }
}

enum UiMsg {
    UpdateUi(mpsc::Sender<()>),
}

fn get_libretro_pointer_info() -> (f32, f32, bool) {
    let input_cb = LIBRETRO_CALLBACKS.lock().input.unwrap();
    let input_cb = |id| unsafe { (input_cb)(0, sys::RETRO_DEVICE_POINTER, 0, id) };

    let pointer_x = input_cb(RETRO_DEVICE_ID_POINTER_X);
    let pointer_y = input_cb(RETRO_DEVICE_ID_POINTER_Y);
    let pointer_pressed = input_cb(RETRO_DEVICE_ID_POINTER_PRESSED);

    let normalized_x = ((pointer_x as i32 + 0x7fff) as f32) / (0x7fff as f32 * 2.);
    let normalized_y = ((pointer_y as i32 + 0x7fff) as f32) / (0x7fff as f32 * 2.);

    (normalized_x, normalized_y, pointer_pressed == 1)
}

fn ui_thread(fb: Arc<Mutex<Vec<u8>>>, recv: mpsc::Receiver<UiMsg>) {
    let image_info = skia_safe::ImageInfo::new(
        (SCREEN_WIDTH as _, SCREEN_HEIGHT as _),
        skia_safe::ColorType::BGRA8888,
        skia_safe::AlphaType::Premul,
        None,
    );

    let mut backend = EguiSkia::new();
    let mut surface =
        Surface::new_raster(&image_info, SCREEN_WIDTH * 4, None).expect("Failed to create surface");

    let mut demo = egui_demo_lib::DemoWindows::default();

    let mut last_pointer_pos = egui::pos2(0., 0.);
    let mut last_pointer_pressed = false;
    let mut frame = 0;

    loop {
        let msg = recv.recv().expect("sender won't go away");
        match msg {
            UiMsg::UpdateUi(done) => {
                let events = {
                    let mut events = Vec::new();

                    let (pointer_x, pointer_y, pointer_pressed) = get_libretro_pointer_info();
                    let pointer_pos = egui::pos2(
                        pointer_x * SCREEN_WIDTH as f32,
                        pointer_y * SCREEN_HEIGHT as f32,
                    );

                    if pointer_pos != last_pointer_pos {
                        last_pointer_pos = pointer_pos;
                        log::info!("pointer_pos: {:?}", pointer_pos);
                        events.push(egui::Event::PointerMoved(pointer_pos));
                    }

                    let pointer_pressed = pointer_pressed;
                    if pointer_pressed != last_pointer_pressed {
                        last_pointer_pressed = pointer_pressed;
                        log::info!("pointer_pressed: {}", pointer_pressed);
                        events.push(egui::Event::PointerButton {
                            pos: pointer_pos,
                            button: egui::PointerButton::Primary,
                            pressed: pointer_pressed,
                            modifiers: egui::Modifiers {
                                alt: false,
                                ctrl: false,
                                shift: false,
                                mac_cmd: false,
                                command: false,
                            },
                        })
                    }

                    events
                };

                let input = egui::RawInput {
                    screen_rect: Some(
                        [
                            egui::Pos2::default(),
                            egui::Pos2::new(surface.width() as f32, surface.height() as f32),
                        ]
                        .into(),
                    ),
                    pixels_per_point: Some(1.),
                    events,
                    ..Default::default()
                };

                // TODO: actually use these return values
                let (_paint_duration, _platform_output) = backend.run(input, |ctx| {
                    demo.ui(ctx);

                    // wide shenanigans
                    {
                        let core_state = CORE_STATE.lock();
                        let emu_fb = EMU_FB.lock();

                        let memory_map = core_state.memory_map.as_ref().unwrap();
                        let bg3hofs = memory_map.read_u16(0x400001C).unwrap();
                        let bg3vofs = memory_map.read_u16(0x400001E).unwrap();
                        // log::debug!("BG3HOFS/BG4VOFS: {}x{}", bg3hofs, bg3vofs);

                        let texture = ctx.load_texture(
                            "my-image",
                            egui::ColorImage::from_rgba_unmultiplied(
                                [emu_fb.width, emu_fb.height],
                                &emu_fb.fb,
                            ),
                            egui::TextureFilter::Nearest,
                        );

                        const EMULATOR_SCALE: f32 = 2.;

                        // sketch code: have the background layer stay fixed in
                        // place relative to the outer window

                        egui::Area::new("emulator")
                            .fixed_pos((
                                (bg3hofs as f32).mul_add(EMULATOR_SCALE, 0.),
                                (bg3vofs as f32).mul_add(EMULATOR_SCALE, 0.),
                            ))
                            .show(ctx, |ui| {
                                ui.image(
                                    &texture,
                                    texture.size_vec2() * egui::Vec2::splat(EMULATOR_SCALE),
                                );
                            });
                    }

                    egui::Window::new("Draw to skia").show(ctx, |ui| {
                        ScrollArea::horizontal().show(ui, |ui| {
                            let (rect, _) = ui
                                .allocate_exact_size(egui::Vec2::splat(300.0), egui::Sense::drag());
                            ctx.request_repaint();

                            frame += 1;
                            let si = (frame as f32 / 120.0).sin();

                            ui.painter().add(egui::PaintCallback {
                                rect,
                                callback: Arc::new(EguiSkiaPaintCallback::new(move |canvas| {
                                    let center = Point::new(150.0, 150.0);
                                    canvas.save();
                                    canvas.rotate(si * 5.0, Some(center));
                                    let mut paint = Paint::default();
                                    paint.set_color(Color::from_argb(255, 255, 255, 255));
                                    canvas.draw_str(
                                        "Hello Skia!",
                                        Point::new(100.0, 150.0),
                                        &Font::default().with_size(20.0).unwrap(),
                                        &paint,
                                    );

                                    let mut circle_paint = Paint::default();
                                    circle_paint.set_path_effect(PathEffect::dash(
                                        &[si.mul_add(5.0, 20.0), si.mul_add(5.0, 20.0)],
                                        1.0,
                                    ));
                                    circle_paint.set_style(skia_safe::PaintStyle::Stroke);
                                    circle_paint.set_stroke_width(10.0);
                                    circle_paint.set_stroke_cap(skia_safe::PaintCap::Round);
                                    circle_paint.set_color(Color::WHITE);
                                    canvas.draw_circle(center, 100.0, &circle_paint);
                                })),
                            })
                        });
                    });
                });

                surface.canvas().clear(Color::TRANSPARENT);
                backend.paint(&mut surface);

                let mut fb = fb.lock();
                let ok = surface.image_snapshot().read_pixels(
                    &image_info,
                    fb.as_mut_slice(),
                    SCREEN_WIDTH * 4,
                    (0, 0),
                    skia_safe::image::CachingHint::Disallow,
                );
                assert!(ok);

                done.send(()).unwrap();
            }
        }
    }
}
