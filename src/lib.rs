#![allow(unused_variables)]

use once_cell::sync::Lazy;
use parking_lot::Mutex;

#[allow(non_upper_case_globals, non_camel_case_types, dead_code)]
mod sys;
use std::ffi::c_void;
use sys::*;

mod memory_map;

static DYLIB: Lazy<Mutex<libloading::Library>> = Lazy::new(|| {
    // TODO: move this to a better place? also, integrate with libretro logging?
    pretty_env_logger::init();

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
fwd! { fn retro_set_input_state(arg1: retro_input_state_t); }
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
struct CoreCallbacks {
    env: retro_environment_t,
    video: retro_video_refresh_t,
}

static LIBRETRO_CALLBACKS: Lazy<Mutex<CoreCallbacks>> = Lazy::new(|| {
    Mutex::new(CoreCallbacks {
        env: None,
        video: None,
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
    static ORIG_FRAMEBUFFER_XRGB8888: Lazy<Mutex<Vec<u8>>> = Lazy::new(|| Mutex::new(Vec::new()));

    // normalize original framebuffer into xrgb8888
    let orig_fb_xrgb_8888 = {
        let mut fb = ORIG_FRAMEBUFFER_XRGB8888.lock();
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

    // for now, use our own framebuffer.
    //
    // TODO: look into what kinds of advanced render targets libretro provides?
    // I really don't want to roll my own drawing primitives on-top of a raw
    // framebuffer...
    static WIDE_FRAMEBUFFER: Lazy<Mutex<Vec<u8>>> = Lazy::new(|| Mutex::new(Vec::new()));
    let mut fb = WIDE_FRAMEBUFFER.lock();
    if fb.len() != SCREEN_WIDTH * SCREEN_HEIGHT * 4 {
        fb.resize(SCREEN_WIDTH * SCREEN_HEIGHT * 4, 0);
        // test pattern
        for (i, b) in fb.iter_mut().enumerate() {
            *b = i as u8;
        }
    }

    // quick test that copies the original framebuffer into the larger one
    let orig_rows = orig_fb_xrgb_8888.chunks_exact(orig_width * 4);
    for (dst_row, src_row) in fb.chunks_mut(SCREEN_WIDTH * 4).skip(100).zip(orig_rows) {
        dst_row[4 * 100..][..orig_width * 4].copy_from_slice(src_row)
    }

    unsafe {
        (LIBRETRO_CALLBACKS.lock().video.unwrap())(
            fb.as_ptr() as *const c_void,
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
            SCREEN_WIDTH * 4,
        )
    }
}

const SCREEN_WIDTH: usize = 1280;
const SCREEN_HEIGHT: usize = 720;

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

unsafe extern "C" fn retro_environment_shim(cmd: u32, data: *mut ::std::os::raw::c_void) -> bool {
    let ret;

    // helper to simplify base-case of passing through commands
    macro_rules! invoke_env_cb {
        () => {
            ret = unsafe { (LIBRETRO_CALLBACKS.lock().env.unwrap())(cmd, data) }
        };
    }

    match retro_environment(cmd) {
        // ==== Libretro harness ==== //
        retro_environment::GET_LOG_INTERFACE => {
            invoke_env_cb!();
            log::info!("intercepted env:GET_LOG_INTERFACE");
        }

        // ==== Memory Map //
        retro_environment::SET_MEMORY_MAPS => {
            let map = data as *const retro_memory_map;
            let memory_map = unsafe { memory_map::MemoryMap::from_retro_memory_map(map) };

            log::info!("intercepted env:SET_MEMORY_MAPS: {:#?}", memory_map);

            CORE_STATE.lock().memory_map = Some(memory_map);

            // pass things along...
            invoke_env_cb!();
        }

        // ==== Audio ==== //
        retro_environment::SET_AUDIO_BUFFER_STATUS_CALLBACK => {
            log::info!("intercepted env:SET_AUDIO_BUFFER_STATUS_CALLBACK");
            invoke_env_cb!();
        }
        retro_environment::SET_MINIMUM_AUDIO_LATENCY => {
            log::info!("intercepted env:SET_MINIMUM_AUDIO_LATENCY");
            invoke_env_cb!();
        }

        // ==== Video ==== //
        retro_environment::SET_PIXEL_FORMAT => {
            let pixel_format = data as *mut i32;
            let pixel_format_orig = match unsafe { *pixel_format } {
                0 => retro_pixel_format::_0RGB1555,
                1 => retro_pixel_format::_XRGB8888,
                2 => retro_pixel_format::_RGB565,
                _ => retro_pixel_format::_UNKNOWN,
            };
            log::info!("intercepted env:SET_PIXEL_FORMAT: {:?}", pixel_format);
            CORE_STATE.lock().pixel_format = Some(pixel_format_orig);

            // normalize underlying framebuffers to use XRGB8888
            unsafe { *pixel_format = retro_pixel_format::_XRGB8888 as i32 };

            invoke_env_cb!();
        }

        // ==== Input ==== //
        retro_environment::GET_INPUT_BITMASKS => {
            invoke_env_cb!();
            log::info!("intercepted env:GET_INPUT_BITMASKS");
        }
        retro_environment::SET_INPUT_DESCRIPTORS => {
            log::info!("intercepted env:SET_INPUT_DESCRIPTORS");
            invoke_env_cb!();
        }

        // ==== Variables ==== //
        retro_environment::GET_CORE_OPTIONS_VERSION => {
            invoke_env_cb!();
            let version = data as *const u32;
            let version = unsafe { *version };
            log::info!("intercepted env:GET_CORE_OPTIONS_VERSION: {}", version);
        }
        retro_environment::GET_VARIABLE_UPDATE => {
            // _silently_ pass this right on through, to avoid logspam
            invoke_env_cb!();
        }
        retro_environment::GET_VARIABLE => {
            // call the callback to get the data, then inspect it
            invoke_env_cb!();

            let var = unsafe { *(data as *const retro_variable) };
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
        }
        retro_environment::SET_VARIABLES => {
            let mut vars: Vec<(String, String)> = Vec::new();

            let mut p = data as *const retro_variable;
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
            invoke_env_cb!();
        }
        retro_environment::SET_CORE_OPTIONS_V2_INTL => {
            log::info!("intercepted env:SET_CORE_OPTIONS_V2_INTL");
            invoke_env_cb!();
        }

        // ==== Other ==== //
        retro_environment::GET_SYSTEM_DIRECTORY => {
            invoke_env_cb!();

            let s = data as *mut *const i8;
            let dir = unsafe { std::ffi::CStr::from_ptr(*s) };
            log::info!("intercepted env:GET_SYSTEM_DIRECTORY: {:?}", dir);
        }
        retro_environment::GET_LANGUAGE => {
            invoke_env_cb!();
            log::info!("intercepted env:GET_LANGUAGE");
        }
        retro_environment::SET_SUPPORT_ACHIEVEMENTS => {
            let support_achievements = data as *const bool;
            let support_achievements = unsafe { *support_achievements };
            log::info!(
                "intercepted env:SET_SUPPORT_ACHIEVEMENTS: {}",
                support_achievements
            );

            invoke_env_cb!();
        }

        // pass the rest right on through
        _ => {
            log::warn!("forwarded unknown env:{:?}", retro_environment(cmd));
            invoke_env_cb!();
        }
    };

    ret
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn retro_run() {
    {
        let dylib = DYLIB.lock();
        let f: libloading::Symbol<'_, unsafe extern "C" fn() -> ()> =
            unsafe { dylib.get("retro_run".as_bytes()).unwrap() };
        unsafe { (f)() };
    }

    let core_state = CORE_STATE.lock();

    log::warn!(
        "BG3VOFS: {}",
        core_state
            .memory_map
            .as_ref()
            .unwrap()
            .read_u16(0x1E)
            .unwrap()
    );
}
