#![allow(unused_variables)]

use once_cell::sync::Lazy;
use parking_lot::Mutex;

#[allow(non_upper_case_globals, non_camel_case_types, dead_code)]
mod sys;
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

fwd! { fn retro_set_video_refresh(arg1: retro_video_refresh_t); }
fwd! { fn retro_set_audio_sample(arg1: retro_audio_sample_t); }
fwd! { fn retro_set_audio_sample_batch(arg1: retro_audio_sample_batch_t); }
fwd! { fn retro_set_input_poll(arg1: retro_input_poll_t); }
fwd! { fn retro_set_input_state(arg1: retro_input_state_t); }
fwd! { fn retro_init(); }
fwd! { fn retro_deinit(); }
fwd! { fn retro_api_version() -> u32; }
fwd! { fn retro_get_system_info(arg1:  *mut retro_system_info); }
fwd! { fn retro_get_system_av_info(info: *mut retro_system_av_info); }
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

/// Container for any callbacks the frontend hands the core.
struct CoreCallbacks {
    env: retro_environment_t,
}

static LIBRETRO_CALLBACKS: Lazy<Mutex<CoreCallbacks>> =
    Lazy::new(|| Mutex::new(CoreCallbacks { env: None }));

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn retro_set_environment(cb: retro_environment_t) {
    log::trace!("intercepted + shim'd retro_set_environment");

    // stash the callback we are given
    LIBRETRO_CALLBACKS.lock().env = cb;

    // ...and hand off the shim'd callback to the underlying core
    let dylib = DYLIB.lock();
    let f: libloading::Symbol<'_, unsafe extern "C" fn(arg1: retro_environment_t) -> ()> =
        unsafe { dylib.get("retro_set_environment".as_bytes()).unwrap() };
    unsafe { (f)(Some(retro_environment_shim)) }
}

/// Container for bits of interesting info the core sends up to the frontend.
struct CoreState {
    memory_map: Option<memory_map::MemoryMap>,
}

static CORE_STATE: Lazy<Mutex<CoreState>> =
    Lazy::new(|| Mutex::new(CoreState { memory_map: None }));

#[inline(never)]
unsafe extern "C" fn retro_environment_shim(cmd: u32, data: *mut ::std::os::raw::c_void) -> bool {
    match retro_environment(cmd) {
        retro_environment::RETRO_ENVIRONMENT_SET_MEMORY_MAPS => {
            let map = data as *const retro_memory_map;
            let memory_map = unsafe { memory_map::MemoryMap::from_retro_memory_map(map) };

            log::info!("intercepted env:SET_MEMORY_MAPS: {:#?}", memory_map);

            CORE_STATE.lock().memory_map = Some(memory_map);

            // pass things along...
            unsafe { (LIBRETRO_CALLBACKS.lock().env.unwrap())(cmd, data) }
        }
        retro_environment::RETRO_ENVIRONMENT_GET_VARIABLE => {
            // call the callback to get the data, then inspect it
            let ret = unsafe { (LIBRETRO_CALLBACKS.lock().env.unwrap())(cmd, data) };

            let null_string = unsafe { std::ffi::CStr::from_ptr(b"NULL\0".as_ptr() as _) };

            let var = unsafe { *(data as *const retro_variable) };
            let key = {
                if !var.key.is_null() {
                    unsafe { std::ffi::CStr::from_ptr(var.key) }
                } else {
                    null_string
                }
            };
            let value = {
                if !var.key.is_null() {
                    unsafe { std::ffi::CStr::from_ptr(var.value) }
                } else {
                    null_string
                }
            };

            log::trace!("intercepted env:GET_VARIABLE: {:?}={:?}", key, value);

            ret
        }
        retro_environment::RETRO_ENVIRONMENT_GET_VARIABLE_UPDATE => {
            // _silently_ pass this right on through, to avoid logspam
            unsafe { (LIBRETRO_CALLBACKS.lock().env.unwrap())(cmd, data) }
        }
        // just pass it right on through
        _ => {
            log::trace!("forwarded env:{:?}", retro_environment(cmd));
            unsafe { (LIBRETRO_CALLBACKS.lock().env.unwrap())(cmd, data) }
        }
    }
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
