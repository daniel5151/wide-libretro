use crate::sys::retro_memory_descriptor;
use crate::sys::retro_memory_map;

pub struct MemoryDescriptor {
    pub flags: u64,
    // technically, this is &'core, but we'll just play fast and loose with
    // lifetimes right now...
    pub buf: Option<&'static [u8]>,
    pub len: usize,
    pub start: usize,
    pub select: usize,
    pub disconnect: usize,
    pub addrspace: Option<String>,
}

impl std::fmt::Debug for MemoryDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MemoryDescriptor")
            .field("flags", &self.flags)
            .field("buf", &format!("[..; {:#x?}]", self.len))
            .field("len", &self.len)
            .field("start", &self.start)
            .field("select", &self.select)
            .field("disconnect", &self.disconnect)
            .field("addrspace", &self.addrspace)
            .finish()
    }
}

impl MemoryDescriptor {
    pub unsafe fn from_retro_memory_descriptor(desc: &retro_memory_descriptor) -> MemoryDescriptor {
        MemoryDescriptor {
            flags: desc.flags,
            buf: if desc.ptr.is_null() {
                None
            } else {
                unsafe {
                    Some(std::slice::from_raw_parts(
                        (desc.ptr as *const u8).add(desc.offset as usize),
                        desc.len,
                    ))
                }
            },
            len: desc.len,
            start: desc.start,
            select: desc.select,
            disconnect: desc.disconnect,
            addrspace: if desc.addrspace.is_null() {
                None
            } else {
                let s = unsafe { std::ffi::CStr::from_ptr(desc.addrspace) };
                Some(s.to_string_lossy().to_string())
            },
        }
    }
}

#[derive(Debug)]
pub struct MemoryMap {
    descriptors: Vec<MemoryDescriptor>,
}

impl MemoryMap {
    pub fn from_retro_memory_map(map: &retro_memory_map) -> MemoryMap {
        assert!(!map.descriptors.is_null());

        let descriptors =
            unsafe { std::slice::from_raw_parts(map.descriptors, map.num_descriptors as usize) };

        let descriptors = descriptors
            .iter()
            .map(|desc| unsafe { MemoryDescriptor::from_retro_memory_descriptor(desc) })
            .collect();

        let descriptors = preprocess_descriptors::process(descriptors);

        MemoryMap { descriptors }
    }

    pub fn read_u16(&self, addr: usize) -> Option<u16> {
        assert!((0x4000000..=0x4000400).contains(&addr));
        let addr = addr - 0x4000000;
        // TODO: actually respect addr lol
        Some(u16::from_le_bytes(
            self.descriptors[10].buf.unwrap()[addr..][..2]
                .try_into()
                .unwrap(),
        ))
    }
}

// yoinked from
// https://github.com/libretro/RetroArch/blob/e9914d660551f97ea23ecc3c8e0c57e77526b20e/runloop.c#L957
//
// not sure if it's useful or anything, but while testing I transcribed it, so I
// might as well leave it in...
mod preprocess_descriptors {
    use super::MemoryDescriptor;

    pub fn process(mut descriptors: Vec<MemoryDescriptor>) -> Vec<MemoryDescriptor> {
        let mut top_addr = 1;

        for desc in descriptors.iter_mut() {
            if desc.select != 0 {
                top_addr |= desc.select
            } else {
                top_addr |= desc.start + desc.len - 1
            }
        }

        top_addr = mmap_add_bits_down(top_addr);

        for desc in descriptors.iter_mut() {
            if desc.select == 0 {
                assert!(desc.len != 0);
                assert!(desc.len & (desc.len - 1) == 0);

                desc.select =
                    top_addr & !mmap_inflate(mmap_add_bits_down(desc.len - 1), desc.disconnect);
            }
            assert!((desc.start & !desc.select) == 0);

            if desc.len == 0 {
                desc.len =
                    mmap_add_bits_down(mmap_reduce(top_addr & !desc.select, desc.disconnect)) + 1;
            }

            let highest_reachable = mmap_inflate(desc.len - 1, desc.disconnect);

            while mmap_highest_bit(top_addr & !desc.select & !desc.disconnect)
                > mmap_highest_bit(highest_reachable)
            {
                desc.disconnect |= mmap_highest_bit(top_addr & !desc.select & !desc.disconnect);
            }
        }

        descriptors
    }

    fn mmap_highest_bit(mut n: usize) -> usize {
        n = mmap_add_bits_down(n);
        n ^ (n >> 1)
    }

    fn mmap_add_bits_down(mut n: usize) -> usize {
        n |= n >> 1;
        n |= n >> 2;
        n |= n >> 4;
        n |= n >> 8;
        n |= n >> 16;
        n |= n >> 32;
        n
    }

    fn mmap_inflate(mut addr: usize, mut mask: usize) -> usize {
        while mask != 0 {
            let tmp = (mask - 1) & !mask;

            addr = ((addr & !tmp) << 1) | (addr & tmp);
            mask = mask & (mask - 1);
        }
        addr
    }

    fn mmap_reduce(mut addr: usize, mut mask: usize) -> usize {
        while mask != 0 {
            let tmp = (mask - 1) & !mask;
            addr = (addr & tmp) | ((addr >> 1) & !tmp);
            mask = (mask & (mask - 1)) >> 1;
        }

        addr
    }
}
