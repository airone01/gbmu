const std = @import("std");

const mem_bytes = 0x10000;

/// DMG memory bus implementation
///
/// Note: area from 0xE000 to 0xFDFF is actually echo RAM in DMG.
/// Meaning the expected behavior when writing to 0xC000 is the same value
/// appearing at 0xE000.
/// This is a problem because for now we're accessing the data directly with the slices,
/// but eventually if we want to "translate" memory access, we will need to
/// implement a function `read(addr)` or something similar.
pub const DmgBus = struct {
    /// general memory buffer
    mem_raw: [mem_bytes]u8 = undefined,

    const ROM_START = 0x0000;
    const ROM_END = 0x8000; // Exclusive (length is 0x8000)

    const VRAM_START = 0x8000;
    const VRAM_END = 0xA000;

    const ERAM_START = 0xA000;
    const ERAM_END = 0xC000;

    const WRAM_START = 0xC000;
    const WRAM_END = 0xE000;

    const OAM_START = 0xFE00;
    const OAM_END = 0xFEA0; // 0xFE9F inclusive -> 0xFEA0 exclusive

    const IO_START = 0xFF00;
    const HRAM_START = 0xFF80;

    pub fn init() DmgBus {
        return DmgBus{
            .mem_raw = std.mem.zeroes([mem_bytes]u8),
        };
    }

    /// 0x0000-0x7FFF: Program Area (32KB)
    pub fn getRom(self: *DmgBus) []u8 {
        return self.raw[ROM_START..ROM_END];
    }

    /// 0x0100-0x014F: Cartridge Header Info
    pub fn getCartHeader(self: *DmgBus) []u8 {
        return self.raw[0x0100..0x0150];
    }

    /// 0x8000-0x9FFF: Video RAM (8KB window)
    pub fn getVram(self: *DmgBus) []u8 {
        return self.raw[VRAM_START..VRAM_END];
    }

    /// 0xA000-0xBFFF: External Expansion RAM
    pub fn getExtRam(self: *DmgBus) []u8 {
        return self.raw[ERAM_START..ERAM_END];
    }

    /// 0xC000-0xDFFF: Work RAM (8KB window)
    /// Covers both fixed Bank 0 and the switchable Bank 1-7 slot.
    pub fn getWram(self: *DmgBus) []u8 {
        return self.raw[WRAM_START..WRAM_END];
    }

    /// 0xFE00-0xFE9F: OAM (Object Attribute Memory)
    /// Holds 40 objects (4 bytes each = 160 bytes)
    pub fn getOam(self: *DmgBus) []u8 {
        return self.raw[OAM_START..OAM_END];
    }

    /// 0xFF80-0xFFFE: High RAM (Stack/Zero-page)
    pub fn getHram(self: *DmgBus) []u8 {
        return self.raw[HRAM_START..0xFFFF]; // Excludes IE register at 0xFFFF
    }
};

pub const DmbCpu = struct {
    totalCycles: u128 = 0,
};
