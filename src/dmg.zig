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

pub const DmgCpu = struct {
    cycles: u128 = 0,

    // 8-bit general purpose registers
    a: u8, // accumulator
    f: u8, // flags
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    // 16-bit control registers
    sp: u16, // stack pointer
    pc: u16, // program counter

    // flag bitmasks
    pub const FLAG_Z: u8 = 0b1000_0000; // zero flag
    pub const FLAG_N: u8 = 0b0100_0000; // subtract flag
    pub const FLAG_H: u8 = 0b0010_0000; // half carry flag
    pub const FLAG_C: u8 = 0b0001_0000; // carry flag

    pub fn init() DmgCpu {
        return DmgCpu{
            // initial values based for DMG boot state
            .a = 0x01, // 0x01 for DMG, 0x11 for CGB
            .f = 0xB0,
            .b = 0x00,
            .c = 0x13,
            .d = 0x00,
            .e = 0xD8,
            .h = 0x01,
            .l = 0x4D,
            .sp = 0xFFFE, // defaults to 0xFFFE on start
            .pc = 0x0100, // entry point usually 0x100
        };
    }

    // --- 16-bit Register Accessors (Pairing) ---
    // The Game Boy is Little Endian, but the register pairs are
    // defined as High-Low (e.g., B is High, C is Low).

    // AF pair (accumulator & flags)
    pub fn get_af(self: *const DmgCpu) u16 {
        return (@as(u16, self.a) << 8) | @as(u16, self.f);
    }

    pub fn set_af(self: *DmgCpu, value: u16) void {
        self.a = @truncate(value >> 8);
        // lower 4 bits of F should always be 0
        self.f = @truncate(value & 0xF0);
    }

    // BC pair
    pub fn get_bc(self: *const DmgCpu) u16 {
        return (@as(u16, self.b) << 8) | @as(u16, self.c);
    }

    pub fn set_bc(self: *DmgCpu, value: u16) void {
        self.b = @truncate(value >> 8);
        self.c = @truncate(value);
    }

    // DE pair
    pub fn get_de(self: *const DmgCpu) u16 {
        return (@as(u16, self.d) << 8) | @as(u16, self.e);
    }

    pub fn set_de(self: *DmgCpu, value: u16) void {
        self.d = @truncate(value >> 8);
        self.e = @truncate(value);
    }

    // HL pair
    // note: usually used for memory addressing
    pub fn get_hl(self: *const DmgCpu) u16 {
        return (@as(u16, self.h) << 8) | @as(u16, self.l);
    }

    pub fn set_hl(self: *DmgCpu, value: u16) void {
        self.h = @truncate(value >> 8);
        self.l = @truncate(value);
    }

    pub fn get_flag(self: *const DmgCpu, mask: u8) bool {
        return (self.f & mask) != 0;
    }

    pub fn set_flag(self: *DmgCpu, mask: u8, condition: bool) void {
        if (condition) {
            self.f |= mask;
        } else {
            self.f &= ~mask;
        }
    }
};
