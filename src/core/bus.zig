const std = @import("std");
const Cartridge = @import("cartridge.zig").Cartridge;
const Timer = @import("timer.zig").Timer;
const Joypad = @import("joypad.zig").Joypad;
// use a placeholder for PPU for now, or the existing DmgPpu
// ideally, PPU should also move to src/core/ppu.zig

pub const Bus = struct {
    // memory
    wram: [0x2000]u8 = undefined, // 8KB work RAM
    hram: [0x80]u8 = undefined, // high RAM
    ie: u8 = 0, // Interrupt enable
    if_reg: u8 = 0, // Interrupt Flag (0xFF0F)

    // components
    cartridge: Cartridge,
    timer: Timer,
    joypad: Joypad,

    // we hold raw memory for VRAM/OAM/IO until PPU is fully decoupled
    // TODO VRAM goes to the PPU struct.
    vram: [0x2000]u8 = undefined,
    oam: [0xA0]u8 = undefined,
    io_regs: [0x80]u8 = undefined, // fallback for unimplemented IO

    pub fn init(allocator: std.mem.Allocator, rom_data: []const u8) !Bus {
        return Bus{
            .cartridge = try Cartridge.init(allocator, rom_data),
            .timer = Timer.init(),
            .joypad = Joypad.init(),
            .wram = std.mem.zeroes([0x2000]u8),
            .hram = std.mem.zeroes([0x80]u8),
            .vram = std.mem.zeroes([0x2000]u8),
            .oam = std.mem.zeroes([0xA0]u8),
            .io_regs = std.mem.zeroes([0x80]u8),
        };
    }

    pub fn deinit(self: *Bus) void {
        self.cartridge.deinit();
    }

    pub fn read(self: *const Bus, addr: u16) u8 {
        switch (addr) {
            0x0000...0x7FFF => return self.cartridge.read(addr),
            0x8000...0x9FFF => return self.vram[addr - 0x8000],
            0xA000...0xBFFF => return self.cartridge.read(addr), // RAM banks
            0xC000...0xDFFF => return self.wram[addr - 0xC000],
            0xE000...0xFDFF => return self.wram[addr - 0xE000], // echo RAM
            0xFE00...0xFE9F => return self.oam[addr - 0xFE00],
            0xFEA0...0xFEFF => return 0xFF,

            // IO registers
            0xFF00 => return self.joypad.read(),
            0xFF04...0xFF07 => return self.timer.read(addr),
            0xFF0F => return self.if_reg,
            0xFF01...0xFF03, 0xFF08...0xFF0E, 0xFF10...0xFF7F => return self.io_regs[addr - 0xFF00], // PPU regs & others

            0xFF80...0xFFFE => return self.hram[addr - 0xFF80],
            0xFFFF => return self.ie,
        }
    }

    pub fn write(self: *Bus, addr: u16, value: u8) void {
        switch (addr) {
            0x0000...0x7FFF => self.cartridge.write(addr, value),
            0x8000...0x9FFF => self.vram[addr - 0x8000] = value,
            0xA000...0xBFFF => self.cartridge.write(addr, value),
            0xC000...0xDFFF => self.wram[addr - 0xC000] = value,
            0xE000...0xFDFF => self.wram[addr - 0xE000] = value,
            0xFE00...0xFE9F => self.oam[addr - 0xFE00] = value,
            0xFEA0...0xFEFF => {},

            // IO
            0xFF00 => self.joypad.write(value),
            0xFF04...0xFF07 => self.timer.write(addr, value),
            0xFF0F => self.if_reg = value,
            0xFF46 => self.dma_transfer(value),
            0xFF01...0xFF03, 0xFF08...0xFF0E, 0xFF10...0xFF45, 0xFF47...0xFF7F => self.io_regs[addr - 0xFF00] = value,

            0xFF80...0xFFFE => self.hram[addr - 0xFF80] = value,
            0xFFFF => self.ie = value,
        }
    }

    fn dma_transfer(self: *Bus, value: u8) void {
        const start_addr = @as(u16, value) << 8;
        for (0..0xA0) |i| {
            const offset = @as(u16, @intCast(i));
            const byte = self.read(start_addr + offset);
            self.oam[offset] = byte;
        }
        // write register itself
        self.io_regs[0x46] = value;
    }
};
