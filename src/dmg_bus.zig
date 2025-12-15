const std = @import("std");

const mem_bytes = 0x10000;

/// DMG memory bus implementation
pub const DmgBus = struct {
    /// general memory buffer
    /// we access it with methods
    mem_raw: [mem_bytes]u8 = undefined,

    boot_rom_enabled: bool = true,

    pub fn init() DmgBus {
        return DmgBus{
            .mem_raw = std.mem.zeroes([mem_bytes]u8),
        };
    }

    /// copies ROM data to beginning of memory map
    /// TODO handle bank switching
    pub fn load_rom(self: *DmgBus, rom_data: []const u8) void {
        const len = @min(rom_data.len, 0x8000);
        @memcpy(self.mem_raw[0..len], rom_data[0..len]);
    }

    pub fn read(self: *const DmgBus, addr: u16) u8 {
        switch (addr) {
            // ROM bank 0 & switchable Bank
            0x0000...0x7FFF => {
                // TODO handle boot ROM overlay here if you implement it
                return self.mem_raw[addr];
            },

            // VRAM
            0x8000...0x9FFF => return self.mem_raw[addr],

            // external (cartridge) RAM
            0xA000...0xBFFF => return self.mem_raw[addr],

            // work RAM (WRAM)
            0xC000...0xDFFF => return self.mem_raw[addr],

            // echo RAM: mirrors WRAM (0xC000 - 0xDDFF)
            // 0xE000 maps to 0xC000, 0xE001 to 0xC001, etc.
            0xE000...0xFDFF => return self.mem_raw[addr - 0x2000],

            // OAM (Object Attribute Memory)
            0xFE00...0xFE9F => return self.mem_raw[addr],

            // unusable mem
            0xFEA0...0xFEFF => return 0xFF, // usually returns garbage or 0xFF

            // IO registers
            0xFF00...0xFF7F => {
                // TODO special read logic needed here later (P1, DIV, LY ...)
                return self.mem_raw[addr];
            },

            // HRAM (High RAM) & IE register
            0xFF80...0xFFFF => return self.mem_raw[addr],
        }
    }

    pub fn write(self: *DmgBus, addr: u16, value: u8) void {
        switch (addr) {
            // ROM (0x0000 - 0x7FFF)
            // writing to ROM does not change memory, it sends commands to the MBC.
            0x0000...0x7FFF => {
                // TODO implement MBC banking logic here
                // i.e. if (addr < 0x2000) ram_enable = (value == 0x0A);
                return;
            },

            // VRAM
            0x8000...0x9FFF => self.mem_raw[addr] = value,

            // external (cartridge) RAM
            0xA000...0xBFFF => self.mem_raw[addr] = value,

            // work RAM (WRAM)
            0xC000...0xDFFF => self.mem_raw[addr] = value,

            // echo RAM: write to WRAM
            0xE000...0xFDFF => self.mem_raw[addr - 0x2000] = value,

            // OAM
            0xFE00...0xFE9F => self.mem_raw[addr] = value,

            // unusable mem
            0xFEA0...0xFEFF => {}, // do nothing

            // IO registers
            0xFF00...0xFF7F => {
                // DMA transfer (initiates OAM DMA)
                if (addr == 0xFF46) {
                    self.start_dma_transfer(value);
                    return;
                }

                // DIV register (0xFF04): writing any value resets it to 0
                if (addr == 0xFF04) {
                    self.mem_raw[addr] = 0;
                    return;
                }

                // DMA transfer (0xFF46): initiates OAM DMA
                if (addr == 0xFF46) {
                    self.start_dma_transfer(value);
                    return;
                }

                // LY (0xFF44) is read-only
                // games might write to it to reset it
                // for now, ignoring writes is safe.
                // TODO
                if (addr == 0xFF44) return;

                self.mem_raw[addr] = value;
            },

            // HRAM & IE
            0xFF80...0xFFFF => self.mem_raw[addr] = value,
        }
    }

    /// basic OAM DMA implementation
    /// note: instant transfer for now
    fn start_dma_transfer(self: *DmgBus, value: u8) void {
        // source address is value * 0x100
        const start_addr = @as(u16, value) << 8;

        // transfer 160 bytes (0xA0) to OAM (0xFE00)
        for (0..0xA0) |i| {
            const offset = @as(u16, @intCast(i));
            const byte = self.read(start_addr + offset);
            self.mem_raw[0xFE00 + offset] = byte;
        }
        // write the value to the register itself (usually typical behavior)
        self.mem_raw[0xFF46] = value;
    }
};
