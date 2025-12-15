const std = @import("std");

pub const Cartridge = struct {
    rom: []u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, rom_data: []const u8) !Cartridge {
        const rom = try allocator.alloc(u8, rom_data.len);
        @memcpy(rom, rom_data);
        return Cartridge{ .rom = rom, .allocator = allocator };
    }

    pub fn deinit(self: *Cartridge) void {
        self.allocator.free(self.rom);
    }

    pub fn read(self: *const Cartridge, addr: u16) u8 {
        if (addr < self.rom.len) return self.rom[addr];
        return 0xFF;
    }

    pub fn write(self: *Cartridge, addr: u16, val: u8) void {
        // TODO implement MBC here
        _ = self;
        _ = addr;
        _ = val;
    }
};
