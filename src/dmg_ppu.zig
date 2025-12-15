const std = @import("std");
const DmgBus = @import("dmg_bus.zig").DmgBus;

pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;
const PALETTE = [_]u32{ 0xE0F8D0, 0x88C070, 0x346856, 0x081820 };

pub const DmgPpu = struct {
    bus: *DmgBus,
    video_buffer: [SCREEN_WIDTH * SCREEN_HEIGHT]u32 = undefined,
    mode_clock: u16 = 0, // previously called line_dots

    pub fn init(bus: *DmgBus) DmgPpu {
        return DmgPpu{
            .bus = bus,
            .video_buffer = std.mem.zeroes([SCREEN_WIDTH * SCREEN_HEIGHT]u32),
        };
    }

    pub fn step(self: *DmgPpu, cycles: u16) void {
        const lcdc = self.bus.mem_raw[0xFF40];
        // lcd enable; if 0 PPU is off
        if ((lcdc & 0x80) == 0) {
            self.mode_clock = 0;
            self.bus.mem_raw[0xFF44] = 0; // LY = 0
            self.bus.mem_raw[0xFF41] &= 0xFC; // mode 0
            return;
        }

        self.mode_clock += cycles;
        var ly = self.bus.mem_raw[0xFF44];
        var mode = self.bus.mem_raw[0xFF41] & 0x03; // read current mode from STAT register

        // state machine
        if (mode == 2) { // OAM scan (80 dots)
            if (self.mode_clock >= 80) {
                self.mode_clock -= 80;
                mode = 3; // enter drawing mode
            }
        } else if (mode == 3) { // pixel transfer (172 dots approx)
            if (self.mode_clock >= 172) {
                self.mode_clock -= 172;
                mode = 0; // enter HBlank

                // end of scanline: render this line to the buffer
                self.render_scanline(ly);
            }
        } else if (mode == 0) { // HBlank (204 dots approx)
            if (self.mode_clock >= 204) {
                self.mode_clock -= 204;
                ly += 1;

                if (ly == 144) {
                    mode = 1; // enter VBlank
                    // request VBlank interrupt (Bit 0 of IF)
                    self.bus.mem_raw[0xFF0F] |= 0x01;
                } else {
                    mode = 2; // back to OAM scan for next line
                }
            }
        } else if (mode == 1) { // VBlank (4560 dots total, 10 lines)
            if (self.mode_clock >= 456) {
                self.mode_clock -= 456;
                ly += 1;

                if (ly > 153) {
                    mode = 2; // restart frame
                    ly = 0;
                }
            }
        }

        // update registers
        self.bus.mem_raw[0xFF44] = ly;
        // update STAT (keep top bits, set mode bits 1-0)
        self.bus.mem_raw[0xFF41] = (self.bus.mem_raw[0xFF41] & 0xFC) | mode;

        // TODO check for LY=LYC STAT interrupts here
    }

    fn render_scanline(self: *DmgPpu, ly: u8) void {
        const lcdc = self.bus.mem_raw[0xFF40];

        if ((lcdc & 0x01) == 0) return; // bit 0 is BG display yes/no

        const scy = self.bus.mem_raw[0xFF42]; // scroll Y
        const scx = self.bus.mem_raw[0xFF43]; // scroll X
        const bgp = self.bus.mem_raw[0xFF47]; // BG palette data

        // which tile map to use (bit 3: 0=9800, 1=9C00)
        const map_base: u16 = if ((lcdc & 0x08) != 0) 0x9C00 else 0x9800;
        // which tile data to use (bit 4: 0=8800, 1=8000)
        // 0x8000 mode uses unsigned indices (0..255)
        // 0x8800 mode uses signed indices (-128..127)
        const use_unsigned_data = (lcdc & 0x10) != 0;
        const data_base: u16 = if (use_unsigned_data) 0x8000 else 0x9000; // (0x9000 is center of 8800 block)

        const y_pos = ly +% scy; // wrap around 256
        const tile_row = y_pos / 8; // which row of tile is this? (tile is 8px)
        const tile_y_off = y_pos % 8; // which line inside the tile is this?

        var x: u16 = 0;
        while (x < SCREEN_WIDTH) : (x += 1) {
            const x_pos = @as(u8, @intCast(x)) +% scx;
            const tile_col = x_pos / 8;
            const tile_x_off = x_pos % 8;

            // tile id from map
            const map_addr = map_base + ((@as(u16, tile_row) * 32) + tile_col);
            const tile_id = self.bus.read(map_addr);

            // tile data addr
            var tile_addr: u16 = 0;
            if (use_unsigned_data) {
                tile_addr = data_base + (@as(u16, tile_id) * 16);
            } else {
                // signed mapping: ID 0-127 is 0x9000-0x97FF, ID 128-255 is 0x8800-0x8FFF
                const signed_id = @as(i8, @bitCast(tile_id));
                tile_addr = @as(u16, @intCast(@as(i32, data_base) + (@as(i32, signed_id) * 16)));
            }

            // read 2 bytes of data for specific line (tile_y_off)
            // each line is 2 bytes (16 bits) representing 8 pixels
            const byte1 = self.bus.read(tile_addr + (tile_y_off * 2));
            const byte2 = self.bus.read(tile_addr + (tile_y_off * 2) + 1);

            // decode color bit (bit 7 is pixel 0, bit 0 is pixel 7)
            const shift = @as(u3, @intCast(7 - tile_x_off));
            const bit_lo = (byte1 >> shift) & 1;
            const bit_hi = (byte2 >> shift) & 1;
            const color_id = (bit_hi << 1) | bit_lo; // 0, 1, 2, or 3

            // apply palette (BGP register)
            // BGP bits: 7-6 (color 3), 5-4 (color 2), 3-2 (color 1), 1-0 (color 0)
            const palette_shift = @as(u3, @intCast(color_id * 2));
            const real_color = (bgp >> palette_shift) & 0x03;

            // write to buffer
            const pixel_index = (@as(usize, ly) * SCREEN_WIDTH) + x;
            self.video_buffer[pixel_index] = PALETTE[real_color];
        }
    }
};
