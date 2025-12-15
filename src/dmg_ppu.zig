const std = @import("std");
const DmgBus = @import("dmg_bus.zig").DmgBus;

pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;
const PALETTE = [_]u32{ 
        0xFFE0F8D0, // White
        0xFF88C070, // Light Gray
        0xFF346856, // Dark Gray
        0xFF081820  // Black
    };

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

        if ((lcdc & 0x01) == 0) return; // BG display?

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

        if ((lcdc & 0x02) == 0) return; // OBJ display?

        // bit 2: OBJ size (0=8x8, 1=8x16)
        const use_8x16 = (lcdc & 0x04) != 0;
        const sprite_height: u8 = if (use_8x16) 16 else 8;

        // OAM is at 0xFE00. there are 40 sprites (4 bytes each).
        // we iterate backwards to handle priority simplisticly (or standard order)
        // Game Boy can only draw 10 sprites per line, but for simplicity's sake
        // we just check them all.
        for (0..40) |i| {
            // OAM Layout:
            // byte 0: Y position + 16
            // byte 1: X position + 8
            // byte 2: tile index
            // byte 3: attributes / flags
            const addr = 0xFE00 + (@as(u16, @intCast(i)) * 4);

            const sprite_y = self.bus.mem_raw[addr] -% 16;
            const sprite_x = self.bus.mem_raw[addr + 1] -% 8;
            var tile_index = self.bus.mem_raw[addr + 2];
            const flags = self.bus.mem_raw[addr + 3];

            // is this sprite on the current scanline (ly)?
            if (ly >= sprite_y and ly < (sprite_y + sprite_height)) {
                var line_in_sprite = ly - sprite_y; // which line of the sprite to draw (0-7 or 0-15)

                // handle Y-flip (Bit 6)
                if ((flags & 0x40) != 0) {
                    line_in_sprite = sprite_height - 1 - line_in_sprite;
                }

                // for 8x16 sprites, tile index lowest bit is ignored
                if (use_8x16) tile_index &= 0xFE;

                const tile_addr = 0x8000 + (@as(u16, tile_index) * 16);
                const byte1 = self.bus.read(tile_addr + (line_in_sprite * 2));
                const byte2 = self.bus.read(tile_addr + (line_in_sprite * 2) + 1);

                // palette: bit 4 (0=OBP0 at 0xFF48, 1=OBP1 at 0xFF49)
                const palette_reg: u16 = if ((flags & 0x10) != 0) 0xFF49 else 0xFF48;
                const palette_data = self.bus.mem_raw[palette_reg];

                // iterate over the 8 pixels in this row
                // iterate backwards to map bit 7 to x2=0
                var x2: u8 = 0; // "x2" bc shadowing
                while (x2 < 8) : (x2 += 1) {
                    // screen X coordinate
                    const pixel_x = sprite_x +% x2;
                    if (pixel_x >= SCREEN_WIDTH) continue; // boundary

                    // handle X-Flip (bit 5)
                    const bit_index = if ((flags & 0x20) != 0) x2 else (7 - x2);

                    const bit_lo = (byte1 >> @intCast(bit_index)) & 1;
                    const bit_hi = (byte2 >> @intCast(bit_index)) & 1;
                    const color_id = (bit_hi << 1) | bit_lo;

                    // color 0 is transparent for sprites
                    if (color_id == 0) continue;

                    // priority (bit 7): 0=above BG, 1=behind non-zero BG colors
                    // TODO for now we just draw on top but eventually complex drawing logic
                    // could be good here

                    // decode color from Palette
                    const palette_shift = @as(u3, @intCast(color_id * 2));
                    const real_color = (palette_data >> palette_shift) & 0x03;

                    const pixel_idx = (@as(usize, ly) * SCREEN_WIDTH) + pixel_x;
                    self.video_buffer[pixel_idx] = PALETTE[real_color];
                }
            }
        }
    }
};
