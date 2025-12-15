const std = @import("std");
const Bus = @import("bus.zig").Bus;

pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;

pub const Ppu = struct {
    bus: *Bus,
    video_buffer: [SCREEN_WIDTH * SCREEN_HEIGHT]u32,
    mode_clock: u16 = 0,

    /// internal counter for window rendering
    /// the window doesn't scroll with SCY; it keeps its own line count
    window_line: u8 = 0,

    // (0xAARRGGBB)
    const PALETTE = [_]u32{
        0xFFE0F8D0, // White
        0xFF88C070, // Light Gray
        0xFF346856, // Dark Gray
        0xFF081820, // Black
    };

    pub fn init(bus: *Bus) Ppu {
        return Ppu{
            .bus = bus,
            .video_buffer = std.mem.zeroes([SCREEN_WIDTH * SCREEN_HEIGHT]u32),
        };
    }

    pub fn step(self: *Ppu, cycles: u16) void {
        const lcdc = self.bus.read(0xFF40);

        // bit 7: LCD Enable
        if ((lcdc & 0x80) == 0) {
            self.mode_clock = 0;
            self.bus.write(0xFF44, 0); // LY = 0

            // set mode 0 in STAT
            const stat = self.bus.read(0xFF41);
            self.bus.write(0xFF41, stat & 0xFC);

            self.window_line = 0;
            return;
        }

        self.mode_clock += cycles;
        var ly = self.bus.read(0xFF44);
        const stat = self.bus.read(0xFF41);
        var mode = stat & 0x03;

        // state machine
        if (mode == 2) { // OAM Scan
            if (self.mode_clock >= 80) {
                self.mode_clock -= 80;
                mode = 3;
            }
        } else if (mode == 3) { // Drawing
            if (self.mode_clock >= 172) {
                self.mode_clock -= 172;
                mode = 0;

                self.render_scanline(ly);
            }
        } else if (mode == 0) { // HBlank
            if (self.mode_clock >= 204) {
                self.mode_clock -= 204;
                ly += 1;

                if (ly == 144) {
                    mode = 1; // VBlank
                    // request interrupt
                    self.bus.if_reg |= 0x01;
                    // reset internal window counter at VBlank start
                    self.window_line = 0;
                } else {
                    mode = 2; // OAM Scan
                }
            }
        } else if (mode == 1) { // VBlank
            if (self.mode_clock >= 456) {
                self.mode_clock -= 456;
                ly += 1;

                if (ly > 153) {
                    mode = 2; // restart
                    ly = 0;
                    self.window_line = 0;
                }
            }
        }

        self.bus.write(0xFF44, ly);
        self.bus.write(0xFF41, (stat & 0xFC) | mode);
    }

    fn render_scanline(self: *Ppu, ly: u8) void {
        const lcdc = self.bus.read(0xFF40);

        // bit 0: BG
        if ((lcdc & 0x01) != 0) {
            self.render_background(ly, lcdc);

            // bit 5: window
            if ((lcdc & 0x20) != 0) {
                self.render_window(ly, lcdc);
            }
        }

        // bit 1: OBJ
        if ((lcdc & 0x02) != 0) {
            self.render_sprites(ly, lcdc);
        }
    }

    fn render_background(self: *Ppu, ly: u8, lcdc: u8) void {
        const scy = self.bus.read(0xFF42);
        const scx = self.bus.read(0xFF43);
        const bgp = self.bus.read(0xFF47);

        // bit 3: BG Tile Map Area (0=9800, 1=9C00)
        const map_base: u16 = if ((lcdc & 0x08) != 0) 0x9C00 else 0x9800;

        // bit 4: BG/Window Tile Data Area (0=8800, 1=8000)
        const use_unsigned = (lcdc & 0x10) != 0;
        const data_base: u16 = if (use_unsigned) 0x8000 else 0x9000;

        const y_pos = ly +% scy;
        const tile_row = y_pos / 8;
        const tile_y = y_pos % 8;

        for (0..SCREEN_WIDTH) |x| {
            const x_pos = @as(u8, @intCast(x)) +% scx;
            const tile_col = x_pos / 8;
            const tile_x = x_pos % 8;

            const map_addr = map_base + ((@as(u16, tile_row) * 32) + tile_col);
            const tile_id = self.bus.read(map_addr);

            var tile_addr: u16 = 0;
            if (use_unsigned) {
                tile_addr = data_base + (@as(u16, tile_id) * 16);
            } else {
                const signed_id = @as(i8, @bitCast(tile_id));
                tile_addr = @as(u16, @intCast(@as(i32, data_base) + (@as(i32, signed_id) * 16)));
            }

            const b1 = self.bus.read(tile_addr + (tile_y * 2));
            const b2 = self.bus.read(tile_addr + (tile_y * 2) + 1);

            const bit = @as(u3, @intCast(7 - tile_x));
            const color_id = ((b2 >> bit) & 1) << 1 | ((b1 >> bit) & 1);

            const pal_shift = @as(u3, @intCast(color_id * 2));
            const color = (bgp >> pal_shift) & 0x03;

            self.video_buffer[(@as(usize, ly) * SCREEN_WIDTH) + x] = PALETTE[color];
        }
    }

    fn render_window(self: *Ppu, ly: u8, lcdc: u8) void {
        const wy = self.bus.read(0xFF4A);
        const wx = self.bus.read(0xFF4B);

        // window not visible on this line
        if (ly < wy) return;
        if (wx >= 167) return; // WX=7 means x=0 so WX=167 is off-screen

        // bit 6: window tile map Area
        const map_base: u16 = if ((lcdc & 0x40) != 0) 0x9C00 else 0x9800;
        const use_unsigned = (lcdc & 0x10) != 0;
        const data_base: u16 = if (use_unsigned) 0x8000 else 0x9000;

        // window coordinates
        const y_pos = self.window_line;
        const tile_row = y_pos / 8;
        const tile_y = y_pos % 8;
        const bgp = self.bus.read(0xFF47);

        // iterate x from 0 to 159
        // window starts at WX-7
        const start_x: i16 = @as(i16, wx) - 7;

        for (0..SCREEN_WIDTH) |x| {
            if (@as(i16, @intCast(x)) < start_x) continue;

            const win_x = @as(u16, @intCast(@as(i16, @intCast(x)) - start_x));
            const tile_col = win_x / 8;
            const tile_x = win_x % 8;

            const map_addr = map_base + ((@as(u16, tile_row) * 32) + tile_col);
            const tile_id = self.bus.read(map_addr);

            var tile_addr: u16 = 0;
            if (use_unsigned) {
                tile_addr = data_base + (@as(u16, tile_id) * 16);
            } else {
                const signed_id = @as(i8, @bitCast(tile_id));
                tile_addr = @as(u16, @intCast(@as(i32, data_base) + (@as(i32, signed_id) * 16)));
            }

            const b1 = self.bus.read(tile_addr + (tile_y * 2));
            const b2 = self.bus.read(tile_addr + (tile_y * 2) + 1);

            const bit = @as(u3, @intCast(7 - tile_x));
            const color_id = ((b2 >> bit) & 1) << 1 | ((b1 >> bit) & 1);

            const pal_shift = @as(u3, @intCast(color_id * 2));
            const color = (bgp >> pal_shift) & 0x03;

            self.video_buffer[(@as(usize, ly) * SCREEN_WIDTH) + x] = PALETTE[color];
        }

        // only increment internal window line counter if window was actually rendered
        self.window_line += 1;
    }

    fn render_sprites(self: *Ppu, ly: u8, lcdc: u8) void {
        const use_8x16 = (lcdc & 0x04) != 0;
        const height: u8 = if (use_8x16) 16 else 8;

        // iterate sprites
        for (0..40) |i| {
            // OAM is at FE00. each sprite is 4 bytes.
            // using Bus.read instead of direct array for cleanliness,
            // though PPU technically owns OAM logic in this architecture.
            const addr = 0xFE00 + (@as(u16, @intCast(i)) * 4);

            const sprite_y = self.bus.read(addr) -% 16;
            const sprite_x = self.bus.read(addr + 1) -% 8;
            var tile_id = self.bus.read(addr + 2);
            const flags = self.bus.read(addr + 3);

            if (ly >= sprite_y and ly < (sprite_y + height)) {
                var line = ly - sprite_y;

                // Y-flip
                if ((flags & 0x40) != 0) {
                    line = height - 1 - line;
                }

                if (use_8x16) tile_id &= 0xFE;

                const tile_addr = 0x8000 + (@as(u16, tile_id) * 16);
                const b1 = self.bus.read(tile_addr + (line * 2));
                const b2 = self.bus.read(tile_addr + (line * 2) + 1);

                const pal_reg: u16 = if ((flags & 0x10) != 0) 0xFF49 else 0xFF48;
                const pal_data = self.bus.read(pal_reg);

                for (0..8) |x| {
                    const pixel_x = sprite_x +% @as(u8, @intCast(x));
                    if (pixel_x >= SCREEN_WIDTH) continue;

                    // X-flip
                    const bit = if ((flags & 0x20) != 0) @as(u3, @intCast(x)) else @as(u3, @intCast(7 - x));
                    const color_id = ((b2 >> bit) & 1) << 1 | ((b1 >> bit) & 1);

                    if (color_id == 0) continue; // transparent

                    // TODO priority check against BG (bit 7)

                    const pal_shift = @as(u3, @intCast(color_id * 2));
                    const color = (pal_data >> pal_shift) & 0x03;

                    self.video_buffer[(@as(usize, ly) * SCREEN_WIDTH) + pixel_x] = PALETTE[color];
                }
            }
        }
    }
};
