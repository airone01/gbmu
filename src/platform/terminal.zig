const std = @import("std");
const GameBoy = @import("../core/gameboy.zig").GameBoy;
const JoypadBtn = @import("../core/joypad.zig").Button;
const Ppu = @import("../core/ppu.zig");

pub const TerminalPlatform = struct {
    original_termios: std.posix.termios,
    stdin: std.fs.File,
    stdout: std.fs.File,
    buffer: []u8,
    buffer_pos: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !TerminalPlatform {
        const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };
        const stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
        const original_termios = try std.posix.tcgetattr(stdin.handle);

        // set terminal to raw mode
        var raw = original_termios;
        raw.lflag.ECHO = false;
        raw.lflag.ICANON = false;
        raw.lflag.ISIG = false;
        raw.lflag.IEXTEN = false;
        raw.iflag.IXON = false;
        raw.iflag.ICRNL = false;
        raw.iflag.BRKINT = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;
        raw.oflag.OPOST = false;
        raw.cflag.CSIZE = .CS8;
        raw.cc[@intFromEnum(std.posix.V.TIME)] = 0;
        raw.cc[@intFromEnum(std.posix.V.MIN)] = 0;

        try std.posix.tcsetattr(stdin.handle, .FLUSH, raw);
        // hide cursor and clear screen
        try stdout.writeAll("\x1b[?25l\x1b[2J\x1b[H");

        // allocate a buffer large enough for the screen
        // 160 width * 72 lines * ~40 bytes per pixel (ANSI codes) + controls
        const buffer = try allocator.alloc(u8, 500000);
        errdefer allocator.free(buffer);

        return TerminalPlatform{
            .original_termios = original_termios,
            .stdin = stdin,
            .stdout = stdout,
            .buffer = buffer,
            .buffer_pos = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TerminalPlatform) void {
        // restore terminal settings
        std.posix.tcsetattr(self.stdin.handle, .FLUSH, self.original_termios) catch {};
        // show cursor and clear screen
        self.stdout.writeAll("\x1b[?25h\x1b[2J\x1b[H") catch {};
        self.allocator.free(self.buffer);
    }

    pub fn render(self: *TerminalPlatform, gb: *GameBoy) !void {
        self.buffer_pos = 0;
        try self.writeStr("\x1b[H"); // move cursor to home position

        var y: usize = 0;
        while (y < Ppu.SCREEN_HEIGHT) : (y += 2) {
            var x: usize = 0;
            while (x < Ppu.SCREEN_WIDTH) : (x += 1) {
                const top_idx = (y * Ppu.SCREEN_WIDTH) + x;
                const bottom_idx = ((y + 1) * Ppu.SCREEN_WIDTH) + x;

                const top_color = gb.ppu.video_buffer[top_idx];
                const bottom_color = if (y + 1 < Ppu.SCREEN_HEIGHT)
                    gb.ppu.video_buffer[bottom_idx]
                else
                    top_color;

                // extract RGB from 0xAARRGGBB
                const top_r = (top_color >> 16) & 0xFF;
                const top_g = (top_color >> 8) & 0xFF;
                const top_b = top_color & 0xFF;
                const bottom_r = (bottom_color >> 16) & 0xFF;
                const bottom_g = (bottom_color >> 8) & 0xFF;
                const bottom_b = bottom_color & 0xFF;

                // use upper half block trick
                // i know better implementations exist but welp
                try self.writeFmt("\x1b[38;2;{};{};{}m\x1b[48;2;{};{};{}m▀", .{
                    top_r,    top_g,    top_b,
                    bottom_r, bottom_g, bottom_b,
                });
            }
            try self.writeStr("\x1b[0m\r\n"); // reset colors at eol
        }
        // try self.writeStr("\r\n\x1b[2mControls: WASD=D-Pad, J=A, K=B, Enter=Start, Space=Select, Q=Quit\x1b[0m");
        // write buffer to stdout
        try self.stdout.writeAll(self.buffer[0..self.buffer_pos]);
    }

    fn writeStr(self: *TerminalPlatform, str: []const u8) !void {
        @memcpy(self.buffer[self.buffer_pos..][0..str.len], str);
        self.buffer_pos += str.len;
    }

    fn writeFmt(self: *TerminalPlatform, comptime fmt: []const u8, args: anytype) !void {
        const written = try std.fmt.bufPrint(self.buffer[self.buffer_pos..], fmt, args);
        self.buffer_pos += written.len;
    }

    /// this is simplified, keys release after one frame
    /// TODO make it better
    pub fn handle_input(self: *TerminalPlatform, gb: *GameBoy) !bool {
        var buf: [16]u8 = undefined;

        // non-blocking read
        const n = self.stdin.read(&buf) catch 0;
        for (buf[0..n]) |ch| {
            switch (ch) {
                'q', 'Q', 27 => return false, // 27 is esc
                'w', 'W' => gb.key_down(.Up),
                's', 'S' => gb.key_down(.Down),
                'a', 'A' => gb.key_down(.Left),
                'd', 'D' => gb.key_down(.Right),
                'j', 'J' => gb.key_down(.A),
                'k', 'K' => gb.key_down(.B),
                '\r' => gb.key_down(.Start), // return
                ' ' => gb.key_down(.Select), // space
                else => {},
            }
        }

        gb.key_up(.Up);
        gb.key_up(.Down);
        gb.key_up(.Left);
        gb.key_up(.Right);
        gb.key_up(.A);
        gb.key_up(.B);
        gb.key_up(.Start);
        gb.key_up(.Select);

        return true;
    }

    /// sleep for approx one frame (60 FPS = ~16.67ms per frame)
    pub fn frame_sleep(self: *TerminalPlatform) void {
        _ = self;
        std.Thread.sleep(16_666_667); // nanoseconds
    }
};
