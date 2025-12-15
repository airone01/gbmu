const std = @import("std");
const GameBoy = @import("core/gameboy.zig").GameBoy;
const Platform = @import("platform/sdl.zig").SdlPlatform;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: gbmu <rom.gb>\n", .{});
        return;
    }

    // Load ROM
    const file = try std.fs.cwd().openFile(args[1], .{});
    defer file.close();
    const size = (try file.stat()).size;
    const rom = try allocator.alloc(u8, size);
    defer allocator.free(rom);
    _ = try file.readAll(rom);

    // Init Core
    var gb = try GameBoy.init(allocator, rom);
    defer gb.deinit();

    // Init Platform
    var platform = try Platform.init();
    defer platform.deinit();

    // Loop
    while (true) {
        if (!platform.handle_input(&gb)) break;
        gb.step_frame();
        platform.render(&gb);
    }
}
