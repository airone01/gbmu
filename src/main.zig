const std = @import("std");
const GameBoy = @import("core/gameboy.zig").GameBoy;
const Platform = @import("platform/sdl.zig").SdlPlatform;
const Config = @import("config.zig").Config;
const yazap = @import("yazap");
const Arg = yazap.Arg;
const App = yazap.App;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var app = App.init(allocator, "gbmu", null);
    defer app.deinit();

    var root = app.rootCommand();
    try root.addArgs(&[_]Arg{
        Arg.positional("ROM_PATH", "Path to the Game Boy ROM file (.gb)", null),
        Arg.booleanOption("debug", 'd', "Enables debug mode for instruction tracing and logging."),
    });
    root.setProperty(.help_on_empty_args);

    const matches = try app.parseProcess();
    // positional arg is guaranteed by parseProcess
    const rom_path = matches.getSingleValue("ROM_PATH").?;
    const debug = matches.containsArg("debug");

    const config = Config{
        .rom_path = rom_path,
        .debug = debug,
    };

    if (config.debug) {
        std.debug.print("DEBUG: DEBUG MODE ENABLED\n", .{});
    }

    // Load ROM
    const file = try std.fs.cwd().openFile(config.rom_path, .{});
    defer file.close();
    const size = (try file.stat()).size;
    const rom = try allocator.alloc(u8, size);
    defer allocator.free(rom);
    _ = try file.readAll(rom);

    // core
    var gb = try GameBoy.init(config, allocator, rom);
    defer gb.deinit();

    // platform
    var platform = try Platform.init();
    defer platform.deinit();

    // loop
    while (true) {
        if (!platform.handle_input(&gb)) break;
        gb.step_frame();
        platform.render(&gb);
    }
}
