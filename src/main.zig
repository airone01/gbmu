const std = @import("std");
const GameBoy = @import("core/gameboy.zig").GameBoy;
const SdlPlatform = @import("platform/sdl.zig").SdlPlatform;
const TerminalPlatform = @import("platform/terminal.zig").TerminalPlatform;
const Config = @import("config.zig").Config;
const yazap = @import("yazap");
const Arg = yazap.Arg;
const App = yazap.App;

const RenderMode = enum {
    sdl,
    terminal,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var app = App.init(allocator, "gbmu", null);
    defer app.deinit();

    var root = app.rootCommand();
    try root.addArgs(&[_]Arg{
        Arg.positional("path", "Path to the DMG ROM file (.gb)", null),
        Arg.booleanOption("debug", 'd', "Enables debug mode."),
        Arg.booleanOption("terminal", 't', "Use terminal renderer instead of SDL window."),
    });
    root.setProperty(.help_on_empty_args);

    const matches = try app.parseProcess();
    // positional arg is guaranteed by parseProcess
    const rom_path = matches.getSingleValue("path").?;
    const debug = matches.containsArg("debug");
    const use_terminal = matches.containsArg("terminal");

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

    // loop
    if (use_terminal) {
        try runWithTerminal(&gb, allocator);
    } else {
        try runWithSdl(&gb);
    }
}

fn runWithTerminal(gb: *GameBoy, allocator: std.mem.Allocator) !void {
    var platform = try TerminalPlatform.init(allocator);
    defer platform.deinit();

    while (true) {
        if (!try platform.handle_input(gb)) break;
        gb.step_frame();
        try platform.render(gb);
        platform.frame_sleep();
    }
}

fn runWithSdl(gb: *GameBoy) !void {
    var platform = try SdlPlatform.init();
    defer platform.deinit();

    while (true) {
        if (!platform.handle_input(gb)) break;
        gb.step_frame();
        platform.render(gb);
    }
}
