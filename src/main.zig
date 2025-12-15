const std = @import("std");

const SDL = @import("sdl2");

const DmgBus = @import("dmg_bus.zig").DmgBus;
const DmgCpu = @import("dmg_cpu.zig").DmgCpu;
const _ppu = @import("dmg_ppu.zig");
const DmgPpu = _ppu.DmgPpu;

// yes, the Game Boy ran at (almost) 60 FPS
const TARGET_FRAME_TIME_MS = 1000 / 60;

/// "alive" indicator
/// @intFromFloat is heavy, so calling it 3 times per frame slows the emu
/// TODO implement a lookup table of colors instead of that
/// this is so over the top but I love it
fn getRainbowColor() u32 {
    const t = @as(f32, @floatFromInt(SDL.SDL_GetTicks()));
    const speed = 0.002;

    // calculate RGB using sine waves phase-shifted by 120 degrees (2pi/3)
    // we multiply by 127 and add 128 to keep values between 1 and 255
    // i found this on SO ofc
    const r: u32 = @intFromFloat(std.math.sin(speed * t) * 127.0 + 128.0);
    const g: u32 = @intFromFloat(std.math.sin(speed * t + (2.0 * std.math.pi / 3.0)) * 127.0 + 128.0);
    const b: u32 = @intFromFloat(std.math.sin(speed * t + (4.0 * std.math.pi / 3.0)) * 127.0 + 128.0);

    // combine into 0xAARRGGBB format
    return (0xFF << 24) | (r << 16) | (g << 8) | b;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("usage: {s} <path_to_rom.gb>\n", .{args[0]});
        return;
    }

    const rom_path = args[1];

    const file = try std.fs.cwd().openFile(rom_path, .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const rom_buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(rom_buffer);

    const bytes_read = try file.readAll(rom_buffer);
    if (bytes_read != file_size) {
        std.debug.print("Error: Could not read entire file.\n", .{});
        return;
    }

    var bus = DmgBus.init();
    bus.load_rom(rom_buffer);
    std.debug.print("loaded ROM '{s}'\n", .{args[1]});

    var cpu = DmgCpu.init(&bus);
    var ppu = DmgPpu.init(&bus);

    if (SDL.SDL_Init(SDL.SDL_INIT_VIDEO | SDL.SDL_INIT_EVENTS | SDL.SDL_INIT_AUDIO) < 0)
        sdl_panic();
    defer SDL.SDL_Quit();

    const window = SDL.SDL_CreateWindow(
        "gbmu?",
        SDL.SDL_WINDOWPOS_CENTERED,
        SDL.SDL_WINDOWPOS_CENTERED,
        640,
        480,
        SDL.SDL_WINDOW_SHOWN,
    ) orelse sdl_panic();
    defer _ = SDL.SDL_DestroyWindow(window);

    const renderer = SDL.SDL_CreateRenderer(window, -1, SDL.SDL_RENDERER_ACCELERATED | SDL.SDL_RENDERER_PRESENTVSYNC) orelse sdl_panic();
    defer _ = SDL.SDL_DestroyRenderer(renderer);

    const texture = SDL.SDL_CreateTexture( // GB screen texture
        renderer,
        SDL.SDL_PIXELFORMAT_ARGB8888,
        SDL.SDL_TEXTUREACCESS_STREAMING,
        _ppu.SCREEN_WIDTH,
        _ppu.SCREEN_HEIGHT,
    ) orelse sdl_panic();
    defer SDL.SDL_DestroyTexture(texture);

    var running = true;
    while (running) {
        const start_time = SDL.SDL_GetTicks();

        var ev: SDL.SDL_Event = undefined;
        while (SDL.SDL_PollEvent(&ev) != 0) {
            switch (ev.type) {
                SDL.SDL_QUIT => running = false,
                else => {},
            }
        }

        step_frame(&cpu, &ppu);

        // debugging render
        const debug_color = getRainbowColor();
        for (0..2) |y| {
            for (0..2) |x| {
                ppu.video_buffer[(y * _ppu.SCREEN_WIDTH) + x] = debug_color;
            }
        }

        _ = SDL.SDL_UpdateTexture(texture, null, @ptrCast(&ppu.video_buffer), _ppu.SCREEN_WIDTH * @sizeOf(u32));
        _ = SDL.SDL_SetRenderDrawColor(renderer, 0x11, 0x11, 0x11, 0xFF);
        _ = SDL.SDL_RenderClear(renderer);
        // scale to window size auto
        _ = SDL.SDL_RenderCopy(renderer, texture, null, null);

        SDL.SDL_RenderPresent(renderer);

        const end_time = SDL.SDL_GetTicks();
        const elapsed = end_time - start_time;

        if (elapsed > TARGET_FRAME_TIME_MS)
            std.debug.print("WARNING: render took {d}ms (> {d}ms)\n", .{ elapsed, TARGET_FRAME_TIME_MS });
    }
}

fn sdl_panic() noreturn {
    const str = @as(?[*:0]const u8, SDL.SDL_GetError()) orelse "unknown error";
    @panic(std.mem.sliceTo(str, 0));
}

const MAX_CYCLES_PER_FRAME = 70224;

fn step_frame(cpu: *DmgCpu, ppu: *DmgPpu) void {
    // cycles taken for the whole fame
    var frame_cycles: u32 = 0; // u32 > MAX_CYCLES_PER_FRAME

    while (frame_cycles < MAX_CYCLES_PER_FRAME) {
        cpu.handle_interrupts();

        // cycles taken this one cpu step
        // manual 2.1: DMG CPU cycles are 0.954 µs (1 machine cycle = 4 clock cycles)
        var cycles_taken: u16 = 0;
        if (cpu.halted) {
            cycles_taken = 4; // when halted just wait
        } else {
            cycles_taken = cpu.step() * 4;
        }

        ppu.step(cycles_taken);
        cpu.update_timers(cycles_taken);

        frame_cycles += cycles_taken;
    }
}
