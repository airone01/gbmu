const std = @import("std");

const DmgBus = @import("dmg").DmgBus;
const DmgCpu = @import("dmg_cpu").DmgCpu;
const SDL = @import("sdl2");

pub fn main() !void {
    if (SDL.SDL_Init(SDL.SDL_INIT_VIDEO | SDL.SDL_INIT_EVENTS | SDL.SDL_INIT_AUDIO) < 0)
        sdl_panic();
    defer SDL.SDL_Quit();

    const window = SDL.SDL_CreateWindow(
        "gbmu?",
        SDL.SDL_WINDOWPOS_CENTERED, SDL.SDL_WINDOWPOS_CENTERED,
        640, 480,
        SDL.SDL_WINDOW_SHOWN,
    ) orelse sdl_panic();
    defer _ = SDL.SDL_DestroyWindow(window);

    const renderer = SDL.SDL_CreateRenderer(window, -1, SDL.SDL_RENDERER_ACCELERATED) orelse sdl_panic();
    defer _ = SDL.SDL_DestroyRenderer(renderer);

    // const dmg_bus = DmgBus.init();

    mainLoop: while (true) {
        var ev: SDL.SDL_Event = undefined;
        while (SDL.SDL_PollEvent(&ev) != 0) {
            if(ev.type == SDL.SDL_QUIT)
                break :mainLoop;
        }

        _ = SDL.SDL_SetRenderDrawColor(renderer, 0xF7, 0xA4, 0x1D, 0xFF);
        _ = SDL.SDL_RenderClear(renderer);

        SDL.SDL_RenderPresent(renderer);
    }
}

fn sdl_panic() noreturn {
    const str = @as(?[*:0]const u8, SDL.SDL_GetError()) orelse "unknown error";
    @panic(std.mem.sliceTo(str, 0));
}

const MAX_CYCLES_PER_FRAME = 70224;

fn step_frame(cpu: *DmgCpu) noreturn {
    // cycles taken for the whole fame
    var frame_cycles: u16 = 0;

    while (frame_cycles < MAX_CYCLES_PER_FRAME) {
        // cycles taken this one cpu step
        const cycles = cpu.step() * 4;

        // then we sync hardware depending on how much time we took
        // update_timers(cycles);
        // update_graphics(cycles);
        // handle_interrupts();
        // ... or something like that
        
        frame_cycles += cycles;
    }
}
