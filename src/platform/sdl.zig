const std = @import("std");
const SDL = @import("sdl2");
const GameBoy = @import("../core/gameboy.zig").GameBoy;
const JoypadBtn = @import("../core/joypad.zig").Button;
const Ppu = @import("../core/ppu.zig");

const SCALE = 3; // multiplier of pixel size

pub const SdlPlatform = struct {
    window: *SDL.SDL_Window,
    renderer: *SDL.SDL_Renderer,
    texture: *SDL.SDL_Texture,

    pub fn init() !SdlPlatform {
        if (SDL.SDL_Init(SDL.SDL_INIT_VIDEO | SDL.SDL_INIT_EVENTS) < 0)
            return error.SdlInitFailed;

        const window = SDL.SDL_CreateWindow(
            "Zig GameBoy",
            SDL.SDL_WINDOWPOS_CENTERED,
            SDL.SDL_WINDOWPOS_CENTERED,
            Ppu.SCREEN_WIDTH * SCALE,
            Ppu.SCREEN_HEIGHT * SCALE,
            SDL.SDL_WINDOW_SHOWN,
        ) orelse return error.SdlWindowFailed;

        const renderer = SDL.SDL_CreateRenderer(window, -1, SDL.SDL_RENDERER_ACCELERATED | SDL.SDL_RENDERER_PRESENTVSYNC) orelse return error.SdlRendererFailed;

        const texture = SDL.SDL_CreateTexture(
            renderer,
            SDL.SDL_PIXELFORMAT_ARGB8888,
            SDL.SDL_TEXTUREACCESS_STREAMING,
            Ppu.SCREEN_WIDTH,
            Ppu.SCREEN_HEIGHT,
        ) orelse return error.SdlTextureFailed;

        return SdlPlatform{
            .window = window,
            .renderer = renderer,
            .texture = texture,
        };
    }

    pub fn deinit(self: *SdlPlatform) void {
        SDL.SDL_DestroyTexture(self.texture);
        SDL.SDL_DestroyRenderer(self.renderer);
        SDL.SDL_DestroyWindow(self.window);
        SDL.SDL_Quit();
    }

    pub fn render(self: *SdlPlatform, gb: *GameBoy) void {
        _ = SDL.SDL_UpdateTexture(self.texture, null, @ptrCast(&gb.ppu.video_buffer), Ppu.SCREEN_WIDTH * @sizeOf(u32));
        _ = SDL.SDL_RenderClear(self.renderer);
        _ = SDL.SDL_RenderCopy(self.renderer, self.texture, null, null);
        SDL.SDL_RenderPresent(self.renderer);
    }

    pub fn handle_input(self: *SdlPlatform, gb: *GameBoy) bool {
        var ev: SDL.SDL_Event = undefined;
        while (SDL.SDL_PollEvent(&ev) != 0) {
            switch (ev.type) {
                SDL.SDL_QUIT => return false,
                SDL.SDL_KEYDOWN => self.map_key(ev.key.keysym.sym, true, gb),
                SDL.SDL_KEYUP => self.map_key(ev.key.keysym.sym, false, gb),
                else => {},
            }
        }
        return true;
    }

    fn map_key(self: *SdlPlatform, key: i32, pressed: bool, gb: *GameBoy) void {
        _ = self;
        const btn = switch (key) {
            SDL.SDLK_UP => JoypadBtn.Up,
            SDL.SDLK_DOWN => JoypadBtn.Down,
            SDL.SDLK_LEFT => JoypadBtn.Left,
            SDL.SDLK_RIGHT => JoypadBtn.Right,
            SDL.SDLK_z => JoypadBtn.A,
            SDL.SDLK_x => JoypadBtn.B,
            SDL.SDLK_RETURN => JoypadBtn.Start,
            SDL.SDLK_RSHIFT => JoypadBtn.Select,
            else => return,
        };

        if (pressed) gb.key_down(btn) else gb.key_up(btn);
    }
};

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
