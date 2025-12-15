const std = @import("std");
const Cpu = @import("cpu.zig").Cpu;
const Bus = @import("bus.zig").Bus;
const Ppu = @import("ppu.zig").Ppu;
const Config = @import("../config.zig").Config;
const Joypad = @import("joypad.zig").Joypad;

pub const GameBoy = struct {
    cpu: Cpu,
    bus: *Bus, // bus heap allocated because it's large
    ppu: Ppu,
    allocator: std.mem.Allocator,

    config: Config,

    pub fn init(config: Config, allocator: std.mem.Allocator, rom_data: []const u8) !GameBoy {
        const bus = try allocator.create(Bus);
        bus.* = try Bus.init(allocator, rom_data);

        const cpu = Cpu.init(config, bus);
        const ppu = Ppu.init(bus);

        return GameBoy{
            .config = config,
            .cpu = cpu,
            .bus = bus,
            .ppu = ppu,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GameBoy) void {
        self.bus.deinit();
        self.allocator.destroy(self.bus);
    }

    /// run one frame of emulation (~70224 cycles)
    pub fn step_frame(self: *GameBoy) void {
        const MAX_CYCLES = 70224;
        var cycles: u32 = 0;

        while (cycles < MAX_CYCLES) {
            // step timer

            // run CPU instructions
            const m_cycles = self.cpu.step();
            const t_cycles = m_cycles * 4;

            // update subsystems
            const timer_irq = self.bus.timer.step(t_cycles);
            if (timer_irq) self.bus.if_reg |= 0x04;

            self.ppu.step(t_cycles);

            // check interrupts for next instruction
            self.cpu.handle_interrupts();

            cycles += t_cycles;
        }
    }

    pub fn key_down(self: *GameBoy, btn: @import("joypad.zig").Button) void {
        self.bus.joypad.set_button(btn, true);
        // request Joypad interrupt
        self.bus.if_reg |= 0x10;
    }

    pub fn key_up(self: *GameBoy, btn: @import("joypad.zig").Button) void {
        self.bus.joypad.set_button(btn, false);
    }
};
