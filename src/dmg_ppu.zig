const std = @import("std");
const DmgBus = @import("dmg_bus.zig").DmgBus;

pub const DmgPpu = struct {
    bus: *DmgBus,

    line_dots: u16 = 0,

    pub fn init(bus: *DmgBus) DmgPpu {
        return DmgPpu{
            .bus = bus,
        };
    }

    pub fn step(self: *DmgPpu, cycles: u16) void {
        self.line_dots += cycles;

        // a scanline is 456 dots long
        while (self.line_dots >= 456) {
            self.line_dots -= 456;

            // increment LY (register 0xFF44)
            // we access mem_raw directly because using bus.write() might trigger
            // write-protection logic if we implemented "Read Only" registers correctly.
            // hardware-level updates bypass CPU write restrictions.
            var ly = self.bus.mem_raw[0xFF44];
            ly += 1;

            // 154 Scanlines total (0-143 Visible, 144-153 VBlank)
            if (ly == 144) {
                // request VBlank interrupt (bit 0 of IF register 0xFF0F)
                const if_val = self.bus.mem_raw[0xFF0F];
                self.bus.mem_raw[0xFF0F] = if_val | 0x01;
            }

            if (ly > 153) {
                ly = 0;
            }

            self.bus.mem_raw[0xFF44] = ly;

            // TODO check for LY=LYC interrupts here
        }
    }
};
