const DmgCpu = @import("cpu.zig");

pub const Timer = struct {
    // registers
    div: u16 = 0, // DIV is high byte (bits 8-15)
    tima: u8 = 0,
    tma: u8 = 0,
    tac: u8 = 0,

    tima_clock: u16 = 0,

    pub fn init() Timer {
        return Timer{};
    }

    /// steps the timer by 'cycles'
    /// returns true if interrupt requested
    pub fn step(self: *Timer, cycles: u16) bool {
        var interrupt = false;

        self.div +%= cycles;

        // update TIMA
        // TIMA only increments if the Timer Enable bit (2) in TAC is set
        const timer_enable = (self.tac & 0x04) != 0;

        if (timer_enable) {
            self.tima_clock += cycles;

            // Frequency from TAC bits 0-1
            const freq_cycles: u16 = switch (self.tac & 0x03) {
                0 => 1024, // 4096 Hz
                1 => 16, // 262144 Hz
                2 => 64, // 65536 Hz
                3 => 256, // 16384 Hz
                else => unreachable,
            };

            while (self.tima_clock >= freq_cycles) {
                self.tima_clock -= freq_cycles;

                if (self.tima == 0xFF) {
                    self.tima = self.tma;
                    interrupt = true;
                } else {
                    self.tima += 1;
                }
            }
        }
        return interrupt;
    }

    pub fn read(self: *const Timer, addr: u16) u8 {
        return switch (addr) {
            0xFF04 => @truncate(self.div >> 8),
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            0xFF07 => self.tac,
            else => 0xFF,
        };
    }

    pub fn write(self: *Timer, addr: u16, val: u8) void {
        switch (addr) {
            0xFF04 => self.div = 0, // writing to DIV resets it
            0xFF05 => self.tima = val,
            0xFF06 => self.tma = val,
            0xFF07 => self.tac = val,
            else => {},
        }
    }
};
