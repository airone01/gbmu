const std = @import("std");
const DmgBus = @import("dmg_bus.zig").DmgBus;

pub const DmgCpu = struct {
    bus: *DmgBus,
    cycles: u128 = 0,
    ime: bool = false, // interrupt master enable
    halted: bool = false, // CPU halted state
    div_clock: u16 = 0,
    tima_clock: u16 = 0,

    // 8-bit general purpose registers
    a: u8, // accumulator
    f: u8, // flags
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    // 16-bit control registers
    sp: u16, // stack pointer
    pc: u16, // program counter

    // flag bitmasks
    pub const FLAG_Z: u8 = 0b1000_0000; // zero flag
    pub const FLAG_N: u8 = 0b0100_0000; // subtract flag
    pub const FLAG_H: u8 = 0b0010_0000; // half carry flag
    pub const FLAG_C: u8 = 0b0001_0000; // carry flag

    pub fn init(bus: *DmgBus) DmgCpu {
        return DmgCpu{
            .bus = bus,
            .ime = false,
            .halted = false,
            .div_clock = 0,
            .tima_clock = 0,
            // initial values based for DMG boot state
            .a = 0x01, // 0x01 for DMG, 0x11 for CGB
            .f = 0xB0,
            .b = 0x00,
            .c = 0x13,
            .d = 0x00,
            .e = 0xD8,
            .h = 0x01,
            .l = 0x4D,
            .sp = 0xFFFE, // defaults to 0xFFFE on start
            .pc = 0x0100, // entry point usually 0x100
        };
    }

    /// helper to read an intermediate u8 val
    fn fetch(self: *DmgCpu) u8 {
        const val = self.bus.read(self.pc);
        // note: simpler to increment the PC here
        self.pc +%= 1;
        return val;
    }

    // --- 16-bit Register Accessors (Pairing) ---
    // The Game Boy is Little Endian, but the register pairs are
    // defined as High-Low (e.g., B is High, C is Low).

    // AF pair (accumulator & flags)
    pub fn get_af(self: *const DmgCpu) u16 {
        return (@as(u16, self.a) << 8) | @as(u16, self.f);
    }

    pub fn set_af(self: *DmgCpu, value: u16) void {
        self.a = @truncate(value >> 8);
        // lower 4 bits of F should always be 0
        self.f = @truncate(value & 0xF0);
    }

    // BC pair
    pub fn get_bc(self: *const DmgCpu) u16 {
        return (@as(u16, self.b) << 8) | @as(u16, self.c);
    }

    pub fn set_bc(self: *DmgCpu, value: u16) void {
        self.b = @truncate(value >> 8);
        self.c = @truncate(value);
    }

    // DE pair
    pub fn get_de(self: *const DmgCpu) u16 {
        return (@as(u16, self.d) << 8) | @as(u16, self.e);
    }

    pub fn set_de(self: *DmgCpu, value: u16) void {
        self.d = @truncate(value >> 8);
        self.e = @truncate(value);
    }

    // HL pair
    // note: usually used for memory addressing
    pub fn get_hl(self: *const DmgCpu) u16 {
        return (@as(u16, self.h) << 8) | @as(u16, self.l);
    }

    pub fn set_hl(self: *DmgCpu, value: u16) void {
        self.h = @truncate(value >> 8);
        self.l = @truncate(value);
    }

    pub fn get_flag(self: *const DmgCpu, mask: u8) bool {
        return (self.f & mask) != 0;
    }

    pub fn set_flag(self: *DmgCpu, mask: u8, condition: bool) void {
        if (condition) {
            self.f |= mask;
        } else {
            self.f &= ~mask;
        }
    }

    /// helper to get a pointer to an 8-bit register based on the 3-bit code
    /// 0=B, 1=C, 2=D, 3=E, 4=H, 5=L, 7=A
    fn decode_register_ptr(self: *DmgCpu, code: u8) *u8 {
        return switch (code) {
            0 => &self.b,
            1 => &self.c,
            2 => &self.d,
            3 => &self.e,
            4 => &self.h,
            5 => &self.l,
            // code 6 is (HL) which is handled explicitly by caller
            7 => &self.a,
            else => unreachable,
        };
    }

    /// helper to read a 16-bit value (little endian)
    fn fetch16(self: *DmgCpu) u16 {
        const low = self.fetch();
        const high = self.fetch();
        return (@as(u16, high) << 8) | @as(u16, low);
    }

    pub fn handle_interrupts(self: *DmgCpu) void {
        const ie = self.bus.read(0xFFFF);
        const if_reg = self.bus.read(0xFF0F);

        const pending = ie & if_reg & 0x1F;
        if (pending == 0) return;

        // if halted, any interrupt wakes the CPU
        if (self.halted) {
            self.halted = false;
        }

        // service interrupt only if IME is ON
        if (self.ime) {
            self.ime = false; // disable further interrupts

            // push PC to stack
            self.sp -= 1;
            self.bus.write(self.sp, @truncate(self.pc >> 8));
            self.sp -= 1;
            self.bus.write(self.sp, @truncate(self.pc));

            // jump to vector and clear IF bit
            // priority: VBlank (0) -> LCD (1) -> timer (2) -> serial (3) -> joypad (4)
            var vector: u16 = 0;
            var bit: u8 = 0;

            if ((pending & 0x01) != 0) { // VBlank
                vector = 0x0040;
                bit = 0x01;
            } else if ((pending & 0x02) != 0) { // LCD stat
                vector = 0x0048;
                bit = 0x02;
            } else if ((pending & 0x04) != 0) { // timer
                vector = 0x0050;
                bit = 0x04;
            } else if ((pending & 0x08) != 0) { // serial
                vector = 0x0058;
                bit = 0x08;
            } else if ((pending & 0x10) != 0) { // joypad
                vector = 0x0060;
                bit = 0x10;
            }

            self.pc = vector;
            // clear the specific interrupt flag just serviced
            self.bus.write(0xFF0F, if_reg & ~bit);
        }
    }

    pub fn update_timers(self: *DmgCpu, cycles: u16) void {
        // update DIV
        // DIV increments at 16384 Hz (every 256 CPU clock cycles)
        self.div_clock += cycles;
        while (self.div_clock >= 256) {
            self.div_clock -= 256;
            // DIV register is the high byte of the counter
            self.bus.mem_raw[0xFF04] +%= 1; // wrapping is fine here idk
        }

        // update TIMA
        // TIMA only increments if the TAC (timer control) is enabled (on bit 2)
        const tac = self.bus.mem_raw[0xFF07];
        const timer_enable = (tac & 0x04) != 0;

        if (timer_enable) {
            self.tima_clock += cycles;

            // frequency determined by TAC bits 1-0:
            // 00=4096Hz (1024 cycles), 01=262144Hz (16 cycles), 10=65536Hz (64 cycles), 11=16384Hz (256 cycles)
            const frequency_cycles: u16 = switch (tac & 0x03) {
                0 => 1024,
                1 => 16,
                2 => 64,
                3 => 256,
                else => unreachable,
            };

            while (self.tima_clock >= frequency_cycles) {
                self.tima_clock -= frequency_cycles;

                const tima = self.bus.mem_raw[0xFF05];

                // overflow check (255 -> 0)
                if (tima == 0xFF) {
                    // set timer interrupt (bit 2 of IF register 0xFF0F)
                    self.bus.mem_raw[0xFF0F] |= 0x04;
                    // reset TIMA to TMA (timer modulo, 0xFF06)
                    self.bus.mem_raw[0xFF05] = self.bus.mem_raw[0xFF06];
                } else {
                    self.bus.mem_raw[0xFF05] = tima + 1;
                }
            }
        }
    }

    // handler for CB-prefixed instructions
    fn step_cb(self: *DmgCpu, opcode: u8) u16 {
        // decode register from lower 3 bits (0=B, 1=C, etc.)
        const r_code = opcode & 0b111;
        const is_hl = (r_code == 0b110); // HL or register

        var val: u8 = if (is_hl)
            self.bus.read(self.get_hl())
        else
            self.decode_register_ptr(r_code).*;

        // operations on (HL) usually cost more
        var cycles: u16 = if (is_hl) 4 else 2;

        // decode instruction type from bits 7-3
        // 0x00-0x3F: rotates/shifts (RLC, RRC, RL, RR, SLA, SRA, SWAP, SRL)
        // 0x40-0x7F: BIT (test Bit)
        // 0x80-0xBF: RES (reset Bit)
        // 0xC0-0xFF: SET (set Bit)

        if (opcode < 0x40) {
            // rotates and shifts
            const op_type = (opcode >> 3) & 0b111;

            switch (op_type) {
                0 => { // RLC (Rotate Left Circular)
                    const carry = (val >> 7) & 1;
                    val = (val << 1) | carry;
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (carry == 1) self.f |= FLAG_C;
                },
                1 => { // RRC (Rotate Right Circular)
                    const carry = val & 1;
                    val = (val >> 1) | (carry << 7);
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (carry == 1) self.f |= FLAG_C;
                },
                2 => { // RL (Rotate Left through Carry)
                    const old_c = if ((self.f & FLAG_C) != 0) @as(u8, 1) else 0;
                    const new_c = (val >> 7) & 1;
                    val = (val << 1) | old_c;
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (new_c == 1) self.f |= FLAG_C;
                },
                3 => { // RR (Rotate Right through Carry)
                    const old_c = if ((self.f & FLAG_C) != 0) @as(u8, 1) else 0;
                    const new_c = val & 1;
                    val = (val >> 1) | (old_c << 7);
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (new_c == 1) self.f |= FLAG_C;
                },
                4 => { // SLA (Shift Left Arithmetic)
                    const carry = (val >> 7) & 1;
                    val = val << 1;
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (carry == 1) self.f |= FLAG_C;
                },
                5 => { // SRA (Shift Right Arithmetic: keep MSB)
                    const carry = val & 1;
                    val = (val >> 1) | (val & 0x80);
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (carry == 1) self.f |= FLAG_C;
                },
                6 => { // SWAP (Swap nibbles)
                    val = (val << 4) | (val >> 4);
                    self.f = if (val == 0) FLAG_Z else 0;
                },
                7 => { // SRL (Shift Right Logical: MSB becomes 0)
                    const carry = val & 1;
                    val = val >> 1;
                    self.f = if (val == 0) FLAG_Z else 0;
                    if (carry == 1) self.f |= FLAG_C;
                },
                else => unreachable,
            }

            // write back result
            if (is_hl) {
                self.bus.write(self.get_hl(), val);
            } else {
                self.decode_register_ptr(r_code).* = val;
            }
        } else if (opcode < 0x80) {
            // BIT n, r (test bit n)
            // bit index is in bits 5-3
            const bit = (opcode >> 3) & 0b111;
            const is_set = (val & (@as(u8, 1) << @intCast(bit))) != 0;

            // Z=1 if bit is 0. N=0, H=1. C is preserved.
            const z_flag = if (!is_set) FLAG_Z else 0;
            self.f = (self.f & FLAG_C) | FLAG_H | z_flag;

            // BIT on (HL) takes 3 machine cycles (12 clocks), others take 2 (8 clocks)
            cycles = if (is_hl) 3 else 2;
        } else if (opcode < 0xC0) {
            // RES n, r (Reset Bit n)
            const bit = (opcode >> 3) & 0b111;
            val &= ~(@as(u8, 1) << @intCast(bit));

            if (is_hl) {
                self.bus.write(self.get_hl(), val);
            } else {
                self.decode_register_ptr(r_code).* = val;
            }
        } else {
            // SET n, r (Set Bit n)
            const bit = (opcode >> 3) & 0b111;
            val |= (@as(u8, 1) << @intCast(bit));

            if (is_hl) {
                self.bus.write(self.get_hl(), val);
            } else {
                self.decode_register_ptr(r_code).* = val;
            }
        }

        return cycles;
    }

    /// execute a CPU step
    /// returns cpu count of cycles taken
    pub fn step(self: *DmgCpu) u16 {
        const opcode: u8 = self.bus.read(self.pc);

        // increment pc to point to the next byte
        self.pc += 1;

        switch (opcode) {
            0x00 => {
                return 1;
            },

            // RLCA (rotate A left circular)
            0x07 => {
                const carry = (self.a >> 7) & 1;
                self.a = (self.a << 1) | carry;
                self.f = if (carry == 1) FLAG_C else 0; // Z, N, H cleared
                return 1;
            },

            // RLA (rotate A left through carry)
            0x17 => {
                const old_c = if ((self.f & FLAG_C) != 0) @as(u8, 1) else 0;
                const new_c = (self.a >> 7) & 1;
                self.a = (self.a << 1) | old_c;
                self.f = if (new_c == 1) FLAG_C else 0;
                return 1;
            },

            // RRCA (rotate A right circular)
            0x0F => {
                const carry = self.a & 1;
                self.a = (self.a >> 1) | (carry << 7);
                self.f = if (carry == 1) FLAG_C else 0;
                return 1;
            },

            // RRA (rotate A right through carry)
            0x1F => {
                const old_c = if ((self.f & FLAG_C) != 0) @as(u8, 1) else 0;
                const new_c = self.a & 1;
                self.a = (self.a >> 1) | (old_c << 7);
                self.f = if (new_c == 1) FLAG_C else 0;
                return 1;
            },

            // PUSH qq (push 16-bit register)
            0xC5, 0xD5, 0xE5, 0xF5 => {
                const val = switch (opcode) {
                    0xC5 => self.get_bc(),
                    0xD5 => self.get_de(),
                    0xE5 => self.get_hl(),
                    0xF5 => self.get_af(),
                    else => unreachable,
                };

                self.sp -= 1;
                self.bus.write(self.sp, @truncate(val >> 8));
                self.sp -= 1;
                self.bus.write(self.sp, @truncate(val));
                return 4;
            },

            // POP qq (pop 16-bit register)
            0xC1, 0xD1, 0xE1, 0xF1 => {
                const low = self.bus.read(self.sp);
                self.sp += 1;
                const high = self.bus.read(self.sp);
                self.sp += 1;
                const val = (@as(u16, high) << 8) | @as(u16, low);

                switch (opcode) {
                    0xC1 => self.set_bc(val),
                    0xD1 => self.set_de(val),
                    0xE1 => self.set_hl(val),
                    0xF1 => self.set_af(val), // F lower nibble cleared by setter
                    else => unreachable,
                }
                return 3;
            },

            // ADD HL, ss (add 16-bit reg to HL)
            0x09, 0x19, 0x29, 0x39 => {
                const val = switch (opcode) {
                    0x09 => self.get_bc(),
                    0x19 => self.get_de(),
                    0x29 => self.get_hl(),
                    0x39 => self.sp,
                    else => unreachable,
                };

                const hl = self.get_hl();
                const result = hl +% val;
                self.set_hl(result);

                // N=0. H=set if carry from bit 11. C=set if carry from bit 15. Z=unchanged.
                const h_flag = if (((hl & 0xFFF) + (val & 0xFFF)) > 0xFFF) FLAG_H else 0;
                const c_flag = if (@as(u32, hl) + @as(u32, val) > 0xFFFF) FLAG_C else 0;

                self.f = (self.f & FLAG_Z) | h_flag | c_flag;
                return 2;
            },

            // ADD A, r (add register r to A)
            0x80...0x87 => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                const a_val = self.a;
                const result = a_val +% val;
                self.a = result;

                // flags: Z, N=0, H (carry from bit 3), C (carry from bit 7)
                const z_flag = if (result == 0) FLAG_Z else 0;
                const h_flag = if (((a_val & 0x0F) + (val & 0x0F)) > 0x0F) FLAG_H else 0;
                const c_flag = if (@as(u16, a_val) + @as(u16, val) > 0xFF) FLAG_C else 0;

                self.f = z_flag | h_flag | c_flag;
                return if (src_code == 0b110) 2 else 1;
            },

            // ADC A, r (add register r to A with carry)
            0x88...0x8F => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                const carry = if ((self.f & FLAG_C) != 0) @as(u8, 1) else 0;
                const a_val = self.a;
                const result = a_val +% val +% carry;

                self.a = result;

                // flags: Z, N=0, H (carry from bit 3), C (carry from bit 7)
                const z_flag = if (result == 0) FLAG_Z else 0;
                // half carry: check if bit 3 overflowed
                const h_flag = if ((a_val & 0x0F) + (val & 0x0F) + carry > 0x0F) FLAG_H else 0;
                // carry: check if bit 7 overflowed
                const c_flag = if (@as(u16, a_val) + @as(u16, val) + @as(u16, carry) > 0xFF) FLAG_C else 0;

                self.f = z_flag | h_flag | c_flag;
                return if (src_code == 0b110) 2 else 1;
            },

            // CP r (compare A with register r)
            0xB8...0xBF => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                const a_val = self.a;

                // flags: Z (set if 0), N=1, H (borrow from bit 4), C (A < val)
                const z_flag = if (a_val == val) FLAG_Z else 0;
                const n_flag = FLAG_N;
                const h_flag = if ((a_val & 0x0F) < (val & 0x0F)) FLAG_H else 0;
                const c_flag = if (a_val < val) FLAG_C else 0;

                self.f = z_flag | n_flag | h_flag | c_flag;
                return if (src_code == 0b110) 2 else 1;
            },

            // LD r, r'
            // manual format: 01 r r' (bits 7-6 are '01')
            // range: 0x40 (01 000 000) to 0x7F (01 111 111)
            0x40...0x75, 0x77...0x7F => {
                // dest bits 5-3 (r)
                const dest_code = (opcode >> 3) & 0b111;
                // source bits 2-0 (r')
                const src_code = opcode & 0b111;

                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl()) // read mem at (HL)
                else
                    self.decode_register_ptr(src_code).*; // read register

                if (dest_code == 0b110) {
                    self.bus.write(self.get_hl(), val); // write memory at (HL)
                } else {
                    self.decode_register_ptr(dest_code).* = val; // write register
                }

                return 1;
            },

            // LD r, n
            // manual format: 00 r 110 (bits 7-6 '00', bits 2-0 '110')
            // specific opcodes: 0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E, 0x3E
            0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E, 0x3E => {
                const dest_code = (opcode >> 3) & 0b111;
                const n = self.fetch(); // load immediate value (n)

                if (dest_code == 0b110) { // LD (HL), n
                    self.bus.write(self.get_hl(), n);
                    return 3; // memory write takes longer
                } else {
                    self.decode_register_ptr(dest_code).* = n;
                    return 2;
                }
            },

            // LD (HLD), A (load A to (HL) and decrement HL)
            0x32 => {
                const addr = self.get_hl();
                self.bus.write(addr, self.a);
                // doing wrapping subtraction (-%) so 0x0000 wraps to 0xFFFF
                self.set_hl(addr -% 1);
                return 2;
            },

            // LD (C), A (write A to 0xFF00 + C)
            0xE2 => {
                const addr = 0xFF00 + @as(u16, self.c);
                self.bus.write(addr, self.a);
                return 2;
            },

            // LD A, (C) (read A from 0xFF00 + C)
            0xF2 => {
                const addr = 0xFF00 + @as(u16, self.c);
                self.a = self.bus.read(addr);
                return 2;
            },

            // LD (nn), SP (save SP to address nn)
            0x08 => {
                const addr = self.fetch16();
                // Little Endian write
                self.bus.write(addr, @truncate(self.sp));
                self.bus.write(addr + 1, @truncate(self.sp >> 8));
                return 5;
            },

            // LDH A, (n) (Load A from (0xFF00 + n))
            0xF0 => {
                const offset = self.fetch();
                const addr = 0xFF00 + @as(u16, offset);
                self.a = self.bus.read(addr);
                return 3;
            },

            // LD A, (HLI) (load A from (HL), then increment HL)
            0x2A => {
                const addr = self.get_hl();
                self.a = self.bus.read(addr);
                self.set_hl(addr +% 1); // wrapping addition (+%)
                return 2;
            },

            // LD (rr), A (store A into address pointed by BC or DE)
            0x02, 0x12 => {
                const addr = if (opcode == 0x02) self.get_bc() else self.get_de();
                self.bus.write(addr, self.a);
                return 2;
            },

            // LD (HL), n
            0x36 => {
                const n = self.fetch();
                self.bus.write(self.get_hl(), n);
                return 3;
            },

            // LD ss, nn (load 16-bit immediate)
            0x01, 0x11, 0x21, 0x31 => {
                const val = self.fetch16();
                const target_code = (opcode >> 4) & 0b11;
                switch (target_code) {
                    0 => self.set_bc(val),
                    1 => self.set_de(val),
                    2 => self.set_hl(val),
                    3 => self.sp = val,
                    else => unreachable,
                }
                return 3;
            },

            // LDH (n), A (write A to (0xFF00 + n))
            0xE0 => {
                const offset = self.fetch();
                const addr = 0xFF00 + @as(u16, offset);
                self.bus.write(addr, self.a);
                return 3;
            },

            // LD (nn), A (store accumulator to absolute address nn)
            0xEA => {
                const addr = self.fetch16();
                self.bus.write(addr, self.a);
                return 4;
            },

            // LD A, (DE) (load A from address pointed by DE)
            0x1A => {
                self.a = self.bus.read(self.get_de());
                return 2;
            },

            // LD (HLI), A (store A to (HL) and increment HL)
            0x22 => {
                const addr = self.get_hl();
                self.bus.write(addr, self.a);
                self.set_hl(addr +% 1); // wrapping addition
                return 2;
            },

            // LD A, (nn) (load A from absolute address nn)
            0xFA => {
                const addr = self.fetch16();
                self.a = self.bus.read(addr);
                return 4;
            },

            // DEC r (decrement 8-bit Register)
            // manual Format: 00 r 101
            0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x3D => {
                const target_code = (opcode >> 3) & 0b111;
                const ptr = self.decode_register_ptr(target_code);

                const original = ptr.*;
                const result = original -% 1;
                ptr.* = result;

                // flags: Z (set if 0), N=1 (Subtract), H (half carry), C (not affected)
                const z_flag = if (result == 0) FLAG_Z else 0;
                const n_flag = FLAG_N; // always set for DEC
                // half carry: set if borrow from bit 4. (original & 0xF) == 0 means result will wrap 0->F
                const h_flag = if ((original & 0x0F) == 0) FLAG_H else 0;

                // preserve C flag
                self.f = (self.f & FLAG_C) | z_flag | n_flag | h_flag;

                return 1;
            },

            // DEC ss (decrement 16-bit register)
            0x0B, 0x1B, 0x2B, 0x3B => {
                const target_code = (opcode >> 4) & 0b11;
                switch (target_code) {
                    0 => self.set_bc(self.get_bc() -% 1),
                    1 => self.set_de(self.get_de() -% 1),
                    2 => self.set_hl(self.get_hl() -% 1),
                    3 => self.sp -%= 1,
                    else => unreachable,
                }
                return 2;
            },

            // JP nn (jump absolute)
            0xC3 => {
                const target = self.fetch16();
                self.pc = target;
                return 4;
            },

            // JP cc, nn (jump absolute conditional)
            0xC2, 0xCA, 0xD2, 0xDA => {
                const target = self.fetch16(); // PC now pointing to instruction after address

                const condition = switch (opcode) {
                    0xC2 => !self.get_flag(FLAG_Z), // NZ
                    0xCA => self.get_flag(FLAG_Z), // Z
                    0xD2 => !self.get_flag(FLAG_C), // NC
                    0xDA => self.get_flag(FLAG_C), // C
                    else => unreachable,
                };

                if (condition) {
                    self.pc = target;
                    return 4;
                } else {
                    return 3;
                }
            },

            // JR n (jump relative)
            // n is a *signed* 8-bit offset
            0x18 => {
                const offset = @as(i8, @bitCast(self.fetch()));
                // wrapping addition with +%= though PC shouldn't overflow in valid ROMs
                self.pc +%= @as(u16, @bitCast(@as(i16, offset)));
                return 3;
            },

            // JR cc, n (jump relative conditional)
            0x20, 0x28, 0x30, 0x38 => {
                const offset = @as(i8, @bitCast(self.fetch()));

                const condition = switch (opcode) {
                    0x20 => !self.get_flag(FLAG_Z), // NZ (not zero)
                    0x28 => self.get_flag(FLAG_Z), // Z (zero)
                    0x30 => !self.get_flag(FLAG_C), // NC (no carry)
                    0x38 => self.get_flag(FLAG_C), // C (carry)
                    else => unreachable,
                };

                if (condition) {
                    self.pc +%= @as(u16, @bitCast(@as(i16, offset))); // see 0x18
                    return 3;
                } else {
                    return 2;
                }
            },

            // CALL nn (call subroutine)
            0xCD => {
                const target = self.fetch16();
                // push current PC (next instruction) onto stack
                self.sp -= 1;
                self.bus.write(self.sp, @truncate(self.pc >> 8)); // high byte
                self.sp -= 1;
                self.bus.write(self.sp, @truncate(self.pc)); // low byte

                self.pc = target;
                return 6;
            },

            // RET (return from subroutine)
            0xC9 => {
                const low = self.bus.read(self.sp);
                self.sp += 1;
                const high = self.bus.read(self.sp);
                self.sp += 1;

                self.pc = (@as(u16, high) << 8) | @as(u16, low);
                return 4;
            },

            // RET cc (return conditional)
            0xC0, 0xC8, 0xD0, 0xD8 => {
                const condition = switch (opcode) {
                    0xC0 => !self.get_flag(FLAG_Z), // NZ
                    0xC8 => self.get_flag(FLAG_Z), // Z
                    0xD0 => !self.get_flag(FLAG_C), // NC
                    0xD8 => self.get_flag(FLAG_C), // C
                    else => unreachable,
                };

                if (condition) {
                    const low = self.bus.read(self.sp);
                    self.sp +%= 1;
                    const high = self.bus.read(self.sp);
                    self.sp +%= 1;
                    self.pc = (@as(u16, high) << 8) | @as(u16, low);
                    return 5;
                } else {
                    return 2;
                }
            },

            // OR r (logical OR A with register r)
            0xB0...0xB7 => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                self.a |= val;

                // flags: Z (if result 0), N=0, H=0, C=0
                const z_flag = if (self.a == 0) FLAG_Z else 0;
                self.f = z_flag; // clears N, H, C automatically

                return if (src_code == 0b110) 2 else 1;
            },

            // XOR A OR (accumulator with itself)
            // common shortcut to set A = 0
            0xAF => {
                self.a = self.a ^ self.a; // always 0
                // flags: Z=1, N=0, H=0, C=0
                self.f = FLAG_Z;
                return 1;
            },

            // AND r (Bitwise AND A with r)
            0xA0...0xA7 => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                self.a &= val;

                // Z=1 if 0, N=0, H=1, C=0
                self.f = (if (self.a == 0) FLAG_Z else 0) | FLAG_H;
                return if (src_code == 0b110) 2 else 1;
            },

            // AND n (AND A with immediate byte n)
            0xE6 => {
                const n = self.fetch();
                self.a &= n;

                // Z=1 if 0, N=0, H=1, C=0
                self.f = (if (self.a == 0) FLAG_Z else 0) | FLAG_H;
                return 2;
            },

            // INC r (increment 8-bit register)
            // manual format: 00 r 100
            0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x3C => {
                const target_code = (opcode >> 3) & 0b111;
                const ptr = self.decode_register_ptr(target_code);

                const original = ptr.*;
                const result = original +% 1;
                ptr.* = result;

                // flags: Z (set if 0), N=0, H (half carry), C (not affected)
                const z_flag = if (result == 0) FLAG_Z else 0;
                // half carry: set if carry from bit 3
                const h_flag = if ((original & 0x0F) + 1 > 0x0F) FLAG_H else 0;

                // preserve C flag, set others
                self.f = (self.f & FLAG_C) | z_flag | h_flag; // N cleared

                return 1;
            },

            // INC ss (increment 16-bit register)
            0x03, 0x13, 0x23, 0x33 => {
                const target_code = (opcode >> 4) & 0b11;
                switch (target_code) {
                    0 => self.set_bc(self.get_bc() +% 1),
                    1 => self.set_de(self.get_de() +% 1),
                    2 => self.set_hl(self.get_hl() +% 1),
                    3 => self.sp +%= 1,
                    else => unreachable,
                }
                return 2;
            },

            // CB Prefix (extended operations)
            0xCB => {
                const cb_op = self.fetch();
                return self.step_cb(cb_op);
            },

            // CP n (compare A with immediate value n)
            0xFE => {
                const n = self.fetch();
                // A - n to set flags but don't store the result
                const a_val = self.a;

                const z_flag = if (a_val == n) FLAG_Z else 0;
                const n_flag = FLAG_N; // N is always set for CP/SUB
                // half carry: set if borrow from bit 4
                const h_flag = if ((a_val & 0x0F) < (n & 0x0F)) FLAG_H else 0;
                // carry: set if borrow (A < n)
                const c_flag = if (a_val < n) FLAG_C else 0;

                self.f = z_flag | n_flag | h_flag | c_flag;
                return 2;
            },

            // SUB r (subtract register r from A)
            0x90...0x97 => {
                const src_code = opcode & 0b111;
                const val = if (src_code == 0b110)
                    self.bus.read(self.get_hl())
                else
                    self.decode_register_ptr(src_code).*;

                const a_val = self.a;
                const result = a_val -% val; // wrapping subtraction
                self.a = result;

                const z_flag = if (result == 0) FLAG_Z else 0;
                const n_flag = FLAG_N;
                const h_flag = if ((a_val & 0x0F) < (val & 0x0F)) FLAG_H else 0;
                const c_flag = if (a_val < val) FLAG_C else 0;

                self.f = z_flag | n_flag | h_flag | c_flag;

                return if (src_code == 0b110) 2 else 1;
            },

            // EI (Enable Interrupts)
            0xFB => {
                // note: irl hardware enables interrupts after the next instruction not during this one
                // idk if it's a problem, assuming not
                self.ime = true;
                return 1;
            },

            // DI (Disable Interrupts)
            0xF3 => {
                self.ime = false;
                return 1;
            },

            // RETI (Return and Enable Interrupts)
            0xD9 => {
                const low = self.bus.read(self.sp);
                self.sp += 1;
                const high = self.bus.read(self.sp);
                self.sp += 1;

                self.pc = (@as(u16, high) << 8) | @as(u16, low);
                self.ime = true;
                return 4;
            },

            // STOP (Stop CPU)
            0x10 => {
                _ = self.fetch(); // STOP is 2 bytes (10 00)
                // TODO handle STOP state (LCD off, wait for button)
                // in the meantime it's like NOP
                return 1;
            },

            // HALT
            // note: this is usually handled inside the 0x40-0x7F block
            0x76 => {
                self.halted = true;
                return 1;
            },

            // RST n (restart / call vector)
            0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF => {
                const target: u16 = opcode & 0x38;
                // push pc
                self.sp -%= 1;
                self.bus.write(self.sp, @truncate(self.pc >> 8));
                self.sp -%= 1;
                self.bus.write(self.sp, @truncate(self.pc));
                self.pc = target;
                return 4;
            },

            // fallback for unimplemented opcodes
            else => {
                std.debug.print("Unknown opcode: 0x{x:0>2}\n", .{opcode});
                return 0;
            },
        }
    }
};
