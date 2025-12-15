const std = @import("std");
const DmgBus = @import("dmg_bus.zig").DmgBus;

pub const DmgCpu = struct {
    bus: *DmgBus,
    cycles: u128 = 0,

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

            // LD r, r'
            // manual format: 01 r r' (bits 7-6 are '01')
            // range: 0x40 (01 000 000) to 0x7F (01 111 111)
            0x40...0x75, 0x77...0x7F => {
                // dest bits 5-3 (r)
                const dest_code = (opcode >> 3) & 0b111;
                // source bits 2-0 (r')
                const src_code = opcode & 0b111;

                // special case: HALT exception (0x76)
                if (opcode == 0x76) {
                    // TODO HALT logic here
                    return 1;
                }

                // note: will need to handle (HL) separately for full support

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
            // opcode: 32, cycles: 8 (2 machine cycles)
            0x32 => {
                const addr = self.get_hl();
                self.bus.write(addr, self.a);
                // doing wrapping subtraction (-%) so 0x0000 wraps to 0xFFFF
                self.set_hl(addr -% 1);
                return 2;
            },

            // DEC ss (decrement 16-bit register)
            // opcodes: 0B (BC), 1B (DE), 2B (HL), 3B (SP)
            // cycles: 8 (2 machine cycles)
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

            // OR r (logical OR A with register r)
            // opcodes: B0...B7
            // cycles: 4 (1 machine cycle) - (HL) is 8 cycles
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

            // JP nn (jump absolute)
            // opcode: C3, cycles: 16 (4 machine cycles)
            0xC3 => {
                const target = self.fetch16();
                self.pc = target;
                return 4;
            },

            // JR n (jump relative)
            // opcode: 18, cycles: 12 (3 machine cycles)
            // n is a *signed* 8-bit offset
            0x18 => {
                const offset = @as(i8, @bitCast(self.fetch()));
                // wrapping addition with +%= though PC shouldn't overflow in valid ROMs
                self.pc +%= @as(u16, @bitCast(@as(i16, offset)));
                return 3;
            },

            // JR cc, n (jump relative conditional)
            // opcodes: 20 (NZ), 28 (Z), 30 (NC), 38 (C)
            // cycles: 12 if jump taken, 8 if not
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
            // opcode: CD, cycles: 24 (6 machine cycles)
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
            // opcode: C9, cycles: 16 (4 machine cycles)
            0xC9 => {
                const low = self.bus.read(self.sp);
                self.sp += 1;
                const high = self.bus.read(self.sp);
                self.sp += 1;

                self.pc = (@as(u16, high) << 8) | @as(u16, low);
                return 4;
            },

            // basic ALU stuff

            // XOR A OR (accumulator with itself)
            // common shortcut to set A = 0
            // opcode: AF, cycles: 4 (1 machine cycle)
            0xAF => {
                self.a = self.a ^ self.a; // always 0
                // flags: Z=1, N=0, H=0, C=0
                self.f = FLAG_Z;
                return 1;
            },

            // INC r (increment 8-bit register)
            // opcodes: 04, 0C, 14, 1C, 24, 2C, 3C
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

            // fallback for unimplemented opcodes
            else => {
                std.debug.print("Unknown opcode: 0x{x:0>2}\n", .{opcode});
                return 0;
            },
        }
    }
};
