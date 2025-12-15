const std = @import("std");

pub const DmgCpu = struct {
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

    pub fn init() DmgCpu {
        return DmgCpu{
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

    /// execute a CPU step
    /// returns cpu count of cycles taken
    fn step(self: *DmgCpu) u16 {
        const opcode: u8 = self.pc;

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
                    self.bus.read(self.cpu.get_hl()) // read mem at (HL)
                else 
                    self.decode_register_ptr(src_code).*; // read register

                if (dest_code == 0b110) {
                    self.bus.write(self.cpu.get_hl(), val); // write memory at (HL)
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
                    self.bus.write(self.cpu.get_hl(), n);
                    return 3; // memory write takes longer
                } else {
                    self.decode_register_ptr(dest_code).* = n;
                    return 2;
                }
            },

            // fallback for unimplemented opcodes
            else => {
                std.debug.print("Unknown Opcode: 0x{x:0>2}\n", .{opcode});
                return 0;
            }
        }
    }
};
