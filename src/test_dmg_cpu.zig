const std = @import("std");
const DmgCpu = @import("dmg_cpu.zig").DmgCpu;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test "CPU register initial state" {
    const cpu = DmgCpu.init();
    // verify DMG startup values
    try expectEqual(@as(u8, 0x01), cpu.a); 
    try expectEqual(@as(u8, 0xB0), cpu.f);
    try expectEqual(@as(u16, 0x0100), cpu.pc);
    try expectEqual(@as(u16, 0xFFFE), cpu.sp);
}

test "CPU 16-bit register pairing (BC)" {
    var cpu = DmgCpu.init();
    
    cpu.b = 0x12;
    cpu.c = 0x34;
    // check if get_bc() combines them correctly (because high-low)
    try expectEqual(@as(u16, 0x1234), cpu.get_bc());

    cpu.set_bc(0xDEAD);
    // check if were updated
    try expectEqual(@as(u8, 0xDE), cpu.b);
    try expectEqual(@as(u8, 0xAD), cpu.c);
}

test "CPU flags logic (F register)" {
    var cpu = DmgCpu.init();
    
    // test setting specific flags
    cpu.set_flag(DmgCpu.FLAG_Z, true); // zero flag
    try expect(cpu.get_flag(DmgCpu.FLAG_Z));
    try expectEqual(@as(u8, 0x80), cpu.f & 0x80);

    // test that lower 4 bits are always 0
    cpu.f = 0xFF; // try to set all bits
    cpu.set_af(0xFFFF); 
    try expectEqual(@as(u8, 0xF0), cpu.f); // lower 4 bits must be cleared
}

