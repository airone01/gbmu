pub const Button = enum { Right, Left, Up, Down, A, B, Select, Start };

pub const Joypad = struct {
    // here the state is true=pressed, which makes more sense to me but is not
    // what the data looks like, it's just simpler for me
    right: bool = false,
    left: bool = false,
    up: bool = false,
    down: bool = false,
    a: bool = false,
    b: bool = false,
    select: bool = false,
    start: bool = false,

    // P1 register (0xFF00)
    // bit 5: select button keys
    // bit 4: select direction keys
    p1_select: u8 = 0x30, // by default nothing selected

    pub fn init() Joypad {
        return Joypad{};
    }

    pub fn write(self: *Joypad, value: u8) void {
        // only bits 4 and 5 are writable
        self.p1_select = value & 0x30;
    }

    pub fn read(self: *const Joypad) u8 {
        // start with 0xCF (upper bits 1, lower bits 1 (released))
        // we OR in the selection bits
        var output: u8 = 0xCF | self.p1_select;

        // 0 means selected
        const select_buttons = (self.p1_select & 0x20) == 0;
        const select_directions = (self.p1_select & 0x10) == 0;

        if (select_buttons) {
            if (self.a) output &= ~@as(u8, 0x01);
            if (self.b) output &= ~@as(u8, 0x02);
            if (self.select) output &= ~@as(u8, 0x04);
            if (self.start) output &= ~@as(u8, 0x08);
        }

        if (select_directions) {
            if (self.right) output &= ~@as(u8, 0x01);
            if (self.left) output &= ~@as(u8, 0x02);
            if (self.up) output &= ~@as(u8, 0x04);
            if (self.down) output &= ~@as(u8, 0x08);
        }

        return output;
    }

    pub fn set_button(self: *Joypad, btn: Button, pressed: bool) void {
        switch (btn) {
            .Right => self.right = pressed,
            .Left => self.left = pressed,
            .Up => self.up = pressed,
            .Down => self.down = pressed,
            .A => self.a = pressed,
            .B => self.b = pressed,
            .Select => self.select = pressed,
            .Start => self.start = pressed,
        }
    }
};
