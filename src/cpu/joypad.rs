//almost entirely taken from https://bugzmanov.github.io/nes_ebook/chapter_7.html

pub struct Joypad {
    strobe: bool,
    button_index: u8,
    button_status: u8,  //
}

#[derive(Debug)]
pub enum JoypadButton {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right
}

impl Joypad {
    pub fn new() -> Self {
        Joypad {
            strobe: false,
            button_index: 0,
            button_status: 0b0000_0000,
        }
    }

    pub fn set_button(&mut self, pressed: bool, button: &JoypadButton) {
        let left_shift_num = match button {
            JoypadButton::A => 0,
            JoypadButton::B => 1,
            JoypadButton::Select => 2,
            JoypadButton::Start => 3,
            JoypadButton::Up => 4,
            JoypadButton::Down => 5,
            JoypadButton::Left => 6,
            JoypadButton::Right => 7,
        };

        if pressed {
            let mask = (pressed as u8) << left_shift_num;
            self.button_status = self.button_status | mask;
        } else {
            let mask = 0b1111_1111 & ((!0b1u8).rotate_left(left_shift_num));
            self.button_status = self.button_status & mask;
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 0b1 == 1;
        if self.strobe {
            self.button_index = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }

        let response = (self.button_status & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index <= 7 {
            self.button_index += 1
        };
        response
    }
}