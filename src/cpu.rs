#![allow(non_snake_case)]
// $0000-$00FF -- Zero Page
// $0100-$01FF -- System Stack
//SP initialised to $FF

//Addressing modes:
//Implicit
//Accumulator
//Immediate
//Indirect -- Only used with JMP
//Zero Page
//Absolute

//Zero Page,X
//Zero Page,Y
//Relative
//Absolute,X
//Absolute,Y
//(Indirect, X)
//(Indirect), Y

//Make functions for all 56 unique instructions
//Make a mapping between opcode -> (instruction, addressing_mode)

//in main loop, increment PC after calculating instruction length. then execute instruction.
//PC increment should be before instruction execution
//clock cycles will be incremented in the instruction functions

//Currently only Mapper 0 is supported in the emulator

pub mod joypad;

use std::ops::Not;
use std::{fs::File, os::windows::fs::FileExt};

pub struct CPU {
    pc : u16,
    sp : u8,
    acc : u8,
    ix : u8,
    iy : u8,
    status : u8,    //NV1B DIZC

    clk_cycles : u32,
    internal_ram : [u8; 0x0800],
    instruction_table : HashMap<u8, (fn(&mut Self, Operand), AddressingMode)>,

    rom: File,
    pub ppu: PPU,
    pub joypad: Joypad,
    prg_rom: [u8; 0x4000],
}

enum StatusBit {
    N, V, B, D, I, Z, C
}

// //i am so sorry
#[derive(Clone, Copy)]
enum Operand {
    Imp,
    Acc,
    Imm(u8),
    ZP(u8),
    ZPX(u8),
    ZPY(u8),
    Rel(i8),
    Abs(u16),
    AX(u16),
    AY(u16),
    Ind(u16),
    IX(u8),
    IY(u8),
}

#[derive(Clone, Copy)]
enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
}

use std::collections::HashMap;
// use std::os::unix::fs::FileExt;
use AddressingMode::*;
use StatusBit::*;
use Operand::*;

use crate::ppu::{PPURegister, PPU};

use self::joypad::Joypad;

impl AddressingMode {
    fn to_operand(&self, data: u16) -> Operand {
        match self {
            AddressingMode::Implied => Imp,
            AddressingMode::Accumulator => Acc,
            AddressingMode::Immediate => Imm(data as u8),
            AddressingMode::ZeroPage => ZP(data as u8),
            AddressingMode::ZeroPageX => ZPX(data as u8),
            AddressingMode::ZeroPageY => ZPY(data as u8),
            AddressingMode::Relative => Rel(data as i8),
            AddressingMode::Absolute => Abs(data),
            AddressingMode::AbsoluteX => AX(data),
            AddressingMode::AbsoluteY => AY(data),
            AddressingMode::Indirect => Ind(data),
            AddressingMode::IndirectX => IX(data as u8),
            AddressingMode::IndirectY => IY(data as u8),
        }
    }
}

fn calculate_instruction_length(mode: AddressingMode) -> u16 {
    match mode {
        AddressingMode::Implied => 1,
        AddressingMode::Accumulator => 1,
        AddressingMode::Immediate => 2,
        AddressingMode::ZeroPage => 2,
        AddressingMode::ZeroPageX => 2,
        AddressingMode::ZeroPageY => 2,
        AddressingMode::Relative => 2,
        AddressingMode::Absolute => 3,
        AddressingMode::AbsoluteX => 3,
        AddressingMode::AbsoluteY => 3,
        AddressingMode::Indirect => 3,
        AddressingMode::IndirectX => 2,
        AddressingMode::IndirectY => 2,
    }
}


impl CPU {
    // resets program counter
    pub fn init(&mut self) {
        for _ in 0..0x4000 {
            self.rom.seek_read(&mut self.prg_rom, 0x10).unwrap();
        }

        self.pc = self.convert_indirect(0xfffc);
        // self.pc = 0xc000;

        self.sp = self.sp.wrapping_sub(3);
        self.set_status_bit(I, true);
        self.clk_cycles += 7;
        for _ in 0..(7*3) {
            self.ppu.tick();
        }
    }

    //returns the number of CPU clock cycles elapsed
    pub fn execute(&mut self) -> (String, u32) {
        let instr = self.mem_read(self.pc);
        // println!("{:4x} {:2x}", self.pc, instr);

        let f = self.instruction_table.get(&instr).unwrap();
        let length = calculate_instruction_length(f.1);

        let prev_clk_cycles = self.clk_cycles;
        let ppu_cycles = self.ppu.get_clk_cycles();

        let s;

        match length {
            1 => {
                s = format!("{:04X}  {:02X}        A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}",
                    self.pc, instr, self.acc, self.ix, self.iy, self.status, self.sp, 
                    ppu_cycles.0, ppu_cycles.1, self.clk_cycles);
                self.pc += 1;
                let g  = self.instruction_table.get(&instr).unwrap();
                g.0(self, g.1.to_operand(0));
            },
            2 => {
                let op = self.mem_read(self.pc + 1);

                s = format!("{:04X}  {:02X} {:02X}     A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}", 
                    self.pc, instr, op, self.acc, self.ix, self.iy, self.status, self.sp, 
                    ppu_cycles.0, ppu_cycles.1, self.clk_cycles);
                // println!("cyc: {}; pc: {:#06x}; instr: {:#04x}; op: {:#04x}; len: 2", self.clk_cycles , self.pc, instr, op);
                self.pc += 2;
                let g  = self.instruction_table.get(&instr).unwrap();
                g.0(self, g.1.to_operand(op as u16));
            },
            3 => {
                let lsb = self.mem_read(self.pc + 1);
                let msb = self.mem_read(self.pc + 2);
                let op = (msb as u16).checked_shl(8).unwrap_or(0) + (lsb as u16);

                s = format!("{:02X}  {:02X} {:02X} {:02X}  A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}", 
                    self.pc, instr, lsb, msb, self.acc, self.ix, self.iy, self.status, self.sp, 
                    ppu_cycles.0, ppu_cycles.1, self.clk_cycles);
                // println!("cyc: {}; pc: {:#06x}; instr: {:#04x}; op: {:#06x}; len: 3", self.clk_cycles , self.pc, instr, op);
                self.pc += 3;
                let g  = self.instruction_table.get(&instr).unwrap();
                g.0(self, g.1.to_operand(op));
            },
            _ => { s = String::new()},
        };

        // let err02 = self.mem_read(0x0002);
        // let err03 = self.mem_read(0x0003);

        // if err02 != 0 || err03 != 0 {
        //     println!("0x02: {}; 0x03: {};", err02, err03);
        //     return None;
        // }
        // sleep(std::time::Duration::from_millis(300));
        
        (s, self.clk_cycles - prev_clk_cycles)
    }

    pub fn new(file_name: &str) -> Result<Self, std::io::Error> {

        let fp = File::open(file_name)?;
        let mut it: HashMap<u8, (fn(&mut Self, Operand), AddressingMode)> = HashMap::new();

        //ADC
        it.insert(0x69, (CPU::instr_ADC, Immediate));
        it.insert(0x65, (CPU::instr_ADC, ZeroPage));
        it.insert(0x75, (CPU::instr_ADC, ZeroPageX));
        it.insert(0x6D, (CPU::instr_ADC, Absolute));
        it.insert(0x7D, (CPU::instr_ADC, AbsoluteX));
        it.insert(0x79, (CPU::instr_ADC, AbsoluteY));
        it.insert(0x61, (CPU::instr_ADC, IndirectX));
        it.insert(0x71, (CPU::instr_ADC, IndirectY));

        //AND
        it.insert(0x29, (CPU::instr_AND, Immediate));
        it.insert(0x25, (CPU::instr_AND, ZeroPage));
        it.insert(0x35, (CPU::instr_AND, ZeroPageX));
        it.insert(0x2D, (CPU::instr_AND, Absolute));
        it.insert(0x3D, (CPU::instr_AND, AbsoluteX));
        it.insert(0x39, (CPU::instr_AND, AbsoluteY));
        it.insert(0x21, (CPU::instr_AND, IndirectX));
        it.insert(0x31, (CPU::instr_AND, IndirectY));

        //ASL
        it.insert(0x0A, (CPU::instr_ASL, Accumulator));
        it.insert(0x06, (CPU::instr_ASL, ZeroPage));
        it.insert(0x16, (CPU::instr_ASL, ZeroPageX));
        it.insert(0x0E, (CPU::instr_ASL, Absolute));
        it.insert(0x1E, (CPU::instr_ASL, AbsoluteX));

        //Branches
        it.insert(0x90, (CPU::instr_BCC, Relative));
        it.insert(0xB0, (CPU::instr_BCS, Relative));
        it.insert(0xF0, (CPU::instr_BEQ, Relative));
        it.insert(0x30, (CPU::instr_BMI, Relative));
        it.insert(0xD0, (CPU::instr_BNE, Relative));
        it.insert(0x10, (CPU::instr_BPL, Relative));
        it.insert(0x50, (CPU::instr_BVC, Relative));
        it.insert(0x70, (CPU::instr_BVS, Relative));

        //BIT
        it.insert(0x24, (CPU::instr_BIT, ZeroPage));
        it.insert(0x2C, (CPU::instr_BIT, Absolute));

        it.insert(0x00, (CPU::instr_BRK, Implied));
        it.insert(0x18, (CPU::instr_CLC, Implied));
        it.insert(0xD8, (CPU::instr_CLD, Implied));
        it.insert(0x58, (CPU::instr_CLI, Implied));
        it.insert(0xB8, (CPU::instr_CLV, Implied));

        //CMP
        it.insert(0xC9, (CPU::instr_CMP, Immediate));
        it.insert(0xC5, (CPU::instr_CMP, ZeroPage));
        it.insert(0xD5, (CPU::instr_CMP, ZeroPageX));
        it.insert(0xCD, (CPU::instr_CMP, Absolute));
        it.insert(0xDD, (CPU::instr_CMP, AbsoluteX));
        it.insert(0xD9, (CPU::instr_CMP, AbsoluteY));
        it.insert(0xC1, (CPU::instr_CMP, IndirectX));
        it.insert(0xD1, (CPU::instr_CMP, IndirectY));

        it.insert(0xE0, (CPU::instr_CPX, Immediate));
        it.insert(0xE4, (CPU::instr_CPX, ZeroPage));
        it.insert(0xEC, (CPU::instr_CPX, Absolute));

        it.insert(0xC0, (CPU::instr_CPY, Immediate));
        it.insert(0xC4, (CPU::instr_CPY, ZeroPage));
        it.insert(0xCC, (CPU::instr_CPY, Absolute));

        it.insert(0xC6, (CPU::instr_DEC, ZeroPage));
        it.insert(0xD6, (CPU::instr_DEC, ZeroPageX));
        it.insert(0xCE, (CPU::instr_DEC, Absolute));
        it.insert(0xDE, (CPU::instr_DEC, AbsoluteX));

        it.insert(0xCA, (CPU::instr_DEX, Implied));
        it.insert(0x88, (CPU::instr_DEY, Implied));

        it.insert(0x49, (CPU::instr_EOR, Immediate));
        it.insert(0x45, (CPU::instr_EOR, ZeroPage));
        it.insert(0x55, (CPU::instr_EOR, ZeroPageX));
        it.insert(0x4D, (CPU::instr_EOR, Absolute));
        it.insert(0x5D, (CPU::instr_EOR, AbsoluteX));
        it.insert(0x59, (CPU::instr_EOR, AbsoluteY));
        it.insert(0x41, (CPU::instr_EOR, IndirectX));
        it.insert(0x51, (CPU::instr_EOR, IndirectY));

        it.insert(0xE6, (CPU::instr_INC, ZeroPage));
        it.insert(0xF6, (CPU::instr_INC, ZeroPageX));
        it.insert(0xEE, (CPU::instr_INC, Absolute));
        it.insert(0xFE, (CPU::instr_INC, AbsoluteX));

        it.insert(0xE8, (CPU::instr_INX, Implied));
        it.insert(0xC8, (CPU::instr_INY, Implied));

        it.insert(0x4C, (CPU::instr_JMP, Absolute));
        it.insert(0x6C, (CPU::instr_JMP, Indirect));
        it.insert(0x20, (CPU::instr_JSR, Absolute));

        it.insert(0xA9, (CPU::instr_LDA, Immediate));
        it.insert(0xA5, (CPU::instr_LDA, ZeroPage));
        it.insert(0xB5, (CPU::instr_LDA, ZeroPageX));
        it.insert(0xAD, (CPU::instr_LDA, Absolute));
        it.insert(0xBD, (CPU::instr_LDA, AbsoluteX));
        it.insert(0xB9, (CPU::instr_LDA, AbsoluteY));
        it.insert(0xA1, (CPU::instr_LDA, IndirectX));
        it.insert(0xB1, (CPU::instr_LDA, IndirectY));

        it.insert(0xA2, (CPU::instr_LDX, Immediate));
        it.insert(0xA6, (CPU::instr_LDX, ZeroPage));
        it.insert(0xB6, (CPU::instr_LDX, ZeroPageY));
        it.insert(0xAE, (CPU::instr_LDX, Absolute));
        it.insert(0xBE, (CPU::instr_LDX, AbsoluteY));

        it.insert(0xA0, (CPU::instr_LDY, Immediate));
        it.insert(0xA4, (CPU::instr_LDY, ZeroPage));
        it.insert(0xB4, (CPU::instr_LDY, ZeroPageX));
        it.insert(0xAC, (CPU::instr_LDY, Absolute));
        it.insert(0xBC, (CPU::instr_LDY, AbsoluteX));

        it.insert(0x4A, (CPU::instr_LSR, Accumulator));
        it.insert(0x46, (CPU::instr_LSR, ZeroPage));
        it.insert(0x56, (CPU::instr_LSR, ZeroPageX));
        it.insert(0x4E, (CPU::instr_LSR, Absolute));
        it.insert(0x5E, (CPU::instr_LSR, AbsoluteX));

        it.insert(0xEA, (CPU::instr_NOP, Implied));

        it.insert(0x09, (CPU::instr_ORA, Immediate));
        it.insert(0x05, (CPU::instr_ORA, ZeroPage));
        it.insert(0x15, (CPU::instr_ORA, ZeroPageX));
        it.insert(0x0D, (CPU::instr_ORA, Absolute));
        it.insert(0x1D, (CPU::instr_ORA, AbsoluteX));
        it.insert(0x19, (CPU::instr_ORA, AbsoluteY));
        it.insert(0x01, (CPU::instr_ORA, IndirectX));
        it.insert(0x11, (CPU::instr_ORA, IndirectY));

        it.insert(0x48, (CPU::instr_PHA, Implied));
        it.insert(0x08, (CPU::instr_PHP, Implied));
        it.insert(0x68, (CPU::instr_PLA, Implied));
        it.insert(0x28, (CPU::instr_PLP, Implied));

        it.insert(0x2A, (CPU::instr_ROL, Accumulator));
        it.insert(0x26, (CPU::instr_ROL, ZeroPage));
        it.insert(0x36, (CPU::instr_ROL, ZeroPageX));
        it.insert(0x2E, (CPU::instr_ROL, Absolute));
        it.insert(0x3E, (CPU::instr_ROL, AbsoluteX));

        it.insert(0x6A, (CPU::instr_ROR, Accumulator));
        it.insert(0x66, (CPU::instr_ROR, ZeroPage));
        it.insert(0x76, (CPU::instr_ROR, ZeroPageX));
        it.insert(0x6E, (CPU::instr_ROR, Absolute));
        it.insert(0x7E, (CPU::instr_ROR, AbsoluteX));

        it.insert(0x40, (CPU::instr_RTI, Implied));
        it.insert(0x60, (CPU::instr_RTS, Implied));

        it.insert(0xE9, (CPU::instr_SBC, Immediate));
        it.insert(0xE5, (CPU::instr_SBC, ZeroPage));
        it.insert(0xF5, (CPU::instr_SBC, ZeroPageX));
        it.insert(0xED, (CPU::instr_SBC, Absolute));
        it.insert(0xFD, (CPU::instr_SBC, AbsoluteX));
        it.insert(0xF9, (CPU::instr_SBC, AbsoluteY));
        it.insert(0xE1, (CPU::instr_SBC, IndirectX));
        it.insert(0xF1, (CPU::instr_SBC, IndirectY));

        it.insert(0x38, (CPU::instr_SEC, Implied));
        it.insert(0xF8, (CPU::instr_SED, Implied));
        it.insert(0x78, (CPU::instr_SEI, Implied));

        it.insert(0x85, (CPU::instr_STA, ZeroPage));
        it.insert(0x95, (CPU::instr_STA, ZeroPageX));
        it.insert(0x8D, (CPU::instr_STA, Absolute));
        it.insert(0x9D, (CPU::instr_STA, AbsoluteX));
        it.insert(0x99, (CPU::instr_STA, AbsoluteY));
        it.insert(0x81, (CPU::instr_STA, IndirectX));
        it.insert(0x91, (CPU::instr_STA, IndirectY));

        it.insert(0x86, (CPU::instr_STX, ZeroPage));
        it.insert(0x96, (CPU::instr_STX, ZeroPageY));
        it.insert(0x8E, (CPU::instr_STX, Absolute));

        it.insert(0x84, (CPU::instr_STY, ZeroPage));
        it.insert(0x94, (CPU::instr_STY, ZeroPageX));
        it.insert(0x8C, (CPU::instr_STY, Absolute));

        it.insert(0xAA, (CPU::instr_TAX, Implied));
        it.insert(0xA8, (CPU::instr_TAY, Implied));
        it.insert(0xBA, (CPU::instr_TSX, Implied));
        it.insert(0x8A, (CPU::instr_TXA, Implied));
        it.insert(0x9A, (CPU::instr_TXS, Implied));
        it.insert(0x98, (CPU::instr_TYA, Implied));

        //Invalid NOP
        it.insert(0x80, (CPU::instr_NOP, Immediate));
        it.insert(0x82, (CPU::instr_NOP, Immediate));
        it.insert(0xC2, (CPU::instr_NOP, Immediate));
        it.insert(0xE2, (CPU::instr_NOP, Immediate));
        it.insert(0x04, (CPU::instr_NOP, ZeroPage));
        it.insert(0x44, (CPU::instr_NOP, ZeroPage));
        it.insert(0x64, (CPU::instr_NOP, ZeroPage));
        it.insert(0x89, (CPU::instr_NOP, Immediate));
        it.insert(0xEA, (CPU::instr_NOP, Implied));
        it.insert(0x0C, (CPU::instr_NOP, Absolute));
        it.insert(0x14, (CPU::instr_NOP, ZeroPageX));
        it.insert(0x34, (CPU::instr_NOP, ZeroPageX));
        it.insert(0x54, (CPU::instr_NOP, ZeroPageX));
        it.insert(0x74, (CPU::instr_NOP, ZeroPageX));
        it.insert(0xD4, (CPU::instr_NOP, ZeroPageX));
        it.insert(0xF4, (CPU::instr_NOP, ZeroPageX));
        it.insert(0x1A, (CPU::instr_NOP, Implied));
        it.insert(0x3A, (CPU::instr_NOP, Implied));
        it.insert(0x5A, (CPU::instr_NOP, Implied));
        it.insert(0x7A, (CPU::instr_NOP, Implied));
        it.insert(0xDA, (CPU::instr_NOP, Implied));
        it.insert(0xFA, (CPU::instr_NOP, Implied));
        it.insert(0x1C, (CPU::instr_NOP, AbsoluteX));
        it.insert(0x3C, (CPU::instr_NOP, AbsoluteX));
        it.insert(0x5C, (CPU::instr_NOP, AbsoluteX));
        it.insert(0x7C, (CPU::instr_NOP, AbsoluteX));
        it.insert(0xDC, (CPU::instr_NOP, AbsoluteX));
        it.insert(0xFC, (CPU::instr_NOP, AbsoluteX));


        

        let this = Self {
            pc: 0x0000,  //tentative
            sp: 0x00,    // i think this is true?
            acc: 0x00,
            ix: 0x00,
            iy: 0x00,
            status: 0b00100000,
            clk_cycles: 0,
            internal_ram: [0; 2048],
            instruction_table: it,

            rom: fp,
            ppu: PPU::new(file_name)?,
            joypad: Joypad::new(),
            prg_rom: [0; 0x4000],
        };


        Ok(this)
    }

    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.internal_ram[(addr % 0x0800) as usize],

            0x2000 => self.ppu.cpu_reg_read(PPURegister::PPUCtrl),
            0x2001 => self.ppu.cpu_reg_read(PPURegister::PPUMask),
            0x2002 => self.ppu.cpu_reg_read(PPURegister::PPUStatus),
            0x2003 => self.ppu.cpu_reg_read(PPURegister::OAMAddr),
            0x2004 => self.ppu.cpu_reg_read(PPURegister::OAMData),
            0x2005 => self.ppu.cpu_reg_read(PPURegister::PPUScroll),
            0x2006 => self.ppu.cpu_reg_read(PPURegister::PPUAddr),
            0x2007 => self.ppu.cpu_reg_read(PPURegister::PPUData),

            0x2008..=0x3FFF => self.mem_read((addr % 8) + 0x2000),

            0x4014 => self.ppu.cpu_reg_read(PPURegister::OAMDMA),

            0x4016 => self.joypad.read(),

            0x8000..=0xFFFF => {
                self.prg_rom[((addr - 0x8000) % 0x4000) as usize]
            }
            _ => 0,
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.internal_ram[(addr % 0x0800) as usize] = data;
            },

            0x2000 => self.ppu.cpu_reg_write(data, PPURegister::PPUCtrl),
            0x2001 => self.ppu.cpu_reg_write(data, PPURegister::PPUMask),
            0x2002 => self.ppu.cpu_reg_write(data, PPURegister::PPUStatus),
            0x2003 => self.ppu.cpu_reg_write(data, PPURegister::OAMAddr),
            0x2004 => self.ppu.cpu_reg_write(data, PPURegister::OAMData),
            0x2005 => self.ppu.cpu_reg_write(data, PPURegister::PPUScroll),
            0x2006 => self.ppu.cpu_reg_write(data, PPURegister::PPUAddr),
            0x2007 => self.ppu.cpu_reg_write(data, PPURegister::PPUData),

            0x2008..=0x3FFF => self.mem_write((addr % 8) + 0x2000, data),

            0x4014 => {
                let page_addr = (data as u16) << 8;
                for i in page_addr..=(page_addr + 0xFF) {
                    let oam_data = self.mem_read(i);
                    self.ppu.cpu_reg_write(oam_data, PPURegister::OAMDMA);
                }

                for _ in 1..=513 {
                    //hoping to god oamdma is only accessed during vblank rn
                    self.ppu.tick();
                }
                
                //idk if this is right but oh well
                if self.clk_cycles % 2 == 1 {
                    self.ppu.tick();
                }
            },

            0x4016 => self.joypad.write(data),

            _ => {},
        }
    }

    fn mem_read_operand(&mut self, operand: Operand) -> u8 {
        match operand {
            Imp | Acc => 0,
            Imm(val) => val,
            ZP(_) | ZPX(_) | ZPY(_) | Rel(_) | Abs(_) |
            AX(_) | AY(_) | Ind(_) | IX(_) | IY(_) => {
                let addr = self.convert_operand_to_address(operand);
                self.mem_read(addr)
            },
        }
    }

    fn mem_write_operand(&mut self, operand: Operand, data: u8) {
        match operand {
            Imp | Acc | Imm(_) => {},
            ZP(_) | ZPX(_) | ZPY(_) | Rel(_) | Abs(_) |
            AX(_) | AY(_) | Ind(_) | IX(_) | IY(_) => {
                let addr = self.convert_operand_to_address(operand);
                self.mem_write(addr, data);
            },
        }
    }

    fn extract_status_bit(&self, s: StatusBit) -> bool {
        match s {
            N => self.status >> 7 != 0,
            V => (self.status >> 6) & 0b01 != 0,
            B => (self.status >> 4) & 0b01 != 0,
            D => (self.status >> 3) & 0b01 != 0,
            I => (self.status >> 2) & 0b01 != 0,
            Z => (self.status >> 1) & 0b01 != 0,
            C => self.status & 0b01 != 0,
        }
    }

    fn set_status_bit(&mut self, s: StatusBit, value: bool) {
        match value {
            true => {
                match s {
                    N => self.status |= 0b01u8 << 7,
                    V => self.status |= 0b01u8 << 6,
                    B => self.status |= 0b01u8 << 4,
                    D => self.status |= 0b01u8 << 3,
                    I => self.status |= 0b01u8 << 2,
                    Z => self.status |= 0b01u8 << 1,
                    C => self.status |= 0b01u8,
                };
            },
            false => {
                match s {
                    N => self.status &= (0b01u8 << 7).not(),
                    V => self.status &= (0b01u8 << 6).not(),
                    B => self.status &= (0b01u8 << 4).not(),
                    D => self.status &= (0b01u8 << 3).not(),
                    I => self.status &= (0b01u8 << 2).not(),
                    Z => self.status &= (0b01u8 << 1).not(),
                    C => self.status &= 0b01u8.not(),
                }
            }
        }
    }


    //Translate addressing modes into their effective addresses
    fn convert_zero_page_x(&self, addr: u8) -> u16 {
        addr.wrapping_add(self.ix) as u16
    }

    fn convert_zero_page_y(&self, addr: u8) -> u16 {
        addr.wrapping_add(self.iy) as u16
    }

    fn convert_relative(&self, offset: i8) -> u16 {
        (offset as i32 + self.pc as i32) as u16
    }

    fn convert_absolute_x(&self, addr: u16) -> u16 {
        let val = addr + (self.ix as u16);
        val
    }

    fn convert_absolute_y(&self, addr: u16) -> u16 {
        let val = addr + (self.iy as u16);
        val
    }

    fn convert_indirect(&mut self, addr: u16) -> u16 {
        //APPARENTLY THERE IS PAGE-WISE WRAP AROUND FOR THIS???? why is this not written anywhere????
        let lsb = self.mem_read(addr) as u16;
        let wrapped_around_addr_lsb = ((addr & 0x00ff) as u8).wrapping_add(1) as u16;
        let msb = self.mem_read((addr & 0xff00) + wrapped_around_addr_lsb) as u16;
        (msb.checked_shl(8).unwrap_or(0)) + lsb
    }

    fn convert_indirect_x(&mut self, addr: u8) -> u16 {
        let effective_addr = addr.wrapping_add(self.ix);
        let lsb = self.mem_read(effective_addr as u16) as u16;
        let msb = self.mem_read(effective_addr.wrapping_add(1) as u16) as u16;
        (msb.checked_shl(8).unwrap_or(0)) + lsb
    }

    fn convert_indirect_y(&mut self, addr: u8) -> u16 {
        let lsb = self.mem_read(addr as u16) as u16;
        //maybe have to deal with wrapping addition in next line
        let msb = self.mem_read(addr.wrapping_add(1) as u16) as u16;
        let msb = msb.checked_shl(8).unwrap_or(0);
        // (msb) + lsb + (self.iy as u16)
        msb.wrapping_add(lsb).wrapping_add(self.iy as u16)
    }

    fn convert_operand_to_address(&mut self, operand: Operand) -> u16 {
        match operand {
            Imp => 0,
            Acc => 0,
            Imm(val) => val as u16,
            ZP(addr) => addr as u16,
            ZPX(addr) => self.convert_zero_page_x(addr),
            ZPY(addr) => self.convert_zero_page_y(addr),
            Rel(offset) => self.convert_relative(offset),
            Abs(addr) => addr,
            AX(addr) => self.convert_absolute_x(addr),
            AY(addr) => self.convert_absolute_y(addr),
            Ind(addr) => self.convert_indirect(addr),
            IX(addr) => self.convert_indirect_x(addr),
            IY(addr) => self.convert_indirect_y(addr),
        }
    }

    fn page_boundary_crossed(&mut self, operand: Operand) -> bool {
        //page boundary crossed if MSB in address changes after adding ix or iy registers
        match operand {
            Rel(offset) => {
                (self.convert_relative(offset) >> 8) ^ (self.pc >> 8) != 0b0
            },
            AX(addr) => {
                let val = self.convert_absolute_x(addr);
                (addr >> 8) ^ (val >> 8) != 0b0
            },
            AY(addr) => {
                let val = self.convert_absolute_y(addr);
                (addr >> 8) ^ (val >> 8) != 0b0
            },
            IY(addr) => {
                let val = self.convert_indirect_y(addr);
                (val >> 8) ^ ((val.wrapping_sub(self.iy as u16)) >> 8) != 0b0
            },
            _ => false
        }
    }

    //useful functions for implementing the below instructions
    fn instruction_branch(&mut self, operand: Operand, s: StatusBit, bit_set: bool) {
        match operand {
            Rel(offset) => {
                if self.extract_status_bit(s) == bit_set {
                    self.clk_cycles += 1;
                    if self.page_boundary_crossed(operand) {
                        self.clk_cycles += 1;
                    }
                    self.pc = self.convert_relative(offset);
                }
                self.clk_cycles += 2;
            }
            _ => {},
        }
    }

    fn instruction_push_stack(&mut self, data: u8) {
        self.mem_write(0x0100 + self.sp as u16, data);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn instruction_pull_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mem_read(0x0100 + self.sp as u16)
    }

    pub fn service_nmi(&mut self) {
        //force interrupt
        self.instruction_push_stack((self.pc >> 8) as u8);
        self.instruction_push_stack((self.pc) as u8);
        self.instruction_push_stack(self.status);  //b flag
        self.set_status_bit(I, true);

        self.pc = self.convert_indirect(0xFFFA);
        self.clk_cycles += 7;
    }


    //Instructions======================================================================================================
    //==================================================================================================================

    fn instr_ADC(&mut self, operand: Operand) {
        let old_acc = self.acc;
        let op = self.mem_read_operand(operand);
        let c = self.extract_status_bit(C) as u8;
        self.acc = self.acc.wrapping_add(op).wrapping_add(c);

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        let result_u16: u16 = old_acc as u16 + op as u16 + c as u16;
        self.set_status_bit(C, result_u16 != self.acc as u16);

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc >> 7 == 0b1);

        let result_i16 = (old_acc as i8) as i16 + (op as i8) as i16 + (c as i8) as i16;
        self.set_status_bit(V, result_i16 != self.acc as i16);

        let op_plus_c = op.wrapping_add(c);

        self.set_status_bit(V,
            (op_plus_c >> 7 != op >> 7) ||
            ((op_plus_c >> 7 == old_acc >> 7) && (self.acc >> 7 != old_acc >> 7)));

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_AND(&mut self, operand: Operand) {
        self.acc &= self.mem_read_operand(operand);

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc >> 7 == 0b1);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_ASL(&mut self, operand: Operand) {
        match operand {
            Acc => {
                self.set_status_bit(C, self.acc >> 7 != 0);
                self.acc = self.acc.checked_shl(1).unwrap_or(0);

                self.set_status_bit(N,
                    self.acc >> 7 == 0b01);

                self.set_status_bit(Z,
                    self.acc == 0);
            }
            ZP(_) | ZPX(_) | Abs(_) | AX(_) => {
                let mut data = self.mem_read_operand(operand);
                self.set_status_bit(C, data >> 7 != 0);
                data = data.checked_shl(1).unwrap_or(0);

                self.set_status_bit(N,
                    data >> 7 == 0b01);

                self.set_status_bit(Z,
                    data == 0);

                self.mem_write_operand(operand, data);
            }
            _ => {}
        }

        match operand {
            Acc => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {}
        }

    }

    fn instr_BCC(&mut self, operand: Operand) {
        self.instruction_branch(operand, C, false);
    }

    fn instr_BCS(&mut self, operand: Operand) {
        self.instruction_branch(operand, C, true);
    }

    fn instr_BEQ(&mut self, operand: Operand) {
        self.instruction_branch(operand, Z, true);
    }

    fn instr_BIT(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);

        self.set_status_bit(Z,
            op & self.acc == 0b0);

        self.set_status_bit(V,
            (op >> 6) & 0b01 != 0);

        self.set_status_bit(N,
            op >> 7 != 0);

        match operand {
            ZP(_) => self.clk_cycles += 3,
            Abs(_) => self.clk_cycles += 4,
            _ => {},
        }
    }

    fn instr_BMI(&mut self, operand: Operand) {
        self.instruction_branch(operand, N, true);
    }

    fn instr_BNE(&mut self, operand: Operand) {
        self.instruction_branch(operand, Z, false);
    }

    fn instr_BPL(&mut self, operand: Operand) {
        self.instruction_branch(operand, N, false);
    }

    fn instr_BRK(&mut self, _operand: Operand) {
        //force interrupt
        self.instruction_push_stack((self.pc >> 8) as u8);
        self.instruction_push_stack((self.pc) as u8);
        self.instruction_push_stack(self.status | 0b00010000);  //b flag
        self.set_status_bit(I, true);

        self.pc = self.convert_indirect(0xFFFE);
        self.clk_cycles += 7;
    }

    fn instr_BVC(&mut self, operand: Operand) {
        self.instruction_branch(operand, V, false);
    }

    fn instr_BVS(&mut self, operand: Operand) {
        self.instruction_branch(operand, V, true);
    }

    fn instr_CLC(&mut self, _operand: Operand) {
        self.set_status_bit(C, false);
        self.clk_cycles += 2;
    }

    fn instr_CLD(&mut self, _operand: Operand) {
        self.set_status_bit(D, false);
        self.clk_cycles += 2;
    }

    fn instr_CLI(&mut self, _operand: Operand) {
        self.set_status_bit(I, false);
        self.clk_cycles += 2;
    }

    fn instr_CLV(&mut self, _operand: Operand) {
        self.set_status_bit(V, false);
        self.clk_cycles += 2;
    }

    fn instr_CMP(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.set_status_bit(C,
            self.acc >= op);

        self.set_status_bit(Z,
            self.acc == op);

        self.set_status_bit(N,
            self.acc.wrapping_sub(op) >> 7 != 0);

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_CPX(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.set_status_bit(C,
            self.ix >= op);

        self.set_status_bit(Z,
            self.ix == op);

        self.set_status_bit(N,
            self.ix.wrapping_sub(op) >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            Abs(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_CPY(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.set_status_bit(C,
            self.iy >= op);

        self.set_status_bit(Z,
            self.iy == op);

        self.set_status_bit(N,
            self.iy.wrapping_sub(op) >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            Abs(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_DEC(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        let dec_op = op.wrapping_sub(1);
        self.mem_write_operand(operand, dec_op);

        self.set_status_bit(Z,
            dec_op == 0);

        self.set_status_bit(N,
            dec_op >> 7 != 0);

        match operand {
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {},
        }
    }

    fn instr_DEX(&mut self, _operand: Operand) {
        self.ix = self.ix.wrapping_sub(1);

        self.set_status_bit(Z,
            self.ix == 0);

        self.set_status_bit(N,
            self.ix >> 7 != 0);

        self.clk_cycles += 2;
    }

    fn instr_DEY(&mut self, _operand: Operand) {
        self.iy = self.iy.wrapping_sub(1);

        self.set_status_bit(Z,
            self.iy == 0);

        self.set_status_bit(N,
            self.iy >> 7 != 0);

        self.clk_cycles += 2;
    }

    fn instr_EOR(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.acc ^= op;

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_INC(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        let inc_op = op.wrapping_add(1);
        self.mem_write_operand(operand, inc_op);

        self.set_status_bit(Z,
            inc_op == 0);

        self.set_status_bit(N,
            inc_op >> 7 != 0);

        match operand {
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {},
        }
    }

    fn instr_INX(&mut self, _operand: Operand) {
        self.ix = self.ix.wrapping_add(1);

        self.set_status_bit(Z,
            self.ix == 0);

        self.set_status_bit(N,
            self.ix >> 7 != 0);

        self.clk_cycles += 2;
    }

    fn instr_INY(&mut self, _operand: Operand) {
        self.iy = self.iy.wrapping_add(1);

        self.set_status_bit(Z,
            self.iy == 0);

        self.set_status_bit(N,
            self.iy >> 7 != 0);

        self.clk_cycles += 2;
    }

    fn instr_JMP(&mut self, operand: Operand) {
        //theres some note here about how the original 6502 doesnt deal with indirect JMPs on page boundary fetches
        //or something. probably doesnt matter for implementation purposes
        match operand {
            Abs(addr) => {
                self.pc = addr;
                self.clk_cycles += 3;
            },
            Ind(addr) => {
                self.pc = self.convert_indirect(addr);
                self.clk_cycles += 5;
            }
            _ => {},
        }

    }

    //again the pc needs to be incremented in the main loop before the instruction is executed
    fn instr_JSR(&mut self, operand: Operand) {
        match operand {
            Abs(target_addr) => {
                let ret_addr = self.pc - 1;
                let lsb = (ret_addr & 0x00FF) as u8;
                let msb = (ret_addr >> 8) as u8;
                self.instruction_push_stack(msb);
                self.instruction_push_stack(lsb);

                self.pc = target_addr;
                self.clk_cycles += 6;
            },
            _ => {},
        }
    }

    fn instr_LDA(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.acc = op;

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_LDX(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.ix = op;

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.ix == 0);

        self.set_status_bit(N,
            self.ix >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPY(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_LDY(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.iy = op;

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.iy == 0);

        self.set_status_bit(N,
            self.iy >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_LSR(&mut self, operand: Operand) {
        match operand {
            Acc => {
                self.set_status_bit(C, self.acc.checked_shl(7).unwrap_or(0) != 0);
                self.acc >>= 1;

                self.set_status_bit(N,
                    self.acc >> 7 == 0b01);

                self.set_status_bit(Z,
                    self.acc == 0);
            }
            ZP(_) | ZPX(_) | Abs(_) | AX(_) => {
                let mut data = self.mem_read_operand(operand);
                self.set_status_bit(C, data.checked_shl(7).unwrap_or(0) != 0);
                data >>= 1;

                self.set_status_bit(N,
                    data >> 7 == 0b01);

                self.set_status_bit(Z,
                    data == 0);

                self.mem_write_operand(operand, data);
            }
            _ => {},
        }

        match operand {
            Acc => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {}
        }
    }

    fn instr_NOP(&mut self, operand: Operand) {
        match operand {
            Imp => self.clk_cycles += 2,
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            _ => return,
        }

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }
    }

    fn instr_ORA(&mut self, operand: Operand) {
        let op = self.mem_read_operand(operand);
        self.acc |= op;

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc >> 7 != 0);

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_PHA(&mut self, _operand: Operand) {
        self.instruction_push_stack(self.acc);
        self.clk_cycles += 3;
    }

    fn instr_PHP(&mut self, _operand: Operand) {
        self.instruction_push_stack(self.status | 0b00010000);
        self.clk_cycles += 3;
    }

    fn instr_PLA(&mut self, _operand: Operand) {
        self.acc = self.instruction_pull_stack();

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc & 0x80 != 0);

        self.clk_cycles += 4;
    }

    fn instr_PLP(&mut self, _operand: Operand) {
        self.status = self.instruction_pull_stack() & 0b11101111;
        self.status |= 0b0010_0000;
        self.clk_cycles += 4;
    }

    fn instr_ROL(&mut self, operand: Operand) {
        match operand {
            Acc => {
                let old_carry = self.extract_status_bit(C) as u8;
                self.set_status_bit(C, self.acc >> 7 != 0);
                self.acc = (self.acc.checked_shl(1).unwrap_or(0)) | old_carry;

                self.set_status_bit(N,
                    self.acc >> 7 == 0b01);

                self.set_status_bit(Z,
                    self.acc == 0);
            },
            ZP(_) | ZPX(_) | Abs(_) | AX(_) => {
                let mut op = self.mem_read_operand(operand);
                let old_carry = self.extract_status_bit(C) as u8;
                self.set_status_bit(C, op >> 7 != 0);

                op = (op.checked_shl(1).unwrap_or(0))  | old_carry;

                self.set_status_bit(N,
                    op >> 7 == 0b01);

                // maybe uncomment
                // self.set_status_bit(Z,
                //     op == 0);

                self.mem_write_operand(operand, op);
            }
            _ => {},
        }

        match operand {
            Acc => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {}
        }
    }

    fn instr_ROR(&mut self, operand: Operand) {
        match operand {
            Acc => {
                let old_carry = self.extract_status_bit(C) as u8;
                self.set_status_bit(C, self.acc & 0x01 != 0);
                self.acc = (self.acc >> 1) | (old_carry.checked_shl(7).unwrap_or(0));

                self.set_status_bit(N,
                    self.acc & 0x80 != 0);

                self.set_status_bit(Z,
                    self.acc == 0);
            },
            ZP(_) | ZPX(_) | Abs(_) | AX(_) => {
                let mut op = self.mem_read_operand(operand);
                let old_carry = self.extract_status_bit(C) as u8;
                self.set_status_bit(C, op & 0x01 != 0);
                op = (op >> 1) | (old_carry.checked_shl(7).unwrap_or(0));

                self.set_status_bit(N,
                    op & 0x80 != 0);

                // maybe uncomment
                // self.set_status_bit(Z,
                //     op == 0);

                self.mem_write_operand(operand, op);
            }
            _ => {},
        }

        match operand {
            Acc => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 5,
            ZPX(_) => self.clk_cycles += 6,
            Abs(_) => self.clk_cycles += 6,
            AX(_) => self.clk_cycles += 7,
            _ => {}
        }
    }

    //idk about this one but we shall see
    fn instr_RTI(&mut self, _operand: Operand) {
        self.status = self.instruction_pull_stack() & 0b11101111;
        self.status |= 0b0010_0000;
        let lsb = self.instruction_pull_stack();
        let msb = self.instruction_pull_stack();
        let new_pc = ((msb as u16).checked_shl(8).unwrap_or(0)) + (lsb as u16);
        self.pc = new_pc;
        self.clk_cycles += 6;
    }

    //obelisk says decrement pulled value by 1 before assigning to PC.
    //im gonna ignore that for now and increment the pulled value,
    //but just writing this here in case self.instruction_table messes things up.
    fn instr_RTS(&mut self, _operand: Operand) {
        let lsb = self.instruction_pull_stack();
        let msb = self.instruction_pull_stack();
        let new_pc = ((msb as u16).checked_shl(8).unwrap_or(0)) + (lsb as u16) + 1;
        self.pc = new_pc;
        self.clk_cycles += 6;
    }

    //can be implemented by twos complement-ing the operand and then just adding i think
    //borrow = 1 => carry = 0
    //borrow = 0 => carry = 1
    fn instr_SBC(&mut self, operand: Operand) {
        let old_acc = self.acc;
        let neg_op = self.mem_read_operand(operand).wrapping_neg();
        let b = (!self.extract_status_bit(C)) as u8;
        self.acc = self.acc.wrapping_add(neg_op).wrapping_sub(b);

        if self.page_boundary_crossed(operand) {
            self.clk_cycles += 1;
        }

        // match self.acc.checked_add(neg_op) {
        //     None => self.set_status_bit(C, true),
        //     Some(num) => {
        //         match num.checked_add(b.wrapping_neg()) {
        //             None => self.set_status_bit(C, true),
        //             Some(_) => self.set_status_bit(C, false),
        //         }
        //     }
        // }

        let result_u16: u16 = old_acc as u16 + neg_op as u16 + b.wrapping_neg() as u16;
        self.set_status_bit(C, result_u16 != self.acc as u16);

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc & 0x80 != 0);

        // let mut overflow_reg = 0;
        // let mut overflow_flag = false;
        // if (neg_op >> 7 == old_acc >> 7) || (neg_op >> 7 == b.wrapping_neg() >> 7) {
        //     overflow_flag = true;
        //     overflow_reg = neg_op;
        // }
        // if old_acc >> 7 == b.wrapping_neg() >> 7 {
        //     overflow_flag = true;
        //     overflow_reg = old_acc;
        // }

        // self.set_status_bit(V,
        //     overflow_flag && (self.acc >> 7 != overflow_reg >> 7));

        let result_i16 = (old_acc as i8) as i16 + (neg_op as i8) as i16 + (b.wrapping_neg() as i8) as i16;
        self.set_status_bit(V, result_i16 != self.acc as i16);

        // self.set_status_bit(V,
        //     old_acc.overflowing_add(neg_op).1 || old_acc.overflowing_add(neg_op).0
        //     .overflowing_add(b.wrapping_neg()).1);

        let neg_op_plus_c = neg_op.wrapping_sub(b);

        self.set_status_bit(V,
            (neg_op_plus_c >> 7 != neg_op >> 7) ||
            ((neg_op_plus_c >> 7 == old_acc >> 7) && (self.acc >> 7 != old_acc >> 7)));
    

        match operand {
            Imm(_) => self.clk_cycles += 2,
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 4,
            AY(_) => self.clk_cycles += 4,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 5,
            _ => {}
        }
    }

    fn instr_SEC(&mut self, _operand: Operand) {
        self.set_status_bit(C, true);
        self.clk_cycles += 2;
    }

    fn instr_SED(&mut self, _operand: Operand) {
        self.set_status_bit(D, true);
        self.clk_cycles += 2;
    }

    fn instr_SEI(&mut self, _operand: Operand) {
        self.set_status_bit(I, true);
        self.clk_cycles += 2;
    }

    fn instr_STA(&mut self, operand: Operand) {
        self.mem_write_operand(operand, self.acc);

        match operand {
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            AX(_) => self.clk_cycles += 5,
            AY(_) => self.clk_cycles += 5,
            IX(_) => self.clk_cycles += 6,
            IY(_) => self.clk_cycles += 6,
            _ => {}
        }
    }

    fn instr_STX(&mut self, operand: Operand) {
        self.mem_write_operand(operand, self.ix);

        match operand {
            ZP(_) => self.clk_cycles += 3,
            ZPY(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_STY(&mut self, operand: Operand) {
        self.mem_write_operand(operand, self.iy);

        match operand {
            ZP(_) => self.clk_cycles += 3,
            ZPX(_) => self.clk_cycles += 4,
            Abs(_) => self.clk_cycles += 4,
            _ => {}
        }
    }

    fn instr_TAX(&mut self, _operand: Operand) {
        self.ix = self.acc;

        self.set_status_bit(Z,
            self.ix == 0);

        self.set_status_bit(N,
            self.ix & 0x80 != 0);

        self.clk_cycles += 2;
    }

    fn instr_TAY(&mut self, _operand: Operand) {
        self.iy = self.acc;

        self.set_status_bit(Z,
            self.iy == 0);

        self.set_status_bit(N,
            self.iy & 0x80 != 0);

        self.clk_cycles += 2;
    }

    fn instr_TSX(&mut self, _operand: Operand) {
        self.ix = self.sp;

        self.set_status_bit(Z,
            self.ix == 0);

        self.set_status_bit(N,
            self.ix & 0x80 != 0);

        self.clk_cycles += 2;
    }

    fn instr_TXA(&mut self, _operand: Operand) {
        self.acc = self.ix;

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc & 0x80 != 0);

        self.clk_cycles += 2;
    }

    fn instr_TXS(&mut self, _operand: Operand) {
        self.sp = self.ix;
        self.clk_cycles += 2;
    }

    fn instr_TYA(&mut self, _operand: Operand) {
        self.acc = self.iy;

        self.set_status_bit(Z,
            self.acc == 0);

        self.set_status_bit(N,
            self.acc & 0x80 != 0);

        self.clk_cycles += 2;
    }

}

