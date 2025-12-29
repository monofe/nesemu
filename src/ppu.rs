// ok so i'll trigger an nmi by setting the field in the PPU struct. The cpu struct will then contain an instance of
// PPU as a field. this is since the PPU does not directly change the properties of the CPU except for triggering an nmi (i think)

//i think bit 6 of ppuctrl should just be 0 


//change cpu such that you can make it run one clock cycle one at a time
//change main loop to run 3 ppu clock cycles, then 1 cpu clock cycle
//draw one pixel into the frame buffer per ppu clock cycle (when not in vblank)
//

use std::{fs::File, os::windows::fs::FileExt};

pub const SYSTEM_PALETTE: [(u8,u8,u8); 64] = [
    (0x66, 0x66, 0x66), (0x00, 0x2a, 0x88), (0x14, 0x12, 0xa7), (0x3b, 0x00, 0xa4), 
    (0x5c, 0x00, 0x7e), (0x6e, 0x00, 0x40), (0x6c, 0x06, 0x00), (0x56, 0x1d, 0x00), 
    (0x33, 0x35, 0x00), (0x0b, 0x48, 0x00), (0x00, 0x52, 0x00), (0x00, 0x4f, 0x08), 
    (0x00, 0x40, 0x4d), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), 
    (0xad, 0xad, 0xad), (0x15, 0x5f, 0xd9), (0x42, 0x40, 0xff), (0x75, 0x27, 0xfe), 
    (0xa0, 0x1a, 0xcc), (0xb7, 0x1e, 0x7b), (0xb5, 0x31, 0x20), (0x99, 0x4e, 0x00), 
    (0x6b, 0x6d, 0x00), (0x38, 0x87, 0x00), (0x0c, 0x93, 0x00), (0x00, 0x8f, 0x32), 
    (0x00, 0x7c, 0x8d), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), 
    (0xff, 0xfe, 0xff), (0x64, 0xb0, 0xff), (0x92, 0x90, 0xff), (0xc6, 0x76, 0xff), 
    (0xf3, 0x6a, 0xff), (0xfe, 0x6e, 0xcc), (0xfe, 0x81, 0x70), (0xea, 0x9e, 0x22), 
    (0xbc, 0xbe, 0x00), (0x88, 0xd8, 0x00), (0x5c, 0xe4, 0x30), (0x45, 0xe0, 0x82), 
    (0x48, 0xcd, 0xde), (0x4f, 0x4f, 0x4f), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), 
    (0xff, 0xfe, 0xff), (0xc0, 0xdf, 0xff), (0xd3, 0xd2, 0xff), (0xe8, 0xc8, 0xff), 
    (0xfb, 0xc2, 0xff), (0xfe, 0xc4, 0xea), (0xfe, 0xcc, 0xc5), (0xf7, 0xd8, 0xa5), 
    (0xe4, 0xe5, 0x94), (0xcf, 0xef, 0x96), (0xbd, 0xf4, 0xab), (0xb3, 0xf3, 0xcc), 
    (0xb5, 0xeb, 0xf2), (0xb8, 0xb8, 0xb8), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00)
];



pub enum PPURegister {
    PPUCtrl,
    PPUMask,
    PPUStatus,
    OAMAddr,
    OAMData,
    PPUScroll,
    PPUAddr,
    PPUData,
    OAMDMA,
}

enum Mirroring {
    Vertical,
    Horizontal,
}

enum Pixel {
    Coloured(u8, u8, u8),
    Transparent,
}

impl Pixel {
    fn to_rgb(self) -> (u8, u8, u8) {
        match self {
            Pixel::Coloured(a,b,c) => (a,b,c),
            Pixel::Transparent => (0,0,0),
        }
    }
}

fn select_bit(bit_no: u8, data: u8) -> u8 {
    let mask = 0b1 << bit_no;

    match data & mask {
        0 => 0,
        _ => 1,
    }
}

pub struct PPU {
    ppuctrl: u8,
    ppumask: u8,
    ppustatus: u8,
    oamaddr: u8,
    oamdata: u8,
    ppuscroll: u8,
    ppuaddr: u16,
    ppudata: u8,

    // vram: [u8; 0x0800],
    // oam: [u8; 256],
    // palette: [u8; 32],
    // chr_rom: [u8; 0x2000],

    vram: Vec<u8>,
    oam: Vec<u8>,
    palette: Vec<u8>,
    chr_rom: Vec<u8>,

    v: u16,     //15 bits
    t: u16,     //15 bits
    x: u8,      //3 bits
    w: bool,    //1 bit
    ppudata_read_buffer: u8,    //should store the previous byte that the CPU attempted to read
    ppuscroll_x: u8,    //least significant 8 bits
    ppuscroll_y: u8,    //least significant 8 bits

    pub frame_buffer: Vec<Vec<(u8, u8, u8)>>,
    rom: File,

    nmi_flag: bool,
    new_frame_ready: bool,
    clk_cycle: u32,

    scanline_no: u16,
    vblank_on: bool,
    mirroring: Mirroring,

    //https://www.nesdev.org/wiki/NMI
    //nmi_occurred is bit 7 of ppustatus
    //nmi_output is bit 7 of ppuctrl
    //nmi_flag should be set as nmi_occurred && nmi_output whenever either one changes
}

impl PPU {
    pub fn new(file_name: &str) -> Result<Self, std::io::Error> {
        //does the power up state matter? i dont know!
        let fp = File::open(file_name)?;

        // let mut buf = [0u8];
        // fp.seek_read(&mut buf, 6)?;

        let mut buf = vec![0u8; 0x2000];
        fp.seek_read(&mut buf, 0x4000 + 0x10)?;

        let ppu = PPU {
            ppuctrl: 0,
            ppumask: 0,
            ppustatus: 0,
            oamaddr: 0,
            oamdata: 0,
            ppuscroll: 0,
            ppuaddr: 0,
            ppudata: 0,

            mirroring: match buf[0] & 0x01 {
                0 => Mirroring::Vertical,
                1 => Mirroring::Horizontal,
                _ => panic!("Mirroring fucked up")
            },

            vram: vec![0; 0x0800],
            oam: vec![0; 256],
            palette: vec![0; 32],
            chr_rom: buf,

            v: 0,
            t: 0,
            x: 0,
            w: false,
            ppudata_read_buffer: 0,
            ppuscroll_x: 0,
            ppuscroll_y: 0,

            frame_buffer: vec![vec![(0u8,0u8,0u8); 256]; 240],
            rom: fp,
            nmi_flag: false,
            new_frame_ready: false,
            clk_cycle: 0,
            scanline_no: 0,
            vblank_on: false,

            
        };

        Ok(ppu)
    }

    pub fn cpu_reg_write(&mut self, data: u8, reg: PPURegister) {
        match reg {
            PPURegister::PPUCtrl => {
                self.ppuctrl = data;
                // https://www.nesdev.org/wiki/PPU_scrolling#PPU_internal_registers
                self.t &= 0b11110011_11111111;
                self.t |= ((data & 0b0000_0011) as u16) << 10;
            },
            PPURegister::PPUMask => {
                self.ppumask = data
            },
            PPURegister::PPUStatus => panic!("Tried to write to read-only PPU register (PPUStatus)"), //ppustatus is read-only
            PPURegister::OAMAddr => self.oamaddr = data,
            PPURegister::OAMData => {
                // need to deal with some case of writes occurring during rendering
                self.oamdata = data;
                self.oam[self.oamaddr as usize] = data;
                self.oamaddr = self.oamaddr.wrapping_add(1);
                // self.oamaddr += 1;  // i think?
            },
            PPURegister::PPUScroll => {
                match self.w {
                    false => {
                        // t: ....... ...ABCDE <- d: ABCDE...
                        // x:              FGH <- d: .....FGH
                        // w:                  <- 1
                        self.t &= 0b11111111_11100000;
                        self.t |= (data >> 3) as u16;
                        self.x = data & 0b0000_0111;
                        self.w = true;
                        
                        self.ppuscroll_x = data;
                    },
                    true => {
                        //t: FGH..AB CDE..... <- d: ABCDEFGH
                        //w:                  <- 0
                        self.t &= 0b10001100_00011111;
                        self.t |= ((data & 0b1100_0000) as u16) << 2;   //AB
                        self.t |= ((data & 0b0011_1000) as u16) << 2;   //CDE
                        self.t |= (data as u16) << 12;
                        self.w = false;

                        self.ppuscroll_y = data;
                    },
                }
            },
            PPURegister::PPUAddr => {
                match self.w {
                    false => {
                        //t: .CDEFGH ........ <- d: ..CDEFGH
                        // <unused>     <- d: AB......
                        // t: Z...... ........ <- 0 (bit Z is cleared)
                        // w:                  <- 1
                        self.t &= 0b01000000_11111111;
                        self.t |= ((data & 0b0011_1111) as u16) << 8;
                        self.ppuaddr = (data as u16) << 8;
                        self.w = true;
                    },
                    true => {
                        //t: ....... ABCDEFGH <- d: ABCDEFGH
                        // v: <...all bits...> <- t: <...all bits...>
                        // w:                  <- 0
                        self.t &= 0b11111111_00000000;
                        self.t |= data as u16;
                        self.v = self.t;
                        self.ppuaddr |= data as u16;
                        self.w = false;
                        // println!("v: {}", self.v);
                    },
                }
            },
            PPURegister::PPUData => {
                let ppuctrl_bit_two = self.ppuctrl & 0b0000_0100;
                self.mem_write(self.v, data);
                let increment = match ppuctrl_bit_two {
                    0 => 1,
                    _ => 32,
                };
                // println!("read v: {}", self.v);

                self.v = (self.v + increment) % 0x4000;
            },
            PPURegister::OAMDMA => {
                //obviously this isnt how an oam dma write from the cpu works
                //this is just writing one byte of data from the CPU into OAM
                self.oam[self.oamaddr as usize] = data;
                self.oamaddr = self.oamaddr.wrapping_add(1);
                //after 256 of these, self.oamaddr should return to its original value
            }
        }
    } 

    pub fn cpu_reg_read(&mut self, reg: PPURegister) -> u8 {
        match reg {
            PPURegister::PPUCtrl | PPURegister::PPUMask | PPURegister::OAMAddr |
            PPURegister::PPUScroll | PPURegister::PPUAddr | PPURegister::OAMDMA => {
                panic!("Tried to read write-only PPU register")
            },
            PPURegister::PPUStatus => {
                self.w = false;
                let value = self.ppustatus;
                self.ppustatus &= 0b0111_1111;
                self.set_nmi_flag();
                value
            },   
            PPURegister::OAMData => self.oamdata,
            PPURegister::PPUData => {
                let ppuctrl_bit_two = self.ppuctrl & 0b0000_0100;
                let value: u8;

                match self.v {
                    0x0000..=0x3EFF => {
                        value = self.ppudata_read_buffer;
                        self.ppudata_read_buffer = self.mem_read(self.v);
                    },
                    _ => {
                        value = self.mem_read(self.v);
                    },
                }

                let increment = match ppuctrl_bit_two {
                    0 => 1,
                    _ => 32,
                };

                self.v = (self.v + increment) % 0x4000;
                value
            },
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => {
                // let mut buf = [0u8];
                // let offset = addr + 0x4000 + 0x10;
                // match self.rom.seek_read(&mut buf, offset as u64) {
                //     Ok(_) => buf[0],
                //     Err(_) => panic!("Error reading chr rom from file i guess"),
                // }
                self.chr_rom[addr as usize]
            },

            //nametable access
            0x2000..=0x2FFF => {
                match self.mirroring {
                    Mirroring::Vertical => {
                        match addr {
                            0x2000..=0x23FF => self.vram[(addr - 0x2000) as usize],
                            0x2400..=0x27FF => self.vram[(addr - 0x2000) as usize],
                            0x2800..=0x2BFF => self.vram[(addr - 0x2800) as usize],
                            0x2C00..=0x2FFF => self.vram[(addr - 0x2800) as usize],
                            _ => panic!("This wont happen"),
                        }
                    },
                    Mirroring::Horizontal => {
                        match addr {
                            0x2000..=0x23FF => self.vram[(addr - 0x2000) as usize],
                            0x2400..=0x27FF => self.vram[(addr - 0x2400) as usize],
                            0x2800..=0x2BFF => self.vram[(addr - 0x2400) as usize],
                            0x2C00..=0x2FFF => self.vram[(addr - 0x2800) as usize],
                            _ => panic!("This wont happen"),
                        }
                    }
                }
            },

            //unused memory
            0x3000..=0x3EFF => {
                self.mem_read(addr - 0x1000)
            },

            //palette
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.palette[(addr - 0x3F10) as usize]
            },
            0x3F00..=0x3F1F => {
                self.palette[(addr - 0x3F00) as usize]
            },
            0x3F20..=0x3FFF => {
                self.mem_read(0x3F00 + (addr % 0x20))
            },
            _ => panic!("PPU address should not be above 0x3FFF"),
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                panic!("Cannot write to chr rom, addr: {}", addr);
            },

            //nametable access
            0x2000..=0x2FFF => {
                match self.mirroring {
                    Mirroring::Vertical => {
                        match addr {
                            0x2000..=0x23FF => self.vram[(addr - 0x2000) as usize] = data,
                            0x2400..=0x27FF => self.vram[(addr - 0x2000) as usize] = data,
                            0x2800..=0x2BFF => self.vram[(addr - 0x2800) as usize] = data,
                            0x2C00..=0x2FFF => self.vram[(addr - 0x2800) as usize] = data,
                            _ => panic!("This wont happen"),
                        }
                    },
                    Mirroring::Horizontal => {
                        match addr {
                            0x2000..=0x23FF => self.vram[(addr - 0x2000) as usize] = data,
                            0x2400..=0x27FF => self.vram[(addr - 0x2400) as usize] = data,
                            0x2800..=0x2BFF => self.vram[(addr - 0x2400) as usize] = data,
                            0x2C00..=0x2FFF => self.vram[(addr - 0x2800) as usize] = data,
                            _ => panic!("This wont happen"),
                        }
                    }
                }
            },

            // //unused memory
            // 0x3000..=0x3EFF => {
            //     self.mem_read(addr - 0x1000)
            // },

            // palette
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.palette[(addr - 0x3F10) as usize] = data
            },
            0x3F00..=0x3F1F => {
                self.palette[(addr - 0x3F00) as usize] = data
            },
            0x3F20..=0x3FFF => {
                self.mem_write(0x3F00 + (addr % 0x20), data)
            },
            _ => {self.mem_write(addr % 0x4000, data)},
        }
    }

    fn read_nametable(&self, nametable_no: u8, addr: u16) -> u8 {
        match self.mirroring {            
            Mirroring::Vertical => {
                match nametable_no {
                    0 | 2 => {
                        // self.mem_read(addr)
                        self.vram[addr as usize]
                    },
                    1 | 3 => {
                        // self.mem_read(addr + 0x0400)
                        self.vram[(addr + 0x0400) as usize]
                    },
                    _ => panic!("This wont happen"),
                }
            },
            Mirroring::Horizontal => {
                match nametable_no {
                    0 | 1 => {
                        // self.mem_read(addr)
                        self.vram[addr as usize]
                    },
                    2 | 3 => {
                        // self.mem_read(addr + 0x0400)
                        self.vram[(addr + 0x0400) as usize]
                    },
                    _ => panic!("This wont happen"),
                }
            }
        }
    }

    pub fn nmi(&mut self) -> bool {
        match self.nmi_flag {
            true => {
                self.nmi_flag = false;
                true
            },
            false => false,
        }
    }

    pub fn new_frame_ready(&mut self) -> bool {
        if self.new_frame_ready {
            self.new_frame_ready = false;
            true
        } else {
            false
        }
    }

    pub fn tick(&mut self) {
        if self.clk_cycle + 1 > 340 {
            self.clk_cycle = 0;
            self.scanline_no = (self.scanline_no + 1) % 262;
        } else {
            self.clk_cycle += 1;
        }

        let sprite_zero_scanline: u16 = self.oam[0] as u16;     //sprite zero y coord
        let sprite_zero_clk_cycle: u32 = self.oam[3] as u32;    //sprite zero x coord

        if self.scanline_no == sprite_zero_scanline && self.clk_cycle == sprite_zero_clk_cycle {
            //sprite zero hit logic
            self.ppustatus |= 0b0100_0000;
            return;
        }

        match (self.scanline_no, self.clk_cycle) {
            (240, 0) => {
                self.render_frame();
                self.new_frame_ready = true;
            },
            (241, 1) => {
                self.vblank_on = true;
                self.ppustatus |= 0b1000_0000;  //set bit 7 on
                self.set_nmi_flag();
            },
            (261, 1) => {
                self.vblank_on = false;
                self.ppustatus &= 0b0001_1111;  //clear all of the flags
                self.set_nmi_flag();
            },
            //https://www.nesdev.org/wiki/PPU_registers#OAMADDR
            (0..=239, 257..=320) | (261, 257..=320) => {
                self.oamaddr = 0;
            }
            _ => {},
        }
    }

    fn render_frame(&mut self) {
        //need to add all the ppu register related stuff-
        //as well as 8x16 sprite support

        //not adding scroll for now
        let nametable_no = self.ppuctrl & 0b0000_0011;  //either 0,1,2 or 3
        let pattern_tbl_no: u16  = match self.ppuctrl & 0b0001_0000 {
            0 => 0b0,
            _ => 0b1,
        };  //either left or right

        for i in 0..0x03c0 {
            let tile_x = i % 32;
            let tile_y = i / 32;
            let tile_number = self.read_nametable(nametable_no, i);

            // DCBA98 76543210
            // ---------------
            // 0HNNNN NNNNPyyy
            // |||||| |||||+++- T: Fine Y offset, the row number within a tile
            // |||||| ||||+---- P: Bit plane (0: less significant bit; 1: more significant bit)
            // ||++++-++++----- N: Tile number from name table
            // |+-------------- H: Half of pattern table (0: "left"; 1: "right")
            // +--------------- 0: Pattern table is at $0000-$1FFF


            //need to multiply tile_x and tile_y by 8 for pixel number

            for y in 0..=7 {
                let tile_row_addr: u16 = ((pattern_tbl_no << 12) 
                    | ((tile_number as u16) << 4)) + y;

                //selecting bit plane bit in address
                //msbit first, lsbit second
                let tile_row = (self.mem_read(tile_row_addr | 0b1000),
                    self.mem_read(tile_row_addr | 0b0000));

                //the below palette addr has it hardcoded in that its only for backgrounds
                let attribute_tbl_block_no = (8 * (tile_y / 4)) + (tile_x / 4);
                let attribute_tbl_byte = self.read_nametable(nametable_no, 0x03C0 + attribute_tbl_block_no);
                let mut palette_addr: u16 = 0b0011_1111_0000_0000;

                //figuring out which quadrant of a block the tile is in
                //then changing the address into the palette based on that
                match (((tile_x % 4) / 2), ((tile_y % 4) / 2)) {
                    (0, 0) => {     //top left quadrant
                        palette_addr |= (attribute_tbl_byte as u16 & 0b0000_0011) << 2;
                    },
                    (1, 0) => {     //top right quadrant
                        palette_addr |= attribute_tbl_byte as u16 & 0b0000_1100;
                    },
                    (0, 1) => {     //bottom left quadrant
                        palette_addr |= (attribute_tbl_byte as u16 & 0b0011_0000) >> 2;
                    },
                    (1, 1) => {     //bottom right quadrant
                        palette_addr |= (attribute_tbl_byte as u16 & 0b1100_0000) >> 4;
                    },
                    _ => {},
                };

                //MAYBE NEED TO REVERSE X HERE IDK
                for x in 0..=7 {
                    let bit_colour = (select_bit(x, tile_row.0) << 1) + 
                        select_bit(x, tile_row.1);

                    let colour;
                    let mut colour_addr = palette_addr;

                    match bit_colour {
                        0b00 => {
                            colour = (0u8, 0u8, 0u8);
                        },
                        _ => {
                            //this is the final address of the colour we need
                            colour_addr |= (bit_colour) as u16;
                            colour = SYSTEM_PALETTE[self.mem_read(colour_addr) as usize];
                        }
                    }
                    //now write this colour into the frame
                    self.frame_buffer[(tile_y*8 + y as u16) as usize][(tile_x * 8 + (7-x) as u16) as usize] = colour;
                }
            }
        }


        for i in 0..64 {
            self.render_sprite(63-i);
        }
    }

    fn render_sprite(&mut self, oam_sprite_index: usize) {
        let sprite_pattern_table_no = match self.ppuctrl & 0b0000_1000 {
            0 => 0b0,
            _ => 0b1,
        };

        //8x8 sprites for now
        let y_pos: u16 = self.oam[oam_sprite_index*4] as u16;       //y position of top of sprite (on screen i think)
        let byte_1 = self.oam[oam_sprite_index*4 + 1];          //tile index number (for 8x8, its slightly different for 8x16)
        let byte_2 = self.oam[oam_sprite_index*4 + 2];          //attributes
        let x_pos: u16 = self.oam[oam_sprite_index*4 + 3] as u16;   //x position of left of sprite

        for y in 0..8 {
            let tile_row_addr: u16 = ((sprite_pattern_table_no << 12) | ((byte_1 as u16) << 4)) + y;

            let tile_row = (self.mem_read(tile_row_addr | 0b1000),
                self.mem_read(tile_row_addr | 0b0000));

            //the below palette addr has it hardcoded in that its only for backgrounds
            let mut palette_addr: u16 = 0b0011_1111_0001_0000;

            //byte 2 palette bits
            palette_addr |= ((byte_2 & 0b0000_0011) << 2) as u16;

            for x in 0..8 {
                let bit_colour = (select_bit(x, tile_row.0) << 1) + 
                    select_bit(x, tile_row.1);
                
                let colour;
                let mut colour_addr = palette_addr;

                match bit_colour {
                    0b00 => {
                        // colour = (0u8, 0u8, 0u8);
                        colour = Pixel::Transparent;
                    },
                    _ => {
                        //this is the final address of the colour we need
                        colour_addr |= (bit_colour) as u16;
                        let tuple_colour = SYSTEM_PALETTE[self.mem_read(colour_addr) as usize];
                        colour = Pixel::Coloured(tuple_colour.0, tuple_colour.1, tuple_colour.2)
                    }
                }

                let effective_x = match byte_2 & 0b0100_0000 {
                    0 => x,
                    _ => 7-x,
                };

                let effective_y = match byte_2 & 0b1000_0000 {
                    0 => y,
                    _ => 7-y,
                };

                //now write this colour into the frame
                if ((y_pos as usize + y as usize) < 240) && ((x_pos as usize + (7-effective_x) as usize) < 256) {
                    match colour {
                        Pixel::Transparent => {},
                        Pixel::Coloured(r,g,b) => {
                            self.frame_buffer[y_pos as usize + effective_y as usize]
                                [x_pos as usize + (7-effective_x) as usize] = (r,g,b);
                        },
                    }

                    // self.frame_buffer[y_pos as usize + effective_y as usize]
                    //     [x_pos as usize + (7-effective_x) as usize] = colour;
                }
            }
        }
    }


    fn set_nmi_flag(&mut self) {
        self.nmi_flag = ((self.ppustatus >> 7) & (self.ppuctrl >> 7)) != 0;
    }

    pub fn get_clk_cycles(&self) -> (u16, u32) {
        (self.scanline_no, self.clk_cycle)
    }
}