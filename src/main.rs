mod cpu;
mod ppu;

use std::collections::HashMap;
use std::error::Error;
use std::io::Write;
use std::thread::sleep;
use std::fs::File;

use sdl2::event::Event;
use sdl2::{log, EventPump};
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

fn main() -> Result<(), Box<dyn Error>> {
    std::env::set_var("RUST_BACKTRACE", "full");

    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("NES emu", 256*2, 240*2)
        .position_centered()
        .build()?;

    let mut canvas = window.into_canvas().present_vsync().build()?;
    let mut event_pump = sdl_context.event_pump()?;
    canvas.set_scale(4.0, 4.0)?;


    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)?;

    let mut key_map = HashMap::new();
    key_map.insert(Keycode::Z, cpu::joypad::JoypadButton::A);
    key_map.insert(Keycode::X, cpu::joypad::JoypadButton::B);
    key_map.insert(Keycode::RShift, cpu::joypad::JoypadButton::Select);
    key_map.insert(Keycode::Return, cpu::joypad::JoypadButton::Start);
    key_map.insert(Keycode::Up, cpu::joypad::JoypadButton::Up);
    key_map.insert(Keycode::Down, cpu::joypad::JoypadButton::Down);
    key_map.insert(Keycode::Left, cpu::joypad::JoypadButton::Left);
    key_map.insert(Keycode::Right, cpu::joypad::JoypadButton::Right);

    let mut f = File::create("mylog.log")?;
    let mut cpu = cpu::CPU::new("C:\\Users\\user\\rust\\nesemu\\donkey kong.nes")?;
    cpu.init();

    
    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    std::process::exit(0)
                },
                Event::KeyDown { keycode, .. } => {
                    if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                        println!("{:?}", key);
                        cpu.joypad.set_button(true, key);
                    }
                },
                Event::KeyUp { keycode, .. } => {
                    if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                        cpu.joypad.set_button(false, key);
                    }
                },

                _ => {},
            }
        }

        let (log_str, cpu_cycles) = cpu.execute();
        // writeln!(f, "{}", log_str);

        for _ in 0..(cpu_cycles*3) {
            cpu.ppu.tick();
        }

        //CHECK FOR NMI
        if cpu.ppu.nmi() {
            // println!("NMI OCCURED");
            // writeln!(f, "NMI OCCURED");
            cpu.service_nmi();

            for _ in 0..(7*3) {
                cpu.ppu.tick();
            }
        }

        

        if cpu.ppu.new_frame_ready() {
            let mut buf = [0u8; 256*240*3];
            let mut i: usize = 0;

            for y in 0..240 {
                for x in 0..256 {
                    buf[i] = cpu.ppu.frame_buffer[y][x].0;
                    buf[i+1] = cpu.ppu.frame_buffer[y][x].1;
                    buf[i+2] = cpu.ppu.frame_buffer[y][x].2;
                    i += 3;
                }
            }

            // println!("{:?}", cpu.ppu.frame_buffer);

            texture.update(None, &buf, 256 * 3)?;
            canvas.copy(&texture, None, None)?;
            canvas.present();
        }
        // sleep(std::time::Duration::from_millis(1));
    }

}
