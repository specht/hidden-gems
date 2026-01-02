require "io/console"
require "fiddle"
require "fiddle/import"

module KeyInput
    module Posix
        extend self
        
        def get_key(paused, stdin: STDIN)
            stdin.raw do
                timeout = paused ? nil : 0
                ready = IO.select([stdin], nil, nil, timeout)
                return nil unless ready
                
                key = stdin.getc
                return nil unless key
                
                if key == "\e"
                    # If ESC is pressed alone, there will be no extra bytes available.
                    c2 = stdin.read_nonblock(1, exception: false)
                    if c2.nil? || c2 == :wait_readable
                        return "esc"
                    end
                    
                    c3 = stdin.read_nonblock(1, exception: false)
                    c3 = "" if c3 == :wait_readable
                    seq = key + c2 + c3
                    
                    case seq
                    when "\e[A" then "up"
                    when "\e[B" then "down"
                    when "\e[C" then "right"
                    when "\e[D" then "left"
                    when "\e[H" then "home"
                    when "\e[F" then "end"
                    else
                        # Some terminals send longer sequences (e.g. \e[1~). If we don't
                        # recognize it, treat it as ESC to avoid "eating" the key.
                        "esc"
                    end
                else
                    # Numbers already come through as "0".."9" here.
                    key
                end
            end
        end
    end
    
    if Gem.win_platform?
        module Windows
            extend self
            
            module Kernel32
                extend Fiddle::Importer
                dlload "kernel32"
                
                typealias "HANDLE", "void*"
                typealias "DWORD",  "unsigned long"
                typealias "WORD",   "unsigned short"
                typealias "BOOL",   "int"
                
                extern "HANDLE GetStdHandle(DWORD)"
                extern "BOOL   GetConsoleMode(HANDLE, void*)"
                extern "BOOL   PeekConsoleInputW(HANDLE, void*, DWORD, void*)"
                extern "BOOL   ReadConsoleInputW(HANDLE, void*, DWORD, void*)"
            end
            
            STD_INPUT_HANDLE = -10
            KEY_EVENT        = 0x0001
            
            VK_MAP = {
                0x25 => "left",
                0x26 => "up",
                0x27 => "right",
                0x28 => "down",
                0x24 => "home",
                0x23 => "end",
            }.freeze
            
            # Virtual key code for Escape
            VK_ESCAPE = 0x1B
            
            def console_stdin?
                h = Kernel32.GetStdHandle(STD_INPUT_HANDLE)
                mode_ptr = Fiddle::Pointer.malloc(4)
                Kernel32.GetConsoleMode(h, mode_ptr) != 0
            rescue
                false
            end
            
            # INPUT_RECORD for KEY_EVENT (20 bytes):
            # WORD EventType; WORD pad;
            # KEY_EVENT_RECORD:
            #   DWORD bKeyDown;
            #   WORD  wRepeatCount;
            #   WORD  wVirtualKeyCode;
            #   WORD  wVirtualScanCode;
            #   WCHAR UnicodeChar;  (WORD)
            #   DWORD dwControlKeyState;
            INPUT_RECORD_SIZE = 20
            
            def read_key_event(paused)
                h = Kernel32.GetStdHandle(STD_INPUT_HANDLE)
                
                record = Fiddle::Pointer.malloc(INPUT_RECORD_SIZE)
                read   = Fiddle::Pointer.malloc(4)
                
                loop do
                    if !paused
                        ok = Kernel32.PeekConsoleInputW(h, record, 1, read)
                        return nil if ok == 0 || read[0, 4].unpack1("L<") == 0
                    end
                    
                    ok = Kernel32.ReadConsoleInputW(h, record, 1, read)
                    return nil if ok == 0 || read[0, 4].unpack1("L<") == 0
                    
                    data = record[0, INPUT_RECORD_SIZE]
                    event_type, key_down, _repeat, vk, _scan, uchar, _ctrl =
                    data.unpack("S< x2 L< S< S< S< S< L<")
                    
                    next unless event_type == KEY_EVENT
                    next unless key_down != 0
                    
                    return "esc" if vk == VK_ESCAPE
                    
                    mapped = VK_MAP[vk]
                    return mapped if mapped
                    
                    if uchar && uchar != 0
                        ch = [uchar].pack("S<").force_encoding("UTF-16LE").encode("UTF-8")
                        
                        return "\n" if ch == "\r"
                        
                        # Numbers will arrive here as "0".."9" already; nothing special needed.
                        return ch
                    end
                    
                    next
                end
            end
            
            def get_key(paused, stdin: STDIN)
                if console_stdin?
                    return read_key_event(paused)
                end
                Posix.get_key(paused, stdin: stdin)
            end
        end
    end

    module_function

    def get_key(paused, stdin: STDIN)
        if Gem.win_platform?
            Windows.get_key(paused, stdin: stdin)
        else
            Posix.get_key(paused, stdin: stdin)
        end
    end
end
