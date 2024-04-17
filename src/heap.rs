const SYS_BRK: u8 = 12;

struct HeapChunk {
    offset: usize,
    size: usize,
    free: bool
}

struct Heap<'a> {
    capacity: usize,
    chunks: Vec<HeapChunk>,
    end: usize,
    asm_src: &'a mut String
}

impl<'a> Heap<'a> {
    pub fn new(capacity: usize, asm_src: &'a mut String) -> Self {
        Self {
            capacity,
            chunks: Vec::new(),
            end: 0,
            asm_src,
        } 
    }
    pub fn alloc(&mut self, size: usize) -> Option<usize> {
        let addr;

        if let Some(idx) = self.chunks.iter().position(|c| c.size >= size && c.free) {
            let chunk = &mut self.chunks[idx];

            addr = chunk.offset;

            *chunk = HeapChunk {
                offset: chunk.offset,
                size,
                free: false
            };

            if chunk.size > size {
                let extra_chunk = HeapChunk {
                    offset: chunk.offset + size,
                    size: chunk.size - size,
                    free: true
                };

                self.chunks.insert(idx, extra_chunk);
            }
        } else {
            addr = self.end;
            if let Err(_) = self.brk(size) {
                return None;
            }  
        }

        Some(addr)
    }

    pub fn brk(&mut self, size: usize) -> Result<(), ()> {
        if self.end + size > self.capacity {
            return Err(());
        }

        *self.asm_src += &format!("mov rax, {SYS_BRK}");
        *self.asm_src += &format!("mov rdi, {size}");

        self.end += size;

        Ok(())
    }

    pub fn free(&mut self, addr: usize, _size: usize) {
        let idx = self.chunks.iter().position(|c| c.offset == addr).unwrap();
        self.chunks[idx].free = true;

        if idx != self.chunks.len() - 1 && self.chunks[idx + 1].free {
            self.chunks[idx].size += self.chunks[idx + 1].size;
            self.chunks.remove(idx + 1);
        }

        if idx != 0 && self.chunks[idx - 1].free {
            self.chunks[idx - 1].size += self.chunks[idx].size;
            self.chunks.remove(idx);
        }
    }
}
