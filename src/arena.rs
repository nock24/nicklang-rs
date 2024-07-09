use std::alloc::{alloc, dealloc, Layout};
use std::mem;
use std::ops::Deref;
use std::fmt;

#[derive(Clone, PartialEq)]
pub struct ArenaPtr<T>
where
    T: std::fmt::Debug
{
    ptr: *mut T,
}
impl<T: std::fmt::Debug> Deref for ArenaPtr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe {
            return &*self.ptr
        }
    } 
}
impl<T: std::fmt::Debug> fmt::Debug for ArenaPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            write!(f, "{:?}", *self.ptr) 
        }
    }
}
pub struct ArenaAllocator {
    buffer: *mut u8,
    offset: *const u8,
    capacity: usize,
}
impl ArenaAllocator {
    pub fn new(capacity: usize) -> ArenaAllocator {
        unsafe {
            let layout = Layout::from_size_align_unchecked(capacity, 1);
            let buffer = alloc(layout);
            return ArenaAllocator {
                buffer,
                offset: buffer,
                capacity,
            };
        }
    }
    pub fn alloc<T: std::fmt::Debug>(&mut self, obj: T) -> ArenaPtr<T> {
        unsafe {
            let obj_ptr = self.raw_alloc(obj).expect("arena overflow");      
            return ArenaPtr {ptr: obj_ptr};
        }
    }
    pub unsafe fn raw_alloc<T: std::fmt::Debug>(&mut self, obj: T) -> Option<*mut T> {
        let obj_sz = mem::size_of::<T>();
        if self.offset.add(obj_sz) > self.buffer.add(self.capacity) {
            return None;
        }
        let obj_ptr = self.offset as *mut T;
        *obj_ptr = obj;
        self.offset = self.offset.add(obj_sz);
        return Some(obj_ptr);
    }
}
impl Drop for ArenaAllocator {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::from_size_align_unchecked(self.capacity, 1);
            dealloc(self.buffer, layout);
        }
    }
}
