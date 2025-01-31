// These constants are defined in the including code.
// const column_count: u64 = ...;
// const first_column_id: u64 = ...;

#[inline]
fn known_to_slice<'a>(known: *mut u32, len: u64) -> &'a mut [u32] {
    let words_per_row = (column_count + 31) / 32;
    let rows = len / column_count;
    let known_len = rows * words_per_row;
    unsafe { std::slice::from_raw_parts_mut(known, known_len as usize) }
}

#[inline]
fn index(global_offset: u64, local_offset: i32, column: u64) -> usize {
    let column = column - first_column_id;
    let row = (global_offset as i64 + local_offset as i64) as u64;
    (row * column_count + column) as usize
}

#[inline]
fn index_known(global_offset: u64, local_offset: i32, column: u64) -> (u64, u64) {
    let column = column - first_column_id;
    let row = (global_offset as i64 + local_offset as i64) as u64;
    let words_per_row = (column_count + 31) / 32;
    (row * words_per_row + column / 32, column % 32)
}

#[inline]
fn get(data: &[FieldElement], global_offset: u64, local_offset: i32, column: u64) -> FieldElement {
    data[index(global_offset, local_offset, column)]
}

#[inline]
fn set(
    data: &mut [FieldElement],
    global_offset: u64,
    local_offset: i32,
    column: u64,
    value: FieldElement,
) {
    let i = index(global_offset, local_offset, column);
    data[i] = value;
}

#[inline]
fn set_known(known: &mut [u32], global_offset: u64, local_offset: i32, column: u64) {
    let (known_idx, known_bit) = index_known(global_offset, local_offset, column);
    known[known_idx as usize] |= 1 << (known_bit);
}

#[inline]
fn get_param(params: &[LookupCell<FieldElement>], i: usize) -> FieldElement {
    match params[i] {
        LookupCell::Input(v) => *v,
        LookupCell::Output(_) => panic!("Output cell used as input"),
    }
}
#[inline]
fn set_param(params: &mut [LookupCell<FieldElement>], i: usize, value: FieldElement) {
    match &mut params[i] {
        LookupCell::Input(_) => panic!("Input cell used as output"),
        LookupCell::Output(v) => **v = value,
    }
}

#[repr(C)]
enum LookupCell<'a, T> {
    /// Value is known (i.e. an input)
    Input(&'a T),
    /// Value is not known (i.e. an output)
    Output(&'a mut T),
}

#[repr(C)]
pub struct MutSlice<T> {
    data: *mut T,
    len: u64,
}

impl<T> From<&mut [T]> for MutSlice<T> {
    #[inline]
    fn from(slice: &mut [T]) -> Self {
        MutSlice {
            data: slice.as_mut_ptr(),
            len: slice.len() as u64,
        }
    }
}

impl<T> MutSlice<T> {
    #[inline]
    fn to_mut_slice<'a>(self) -> &'a mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.data, self.len as usize) }
    }
}

#[repr(C)]
pub struct WitgenFunctionParams<'a, T: 'a> {
    data: MutSlice<T>,
    known: *mut u32,
    row_offset: u64,
    params: MutSlice<LookupCell<'a, T>>,
    mutable_state: *const std::ffi::c_void,
    call_machine: extern "C" fn(*const std::ffi::c_void, u64, MutSlice<LookupCell<'_, T>>) -> bool,
    fixed_data: *const std::ffi::c_void,
    get_fixed_value: extern "C" fn(*const std::ffi::c_void, u64, u64) -> T,
}

// TODO The following is copied from jit-compiler/src/compiler.rs

impl From<ibig::IBig> for FieldElement {
    fn from(x: ibig::IBig) -> Self {
        FieldElement(u64::try_from(x).unwrap())
    }
}
impl From<FieldElement> for ibig::IBig {
    fn from(x: FieldElement) -> Self {
        ibig::IBig::from(x.0)
    }
}

#[derive(Clone)]
enum Callable<Args, Ret> {
    Fn(fn(Args) -> Ret),
    Closure(std::sync::Arc<dyn Fn(Args) -> Ret + Send + Sync>),
}
impl<Args, Ret> Callable<Args, Ret> {
    #[inline(always)]
    fn call(&self, args: Args) -> Ret {
        match self {
            Callable::Fn(f) => f(args),
            Callable::Closure(f) => f(args),
        }
    }
}

#[derive(Clone)]
struct PilVec<T>(std::sync::Arc<Vec<T>>);

impl<T> PilVec<T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl<T> From<Vec<T>> for PilVec<T> {
    fn from(v: Vec<T>) -> Self {
        PilVec(std::sync::Arc::new(v))
    }
}
impl<T> std::ops::Index<usize> for PilVec<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
        &self.0[index]
    }
}

impl<T: Clone> std::ops::Add for PilVec<T> {
    fn add(a: Self, b: Self) -> Self {
        // TODO for a regular "push" or array::map this is very slow.
        // We could optimize this by sharing a larger backing vector
        // across prefix instances, allowing to extend the backing vector if
        // our view is the full vector.
        PilVec(std::sync::Arc::new(
            a.0.as_ref()
                .iter()
                .chain(b.0.as_ref())
                .cloned()
                .collect::<Vec<_>>(),
        ))
    }
}

trait FromLiteral {
    fn from_u64(x: u64) -> Self;
}
impl FromLiteral for ibig::IBig {
    fn from_u64(x: u64) -> Self {
        ibig::IBig::from(x)
    }
}
impl FromLiteral for FieldElement {
    fn from_u64(x: u64) -> Self {
        FieldElement::from(x)
    }
}
