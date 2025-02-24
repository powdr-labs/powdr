static DEGREE: std::sync::RwLock<Option<ibig::IBig>> = std::sync::RwLock::new(None);

#[no_mangle]
pub extern "C" fn __set_degree(degree: u64) {
    *DEGREE.write().unwrap() = Some(ibig::IBig::from(degree));
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

    fn as_slice(&self) -> &[T] {
        self.0.as_ref()
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

trait Add {
    fn add(a: Self, b: Self) -> Self;
}

impl<T: Clone> Add for PilVec<T> {
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

impl<X: std::ops::Add<X, Output = X>> Add for X {
    fn add(a: Self, b: Self) -> Self {
        std::ops::Add::add(a, b)
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
