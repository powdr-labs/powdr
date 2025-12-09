pub trait ExecutionState {
    type Pc;
    type Address;
    type Value;

    fn pc(&self) -> Self::Pc;

    fn read(&self, address: Self::Address) -> Self::Value;
}
