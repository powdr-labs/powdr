machine Trait with degree: 8 {
    reg pc[@pc];
    reg X[<=];
    reg A;

    trait ArraySum<T> {
        array_sum: T[4 + 1] -> T,
    }
	
    function main {
        A <=X= 1;
    }
}