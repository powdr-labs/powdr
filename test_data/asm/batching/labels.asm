machine Main with degree: 256 {
    reg pc[@pc];
    reg X[<=];
    reg A;

    function main {
        label_with_next:
        A <=X= 1;
        // END BATCH Label

        labels_with_next:
        other_label_just_after:
        A <=X= 2;
        // END BATCH Unimplemented
        A <=X= 2;
        // END BATCH Label

        end:
        return;
        // END BATCH
    }
}
