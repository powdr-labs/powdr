use k256::FieldElement;


pub fn affine_add(
    p1_x: &FieldElement,
    p1_y: &FieldElement,
    p2_x: &FieldElement,
    p2_y: &FieldElement,
) -> Option<(FieldElement, FieldElement)> {
        
    let dx = (*p2_x - *p1_x).normalize();

    if dx.is_zero().into() {
        return None;
    }

    let invert = FieldElement::from_u64(1).normalize(); // invert hint should be provided here
    //let invert = dx.invert().unwrap().normalize();

    let dy = *p2_y - p1_y;
    let lambda = dy * invert;

    assert_eq!(FieldElement::from_u64(1).normalize(), (invert*dx).normalize());

    let x3 = lambda.square() - p1_x - p2_x;
    let y3 = lambda * (*p1_x + x3.negate(5)) - *p1_y;


    Some((x3.normalize(), y3.normalize()))
}