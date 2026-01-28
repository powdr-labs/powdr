use itertools::Itertools;
use powdr_number::FieldElement;

use crate::{constraint_system::BusInteraction, range_constraint::RangeConstraint};

/// The sent / received data could not be received / sent.
#[derive(Debug)]
pub struct ViolatesBusRules {}

/// A trait for handling bus interactions.
pub trait BusInteractionHandler<T: FieldElement> {
    /// Handles a bus interaction, by transforming taking a bus interaction
    /// (with the fields represented by range constraints) and returning
    /// updated range constraints.
    /// The idea is that a certain combination of range constraints on elements
    /// can be further restricted given internal knowledge about the specific
    /// bus interaction, in particular if some elements are restricted to just
    /// a few or even concrete values.
    /// The range constraints are intersected with the previous ones by the
    /// caller, so there is no need to do that in the implementation of this
    /// trait.
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>>;

    /// Like handle_bus_interaction, but returns an error if the current bus
    /// interaction violates the rules of the bus (e.g. [1234] in [BYTES]).
    fn handle_bus_interaction_checked(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> Result<BusInteraction<RangeConstraint<T>>, ViolatesBusRules> {
        let previous_constraints = bus_interaction.clone();
        let new_constraints = self.handle_bus_interaction(bus_interaction);

        // Intersect the old and new range constraints. If they don't overlap,
        // there is a contradiction.
        for (previous_rc, new_rc) in previous_constraints
            .fields()
            .zip_eq(new_constraints.fields())
        {
            if previous_rc.is_disjoint(new_rc) {
                return Err(ViolatesBusRules {});
            }
        }
        Ok(new_constraints)
    }
}

/// A default bus interaction handler that does nothing. Using it is
/// equivalent to ignoring bus interactions.
#[derive(Default, Clone)]
pub struct DefaultBusInteractionHandler<T: FieldElement> {
    _marker: std::marker::PhantomData<T>,
}

impl<T: FieldElement> BusInteractionHandler<T> for DefaultBusInteractionHandler<T> {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>> {
        bus_interaction
    }
}
