use std::collections::BTreeMap;

use itertools::Itertools;
use powdr_number::{FieldElement, LargeInt};

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

/// A handler for a single bus ID, taking the current range constraints
/// for the payload and the multiplicity, returning updated range constraints
/// for the payload.
pub type SingleBusInteractionHandler<T> =
    Box<dyn Fn(&[RangeConstraint<T>], T) -> Vec<RangeConstraint<T>>>;

/// A bus interaction handler that automatically deals with resolving the bus ID.
/// You only need to provide a handler for each bus ID that gets supplied with the
/// payload and multiplicity and returns updated range constraints for the payload.
pub struct DelegatingBusInteractionHandler<T: FieldElement> {
    handler_map: BTreeMap<u64, SingleBusInteractionHandler<T>>,
}

impl<T: FieldElement> DelegatingBusInteractionHandler<T> {
    pub fn new(handlers: impl IntoIterator<Item = (u64, SingleBusInteractionHandler<T>)>) -> Self {
        Self {
            handler_map: handlers.into_iter().collect(),
        }
    }
}

impl<T: FieldElement> BusInteractionHandler<T> for DelegatingBusInteractionHandler<T> {
    fn handle_bus_interaction(
        &self,
        bus_interaction: BusInteraction<RangeConstraint<T>>,
    ) -> BusInteraction<RangeConstraint<T>> {
        let (Some(bus_id), Some(multiplicity)) = (
            bus_interaction.bus_id.try_to_single_value(),
            bus_interaction.multiplicity.try_to_single_value(),
        ) else {
            return bus_interaction;
        };

        if multiplicity.is_zero() {
            return bus_interaction;
        }

        if let Some(handler) = self
            .handler_map
            .get(&bus_id.to_integer().try_into_u64().unwrap())
        {
            let payload_constraints = handler(&bus_interaction.payload, multiplicity);
            return BusInteraction {
                payload: payload_constraints,
                ..bus_interaction
            };
        }
        bus_interaction
    }
}
