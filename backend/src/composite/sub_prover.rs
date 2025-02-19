use std::{
    collections::BTreeMap,
    sync::{
        mpsc::{Receiver, SyncSender},
        Arc, Mutex,
    },
    thread::{Scope, ScopedJoinHandle},
};

use crate::{Backend, Error};
use powdr_executor::witgen::WitgenCallback;
use powdr_number::FieldElement;

/// Runs a prover until it either completes or challenges the caller, in
/// which case the execution can be resumed from the returned `SubProver`.
pub fn run<'s, 'env, F: FieldElement>(
    scope: &'s Scope<'s, 'env>,
    prover: &'env Mutex<Box<dyn Backend<F>>>,
    witness: Vec<(String, Vec<F>)>,
) -> RunStatus<'s, F>
where
{
    let (challenge_sender, challenge_receiver) = std::sync::mpsc::sync_channel(0);
    let (response_sender, response_receiver) = std::sync::mpsc::sync_channel(0);

    let thread = scope.spawn(move || {
        // We must ensure the backend can call the callback from multiple
        // threads (needed by Plonky3, as it requires WitgenCallback to be
        // Sync). So we wrap the data in a Mutex.
        let callback_data = Mutex::new((challenge_sender, response_receiver, 1u8));
        let callback = WitgenCallback::<F>::new(Arc::new(move |_, _, challenge, stage| {
            // Locking guarantees the sequencing of the stages:
            let mut receiver_guard = callback_data.lock().unwrap();
            let (challenge_sender, response_receiver, expected_stage) = &mut *receiver_guard;

            assert_eq!(stage, *expected_stage);
            *expected_stage += 1;

            challenge_sender.send(challenge).unwrap();
            response_receiver.recv().unwrap()
        }));

        // TODO: this witness will be held in memory until the end of the
        // proof, even if it's not needed anymore. We should probably change
        // this API so the Vec is moved into the prover, and returned in the
        // callback and result.
        prover.lock().unwrap().prove(&witness, &BTreeMap::default(), None, callback)
    });

    SubProver {
        thread,
        challenge_receiver,
        response_sender,
    }
    .wait()
}

pub enum RunStatus<'s, F: FieldElement> {
    Completed(Result<Vec<u8>, Error>),
    Challenged(SubProver<'s, F>, BTreeMap<u64, F>),
}

/// Runs a prover in its own thread, storing the state in between
/// challenges.
pub struct SubProver<'s, F: FieldElement> {
    pub thread: ScopedJoinHandle<'s, Result<Vec<u8>, Error>>,
    pub challenge_receiver: Receiver<BTreeMap<u64, F>>,
    pub response_sender: SyncSender<Vec<(String, Vec<F>)>>,
}

impl<'s, F: FieldElement> SubProver<'s, F> {
    pub fn resume(self, response: Vec<(String, Vec<F>)>) -> RunStatus<'s, F> {
        self.response_sender.send(response).unwrap();
        self.wait()
    }

    fn wait(self) -> RunStatus<'s, F> {
        match self.challenge_receiver.recv() {
            Ok(challenge) => RunStatus::Challenged(self, challenge),
            Err(_) => match self.thread.join() {
                Ok(proof) => RunStatus::Completed(proof),
                Err(err) => std::panic::resume_unwind(err),
            },
        }
    }
}
