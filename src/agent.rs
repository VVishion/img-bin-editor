extern crate base64;
use base64::encode;
use serde::{Deserialize, Serialize};
use yew_agent::{HandlerId, Public, WorkerLink};

use crate::{Image, Sampler};

pub struct Worker {
    link: WorkerLink<Self>,
}

#[derive(Serialize, Deserialize)]
pub struct WorkerInput {
    pub image: Image,
    pub sampler: Sampler,
}

#[derive(Serialize, Deserialize)]
pub struct WorkerOutput {
    pub value: String,
}

impl yew_agent::Worker for Worker {
    type Input = WorkerInput;
    type Message = ();
    type Output = WorkerOutput;
    type Reach = Public<Self>;

    fn create(link: WorkerLink<Self>) -> Self {
        Self { link }
    }

    fn update(&mut self, _msg: Self::Message) {
        // no messaging
    }

    fn handle_input(&mut self, msg: Self::Input, id: HandlerId) {
        // this runs in a web worker
        // and does not block the main
        // browser thread!

        let sampler = msg.sampler;
        let image = msg.image;

        let encoded = encode(image.write(&sampler));

        let output = Self::Output { value: encoded };

        self.link.respond(id, output);
    }

    fn name_of_resource() -> &'static str {
        "worker.js"
    }

    fn resource_path_is_relative() -> bool {
        true
    }
}
