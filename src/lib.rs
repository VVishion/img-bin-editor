use crate::agent::{Worker, WorkerInput, WorkerOutput};
use bytemuck::cast_slice;
use gloo::file::callbacks::FileReader;
use gloo::file::File;
use gloo::utils::document;
use image::io::Reader as ImageReader;
use image::{ColorType, DynamicImage, ImageBuffer, ImageFormat, Luma, LumaA, Rgb, Rgba};
use itertools::iproduct;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io::Cursor;
use std::rc::Rc;
use std::str::FromStr;
use wasm_bindgen::JsCast;
use web_sys::HtmlElement;
use web_sys::{DragEvent, Event, FileList, HtmlInputElement, WheelEvent};
use yew::html::TargetCast;
use yew::{function_component, html, Callback, Component, Context, Html, Properties};
use yew_agent::{Bridge, Bridged};

pub mod agent;

// Da ein ganzes Bild nicht auf den Bildschirm passt, da ein Pixel als Quad mit vier inneren Quads (4 Channel) und seinem Primitiv dargestellt wird, nutze ein Fenster,
// welches Scrollbar ist
// Fenster wird auch im Preview angezeigt und kann dirch klicken auf das Preview gesetzt werden - ähnlich wie Code minimap

pub enum Msg {
    Load(Vec<File>),
    Loaded(String, String, Vec<u8>),
    Save,
    Saved(WorkerOutput),
    PrimitiveEdit(PrimitiveEdit),
    Scroll(WheelEvent),
}

#[derive(Serialize, Deserialize, Clone)]
pub enum PrimitiveType {
    Uint8,
    Uint16,
    Float32,
}

#[derive(PartialEq, Clone)]
pub enum Primitive {
    Uint8(u8),
    Uint16(u16),
    Float32(f32),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Uint8(a) => write!(f, "{}", a),
            Self::Uint16(a) => write!(f, "{}", a),
            Self::Float32(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsePrimitiveError;

impl Primitive {
    pub fn parse(primitive_type: &PrimitiveType, input: &str) -> Result<Self, ParsePrimitiveError> {
        match primitive_type {
            PrimitiveType::Uint8 => Ok(Primitive::Uint8(
                u8::from_str(input).map_err(|_| ParsePrimitiveError)?,
            )),
            PrimitiveType::Uint16 => Ok(Primitive::Uint16(
                u16::from_str(input).map_err(|_| ParsePrimitiveError)?,
            )),
            PrimitiveType::Float32 => Ok(Primitive::Float32(
                f32::from_str(input).map_err(|_| ParsePrimitiveError)?,
            )),
        }
    }

    pub fn primitive_type(&self) -> PrimitiveType {
        match self {
            Self::Uint8(_) => PrimitiveType::Uint8,
            Self::Uint16(_) => PrimitiveType::Uint16,
            Self::Float32(_) => PrimitiveType::Float32,
        }
    }
}

#[derive(PartialEq)]
pub enum PrimitiveRef<'a> {
    Uint8(&'a u8),
    Uint16(&'a u16),
    Float32(&'a f32),
}

impl<'a> PrimitiveRef<'a> {
    pub fn to_owned(self) -> Primitive {
        match self {
            Self::Uint8(a) => Primitive::Uint8(*a),
            Self::Uint16(a) => Primitive::Uint16(*a),
            Self::Float32(a) => Primitive::Float32(*a),
        }
    }
}

impl PrimitiveType {
    pub fn byte_length(&self) -> u8 {
        match self {
            Self::Uint8 => 1,
            Self::Uint16 => 2,
            Self::Float32 => 4,
        }
    }
}

impl From<&Primitive> for PrimitiveType {
    fn from(primitive: &Primitive) -> Self {
        primitive.primitive_type()
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Sampler {
    pub channels: u8,
    pub primitive_type: PrimitiveType,
}

impl Sampler {
    pub fn byte_length_per_pixel(&self) -> u8 {
        self.primitive_type.byte_length() * self.channels
    }

    pub fn read_pixel<'a>(&'_ self, image: &'a Image, i: usize) -> Vec<PrimitiveRef<'a>> {
        let index = i * self.channels as usize;
        (index..index + self.channels as usize)
            .into_iter()
            .map(|i| self.read(image, i))
            .collect()
    }

    pub fn read<'a>(&'_ self, image: &'a Image, i: usize) -> PrimitiveRef<'a> {
        image.buffer.get(i)
    }

    pub fn write<'a>(&'_ self, image: &'a mut Image, i: usize, primitive: Primitive) {
        image.buffer.insert(i, primitive);
    }

    pub fn iter_pixels<'a>(
        &'a self,
        image: &'a Image,
    ) -> impl Iterator<Item = Vec<PrimitiveRef<'a>>> {
        (0..image.area()).map(|index| self.read_pixel(image, index))
    }
}

#[derive(Clone)]
pub struct Rect {
    pub min: (usize, usize),
    pub max: (usize, usize),
}

impl Rect {
    pub fn new(min: (usize, usize), max: (usize, usize)) -> Self {
        Self { min, max }
    }

    pub fn transposed(&self) -> Self {
        Self {
            min: (self.min.1, self.min.0),
            max: (self.max.1, self.max.0),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, usize)> {
        iproduct!(self.min.0..self.max.0, self.min.1..self.max.1)
    }
}

type Dimensions = (usize, usize);

impl Rect {
    pub fn contains(&self, index: (usize, usize)) -> bool {
        self.min.0 <= index.0
            && self.min.1 <= index.1
            && index.0 < self.max.0
            && index.1 < self.max.1
    }

    pub fn dimensions(&self) -> Dimensions {
        (self.max.0 - self.min.0, self.max.1 - self.min.1)
    }

    pub fn area(&self) -> usize {
        let dimensions = self.dimensions();
        dimensions.0 * dimensions.1
    }

    pub fn is_empty(&self) -> bool {
        self.area() <= 0
    }
}

fn wrap_index<T, U>(
    index: T,
    wrap_after: U,
) -> (
    <T as std::ops::Rem<U>>::Output,
    <T as std::ops::Div<U>>::Output,
)
where
    T: Copy,
    U: Copy,
    T: std::ops::Rem<U>,
    T: std::ops::Div<U>,
{
    (index % wrap_after, index / wrap_after)
}

fn unwrap_index<T, U>(
    index: (T, T),
    wrapped_after: U,
) -> <T as std::ops::Add<<T as std::ops::Mul<U>>::Output>>::Output
where
    T: std::ops::Mul<U>,
    T: std::ops::Add<<T as std::ops::Mul<U>>::Output>,
{
    index.0 + index.1 * wrapped_after
}

#[derive(Serialize, Deserialize, Clone)]
pub enum Buffer {
    Uint8(Vec<u8>),
    Uint16(Vec<u16>),
    Float32(Vec<f32>),
}

impl Buffer {
    pub fn new(data: Vec<u8>) -> Self {
        Self::Uint8(data)
    }

    pub fn bytes(&self) -> &[u8] {
        match self {
            Self::Uint8(s) => s,
            Self::Uint16(s) => cast_slice(s),
            Self::Float32(s) => cast_slice(s),
        }
    }

    pub fn primitive_type(&self) -> PrimitiveType {
        match self {
            Self::Uint8(_) => PrimitiveType::Uint8,
            Self::Uint16(_) => PrimitiveType::Uint16,
            Self::Float32(_) => PrimitiveType::Float32,
        }
    }

    pub fn cast(self, primitive_type: &PrimitiveType) -> Self {
        let bytes = self.bytes();

        match primitive_type {
            PrimitiveType::Uint8 => Buffer::Uint8(bytes.to_owned()),
            PrimitiveType::Uint16 => Buffer::Uint16(cast_slice(bytes).to_owned()),
            PrimitiveType::Float32 => Buffer::Float32(cast_slice(bytes).to_owned()),
        }
    }

    pub fn get(&self, index: usize) -> PrimitiveRef {
        match self {
            Self::Uint8(primitives) => PrimitiveRef::Uint8(
                primitives
                    .get(index)
                    .expect("Accessing index out of buffer bounds."),
            ),
            Self::Uint16(primitives) => PrimitiveRef::Uint16(
                primitives
                    .get(index)
                    .expect("Accessing index out of buffer bounds."),
            ),
            Self::Float32(primitives) => PrimitiveRef::Float32(
                primitives
                    .get(index)
                    .expect("Accessing index out of buffer bounds."),
            ),
        }
    }

    pub fn insert(&mut self, index: usize, primitive: Primitive) {
        match primitive {
            Primitive::Uint8(a) => {
                if let Self::Uint8(primitives) = self {
                    primitives[index] = a;
                } else {
                    panic!("Tried to insert u8 to buffer of different primitive type.");
                }
            }
            Primitive::Uint16(a) => {
                if let Self::Uint16(primitives) = self {
                    primitives[index] = a;
                } else {
                    panic!("Tried to insert u16 to buffer of different primitive type.");
                }
            }
            Primitive::Float32(a) => {
                if let Self::Float32(primitives) = self {
                    primitives[index] = a;
                } else {
                    panic!("Tried to insert f32 to buffer of different primitive type.");
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Image {
    pub buffer: Buffer,
    pub dimensions: Dimensions,
}

pub struct FileState {
    pub name: String,
    pub mime: String,
    pub image: Image,
    pub sampler: Sampler,
    pub unsaved_changes: bool,
    pub encoded: Option<String>,
}

pub struct EditState {
    pub frame: Rect,
}

pub struct ErrorState {
    pub errors: Vec<Html>,
}

pub struct AppState {
    file_state: Option<FileState>,
    edit_state: EditState,
    error_state: ErrorState,
}

pub struct App {
    state: AppState,
    readers: HashMap<String, FileReader>,
    worker: Box<dyn Bridge<Worker>>,
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let callback = {
            let link = ctx.link().clone();
            move |output| link.send_message(Self::Message::Saved(output))
        };
        let worker = Worker::bridge(Rc::new(callback));

        Self {
            state: AppState {
                file_state: None,
                edit_state: EditState {
                    frame: Rect::new((0, 0), (6, 6)),
                },
                error_state: ErrorState { errors: vec![] },
            },
            readers: HashMap::default(),
            worker,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Loaded(name, mime, data) => {
                let mut reader = ImageReader::new(Cursor::new(data));
                reader.set_format(ImageFormat::from_mime_type(&mime).unwrap());
                self.readers.remove(&name);

                let image = reader.decode().unwrap();
                let color = image.color();
                let sampler: Sampler = color.into();
                let dimensions = (image.width() as usize, image.height() as usize);
                let image = Image {
                    buffer: Buffer::new(image.into_bytes()).cast(&sampler.primitive_type),
                    dimensions,
                };

                self.state.file_state = Some(FileState {
                    name,
                    mime,
                    image,
                    sampler,
                    unsaved_changes: false,
                    encoded: None,
                });

                true
            }
            Msg::Load(files) => {
                for file in files.into_iter() {
                    let file_name = file.name();
                    let mime = file.raw_mime_type();

                    let task = {
                        let link = ctx.link().clone();
                        let file_name = file_name.clone();

                        gloo::file::callbacks::read_as_bytes(&file, move |res| {
                            link.send_message(Msg::Loaded(
                                file_name,
                                mime,
                                res.expect("failed to read file"),
                            ))
                        })
                    };
                    self.readers.insert(file_name, task);
                }
                true
            }
            Msg::Save => {
                let file_state = self.state.file_state.as_ref().unwrap();
                self.worker.send(WorkerInput {
                    image: file_state.image.clone(),
                    sampler: file_state.sampler.clone(),
                });

                false
            }
            Msg::Saved(output) => {
                self.state.file_state.as_mut().unwrap().encoded = Some(output.value);

                true
            }
            Msg::PrimitiveEdit(primitive_edit) => {
                // let file_state = match self.state.file_state {
                //     Some(file_state) => file_state,
                //     None => return false,
                // };

                primitive_edit.primitive.map(|primitive| {
                    let file_state = self.state.file_state.as_mut().unwrap();
                    let sampler = &file_state.sampler;

                    sampler.write(
                        &mut file_state.image,
                        primitive_edit.pixel * file_state.sampler.channels as usize
                            + primitive_edit.channel as usize,
                        primitive,
                    );
                });

                true
            }
            Msg::Scroll(wheel_event) => {
                let file_state = match self.state.file_state.as_ref() {
                    Some(file_state) => file_state,
                    None => return false,
                };

                let image_dimensions = file_state.image.dimensions;
                let frame_dimensions = self.state.edit_state.frame.dimensions();

                let frame = &mut self.state.edit_state.frame;

                if wheel_event.shift_key() {
                    if wheel_event.delta_y() > 0. {
                        frame.min.0 = frame
                            .min
                            .0
                            .saturating_add(frame_dimensions.0)
                            .min(image_dimensions.0.saturating_sub(frame_dimensions.0));
                        frame.max.0 = frame.min.0.saturating_add(frame_dimensions.0);
                    } else {
                        frame.min.0 = frame.min.0.saturating_sub(frame_dimensions.0);
                        frame.max.0 = frame.min.0.saturating_add(frame_dimensions.0);
                    }
                } else {
                    if wheel_event.delta_y() > 0. {
                        frame.min.1 = frame
                            .min
                            .1
                            .saturating_add(frame_dimensions.1)
                            .min(image_dimensions.1.saturating_sub(frame_dimensions.1));
                        frame.max.1 = frame.min.1.saturating_add(frame_dimensions.1);
                    } else {
                        frame.min.1 = frame.min.1.saturating_sub(frame_dimensions.1);
                        frame.max.1 = frame.min.1.saturating_add(frame_dimensions.1);
                    }
                }

                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let data = self
            .state
            .file_state
            .as_ref()
            .map(|file_state| (self.frame(file_state.image.dimensions), file_state));

        html! {
            <div id="wrapper">
                if let Some((frame, file_state)) = data {
                    <div id="file-actions">
                        <button id="file-action-save" onclick={ctx.link().callback(|_| Msg::Save)}>{ "Save" }</button>
                    </div>

                    if let Some(encoded) = &file_state.encoded {
                        <a
                            id="file-save"
                            href={format!("data:{};base64,{}", file_state.mime, encoded)}
                            download={file_state.name.clone()}
                        />
                    }

                    <div
                        id="edit-frame"
                        style={ format!("grid-template-columns: 64px repeat({}, 128px); grid-template-rows: 64px repeat({}, 128px);", frame.dimensions().0, frame.dimensions().1) }
                        onwheel={ctx.link().callback(move |e: WheelEvent| {
                            e.prevent_default();
                            Msg::Scroll(e)
                        })}
                    >
                        <div id="x-axis" style={ format!("grid-column: span {}; grid-template-columns: 64px repeat({}, 128px);", frame.dimensions().0 + 1, frame.dimensions().0) }>
                            <div></div>
                            { for (frame.min.0..frame.max.0).map(|i| { html! { <div><span>{ i + 1 }</span></div> } }) }
                        </div>
                        <div id="y-axis" style={ format!("grid-row: span {0}; grid-template-rows: repeat({0}, 128px);", frame.dimensions().1) }>
                            { for (frame.min.1..frame.max.1).map(|i| { html! { <div><span>{ i + 1 }</span></div> } }) }
                        </div>
                        <div id="edit-pixels" style={ format!("grid-template-columns: repeat({0}, 128px); grid-template-rows: repeat({1}, 128px); grid-column: span {0}; grid-row: span {1};", frame.dimensions().0, frame.dimensions().1) }>
                            { self.view_pixels(ctx, frame) }
                        </div>
                    </div>
                } else {
                    <p id="title">{ "Open" }</p>
                    <label for="file-upload">
                        <div
                            id="drop-container"
                            ondrop={ctx.link().callback(|event: DragEvent| {
                                event.prevent_default();
                                let files = event.data_transfer().unwrap().files();
                                Self::upload_files(files)
                            })}
                            ondragover={Callback::from(|event: DragEvent| {
                                event.prevent_default();
                            })}
                            ondragenter={Callback::from(|event: DragEvent| {
                                event.prevent_default();
                            })}
                        >
                            <p>{ "Drop your images here or click to select" }</p>
                        </div>
                    </label>
                    <input
                        id="file-upload"
                        type="file"
                        accept="image/*"
                        multiple={false}
                        onchange={ctx.link().callback(move |e: Event| {
                            let input: HtmlInputElement = e.target_unchecked_into();
                            Self::upload_files(input.files())
                        })}
                    />
                }
            </div>
        }
    }

    fn rendered(&mut self, _: &Context<Self>, _: bool) {
        if let Some(element) = document().get_element_by_id("file-save") {
            if let Some(file_state) = self.state.file_state.as_mut() {
                file_state.encoded = None;
            }
            element.unchecked_into::<HtmlElement>().click();
        }
    }
}

impl App {
    fn frame(&self, image_dimensions: Dimensions) -> Rect {
        let mut frame = self.state.edit_state.frame.clone();
        let frame_dimensions = frame.dimensions();

        frame.max.0 = frame.max.0.min(image_dimensions.0);
        frame.max.1 = frame.max.1.min(image_dimensions.1);

        frame.min.0 = frame.max.0.saturating_sub(frame_dimensions.0);
        frame.min.1 = frame.max.1.saturating_sub(frame_dimensions.1);
        frame
    }

    fn view_pixels(&self, ctx: &Context<Self>, frame: Rect) -> Html {
        self.state.file_state.as_ref().map_or_else(
            || html! {},
            |file_state| {
                let edit_callback = ctx
                    .link()
                    .callback(|primitive_edit| Msg::PrimitiveEdit(primitive_edit));

                frame
                    .transposed()
                    .iter()
                    .map(|(y, x)| {
                        let index = unwrap_index((x, y), file_state.image.dimensions.0);
                        let primitive_refs =
                            file_state.sampler.read_pixel(&file_state.image, index);

                        let edit_callback = edit_callback.clone();
                        let channel_values: Vec<_> = primitive_refs
                            .into_iter()
                            .map(|primitive_ref| primitive_ref.to_owned())
                            .collect();
                        html! { <Pixel {edit_callback} {index} {channel_values}/> }
                    })
                    .collect()
            },
        )
    }

    fn upload_files(files: Option<FileList>) -> Msg {
        let mut result = Vec::new();

        if let Some(files) = files {
            let files = js_sys::try_iter(&files)
                .unwrap()
                .unwrap()
                .map(|v| web_sys::File::from(v.unwrap()))
                .map(File::from);
            result.extend(files);
        }
        Msg::Load(result)
    }
}

pub struct PrimitiveEdit {
    pub pixel: usize,
    pub channel: usize,
    pub primitive: Option<Primitive>,
}

#[derive(Properties, PartialEq)]
struct ChannelProps {
    pub pixel: usize,
    pub index: usize,
    pub edit_callback: Callback<PrimitiveEdit>,
    pub value: Primitive,
}

#[function_component]
fn Channel(props: &ChannelProps) -> Html {
    let pixel = props.pixel;
    let channel = props.index;

    let input_value = |e: &Event| {
        let input: HtmlInputElement = e.target_unchecked_into();
        input.value()
    };

    let on_change = {
        let edit_callback = props.edit_callback.clone();
        let primitive_type = props.value.primitive_type();

        move |e: Event| {
            let input = input_value(&e);

            let primitive = Primitive::parse(&primitive_type, &input).ok();
            edit_callback.emit(PrimitiveEdit {
                pixel,
                channel,
                primitive,
            });
        }
    };

    html! {
        <div class="edit-channel">
            <input
                class="edit-channel-input"
                type="text"
                onchange={on_change}
                value={props.value.to_string()}
            />
        </div>
    }
}

#[derive(Properties, PartialEq)]
struct PixelProps {
    pub index: usize,
    pub edit_callback: Callback<PrimitiveEdit>,
    pub channel_values: Vec<Primitive>,
}

#[function_component]
fn Pixel(props: &PixelProps) -> Html {
    html! {
        <div class="edit-pixel">
            <div class="edit-channels">
                {
                    for props.channel_values.iter().enumerate().map(|(index, value)| {
                        let edit_callback = props.edit_callback.clone();
                        html! { <Channel pixel={props.index} {index} {edit_callback} value={value.clone()}/> }
                    })
                }
            </div>
        </div>
    }
}

impl From<ColorType> for Sampler {
    fn from(color_type: ColorType) -> Self {
        match color_type {
            ColorType::L8 => Sampler {
                channels: 1,
                primitive_type: PrimitiveType::Uint8,
            },
            ColorType::L16 => Sampler {
                channels: 1,
                primitive_type: PrimitiveType::Uint16,
            },
            ColorType::La8 => Sampler {
                channels: 2,
                primitive_type: PrimitiveType::Uint8,
            },
            ColorType::La16 => Sampler {
                channels: 2,
                primitive_type: PrimitiveType::Uint16,
            },
            ColorType::Rgb8 => Sampler {
                channels: 3,
                primitive_type: PrimitiveType::Uint8,
            },
            ColorType::Rgb16 => Sampler {
                channels: 3,
                primitive_type: PrimitiveType::Uint16,
            },
            ColorType::Rgb32F => Sampler {
                channels: 3,
                primitive_type: PrimitiveType::Float32,
            },
            ColorType::Rgba8 => Sampler {
                channels: 4,
                primitive_type: PrimitiveType::Uint8,
            },
            ColorType::Rgba16 => Sampler {
                channels: 4,
                primitive_type: PrimitiveType::Uint16,
            },
            ColorType::Rgba32F => Sampler {
                channels: 4,
                primitive_type: PrimitiveType::Float32,
            },
            _ => unreachable!(),
        }
    }
}

impl Image {
    pub fn area(&self) -> usize {
        self.dimensions.0 * self.dimensions.1
    }

    pub fn write(&self, sampler: &Sampler) -> Vec<u8> {
        let mut bytes = Vec::new();

        match &self.buffer {
            Buffer::Uint8(buffer) => match sampler.channels {
                1 => {
                    let buffer: ImageBuffer<Luma<u8>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                2 => {
                    let buffer: ImageBuffer<LumaA<u8>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                3 => {
                    let buffer: ImageBuffer<Rgb<u8>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                4 => {
                    let buffer: ImageBuffer<Rgba<u8>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                _ => panic!(),
            },
            Buffer::Uint16(buffer) => match sampler.channels {
                1 => {
                    let buffer: ImageBuffer<Luma<u16>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                2 => {
                    let buffer: ImageBuffer<LumaA<u16>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                3 => {
                    let buffer: ImageBuffer<Rgb<u16>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                4 => {
                    let buffer: ImageBuffer<Rgba<u16>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                _ => panic!(),
            },
            Buffer::Float32(buffer) => match sampler.channels {
                3 => {
                    let buffer: ImageBuffer<Rgb<f32>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                4 => {
                    let buffer: ImageBuffer<Rgba<f32>, _> = ImageBuffer::from_raw(
                        self.dimensions.0 as u32,
                        self.dimensions.1 as u32,
                        &buffer[..],
                    )
                    .unwrap();

                    buffer
                        .write_to(&mut Cursor::new(&mut bytes), image::ImageOutputFormat::Png)
                        .unwrap();

                    bytes
                }
                _ => panic!(),
            },
        }
    }
}
