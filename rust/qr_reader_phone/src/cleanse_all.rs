use anyhow::anyhow;
use constants::{CHUNK_SIZE, FOUNTAIN_LIMIT, FOUNTAIN_MARKER};
use std::convert::TryInto;
use std::sync::{Arc, RwLock};

#[derive(Debug, Eq, PartialEq)]
/// QR code reader errors.
pub enum ErrorQr {
    EmptyFrame,
    FountainDifferentLength,
    FountainFrameTooShort { raw_frame: Vec<u8> },
    FountainInterruptedByLegacy,
    FountainInterruptedByStatic,
    FountainPacketEmpty { raw_frame: Vec<u8> },
    LegacyDifferentLength,
    LegacyInterruptedByFountain,
    LegacyInterruptedByStatic,
    LegacySameOrderDifferentContent,
    LegacyTooShortNumberOfFrames { raw_frame: Vec<u8> },
    LegacyTooShortOrder { raw_frame: Vec<u8> },
    LegacyOrderTooHigh { order: u16, number_of_frames: u16 },
    LegacyZeroFrames { raw_frame: Vec<u8> },
    PoisonedLock,
}

impl ErrorQr {
    pub fn anyhow(&self) -> anyhow::Error {
        match self {
            ErrorQr::EmptyFrame => anyhow!("Empty frame."),
            ErrorQr::FountainDifferentLength => anyhow!("While collecting fountain QR code, encountered a frame for different payload length."),
            ErrorQr::FountainFrameTooShort{raw_frame} => anyhow!("Frame appears to be a fountain QR code frame, but payload {} is too short to get payload length.", show_raw_payload(raw_frame)),
            ErrorQr::FountainInterruptedByLegacy => anyhow!("Collecting fountain QR code was interrupted by a legacy multiframe QR frame."),
            ErrorQr::FountainInterruptedByStatic => anyhow!("Collecting fountain QR code was interrupted by a static QR frame."),
            ErrorQr::FountainPacketEmpty{raw_frame} => anyhow!("Frame appears to be a fountain QR code frame, but payload {} contains empty packet.", show_raw_payload(raw_frame)),
            ErrorQr::LegacyDifferentLength => anyhow!("While collecting legacy multiframe QR code, encountered a frame from a different one."),
            ErrorQr::LegacyInterruptedByFountain => anyhow!("Collecting legacy multiframe QR code was interrupted by a fountain QR frame."),
            ErrorQr::LegacyInterruptedByStatic => anyhow!("Collecting legacy multiframe QR code was interrupted by a static QR frame."),
            ErrorQr::LegacySameOrderDifferentContent => anyhow!("While collecting legacy multiframe QR code, encountered two different frames with same order."),
            ErrorQr::LegacyTooShortNumberOfFrames{raw_frame} => anyhow!("Frame appears to be a legacy multiframe QR code frame, but payload {} is too short to get total number of frames.", show_raw_payload(raw_frame)),
            ErrorQr::LegacyTooShortOrder{raw_frame} => anyhow!("Frame appears to be a legacy multiframe QR code frame, but payload {} is too short to get frame order.", show_raw_payload(raw_frame)),
            ErrorQr::LegacyOrderTooHigh{order, number_of_frames} => anyhow!("Frame appears to be a legacy multiframe QR code frame, but frame order {} is too high for expected number of frames {}.", order, number_of_frames),
            ErrorQr::LegacyZeroFrames{raw_frame} => anyhow!("Frame appears to be a legacy multiframe QR code frame, but payload {} corresponds to 0 total frames.", show_raw_payload(raw_frame)),
            ErrorQr::PoisonedLock => anyhow!("Lock is poisoned."),
        }
    }
}

/// Display questionable QR code payload, for error display.
fn show_raw_payload(raw_frame: &[u8]) -> String {
    if raw_frame.len() > 8 {
        format!(
            "{}..{}",
            hex::encode(&raw_frame[..4]),
            hex::encode(&raw_frame[raw_frame.len() - 4..])
        )
    } else {
        hex::encode(raw_frame)
    }
}

/// Collected and processed frames interacting with the outside code through
/// `uniffi`
#[derive(Debug)]
pub struct Collection {
    pub collection: RwLock<CollectionBody>,
}

impl Collection {
    /// Make new [`Collection`].
    pub fn new() -> Self {
        Collection {
            collection: RwLock::new(CollectionBody::Empty),
        }
    }

    /// Clean existing [`Collection`].
    pub fn clean(self: &Arc<Self>) -> anyhow::Result<()> {
        let mut collection = self
            .collection
            .write()
            .map_err(|_| ErrorQr::PoisonedLock.anyhow())?;
        *collection = CollectionBody::Empty;
        Ok(())
    }

    /// Process new frame and modify [`Collection`]. Outputs optional final
    /// result, indicating to UI that it is time to proceed.
    pub fn process_frame(self: &Arc<Self>, raw_frame: Vec<u8>) -> anyhow::Result<Payload> {
        let mut collection = self
            .collection
            .write()
            .map_err(|_| ErrorQr::PoisonedLock.anyhow())?;
        match &*collection {
            CollectionBody::Empty => {
                *collection = CollectionBody::init(raw_frame).map_err(|e| e.anyhow())?;
                if let CollectionBody::Ready { payload } = &*collection {
                    Ok(Payload {
                        payload: Some(hex::encode(payload)),
                    })
                } else {
                    Ok(Payload { payload: None })
                }
            }
            CollectionBody::Ready { .. } => Ok(Payload { payload: None }),
            CollectionBody::NotReady { multi } => {
                *collection = CollectionBody::add_frame(raw_frame, multi.to_owned())
                    .map_err(|e| e.anyhow())?;
                if let CollectionBody::Ready { payload } = &*collection {
                    Ok(Payload {
                        payload: Some(hex::encode(payload)),
                    })
                } else {
                    Ok(Payload { payload: None })
                }
            }
        }
    }

    pub fn frames(&self) -> anyhow::Result<Option<Frames>> {
        let collection = self
            .collection
            .read()
            .map_err(|_| ErrorQr::PoisonedLock.anyhow())?;
        match &*collection {
            CollectionBody::Empty => Ok(None),
            CollectionBody::Ready { .. } => Ok(None),
            CollectionBody::NotReady { multi } => Ok(Some(Frames {
                current: multi.currently_collected_frames(),
                total: multi.total_expected_frames(),
            })),
        }
    }
}

impl Default for Collection {
    fn default() -> Self {
        Self::new()
    }
}

/// Collected and processed frames
#[derive(Debug)]
pub enum CollectionBody {
    /// Initiated, no frames yet received
    Empty,

    /// No more frames needed, result is ready
    Ready { payload: Vec<u8> },

    /// Need more frames, for multiframe QRs only
    NotReady { multi: Multi },
}

#[derive(Clone, Debug)]
/// Partially collected and processed multiframe QR data
pub enum Multi {
    /// Fountain QR
    Fountain {
        /// packet set
        content: Vec<Vec<u8>>,

        /// total payload length in bytes
        payload_length: u32,

        /// minimal number of frames at which the decoding is tried
        optimistic_total_expected_frames: u32,
    },

    /// Legacy multiframe QR
    Legacy {
        /// collected frames content
        content: Vec<LegacyMultiContent>,

        /// total number of frames, all must be collected
        total_expected_frames: u16,
    },
}

#[derive(Clone, Debug)]
/// Individual frame content for legacy multiframe QRs
pub struct LegacyMultiContent {
    /// order [0..total_expected_frames)
    pub order: u16,

    /// corresponding data
    pub data: Vec<u8>,
}

/// Object to move output through uniffi
pub struct Payload {
    pub payload: Option<String>,
}

/// Object to move number of frames through uniffi
pub struct Frames {
    pub current: u32,
    pub total: u32,
}

impl CollectionBody {
    /// Initiate collection with the first frame.
    fn init(raw_frame: Vec<u8>) -> Result<Self, ErrorQr> {
        match Frame::from_raw(&raw_frame)? {
            Frame::Fountain {
                frame_content,
                frame_payload_length,
            } => {
                if frame_content.is_empty() {
                    return Err(ErrorQr::FountainPacketEmpty { raw_frame });
                }

                // this is minimal number of frames at which the Raptor decoder
                // is tried
                //
                // expected number of frames displayed to user is higher by one
                let optimistic_total_expected_frames =
                    frame_payload_length / (frame_content.len() as u32);
                let content = vec![frame_content];
                if optimistic_total_expected_frames == 1 {
                    match try_fountain(&content, frame_payload_length) {
                        Some(payload) => Ok(CollectionBody::Ready { payload }),
                        None => Ok(CollectionBody::NotReady {
                            multi: Multi::Fountain {
                                content,
                                payload_length: frame_payload_length,
                                optimistic_total_expected_frames,
                            },
                        }),
                    }
                } else {
                    Ok(CollectionBody::NotReady {
                        multi: Multi::Fountain {
                            content,
                            payload_length: frame_payload_length,
                            optimistic_total_expected_frames,
                        },
                    })
                }
            }
            Frame::LegacyMulti {
                frame_content,
                frame_total_expected_frames,
            } => {
                if frame_total_expected_frames == 1 {
                    Ok(CollectionBody::Ready {
                        payload: frame_content.data,
                    })
                } else {
                    Ok(CollectionBody::NotReady {
                        multi: Multi::Legacy {
                            content: vec![frame_content],
                            total_expected_frames: frame_total_expected_frames,
                        },
                    })
                }
            }
            Frame::Static => Ok(CollectionBody::Ready { payload: raw_frame }),
        }
    }

    /// Add one more frame to already initiated, but not yet completed
    /// collection, and check if collection becomes completed.
    fn add_frame(raw_frame: Vec<u8>, collection_not_ready: Multi) -> Result<Self, ErrorQr> {
        match Frame::from_raw(&raw_frame)? {
            Frame::Fountain {
                frame_content,
                frame_payload_length,
            } => match collection_not_ready {
                Multi::Fountain {
                    mut content,
                    payload_length,
                    optimistic_total_expected_frames,
                } => {
                    if frame_payload_length != payload_length {
                        Err(ErrorQr::FountainDifferentLength)
                    } else if !content.contains(&frame_content) {
                        content.push(frame_content);
                        Ok(frame_added_to_fountain(
                            content,
                            payload_length,
                            optimistic_total_expected_frames,
                        ))
                    } else {
                        Ok(CollectionBody::NotReady {
                            multi: Multi::Fountain {
                                content,
                                payload_length,
                                optimistic_total_expected_frames,
                            },
                        })
                    }
                }
                Multi::Legacy { .. } => Err(ErrorQr::LegacyInterruptedByFountain),
            },
            Frame::LegacyMulti {
                frame_content,
                frame_total_expected_frames,
            } => match collection_not_ready {
                Multi::Fountain { .. } => Err(ErrorQr::FountainInterruptedByLegacy),
                Multi::Legacy {
                    content,
                    total_expected_frames,
                } => add_frame_to_legacy(
                    frame_content,
                    frame_total_expected_frames,
                    content,
                    total_expected_frames,
                ),
            },
            Frame::Static => match collection_not_ready {
                Multi::Fountain { .. } => Err(ErrorQr::FountainInterruptedByStatic),
                Multi::Legacy { .. } => Err(ErrorQr::LegacyInterruptedByStatic),
            },
        }
    }
}

impl Multi {
    /// Number of good different frames already collected.
    fn currently_collected_frames(&self) -> u32 {
        match self {
            Multi::Fountain { content, .. } => content.len() as u32,
            Multi::Legacy { content, .. } => content.len() as u32,
        }
    }

    /// Display pessimistic number of frames needed to process QR sequence, for
    /// UI.
    ///
    /// In fountain QR decoding the decoding algorithm is probabilistic, see
    /// documentation of the `raptorq` crate.
    ///
    /// Rephrasing from there, the probability to decode message with h
    /// additional packets is 1 - 1/256^(h+1).
    ///
    /// Thus, if there are no additional packets, probability is ~ 0.99609.
    ///
    /// If one additional packet is added, it is ~ 0.99998.
    ///
    /// It was decided to add one additional packet in the printed estimate, so
    /// that the user expectations are lower.
    ///
    /// In legacy multiframe QR codes all existing frames must be collected.
    fn total_expected_frames(&self) -> u32 {
        match self {
            Multi::Fountain {
                optimistic_total_expected_frames,
                ..
            } => *optimistic_total_expected_frames + 1,
            Multi::Legacy {
                total_expected_frames,
                ..
            } => *total_expected_frames as u32,
        }
    }
}

/// Individual QR code processed.
enum Frame {
    /// Supposedly a part of fountain QR.
    Fountain {
        /// Packet, one of many to be fed into `raptorq` decoder.
        frame_content: Vec<u8>,

        /// Decoded payload length, in bytes.
        frame_payload_length: u32,
    },

    /// Supposedly a part of legacy multiframe QR.
    LegacyMulti {
        /// Individual frame [`LegacyMultiContent`].
        frame_content: LegacyMultiContent,

        /// Total number of frames, will need them all to decode.
        frame_total_expected_frames: u16,
    },

    /// Static QR.
    Static,
}

impl Frame {
    /// Process raw payload and get the [`Frame`].
    fn from_raw(raw: &[u8]) -> Result<Self, ErrorQr> {
        let first_byte = match raw.get(0) {
            Some(a) => *a,
            None => return Err(ErrorQr::EmptyFrame),
        };
        if first_byte >= FOUNTAIN_MARKER {
            let frame_payload_length = match raw.get(..4) {
                Some(a) => {
                    let payload_length_piece: [u8; 4] =
                        a.try_into().expect("constant size, always fits");
                    u32::from_be_bytes(payload_length_piece) - FOUNTAIN_LIMIT
                }
                None => {
                    return Err(ErrorQr::FountainFrameTooShort {
                        raw_frame: raw.to_vec(),
                    })
                }
            };
            let frame_content = raw[4..].to_vec();
            Ok(Frame::Fountain {
                frame_content,
                frame_payload_length,
            })
        } else if first_byte == 0 {
            let frame_total_expected_frames = match raw.get(1..3) {
                Some(a) => {
                    let total_expected_frames_piece: [u8; 2] =
                        a.try_into().expect("constant size, always fits");
                    u16::from_be_bytes(total_expected_frames_piece)
                }
                None => {
                    return Err(ErrorQr::LegacyTooShortNumberOfFrames {
                        raw_frame: raw.to_vec(),
                    });
                }
            };
            if frame_total_expected_frames == 0 {
                return Err(ErrorQr::LegacyZeroFrames {
                    raw_frame: raw.to_vec(),
                });
            }
            let order = match raw.get(3..5) {
                Some(a) => {
                    let order_piece: [u8; 2] = a.try_into().expect("constant size, always fits");
                    u16::from_be_bytes(order_piece)
                }
                None => {
                    return Err(ErrorQr::LegacyTooShortOrder {
                        raw_frame: raw.to_vec(),
                    })
                }
            };
            if order >= frame_total_expected_frames {
                return Err(ErrorQr::LegacyOrderTooHigh {
                    order,
                    number_of_frames: frame_total_expected_frames,
                });
            }
            let data = raw[5..].to_vec();
            let frame_content = LegacyMultiContent { order, data };
            Ok(Frame::LegacyMulti {
                frame_content,
                frame_total_expected_frames,
            })
        } else {
            Ok(Frame::Static)
        }
    }
}

/// Helper function, to process the newly updated fountain QR data.
fn frame_added_to_fountain(
    content: Vec<Vec<u8>>,
    payload_length: u32,
    optimistic_total_expected_frames: u32,
) -> CollectionBody {
    if content.len() as u32 >= optimistic_total_expected_frames {
        match try_fountain(&content, payload_length) {
            Some(payload) => CollectionBody::Ready { payload },
            None => CollectionBody::NotReady {
                multi: Multi::Fountain {
                    content,
                    payload_length,
                    optimistic_total_expected_frames,
                },
            },
        }
    } else {
        CollectionBody::NotReady {
            multi: Multi::Fountain {
                content,
                payload_length,
                optimistic_total_expected_frames,
            },
        }
    }
}

/// Try to assemble the fountain data. `Some(_)` if successful, `None` if not
/// yet enough data is gathered.
///
/// Warnning. This panics in upstream (`raptorq` crate) if the content is not
/// really a part of processable fountain data. For example, mock frames like
/// `vec![128, 0, 0, 0, 5]` will panic here, as decoder addresses parts of
/// vector directly.
fn try_fountain(content: &[Vec<u8>], payload_length: u32) -> Option<Vec<u8>> {
    let config =
        raptorq::ObjectTransmissionInformation::with_defaults(payload_length as u64, CHUNK_SIZE);
    let mut decoder = raptorq::Decoder::new(config);
    let mut result = None;
    for x in content.iter() {
        result = decoder.decode(raptorq::EncodingPacket::deserialize(x));
    }
    result
}

/// Adds a frame to legacy multiframe QR data, finalizes the collection if all
/// frames assembled.
fn add_frame_to_legacy(
    frame_content: LegacyMultiContent,
    frame_total_expected_frames: u16,
    mut content: Vec<LegacyMultiContent>,
    total_expected_frames: u16,
) -> Result<CollectionBody, ErrorQr> {
    if frame_total_expected_frames != total_expected_frames {
        return Err(ErrorQr::LegacyDifferentLength);
    }
    let mut is_frame_in_set = false;
    for element in content.iter() {
        if element.order == frame_content.order {
            if element.data != frame_content.data {
                return Err(ErrorQr::LegacySameOrderDifferentContent);
            }
            is_frame_in_set = true;
            break;
        }
    }
    if !is_frame_in_set {
        content.push(frame_content);
        // `as u16`, because there could be no more elements than
        // `total_expected_frames` (only correct order gets in, order is checked
        // to be below `total_expected_frames`)
        if content.len() as u16 == total_expected_frames {
            let mut payload: Vec<u8> = Vec::new();
            content.sort_by(|a, b| a.order.cmp(&b.order));
            for element in content.iter() {
                payload.extend_from_slice(&element.data);
            }
            Ok(CollectionBody::Ready { payload })
        } else {
            Ok(CollectionBody::NotReady {
                multi: Multi::Legacy {
                    content,
                    total_expected_frames,
                },
            })
        }
    } else {
        Ok(CollectionBody::NotReady {
            multi: Multi::Legacy {
                content,
                total_expected_frames,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn init_no_panic() {
        assert_eq!(
            CollectionBody::init(vec![]).unwrap_err(),
            ErrorQr::EmptyFrame
        );
        assert_eq!(
            CollectionBody::init(vec![128]).unwrap_err(),
            ErrorQr::FountainFrameTooShort {
                raw_frame: vec![128]
            }
        );
        assert_eq!(
            CollectionBody::init(vec![128, 155, 100, 108]).unwrap_err(),
            ErrorQr::FountainPacketEmpty {
                raw_frame: vec![128, 155, 100, 108]
            }
        );
        assert_eq!(
            CollectionBody::init(vec![0]).unwrap_err(),
            ErrorQr::LegacyTooShortNumberOfFrames { raw_frame: vec![0] }
        );
        assert_eq!(
            CollectionBody::init([0; 3].to_vec()).unwrap_err(),
            ErrorQr::LegacyZeroFrames {
                raw_frame: [0; 3].to_vec()
            }
        );
        assert_eq!(
            CollectionBody::init(vec![0, 1, 0, 5]).unwrap_err(),
            ErrorQr::LegacyTooShortOrder {
                raw_frame: vec![0, 1, 0, 5]
            }
        );
        assert_eq!(
            CollectionBody::init(vec![0, 0, 5, 0, 8]).unwrap_err(),
            ErrorQr::LegacyOrderTooHigh {
                order: 8,
                number_of_frames: 5
            }
        );
    }

    #[test]
    fn show_raw_payload_correctly() {
        let raw_frame = [0, 0, 5, 0, 1];
        assert_eq!(show_raw_payload(&raw_frame), "0000050001");
        let raw_frame = [0, 0, 5, 0, 1, 3, 14];
        assert_eq!(show_raw_payload(&raw_frame), "0000050001030e");
        let raw_frame = [0, 0, 5, 0, 1, 3, 14, 15];
        assert_eq!(show_raw_payload(&raw_frame), "0000050001030e0f");
        let raw_frame = [0, 0, 5, 0, 1, 3, 14, 15, 92];
        assert_eq!(show_raw_payload(&raw_frame), "00000500..030e0f5c");
        let raw_frame = [0, 0, 5, 0, 1, 3, 14, 15, 92, 6, 54];
        assert_eq!(show_raw_payload(&raw_frame), "00000500..0f5c0636");
    }
}
