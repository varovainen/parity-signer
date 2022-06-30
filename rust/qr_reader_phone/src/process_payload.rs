use anyhow::anyhow;
use constants::{CHUNK_SIZE, FOUNTAIN_LIMIT, FOUNTAIN_MARKER};
use raptorq;
use std::convert::TryInto;

#[derive(Debug, PartialEq)]
pub struct Fountain {
    decoder: raptorq::Decoder,
    collected_ser_packets: Vec<Vec<u8>>,
    length: u32,
    pub total: usize,
}

impl Fountain {
    pub fn collected(&self) -> usize {
        self.collected_ser_packets.len()
    }
}

#[derive(Debug, PartialEq)]
pub struct LegacyMulti {
    length: u16,
    elements: Vec<Element>,
}

#[derive(Debug, PartialEq)]
pub struct Element {
    number: u16,
    contents: Vec<u8>,
}

#[derive(Debug, PartialEq)]
pub enum InProgress {
    None,
    Fountain(Fountain),
    LegacyMulti(LegacyMulti),
}

#[derive(Debug, PartialEq)]
pub enum Ready {
    NotYet(InProgress),
    Yes(Vec<u8>),
}

// Warning: upstream `raptorq` panics often if it meets unexpected payload
// format.
pub fn process_decoded_payload(
    frame_data: &mut Vec<u8>,
    mut decoding: InProgress,
) -> anyhow::Result<Ready> {
    let frame_hex = hex::encode(&frame_data);
    match qr_type(frame_data)? {
        QrType::Fountain { payload_length } => match decoding {
            InProgress::None => {
                if frame_data.is_empty() {
                    return Err(anyhow!("Frame 0x{} appears to be a fountain QR code frame with empty associated packet.", frame_hex));
                }
                let total = (payload_length as usize) / frame_data.len() + 1;
                let collected_ser_packets = vec![frame_data.to_vec()];
                let config = raptorq::ObjectTransmissionInformation::with_defaults(
                    payload_length as u64,
                    CHUNK_SIZE,
                );
                let mut decoder = raptorq::Decoder::new(config);
                match try_fountain(&collected_ser_packets, &mut decoder) {
                    Some(v) => Ok(Ready::Yes(v)),
                    None => {
                        let in_progress = Fountain {
                            decoder,
                            collected_ser_packets,
                            length: payload_length,
                            total,
                        };
                        decoding = InProgress::Fountain(in_progress);
                        Ok(Ready::NotYet(decoding))
                    }
                }
            }
            InProgress::Fountain(mut in_progress) => {
                if in_progress.length != payload_length {
                    return Err(anyhow!("Was decoding fountain qr code with payload length {}, got interrupted by fountain qr code with payload length {}", in_progress.length, payload_length));
                }
                if !in_progress.collected_ser_packets.contains(frame_data) {
                    in_progress.collected_ser_packets.push(frame_data.to_vec());
                    match try_fountain(&in_progress.collected_ser_packets, &mut in_progress.decoder)
                    {
                        Some(v) => Ok(Ready::Yes(v)),
                        None => Ok(Ready::NotYet(InProgress::Fountain(in_progress))),
                    }
                } else {
                    Ok(Ready::NotYet(InProgress::Fountain(in_progress)))
                }
            }
            InProgress::LegacyMulti(_) => {
                return Err(anyhow!(
                    "Was decoding legacy multi-element qr, and got interrupted by a fountain one."
                ))
            }
        },
        QrType::LegacyMulti { number_of_frames } => {
            if number_of_frames == 0 {
                return Err(anyhow!(
                    "Frame 0x{} appears to be a legacy multiframe QR code part with zero frames.",
                    frame_hex
                ));
            }
            let element_number_piece: [u8; 2] = match frame_data.get(..2) {
                Some(a) => a.try_into()
                            .expect("constant vector slice size, always fits"),
                None => return Err(anyhow!("Frame 0x{} appears to be a legacy multiframe QR code part, but is too short to get current frame number.", frame_hex))
            };
            let number = u16::from_be_bytes(element_number_piece);
            if number >= number_of_frames {
                return Err(anyhow!("Number of element {} in legacy multiframe QR sequence exceeds expected total number of frames {}.", number, number_of_frames));
            }
            let contents = frame_data[2..].to_vec();
            let new_element = Element { number, contents };
            match decoding {
                InProgress::None => {
                    let mut collected = LegacyMulti {
                        length: number_of_frames,
                        elements: vec![new_element],
                    };
                    match try_legacy(&mut collected) {
                        Some(v) => Ok(Ready::Yes(v)),
                        None => Ok(Ready::NotYet(InProgress::LegacyMulti(collected))),
                    }
                }
                InProgress::Fountain(_) => {
                    return Err(anyhow!(
                    "Was decoding fountain qr code, and got interrupted by a legacy multi-element one."
                ))
                }
                InProgress::LegacyMulti(mut collected) => {
                    if collected.length != number_of_frames {
                        return Err(anyhow!("Was decoding legacy multi-element qr code with {} elements, got interrupted by legacy multi-element qr code with {} elements", collected.length, number_of_frames));
                    }
                    if !collected.elements.contains(&new_element) {
                        for x in collected.elements.iter() {
                            if x.number == number {
                                return Err(anyhow!("Encountered two legacy multi-element qr code fragments with same number."));
                            }
                        }
                        collected.elements.push(new_element);
                        match try_legacy(&mut collected) {
                            Some(v) => Ok(Ready::Yes(v)),
                            None => Ok(Ready::NotYet(InProgress::LegacyMulti(collected))),
                        }
                    } else {
                        Ok(Ready::NotYet(InProgress::LegacyMulti(collected)))
                    }
                }
            }
        }
        QrType::Static => {
            if let InProgress::None = decoding {
                Ok(Ready::Yes(frame_data.to_vec()))
            } else {
                return Err(anyhow!(
                    "Was reading dynamic qr, and got interrupted by a static one."
                ));
            }
        }
    }
}

fn try_fountain(
    collected_ser_packets: &[Vec<u8>],
    current_decoder: &mut raptorq::Decoder,
) -> Option<Vec<u8>> {
    let mut result = None;
    for x in collected_ser_packets.iter() {
        result = current_decoder.decode(raptorq::EncodingPacket::deserialize(x));
    }
    result
}

fn try_legacy(collected: &mut LegacyMulti) -> Option<Vec<u8>> {
    if collected.length < collected.elements.len() as u16 {
        None
    } else {
        collected.elements.sort_by_key(|element| element.number);
        let mut out: Vec<u8> = Vec::new();
        for x in collected.elements.iter() {
            out.extend_from_slice(&x.contents);
        }
        Some(out)
    }
}

#[derive(Debug)]
pub enum QrType {
    Fountain { payload_length: u32 },
    LegacyMulti { number_of_frames: u16 },
    Static,
}

pub fn qr_type(frame_data: &mut Vec<u8>) -> anyhow::Result<QrType> {
    let first_byte = match frame_data.get(0) {
        Some(a) => *a,
        None => return Err(anyhow!("Empty frame.")),
    };
    if first_byte >= FOUNTAIN_MARKER {
        let payload_length = match frame_data.get(..4) {
            Some(a) => {
                let length_piece: [u8; 4] = a.try_into()
                    .expect("constant size, always fits");
                u32::from_be_bytes(length_piece) - FOUNTAIN_LIMIT
            },
            None => return Err(anyhow!("Frame 0x{} appears to be a fountain QR code frame, but is too short to get expected payload length.", hex::encode(frame_data)))
        };
        *frame_data = frame_data[4..].to_vec();
        Ok(QrType::Fountain { payload_length })
    } else if first_byte == 0 {
        let number_of_frames = match frame_data.get(1..3) {
            Some(a) => {
                let number_of_frames_piece: [u8; 2] = a.try_into()
                    .expect("constant size, always fits");
                u16::from_be_bytes(number_of_frames_piece)
            },
            None => return Err(anyhow!("Frame 0x{} appears to be a legacy multiframe QR code part, but is too short to get expected number of frames.", hex::encode(frame_data)))
        };
        *frame_data = frame_data[3..].to_vec();
        Ok(QrType::LegacyMulti { number_of_frames })
    } else {
        Ok(QrType::Static)
    }
}

pub fn number_of_frames(frame_data: &mut Vec<u8>) -> anyhow::Result<u32> {
    let frame_hex = hex::encode(&frame_data);
    match qr_type(frame_data)? {
        QrType::Fountain { payload_length } => {
            if frame_data.is_empty() {
                return Err(anyhow!("Frame 0x{} appears to be a fountain QR code frame with empty associated packet.", frame_hex));
            }
            Ok(payload_length / (frame_data.len() as u32) + 1)
        }
        QrType::LegacyMulti { number_of_frames } => Ok(number_of_frames as u32),
        QrType::Static => Ok(1),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_qr_type_no_panic() {
        assert_eq!(
            qr_type(&mut vec![]).unwrap_err().to_string(),
            "Empty frame."
        );
        assert_eq!(
            qr_type(&mut vec![128]).unwrap_err().to_string(),
            "Frame 0x80 appears to be a fountain QR code frame, but is too short to get expected payload length."
        );
        assert!(qr_type(&mut vec![128, 155, 100, 108]).is_ok());
        assert_eq!(
            qr_type(&mut vec![0]).unwrap_err().to_string(),
            "Frame 0x00 appears to be a legacy multiframe QR code part, but is too short to get expected number of frames."
        );
        assert!(qr_type(&mut [0; 3].to_vec()).is_ok());
        assert!(qr_type(&mut vec![0, 1, 0, 5]).is_ok());
    }

    #[test]
    fn get_number_of_frames_no_panic() {
        assert_eq!(
            number_of_frames(&mut vec![]).unwrap_err().to_string(),
            "Empty frame."
        );
        assert_eq!(
            number_of_frames(&mut vec![128]).unwrap_err().to_string(),
            "Frame 0x80 appears to be a fountain QR code frame, but is too short to get expected payload length."
        );
        assert_eq!(
            number_of_frames(&mut vec![128, 155, 100, 108])
                .unwrap_err()
                .to_string(),
            "Frame 0x809b646c appears to be a fountain QR code frame with empty associated packet."
        );
        assert_eq!(
            number_of_frames(&mut vec![0]).unwrap_err().to_string(),
            "Frame 0x00 appears to be a legacy multiframe QR code part, but is too short to get expected number of frames."
        );
        assert!(number_of_frames(&mut [0; 3].to_vec()).is_ok());
        assert!(number_of_frames(&mut vec![0, 1, 0, 5]).is_ok());
    }
}
