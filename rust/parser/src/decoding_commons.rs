//! Decoder elements common for all metadata versions
//!
use num_bigint::{BigInt, BigUint};
use parity_scale_codec::{Compact, Decode, HasCompact};
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use sp_core::crypto::AccountId32;
use std::{convert::TryInto, mem::size_of};

use definitions::error_signer::{ParserDecodingError, ParserError, ParserMetadataError};
use printing_balance::AsBalance;

use crate::cards::{ParserCard, Specialty};
use crate::decoding_sci_ext::{Ext, SpecialExt};

#[derive(Clone)]
pub struct OutputCard {
    pub card: ParserCard,
    pub indent: u32,
}

/// Struct to store results of searching Vec<u8> for encoded compact:
/// consists of actual number decoded, and, if it exists, the beginning position for data after the compact
pub struct CutCompact<T: HasCompact> {
    pub compact_found: T,
    pub start_next_unit: Option<usize>,
}

pub fn cut_compact<T>(data: &[u8]) -> Result<CutCompact<T>, ParserError>
where
    T: HasCompact,
    Compact<T>: Decode,
{
    if data.is_empty() {
        return Err(ParserError::Decoding(ParserDecodingError::DataTooShort));
    }
    let mut out = None;
    for i in 0..data.len() {
        let mut hippo = &data[..=i];
        let unhippo = <Compact<T>>::decode(&mut hippo);
        if let Ok(hurray) = unhippo {
            let start_next_unit = {
                if data.len() == i {
                    None
                } else {
                    Some(i + 1)
                }
            };
            out = Some(CutCompact {
                compact_found: hurray.0,
                start_next_unit,
            });
            break;
        }
    }
    match out {
        Some(c) => Ok(c),
        None => Err(ParserError::Decoding(ParserDecodingError::NoCompact)),
    }
}

/// Function to search &[u8] for shortest compact <T> by brute force.
/// Outputs CutCompact value in case of success.
pub(crate) fn get_compact<T>(data: &mut Vec<u8>) -> Result<T, ParserError>
where
    T: HasCompact,
    Compact<T>: Decode,
{
    let cut_compact = cut_compact::<T>(data)?;
    *data = match cut_compact.start_next_unit {
        Some(start) => data[start..].to_vec(),
        None => Vec::new(),
    };
    Ok(cut_compact.compact_found)
}

pub(crate) trait StLen: Sized {
    fn decode_value(data: &mut Vec<u8>) -> Result<Self, ParserError>;
}

macro_rules! impl_stable_length_decodable {
    ($($ty: ty), *) => {
        $(
            impl StLen for $ty {
                fn decode_value(data: &mut Vec<u8>) -> Result<Self, ParserError> {
                    let length = size_of::<Self>();
                    match data.get(..length) {
                        Some(slice_to_decode) => {
                            let out = <Self>::decode(&mut &slice_to_decode[..])
                                .map_err(|_| ParserError::Decoding(ParserDecodingError::PrimitiveFailure(stringify!($ty))))?;
                            *data = data[length..].to_vec();
                            Ok(out)
                        },
                        None => Err(ParserError::Decoding(ParserDecodingError::DataTooShort))
                    }
                }
            }
        )*
    }
}

impl_stable_length_decodable!(
    bool,
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
    PerU16,
    Percent,
    Permill,
    Perbill,
    Perquintill
);

macro_rules! impl_stable_length_big {
    ($($big: ty, $get: ident), *) => {
        $(
            impl StLen for $big {
                fn decode_value(data: &mut Vec<u8>) -> Result<Self, ParserError> {
                    match data.get(0..32) {
                        Some(slice_to_big256) => {
                            let out = Self::$get(slice_to_big256);
                            *data = data[32..].to_vec();
                            Ok(out)
                        },
                        None => Err(ParserError::Decoding(ParserDecodingError::DataTooShort)),
                    }
                }
            }
        )*
    }
}

impl_stable_length_big!(BigUint, from_bytes_le);
impl_stable_length_big!(BigInt, from_signed_bytes_le);

impl StLen for char {
    fn decode_value(data: &mut Vec<u8>) -> Result<Self, ParserError> {
        match data.get(0..4) {
            Some(slice_to_char) => match char::from_u32(<u32>::from_le_bytes(
                slice_to_char
                    .try_into()
                    .expect("contstant length, always fit"),
            )) {
                Some(ch) => {
                    *data = data[4..].to_vec();
                    Ok(ch)
                }
                None => Err(ParserError::Decoding(
                    ParserDecodingError::PrimitiveFailure("char"),
                )),
            },
            None => Err(ParserError::Decoding(ParserDecodingError::DataTooShort)),
        }
    }
}

pub(crate) trait StLenCheckSpecialtyCompact:
    StLen + AsBalance + HasCompact + std::fmt::Display
{
    fn decode_checked(
        data: &mut Vec<u8>,
        indent: u32,
        possible_ext: &mut Option<&mut Ext>,
        balance_flag: bool,
        compact_flag: bool,
    ) -> Result<OutputCard, ParserError>;
    fn default_card_name() -> &'static str;
}

macro_rules! impl_check_specialty_compact {
    ($($ty: ty, $enum_variant: ident), *) => {
        $(
            impl StLenCheckSpecialtyCompact for $ty {
                fn decode_checked(data: &mut Vec<u8>, indent: u32, possible_ext: &mut Option<&mut Ext>, balance_flag: bool, compact_flag: bool) -> Result<OutputCard, ParserError> {
                    let specialty = specialty(possible_ext, balance_flag);
                    let value = {
                        if compact_flag {get_compact::<Self>(data)?}
                        else {<Self>::decode_value(data)?}
                    };
                    if let Some(ext) = possible_ext {
                        if let SpecialExt::SpecVersion = ext.specialty {
                            ext.found_ext.network_version_printed = match ext.found_ext.network_version_printed {
                                Some(_) => {
                                    return Err(ParserError::FundamentallyBadV14Metadata(
                                        ParserMetadataError::SpecVersionTwice,
                                    ))
                                }
                                None => Some(value.to_string()),
                            };
                        }
                    }
                    Ok(OutputCard {
                        card: ParserCard::$enum_variant{value, specialty},
                        indent,
                    })
                }
                fn default_card_name() -> &'static str {
                    stringify!($ty)
                }
            }
        )*
    }
}

impl_check_specialty_compact!(u8, PrimitiveU8);
impl_check_specialty_compact!(u16, PrimitiveU16);
impl_check_specialty_compact!(u32, PrimitiveU32);
impl_check_specialty_compact!(u64, PrimitiveU64);
impl_check_specialty_compact!(u128, PrimitiveU128);

pub(crate) trait StLenCheckCompact: StLen {
    fn decode_checked(
        data: &mut Vec<u8>,
        indent: u32,
        compact_flag: bool,
    ) -> Result<OutputCard, ParserError>;
}

macro_rules! impl_allow_compact {
    ($($ty: ty, $enum_variant: ident), *) => {
        $(
            impl StLenCheckCompact for $ty where $ty: HasCompact {
                fn decode_checked(data: &mut Vec<u8>, indent: u32, compact_flag: bool) -> Result<OutputCard, ParserError> {
                    let value = {
                        if compact_flag {get_compact::<Self>(data)?}
                        else {<Self>::decode_value(data)?}
                    };
                    Ok(OutputCard {
                        card: ParserCard::$enum_variant(value),
                        indent,
                    })
                }
            }
        )*
    }
}

impl_allow_compact!(PerU16, PerU16);
impl_allow_compact!(Percent, Percent);
impl_allow_compact!(Permill, Permill);
impl_allow_compact!(Perbill, Perbill);
impl_allow_compact!(Perquintill, Perquintill);

macro_rules! impl_block_compact {
    ($($ty: ty, $enum_variant: ident), *) => {
        $(
            impl StLenCheckCompact for $ty {
                fn decode_checked(data: &mut Vec<u8>, indent: u32, compact_flag: bool) -> Result<OutputCard, ParserError> {
                    let value = {
                        if compact_flag {return Err(ParserError::Decoding(
                            ParserDecodingError::UnexpectedCompactInsides,
                        ))}
                        else {<Self>::decode_value(data)?}
                    };
                    Ok(OutputCard {
                        card: ParserCard::$enum_variant(value),
                        indent,
                    })
                }
            }
        )*
    }
}

impl_block_compact!(bool, PrimitiveBool);
impl_block_compact!(char, PrimitiveChar);
impl_block_compact!(i8, PrimitiveI8);
impl_block_compact!(i16, PrimitiveI16);
impl_block_compact!(i32, PrimitiveI32);
impl_block_compact!(i64, PrimitiveI64);
impl_block_compact!(i128, PrimitiveI128);
impl_block_compact!(BigInt, PrimitiveI256);
impl_block_compact!(BigUint, PrimitiveU256);

pub(crate) fn specialty(possible_ext: &mut Option<&mut Ext>, balance_flag: bool) -> Specialty {
    if let Some(ext) = possible_ext {
        match ext.specialty {
            SpecialExt::Nonce => Specialty::Nonce,
            SpecialExt::SpecVersion => Specialty::SpecVersion,
            SpecialExt::Tip => Specialty::Tip,
            SpecialExt::TxVersion => Specialty::TxVersion,
            _ => Specialty::None,
        }
    } else if balance_flag {
        Specialty::Balance
    } else {
        Specialty::None
    }
}

/// Function to decode of AccountId special case and transform the result into base58 format.
///
/// The function decodes only a single AccountId type entry,
/// removes already decoded part of input data Vec<u8>,
/// and returns whatever remains as DecodedOut field remaining_vector, which is processed later separately.
///
/// The function takes as arguments
/// - data (remaining Vec<u8> of data),
/// - indent used for creating properly formatted js cards.
/// - short_specs (taking base58 prefix from there).
///
/// The function outputs the DecodedOut value in case of success.
///
/// Resulting AccountId in base58 form is added to fancy_out on js card "Id".
pub(crate) fn special_case_account_id(
    data: &mut Vec<u8>,
    indent: u32,
) -> Result<OutputCard, ParserError> {
    match data.get(0..32) {
        Some(a) => {
            let array_decoded: [u8; 32] = a.try_into().expect("constant length, always fits");
            *data = data[32..].to_vec();
            let account_id = AccountId32::new(array_decoded);
            let out = OutputCard {
                card: ParserCard::Id(account_id),
                indent,
            };
            Ok(out)
        }
        None => Err(ParserError::Decoding(ParserDecodingError::DataTooShort)),
    }
}
