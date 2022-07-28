//! Parsed cards to display decoded call and extensions data
use num_bigint::{BigInt, BigUint};
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use sp_core::{
    crypto::{AccountId32, Ss58AddressFormat, Ss58Codec},
    H256,
};
use sp_runtime::generic::Era;

use definitions::network_specs::ShortSpecs;

use crate::decoding_commons::StLenCheckSpecialtyCompact;

#[derive(Clone, Copy, Debug)]
pub enum Specialty {
    None,
    Balance,
    Tip,
    Nonce,
    SpecVersion,
    TxVersion,
}

#[derive(Clone)]
pub struct Call {
    pub pallet: String,
    pub call: String,
    pub docs: String,
    pub content: Vec<ParserCard>,
}

impl Call {
    pub fn is_balance_display(&self) -> bool {
        self.pallet == "Balances" || self.pallet == "Staking"
    }
}
/*
#[derive(Clone)]
pub struct EnumVariantInfo {
    pub enum_path: Path<PortableForm>,
    pub enum_docs: String,
    pub variant_name: String,
    pub variant_docs: String,
    pub variant_content: Vec<FieldInfo>,
}

#[derive(Clone)]
pub struct FieldInfo {
    pub field_name: Option<String>,
    pub type_name: String,
    pub field_docs: String,
    pub field_content: Vec<Info>,
}

#[derive(Clone)]
pub struct Info {
    pub card: ParserCard,
    pub 
}

// vectors and arrays
#[derive(Clone)]
pub struct HomogeneousSet {
    content: Vec<ParserCard>,
}

impl HomogeneousSet {
    pub fn length(&self) -> usize {
        self.content.len()
    }
}

// structs
#[derive(Clone)]
pub struct NonHomogeneousSet {
    content: Vec<ParserCard>,
}
*/
#[derive(Clone)]
pub enum ParserCard {
    Call(Call),
    Pallet(String), // pallet name
    Method {
        method_name: String,
        docs: String,
    },
    Varname(String),
    PrimitiveBool(bool),
    PrimitiveChar(char),
    PrimitiveU8 {
        value: u8,
        specialty: Specialty,
    },
    PrimitiveU16 {
        value: u16,
        specialty: Specialty,
    },
    PrimitiveU32 {
        value: u32,
        specialty: Specialty,
    },
    PrimitiveU64 {
        value: u64,
        specialty: Specialty,
    },
    PrimitiveU128 {
        value: u128,
        specialty: Specialty,
    },
    PrimitiveU256(BigUint),
    PrimitiveI8(i8),
    PrimitiveI16(i16),
    PrimitiveI32(i32),
    PrimitiveI64(i64),
    PrimitiveI128(i128),
    PrimitiveI256(BigInt),
    PerU16(PerU16),
    Percent(Percent),
    Permill(Permill),
    Perbill(Perbill),
    Perquintill(Perquintill),
    Default(String),
    Text(String),
    Id(AccountId32),
    None,
    IdentityField(String),
    BitVec(String), // String from printing BitVec
    FieldName {
        name: String,
        docs_field_name: String,
        path_type: String,
        docs_type: String,
    },
    FieldNumber {
        number: usize,
        docs_field_number: String,
        path_type: String,
        docs_type: String,
    },
    EnumVariantName {
        name: String,
        docs_enum_variant: String,
    },
    Era(Era),
    BlockHash(H256),
}

impl ParserCard {
    pub fn show_no_docs(&self, indent: u32, short_specs: &ShortSpecs) -> String {
        match &self {
            ParserCard::Call(call) => {
                let mut out = [
                    readable(indent, "pallet", &call.pallet),
                    String::from("\n"),
                    readable(indent+1, "method", &call.call),
                ].concat();
                for x in call.content.iter() {
                    out.push('\n');
                    out.push_str(&x.show_no_docs(indent+2, short_specs));
                }
                out
            },
            ParserCard::Pallet(pallet_name) => readable(indent, "pallet", pallet_name),
            ParserCard::Method {
                method_name,
                docs: _,
            } => readable(indent, "method", method_name),
            ParserCard::Varname(varname) => readable(indent, "varname", varname),
            ParserCard::PrimitiveBool(a) => readable(indent, "bool", &a.to_string()),
            ParserCard::PrimitiveChar(a) => readable(indent, "char", &a.to_string()),
            ParserCard::PrimitiveU8 { value, specialty } => {
                display_with_specialty::<u8>(*value, indent, *specialty, short_specs)
            }
            ParserCard::PrimitiveU16 { value, specialty } => {
                display_with_specialty::<u16>(*value, indent, *specialty, short_specs)
            }
            ParserCard::PrimitiveU32 { value, specialty } => {
                display_with_specialty::<u32>(*value, indent, *specialty, short_specs)
            }
            ParserCard::PrimitiveU64 { value, specialty } => {
                display_with_specialty::<u64>(*value, indent, *specialty, short_specs)
            }
            ParserCard::PrimitiveU128 { value, specialty } => {
                display_with_specialty::<u128>(*value, indent, *specialty, short_specs)
            }
            ParserCard::PrimitiveU256(a) => readable(indent, "u256", &a.to_string()),
            ParserCard::PrimitiveI8(a) => readable(indent, "i8", &a.to_string()),
            ParserCard::PrimitiveI16(a) => readable(indent, "i16", &a.to_string()),
            ParserCard::PrimitiveI32(a) => readable(indent, "i32", &a.to_string()),
            ParserCard::PrimitiveI64(a) => readable(indent, "i64", &a.to_string()),
            ParserCard::PrimitiveI128(a) => readable(indent, "i128", &a.to_string()),
            ParserCard::PrimitiveI256(a) => readable(indent, "i256", &a.to_string()),
            ParserCard::PerU16(a) => readable(indent, "per_u16", &a.deconstruct().to_string()),
            ParserCard::Percent(a) => readable(indent, "percent", &a.deconstruct().to_string()),
            ParserCard::Permill(a) => readable(indent, "permill", &a.deconstruct().to_string()),
            ParserCard::Perbill(a) => readable(indent, "perbill", &a.deconstruct().to_string()),
            ParserCard::Perquintill(a) => {
                readable(indent, "perquintill", &a.deconstruct().to_string())
            }
            ParserCard::Default(decoded_string) => readable(indent, "default", decoded_string),
            ParserCard::Text(decoded_text) => readable(indent, "text", decoded_text),
            ParserCard::Id(id) => readable(
                indent,
                "Id",
                &id.to_ss58check_with_version(Ss58AddressFormat::custom(short_specs.base58prefix)),
            ),
            ParserCard::None => readable(indent, "none", ""),
            ParserCard::IdentityField(variant) => readable(indent, "identity_field", variant),
            ParserCard::BitVec(bv) => readable(indent, "bitvec", bv),
            ParserCard::FieldName {
                name,
                docs_field_name: _,
                path_type: _,
                docs_type: _,
            } => readable(indent, "field_name", name),
            ParserCard::FieldNumber {
                number,
                docs_field_number: _,
                path_type: _,
                docs_type: _,
            } => readable(indent, "field_number", &number.to_string()),
            ParserCard::EnumVariantName {
                name,
                docs_enum_variant: _,
            } => readable(indent, "enum_variant_name", name),
            ParserCard::Era(era) => match era {
                Era::Immortal => readable(indent, "era", "Immortal"),
                Era::Mortal(period, phase) => readable(
                    indent,
                    "era",
                    &format!("Mortal, phase: {}, period: {}", phase, period),
                ),
            },
            ParserCard::BlockHash(block_hash) => {
                readable(indent, "block_hash", &hex::encode(block_hash))
            }
        }
    }
}

fn readable(indent: u32, card_type: &str, card_payload: &str) -> String {
    format!(
        "{}{}: {}",
        "  ".repeat(indent as usize),
        card_type,
        card_payload
    )
}

fn display_with_specialty<T: StLenCheckSpecialtyCompact>(
    value: T,
    indent: u32,
    specialty: Specialty,
    short_specs: &ShortSpecs,
) -> String {
    match specialty {
        Specialty::None => readable(indent, T::default_card_name(), &value.to_string()),
        Specialty::Balance => {
            let balance =
                <T>::convert_balance_pretty(value, short_specs.decimals, &short_specs.unit);
            readable(
                indent,
                "balance",
                &format!("{} {}", balance.number, balance.units),
            )
        }
        Specialty::Tip => {
            let tip = <T>::convert_balance_pretty(value, short_specs.decimals, &short_specs.unit);
            readable(indent, "tip", &format!("{} {}", tip.number, tip.units))
        }
        Specialty::Nonce => readable(indent, "nonce", &value.to_string()),
        Specialty::SpecVersion => {
            readable(indent, "network", &format!("{}{}", short_specs.name, value))
        }
        Specialty::TxVersion => readable(indent, "tx_version", &value.to_string()),
    }
}
