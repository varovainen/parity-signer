# Scope  

# Terminology  

Signer receives information over the air-gap as QR codes. QR codes are read as
`u8` vectors, and must always be parsed by Signer before use.  

QR codes could contain information that user wants to sign with one of the
Signer keys or the update information to ensure smooth Signer operation without
reset or connecting to the network.  

## QR code types  

- Signable, to generate and export signature:  

   - Transaction: from call, gets later processed on chain if signature goes
   through online client  

   - Message
   
- Update, for Signer inner functionality:  

   - add network specs

   - load metadata
   
   - load types
   
- Derivations import, for bulk derivations import

- Testing

# QR code structure  

(padding, length indicator, shift)

Every QR code content starts with a prelude `[0x53, 0x<encryption code>,
0x<payload code>]`.

`0x53` is always expected and indicates Substrate-related content.

`<encryption code>` for signables indicates encryption algorithm that will be
 used to generate the signature:

- `0x00` stands for Ed25519
- `0x01` stands for Sr25519
- `0x02` stands for Ecdsa

`<encryption code>` for updates indicates encryption algorithm that was used to
sign the update:

- `0x00` stands for Ed25519
- `0x01` stands for Sr25519
- `0x02` stands for Ecdsa
- `0xff` means the update is not signed

Derivations import and testing are always unsigned, with `<encryption code>`
always `0xff`.

Signer supports following `<payload code>` variants:

- `0x00` legacy mortal transaction
- `0x02` transaction (both mortal and immortal)
- `0x03` message
- `0x80` load metadata update
- `0x81` load types update
- `0xc1` add specs update
- `0xde` derivations import
- `0xf0` testing parser card display

Note: old UOS specified `0x00` as mortal transaction and `0x02` as immortal one,
but currently both mortal and immortal transactions from polkadot-js are `0x02`.

## Transaction

Transaction has following structure:

<table>
    <tr>
        <td>prelude</td><td>public key</td><td>SCALE-encoded call data</td><td>SCALE-encoded extensions</td><td>network genesis hash</td>
    </tr>
</table>

Public key is the key that can sign the transaction. Its length depends on the
`<encryption code>` declared in transaction prelude:

| Encryption | Public key length, bytes |
|:-|:-|
| Ed25519 | 32 |
| Sr25519 | 32 |
| Ecdsa | 33 |

Call data is `Vec<u8>` representation of transaction content. Call data must be
parsed by Signer prior to signature generation and becomes a part of signed
blob. Within transaction, the call data is SCALE-encoded, i.e. effectively is
prefixed with compact of its length in bytes.

Extensions contain data additional to the call data, and also are part of a
signed blob. Typical extensions are Era, Nonce, metadata version, etc.
Extensions content and order, in principle, can vary between the networks and
metadata versions.

Network genesis hash determines the network in which the transaction is created.
At the moment genesis hash is fixed-length 32 bytes.

Thus, the transaction structure could also be represented as:

<table>
    <tr>
        <td>prelude</td><td>public key</td><td>compact of call data length</td><td>**call data**</td><td>**SCALE-encoded extensions**</td><td>network genesis hash</td>
    </tr>
</table>

Bold-marked transaction pieces are used in the blob for which the signature is
produced. If the blob is short, 257 bytes or below, the signature is produced
for it as is. For blobs longer than 257 bytes, 32 byte hash (`blake2-rfc`) is
signed instead. This is inherited from earlier Signer versions, and is currently
compatible with polkadot-js.

Signer can generate a signature for transaction only if:

- the Signer has network specs on file for the network, with matching encryption
- the Signer has metadata on file that corresponds to the metadata used to
create the transaction on the hot side
- key used exists in Signer and is associated with the network

### Example

Alice makes transfer to Bob in Westend network.

Transaction:

`530102d43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27da40403008eaf04151687736326c9fea17e25fc5287613693c912909cb226aa4794f26a480700e8764817b501b8003223000005000000e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e538a7d7a0ac17eb6dd004578cb8e238c384a10f57c999a3fa1200409cd9b3f33e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e`

| Part | Meaning | Byte position |
|:-|:-|:-|
| `53` | Substrate-related content | 0 |
| `01` | Sr25519 encryption algorithm | 1 |
| `02` | Transaction | 2 |
| `d43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27d` | Alice public key | 3..=34 |
| `a40403008eaf04151687736326c9fea17e25fc5287613693c912909cb226aa4794f26a480700e8764817` | SCALE-encoded call data | 35..=76 |
| `a4` | Compact call data length, 41 | 35 |
| `0403008eaf04151687736326c9fea17e25fc5287613693c912909cb226aa4794f26a480700e8764817` | Call data | 36..=76 |
| `04` | Pallet index 4 in metadata, entry point for decoding | 36 |
| `b501b8003223000005000000e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e538a7d7a0ac17eb6dd004578cb8e238c384a10f57c999a3fa1200409cd9b3f33` | Extensions | 77..=153 |
| `e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e` | Westend genesis hash | 154..=185 |

#### Call content is parsed using Westend metadata, in this particular case westend9010

| Call part | Meaning |
|:-|:-|
| `04` | Pallet index 4 (`Balances`) in metadata, entry point for decoding |
| `03` | Call index 3 in pallet 4 (`transfer_keep_alive`), search in metadata what the call contains. Here it is `MultiAddress` for transfer destination and `Compact(u128)` balance. |
| `00` | Enum variant in `MultiAddress`, `AccountId` |
| `8eaf04151687736326c9fea17e25fc5287613693c912909cb226aa4794f26a48` | Associated `AccountId` data, Bob public key |
| `0700e8764817` | `Compact(u128)` balance. Amount paid: 100000000000 or, with Westend decimals and unit, 100.000000000 mWND. |

#### Extensions content

| Extensions part | Meaning |
|:-|:-|
| `b501` | Era: phase 27, period 64 |
| `b8` | Nonce: 46 |
| `00` | Tip: 0 pWND |
| `32230000` | Metadata version: 9010 |
| `05000000` | Tx version: 5 |
| `e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e` | Westend genesis hash |
| `538a7d7a0ac17eb6dd004578cb8e238c384a10f57c999a3fa1200409cd9b3f33` | Block hash |

## Message

Message has following structure:

<table>
    <tr>
        <td>prelude</td><td>public key</td><td>message payload</td><td>network genesis hash</td>
    </tr>
</table>

There are two types of the message payload allowed in the Signer:

| Message payload | What is rendered to user | What gets signed |
|:-|:-|:-|
| SCALE-encoded String | String | SCALE-encoded String |
| `<Bytes>..</Bytes>` wrapped `[u8]` slice | `[u8]` slice | `<Bytes>..</Bytes>` wrapped `[u8]` slice |

`<Bytes>..</Bytes>` wrapped `[u8]` slice is represented as String if all bytes
are valid UTF-8. If not all bytes are valid UTF-8, `[u8]` slice is represented
as hexadecimal string. Signer specifies which representation is used.

It is critical that the message payloads are always clearly distinguishable from
the transaction payloads, i.e. it is never possible to trick user to sign
transaction posing as a message.

SCALE-encoded String contains compact of the string length followed by `u8`
representation of the string symbols. Transaction could be parsed by the message
parser only if there are no transaction extensions including `Nonce`, thus they
are clearly distinguished. There are networks on `polkadot-js` that have zero
length extensions (Shell, for one), so this is also not a truly safe situation.
Shell, however, has `DisallowSigned` in extensions, so there could be some check
designed, maybe. If another network does not name extension translating the same
information as `ForbidSigned` or something along the lines.

`<Bytes>` wrapped messages imply the would-be sneaked call is done in pallet `<`
with method `B`, and that the extensions (typically, the last one is block hash)
end in `</Bytes>`. Although unlikely, this is possible.

## Update

Update has following general structure:

<table>
    <tr>
        <td>prelude</td><td>verifier public key (if signed)</td><td>update payload</td><td>reserved tail</td><td>signature (if signed)</td>
    </tr>
</table>

Note that the `verifier public key` and `signature` parts appear only in signed
uploads. Preludes `[0x53, 0xff, 0x<payload code>]` are followed only by the
update payload.

`reserved tail` currently is not used and is expected to be empty. It could be
used later if the multisignatures are introduced for the updates. Expecting
`reserved tail` in update processing is done to keep code continuity in case
multisignatures introduction ever happens.

Because of the `reserved tail`, the `update payload` length has to be always
exactly declared, so that the `update payload` part could be cut correctly from
the update.

Detailed description of the update payloads and form in which they are used in
update itself and for generating update signature, could be found in Rust module
`definitions::qr_transfers`.

### `add_specs` update payload

Introduces a new network to Signer, i.e. adds network specs to the Signer
database.

Update payload consists of **double** SCALE-encoded `NetworkSpecsToSend` (second
SCALE is to have the exact payload length).

Payload signature is generated for SCALE-encoded `NetworkSpecsToSend`.

### `load_metadata` update payload

Loads metadata for a network already known to Signer, i.e. for a network with
network specs in the Signer database.

Update payload consists of concatenated SCALE-encoded metadata `Vec<u8>` and
network genesis hash (H256, always 32 bytes).

Same blob is used to generate the signature.

### `load_types` update payload

Loads types information.

Type information is needed to decode transactions made in networks with metadata
RuntimeMetadata version V12 or V13.

Most of the networks are already using RuntimeMetadata version V14, which has
types information incorporated in the metadata itself.

The `load_types` update is expected to become obsolete soon.

Update payload consists of **double** SCALE-encoded `Vec<TypeEntry>` (second
SCALE is to have the exact payload length).

Payload signature is generated for SCALE-encoded `Vec<TypeEntry>`.

### Verifiers

Signer can accept both verified and non-verified updates, however, information
once verified can not be replaced or updated by a weaker verifier without full
Signer reset.

A verifier could be `Some(_)` with corresponding public key inside or `None`.
All verifiers for the data follow trust on first use principle. If new
verifier is set up instead of the old one, all the data that was verified by the
old verifier gets removed to avoid confusion regarding which verifier has signed
the data.

General verifier is the strongest and the most reliable verifier known to the
Signer. General verifier could sign all kinds of updates. By default the Signer
uses Parity-associated key as general verifier, but users can remove it and set
their own. There could be only one general verifier at any time.







