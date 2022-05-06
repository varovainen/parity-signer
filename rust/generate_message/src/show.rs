use blake2_rfc::blake2b::blake2b;

use constants::{ADDRESS_BOOK, HOT_DB_NAME, METATREE};
use db_handling::helpers::{open_db, open_tree, try_get_meta_values_by_name_version};
use definitions::{
    error_active::{Active, Check, ErrorActive, IncomingMetadataSourceActiveStr},
    metadata::{AddressBookEntry, MetaValues},
};

pub fn show_database() -> Result<(), ErrorActive> {
    let database = open_db::<Active>(HOT_DB_NAME)?;
    let metadata = open_tree::<Active>(&database, METATREE)?;
    if metadata.is_empty() {
        println!("Database has no metadata entries.");
        return Ok(());
    }
    println!("Database has metadata information for following networks:");
    for x in metadata.iter().flatten() {
        let meta_values = MetaValues::from_entry_checked::<Active>(x)?;
        println!(
            "\t{} {}, metadata hash {}",
            meta_values.name,
            meta_values.version,
            hash_string(&meta_values.meta)
        );
    }
    Ok(())
}

pub fn show_address_book() -> Result<(), ErrorActive> {
    let database = open_db::<Active>(HOT_DB_NAME)?;
    let address_book = open_tree::<Active>(&database, ADDRESS_BOOK)?;
    if address_book.is_empty() {
        println!("Address book is empty.");
        return Ok(());
    }
    println!("Address book has entries for following networks:");
    for x in address_book.iter().flatten() {
        let (title, address_book_entry) = AddressBookEntry::process_entry(x)?;
        if address_book_entry.def {
            println!(
                "\t{} at {}, encryption {} (default)",
                title,
                address_book_entry.address,
                address_book_entry.encryption.show()
            );
        } else {
            println!(
                "\t{} at {}, encryption {}",
                title,
                address_book_entry.address,
                address_book_entry.encryption.show()
            );
        }
    }
    Ok(())
}

pub fn check_file(path: String) -> Result<(), ErrorActive> {
    let meta_str = match std::fs::read_to_string(&path) {
        Ok(a) => a,
        Err(e) => {
            return Err(ErrorActive::Check {
                filename: path,
                check: Check::MetadataFile(e),
            })
        }
    };
    let from_file = MetaValues::from_str_metadata(
        meta_str.trim(),
        IncomingMetadataSourceActiveStr::Check { filename: path },
    )?;
    match try_get_meta_values_by_name_version::<Active>(
        HOT_DB_NAME,
        &from_file.name,
        from_file.version,
    )? {
        Some(from_database) => {
            if from_database.meta == from_file.meta {
                println!(
                    "{}{}, metadata hash {}, in the database",
                    from_file.name,
                    from_file.version,
                    hash_string(&from_file.meta)
                )
            } else {
                println!(
                    "{}{}, metadata hash {}, same version metadata in the database has different hash {}",
                    from_file.name,
                    from_file.version,
                    hash_string(&from_file.meta),
                    hash_string(&from_database.meta)
                )
            }
        }
        None => {
            println!(
                "{}{}, metadata hash {}, not in the database",
                from_file.name,
                from_file.version,
                hash_string(&from_file.meta)
            )
        }
    }
    Ok(())
}

fn hash_string(meta: &[u8]) -> String {
    hex::encode(blake2b(32, &[], meta).as_bytes())
}
