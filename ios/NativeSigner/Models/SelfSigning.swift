//
//  SelfSigning.swift
//  NativeSigner
//
//  Created by Alexander Slesarev on 23.10.2021.
//
// This is all UI interaction logic behind self-signing updates
// More of this should eventually sink in Rust

import Foundation
import SwiftUI

struct Allkey: Codable, Equatable {
    var public_key: String
    var encryption: String
    var ss58: String
    var path: String
    var has_password: String
    var name: String
    var seed_name: String
}

extension SignerDataModel {
    
    /**
     * get all keys to select from in self-signing mode
     */
    func getAllKeys() {
        var err = ExternError()
        let err_ptr = UnsafeMutablePointer(&err)
        let res = get_all_identities(err_ptr, dbName)
        if err_ptr.pointee.code == 0 {
            if let keysJSON = String(cString: res!).data(using: .utf8) {
                guard let keys = try? JSONDecoder().decode([Allkey].self, from: keysJSON) else {
                    print("JSON decoder failed on keys")
                    print(String(cString: res!))
                    signer_destroy_string(res!)
                    return
                }
                self.allKeys = keys.sorted(by: {
                    if $0.seed_name == $1.seed_name {
                        return $0.path < $1.path
                    } else {
                        return $0.seed_name < $1.seed_name
                    }
                })
            } else {
                print("keysJSON corrupted")
            }
            signer_destroy_string(res!)
            print("success1")
        } else {
            self.transactionError = String(cString: err_ptr.pointee.message)
            print(self.transactionError)
            signer_destroy_string(err_ptr.pointee.message)
        }
    }
    
    /**
     * Self-sign metadata!
     */
    //TODO: this is a hotfix; we need to figure out how we want this to be done.
    func signLoadMetadata(selectedKey: Allkey, version: String, password: String) {
        var err = ExternError()
        let err_ptr = UnsafeMutablePointer(&err)
        let res = sign_load_metadata(err_ptr, self.metadataName, Int32(version)!, selectedKey.public_key, selectedKey.encryption, self.getSeed(seedName: selectedKey.seed_name), password, self.dbName)
        if err_ptr.pointee.code == 0 {
            self.result = String(cString: res!)
            signer_destroy_string(res!)
            if let imageData = Data(fromHexEncodedString: self.result ?? "") {
                print("Success")
                print(self.result)
                self.ssSign = UIImage(data: imageData)!
            } else {
                self.transactionError = "QR code generation error"
                self.ssSign = UIImage()
            }
        } else {
            self.transactionError = String(cString: err_ptr.pointee.message)
            print(self.transactionError)
            signer_destroy_string(err_ptr.pointee.message)
            self.totalRefresh()
            self.ssSign = UIImage()
        }
    }
}
