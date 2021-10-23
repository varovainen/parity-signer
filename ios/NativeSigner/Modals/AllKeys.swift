//
//  AllKeys.swift
//  NativeSigner
//
//  Created by Alexander Slesarev on 23.10.2021.
//

import SwiftUI

struct AllKeys: View {
    @EnvironmentObject var data: SignerDataModel
    @State var password = ""
    var body: some View {
        ZStack {
            ModalBackdrop()
            ScrollView {
                TextField("Password", text: $password/*, prompt: Text("(optional)")*/)
                    .foregroundColor(Color("textEntryColor"))
                    .background(Color("textFieldColor"))
                    .font(.largeTitle)
                    .disableAutocorrection(true)
                    .autocapitalization(.none)
                    .keyboardType(.asciiCapable)
                    .onChange(of: data.suggestedName, perform: {_ in data.lastError = ""
                    })
                    .border(Color("AccentColor"), width: 1)
                LazyVStack {
                    ForEach(data.allKeys, id: \.public_key) {
                        address in
                        Button(action: {
                            data.signLoadMetadata(selectedKey: address, version: data.metadataVersion, password: password)
                            print("here-1")
                            data.keyManagerModal = .showSSQr
                        })
                        {
                            ZStack {
                                RoundedRectangle(cornerRadius: 4).foregroundColor(Color("backgroundCard")).frame(height: 44)
                                HStack {
                                    Image(uiImage: UIImage(data: Data(fromHexEncodedString: String(cString: identicon(nil, address.public_key, 32))) ?? Data()) ?? UIImage())
                                        .resizable(resizingMode: .stretch)
                                        .frame(width: 28, height: 28)
                                    VStack (alignment: .leading) {
                                        HStack {
                                            Text(address.seed_name + address.path)
                                                .foregroundColor(Color("cryptoColor"))
                                        }.font(.system(size: 12, weight: .semibold, design: .monospaced))
                                        //Here we could have shortened base58 address when buttons are shown, but we don't need to
                                        Text(address.public_key)
                                            .foregroundColor(Color("textFadedColor"))
                                            .font(.system(size: 12, design: .monospaced))
                                    }
                                    Spacer()
                                }.padding(.horizontal, 8)
                            }
                        }
                    }
                }
            }
        }
    }
}

/*
 struct AllKeys_Previews: PreviewProvider {
 static var previews: some View {
 AllKeys()
 }
 }
 */
