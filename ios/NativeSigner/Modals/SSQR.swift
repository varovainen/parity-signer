//
//  SSQR.swift
//  NativeSigner
//
//  Created by Alexander Slesarev on 23.10.2021.
//

import SwiftUI

struct SSQR: View {
    @EnvironmentObject var data: SignerDataModel
    var body: some View {
        ZStack {
            ModalBackdrop()
            Image(uiImage: data.ssSign)
                .resizable()
                .aspectRatio(contentMode: .fit)
        }.onAppear{print("showqr")}
    }
}

/*
struct SSQR_Previews: PreviewProvider {
    static var previews: some View {
        SSQR()
    }
}
*/
