//
//  NativeSignerApp.swift
//  NativeSigner
//
//  Created by Alexander Slesarev on 19.7.2021.
//

import SwiftUI

@main
struct NativeSignerApp: App {
    @StateObject var data = SignerDataModel()
    @StateObject var canary = Canary()
    var body: some Scene {
        WindowGroup {
            MainScreenContainer()
                .environmentObject(data)
                .environmentObject(canary)
                .background(Color("backgroundColor"))
        }
    }
}