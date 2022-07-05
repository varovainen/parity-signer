package io.parity.signer.models

import android.annotation.SuppressLint
import android.util.Log
import androidx.camera.core.ImageProxy
import com.google.mlkit.vision.barcode.BarcodeScanner
import com.google.mlkit.vision.common.InputImage
import io.parity.signer.uniffi.Action
import io.parity.signer.uniffi.Payload

/**
 * Barcode detecting function.
 * This uses experimental features
 */
@OptIn(ExperimentalUnsignedTypes::class)
@SuppressLint("UnsafeOptInUsageError")
fun processFrame(
	barcodeScanner: BarcodeScanner,
	imageProxy: ImageProxy,
	button: (Action, String, String) -> Unit,
	submitFrame: (List<UByte>) -> Payload
) {
	if (imageProxy.image == null) return
	val inputImage = InputImage.fromMediaImage(
		imageProxy.image!!,
		imageProxy.imageInfo.rotationDegrees
	)

	barcodeScanner.process(inputImage)
		.addOnSuccessListener { barcodes ->
			barcodes.forEach {
				it?.rawBytes?.toUByteArray()?.toList()?.let { payload ->
					submitFrame(payload)
				}?.payload?.let { payload ->
					// This is pressed only once, that's checked in rust backend
					// by sending complete payload only once
					button(Action.TRANSACTION_FETCHED, payload, "")
				}
			}
		}
		.addOnFailureListener {
			Log.e("Scan failed", it.message.toString())
		}
		.addOnCompleteListener {
			imageProxy.close()
		}
}
