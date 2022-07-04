package io.parity.signer.models

import android.annotation.SuppressLint
import android.util.Log
import androidx.camera.core.ImageProxy
import androidx.lifecycle.LiveData
import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel
import com.google.mlkit.vision.barcode.BarcodeScanner
import com.google.mlkit.vision.common.InputImage
import io.parity.signer.uniffi.Action

class CameraModel {
	private val _cameraState: MutableLiveData<String> = MutableLiveData("init")
	val cameraState: LiveData<String> = _cameraState

	fun submitFrame(frame: List<UByte>) {
		_cameraState.value = frame.toString()
		Log.d("Submitted frame", cameraState.value?:"failure")
	}
}

/**
 * Barcode detecting function.
 * This uses experimental features
 */
@SuppressLint("UnsafeOptInUsageError")
fun CameraModel.processFrame(
	barcodeScanner: BarcodeScanner,
	imageProxy: ImageProxy,
	button: (Action, String, String) -> Unit
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

/**
 * Clears camera progress
 */
fun SignerDataModel.resetScanValues() {
	bucket = arrayOf()
	_captured.value = null
	_total.value = null
	_progress.value = 0.0f
}
