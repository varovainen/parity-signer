package io.parity.signer.modals

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.unit.dp
import io.parity.signer.components.BigButton
import io.parity.signer.components.HeaderBar
import io.parity.signer.components.KeyCard
import io.parity.signer.components.SingleTextInput
import io.parity.signer.ui.theme.Bg200
import io.parity.signer.ui.theme.modal
import io.parity.signer.uniffi.Action
import io.parity.signer.uniffi.MEnterPassword

@Composable
fun EnterPassword(
	enterPassword: MEnterPassword,
	button: (Action, String) -> Unit
) {
	val password = remember {
		mutableStateOf("")
	}
	val focusManager = LocalFocusManager.current
	val focusRequester = remember { FocusRequester() }

	Surface(
		color = MaterialTheme.colors.Bg200,
		shape = MaterialTheme.shapes.modal,
		modifier = Modifier
			.fillMaxSize(1f)
			.padding(8.dp)
	) {
		Column(
			horizontalAlignment = Alignment.CenterHorizontally,
			modifier = Modifier.padding(20.dp)
		) {
			HeaderBar(line1 = "SECRET PATH", line2 = "///password")
			KeyCard(
				identity = enterPassword.authorInfo
			)
			if (enterPassword.counter > 0u) {
				Text("Attempt " + enterPassword.counter.toString() + " of 3")
			}
			SingleTextInput(
				content = password,
				update = { password.value = it },
				onDone = {
					if (password.value.isNotBlank()) {
						button(
							Action.GO_FORWARD,
							password.value
						)
					}
				},
				prefix = { Text("///") },
				focusManager = focusManager,
				focusRequester = focusRequester
			)
			BigButton(
				text = "Next",
				isCrypto = true,
				action = {
					button(
						Action.GO_FORWARD,
						password.value
					)
				},
				isDisabled = password.value.isBlank()
			)
		}
	}
}
