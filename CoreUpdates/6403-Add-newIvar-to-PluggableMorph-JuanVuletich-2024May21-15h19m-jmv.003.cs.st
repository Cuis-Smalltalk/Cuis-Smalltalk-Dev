'From Cuis7.1 [latest update: #6387] on 21 May 2024 at 3:19:31 pm'!

"Postscript:
The MethodInCallStackToBecomeInvalid exception happens because
installing this change set is done during the #performAction of a PluggableButtonMorph.
But no button state will be accessed. In this case, the reshape is safe.
So, resume the MethodInCallStackToBecomeInvalid exception and effectively do the reshape"
	[
		BoxedMorph subclass: #PluggableMorph
			instanceVariableNames: 'model menuNshortcutsKey'
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Morphic-Widgets'.
	] on: MethodInCallStackToBecomeInvalid do: [ :ex | ex resume: true ]
!

