'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 7:48:02 am'!

!SHTextStyler methodsFor: 'private' stamp: 'jmv 27/Nov/2025 17:04:34'!
terminateBackgroundStylingProcess
	self mutex critical: [
		backgroundProcess
			ifNotNil: [
				backgroundProcess isTerminated ifFalse: [
					backgroundProcess suspend.
					backgroundProcess terminate ].
				backgroundProcess := nil ].
	]! !

