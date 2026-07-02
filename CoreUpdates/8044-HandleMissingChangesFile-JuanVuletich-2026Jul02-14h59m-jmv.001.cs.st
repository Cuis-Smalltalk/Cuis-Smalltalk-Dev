'From Cuis7.9 [latest update: #8043] on 2 July 2026 at 2:59:55 pm'!

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 2/Jul/2026, 6:59:42 pm (UTC)'!
withChangesFileDo: aBlock

	^self currentChangesName ifNotNil: [ :fn |
		fn asFullFileEntry readStreamDo: aBlock ]! !

