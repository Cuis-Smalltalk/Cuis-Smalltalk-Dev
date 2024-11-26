'From Cuis7.1 [latest update: #6850] on 26 November 2024 at 11:34:39 am'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 10:29:16'!
doItContext
	^nil! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 11/26/2024 10:29:22'!
doItReceiver
	^nil! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/26/2024 10:30:16'!
afterCompiling: aSourceCode do: aBlock ifFail: failBlock

	^ self
		afterCompiling: aSourceCode
		do: aBlock
		for: self codeProvider doItReceiver
		in: self codeProvider doItContext
		ifFail: failBlock.! !

