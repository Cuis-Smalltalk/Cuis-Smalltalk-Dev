'From Cuis 6.0 [latest update: #5096] on 4 April 2022 at 5:43:09 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/4/2022 17:41:25'!
afterCompiling: aSourceCode do: aBlock ifFail: failBlock

	| context provider receiver |
	
	provider := self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver := provider doItReceiver.
			context := provider doItContext]
		ifFalse: [
			receiver := context := nil].

	^self afterCompiling: aSourceCode do: aBlock for: receiver in: context ifFail: failBlock.
	! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/4/2022 17:42:02'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selectionAsStream upToEnd
		do: [ :compiler :method :receiver :context | method ifNotNil: [ self debug: method receiver: receiver in: context ]]
		ifFail: [].! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/4/2022 17:42:30'!
evaluate: aSourceCode andDo: aBlock ifFail: failBlock profiled: doProfile
	
	| result | 
	
	^ self 
		afterCompiling: aSourceCode 
		do: [ :compiler :method :receiver :context | method ifNotNil: [
			result := compiler evaluateMethod: method to: receiver logged: true profiled: doProfile.
			aBlock value: result ]]
		ifFail: failBlock.! !

!methodRemoval: SmalltalkEditor #afterCompiling:do: stamp: 'jmv 4/4/2022 17:42:19'!
SmalltalkEditor removeSelector: #afterCompiling:do:!
