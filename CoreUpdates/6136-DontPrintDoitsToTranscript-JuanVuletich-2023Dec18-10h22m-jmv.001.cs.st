'From Cuis6.0 [latest update: #6134] on 18 December 2023 at 10:30:51 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 12/18/2023 10:25:29'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self evaluateSelectionAndDo: nil ifFail: nil profiled: aBoolean.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 12/18/2023 10:28:37'!
evaluate: aSourceCode andDo: aBlock ifFail: failBlock profiled: doProfile
	
	| result | 
	
	^ self 
		afterCompiling: aSourceCode 
		do: [ :compiler :method :receiver :context | method ifNotNil: [
			result := compiler evaluateMethod: method to: receiver logged: true profiled: doProfile.
			aBlock notNil ifTrue: [
				aBlock value: result ]]]
		ifFail: failBlock.! !

