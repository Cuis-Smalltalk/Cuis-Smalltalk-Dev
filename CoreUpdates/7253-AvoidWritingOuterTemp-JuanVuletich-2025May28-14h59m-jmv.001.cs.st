'From Cuis7.3 [latest update: #7249] on 28 May 2025 at 3:03:01 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/28/2025 15:02:33'!
evaluate: aSourceCode andDo: aBlock ifFail: failBlock profiled: doProfile

	^ self 
		afterCompiling: aSourceCode 
		do: [ :compiler :method :receiver :context |
			method ifNotNil: [
				| result |
				result := compiler evaluateMethod: method to: receiver logged: true profiled: doProfile.
				aBlock notNil ifTrue: [
					aBlock value: result ]]]
		ifFail: failBlock.! !

