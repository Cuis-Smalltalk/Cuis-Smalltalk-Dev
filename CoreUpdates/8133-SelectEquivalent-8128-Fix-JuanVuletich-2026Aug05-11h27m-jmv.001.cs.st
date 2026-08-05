'From Cuis7.9 [latest update: #8131] on 5 August 2026 at 11:33:09 am'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 5/Aug/2026, 2:33:02 pm (UTC)'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change := changeList at: i.
		listSelections at: i put:
			((change isMethodDefinition and: [
				(class := change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode := [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange currentProperties |
							current := (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							currentProperties := current propertiesAsArray.
							inChange := cmWithNode method copyWithTrailerBytes: #(0).
							(change properties isNil or: [ change properties = currentProperties ]) and: [
								current = inChange or: [
									| currentCmWithNode |
									currentCmWithNode := [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
											notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
									(currentCmWithNode notNil and: [
										current := currentCmWithNode method copyWithTrailerBytes: #(0).
										current = inChange ])]]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !

