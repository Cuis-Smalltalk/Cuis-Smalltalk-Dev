'From Cuis7.9 [latest update: #7991] on 9 June 2026 at 5:00:06 pm'!

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 9/Jun/2026 16:57:25'!
libraryName
	^libraryName! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 9/Jun/2026 16:59:48'!
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
							| current inChange |
							current := (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange := cmWithNode method copyWithTrailerBytes: #(0).
							change libraryName ifNotNil: [ :libName | inChange propertyValueAt: #libraryName put: libName ].
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode := [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current := currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !

