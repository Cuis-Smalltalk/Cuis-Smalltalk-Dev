'From Cuis7.3 [latest update: #7102] on 28 April 2025 at 8:21:53 pm'!

!Categorizer class methodsFor: 'well known categories' stamp: 'HAW 3/24/2025 18:18:48'!
accessing

	^'accessing'! !


!Browser methodsFor: 'class functions' stamp: 'HAW 3/24/2025 18:19:32'!
createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage := aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: Categorizer accessing ].
			(aClass canUnderstand: (setter := aName , ':') asSymbol) ifFalse: [
				newMessage := setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' := anObject'.
				aClass
					compile: newMessage
					classified: Categorizer accessing ]]]! !

