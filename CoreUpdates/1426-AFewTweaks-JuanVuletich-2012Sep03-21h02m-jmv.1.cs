'From Cuis 4.0 of 21 April 2012 [latest update: #1425] on 3 September 2012 at 9:10 pm'!
!classDefinition: #ObjectExplorerWindow category: #'Morphic-Tools'!
SystemWindow subclass: #ObjectExplorerWindow
	instanceVariableNames: 'listMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!HierarchicalListMorph methodsFor: 'accessing' stamp: 'jmv 9/3/2012 18:22'!
selectedMorph
	^selectedMorph! !


!ListItemWrapper methodsFor: 'accessing' stamp: 'jmv 9/3/2012 08:20'!
item
	^item! !


!SystemDictionary methodsFor: 'printing' stamp: 'jmv 9/3/2012 18:04'!
printOn: aStream
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]! !


!Array methodsFor: 'printing' stamp: 'jmv 9/2/2012 22:10'!
printOn: aStream

	self == Smalltalk specialObjectsArray
		ifTrue: [
			aStream nextPutAll: 'Smalltalk specialObjectsArray' ]
		ifFalse: [
			aStream nextPut: $#.
			self printElementsOn: aStream ]! !


!Dictionary methodsFor: 'comparing' stamp: 'jmv 9/3/2012 08:37'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"stop recursion"
	(aDictionary isKindOf: Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !


!LookupKey methodsFor: 'printing' stamp: 'jmv 9/2/2012 21:43'!
printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPut: $)! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 9/3/2012 18:21'!
buildMorphicWindow
	| textMorph |
	.listMorph _ HierarchicalListMorph
			model: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #genericMenu
			keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	textMorph _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: (model rootObject printStringLimitedTo: 64)! !


!ObjectExplorerWrapper methodsFor: 'converting' stamp: 'jmv 9/3/2012 20:53'!
asString
	| explorerString string |
	explorerString _ [ item printString ]
			on: Error 
			do: ['<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	string _ itemName , ': ' , explorerString.
	^ string withBlanksCondensed! !


!PointerExplorer methodsFor: 'accessing' stamp: 'jmv 9/2/2012 22:41'!
getList

	| w |
	w _ PointerExplorerWrapper
		with: rootObject
		name: rootObject identityHash asString
		model: self.
	^Array with: w! !


!PointerExplorerWrapper methodsFor: 'testing' stamp: 'jmv 9/2/2012 22:45'!
hasContents
	"Correct, albeit slow, answer"
	"^self contents notEmpty"
	^true! !


!ObjectExplorerWrapper reorganize!
('as yet unclassified' canBeDragged contents hasContents parent parent: selector setItem:name:model: setItem:name:model:parent:)
('converting' asString itemName)
('monitoring' refresh)
('nil')
!

!classDefinition: #ObjectExplorerWindow category: #'Morphic-Tools'!
SystemWindow subclass: #ObjectExplorerWindow
	instanceVariableNames: 'listMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!
