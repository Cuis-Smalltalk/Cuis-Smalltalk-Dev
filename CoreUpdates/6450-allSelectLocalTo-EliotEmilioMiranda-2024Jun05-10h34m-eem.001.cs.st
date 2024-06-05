'From Cuis7.1 [latest update: #6449] on 5 June 2024 at 10:48:12 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'eem 6/5/2024 10:37:47'!
allSelect: aUnaryBlock localTo: aClass
	"Answer a SortedCollection of each method in the hierarchy of aClass
	 for which aUnaryBlock answers true when evaluated with the method."
	| aCollection |
	aCollection := SortedCollection new.
	aClass allSuperclasses, aClass withAllSubclasses do:
		[:class |
		class selectorsAndMethodsDo:
			[:sel :meth|
			(aUnaryBlock value: meth) ifTrue:
				[aCollection add: (MethodReference class: class selector: sel)]]].
	^aCollection! !

!SystemDictionary methodsFor: 'browsing' stamp: 'eem 6/5/2024 10:42:36'!
browseAllSelect: aUnaryBlock localTo: aClass
	"Create and schedule a message browser on each method in the hierarchy of aClass
	 for which aUnaryBlock answers true when evaluated with the method.
	For example, Smalltalk browseAllSelect: [:m| m sendsSelector: #size] localTo: Collection."
	self browseMessageList: (self allSelect: aUnaryBlock localTo: aClass) name: 'selected messages in', aClass name! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'eem 6/5/2024 10:37:34'!
allSelect: aUnaryBlock
	"Answer a SortedCollection of each method in the system
	 for which aUnaryBlock answers true when evaluated with the method."
	| aCollection |
	aCollection := SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :meth |
			(aUnaryBlock value: meth) ifTrue: [
				aCollection add: (MethodReference class: class selector: sel)
			]]].
	^ aCollection! !

