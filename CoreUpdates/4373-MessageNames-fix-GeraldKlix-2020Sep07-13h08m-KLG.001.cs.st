'From Cuis 5.0 [latest update: #4372] on 7 September 2020 at 1:10:40 pm'!

!MessageNames methodsFor: 'class list' stamp: 'KLG 9/1/2020 21:48:42'!
selectedClassOrMetaClass
	"Answer the currently selected class (or metaclass).
	
	If no method is selected, try to interpret the selected message name
	as a class"
	
	^ super selectedClassOrMetaClass ifNil: [
		self selectedMessageName ifNotNil: [ :className | | mayBeClass |
			(mayBeClass _ Smalltalk
				at: className ifAbsent: [^ nil ]) isBehavior
					ifTrue: [ mayBeClass ]
					ifFalse: [ mayBeClass class ] ] ].! !

