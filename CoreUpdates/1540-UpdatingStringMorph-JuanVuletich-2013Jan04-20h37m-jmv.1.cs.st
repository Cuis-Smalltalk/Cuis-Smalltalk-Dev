'From Cuis 4.1 of 12 December 2012 [latest update: #1538] on 4 January 2013 at 8:39:07 pm'!
!classDefinition: #UpdatingStringMorph category: #'Morphic-Basic'!
StringMorph subclass: #UpdatingStringMorph
	instanceVariableNames: 'target getSelector stepTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

!UpdatingStringMorph commentStamp: '<historical>' prior: 0!
UpdatingStringMorph new
     target: [self runningWorld activeHand position asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld!


!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
getSelector: aSymbol
	getSelector _ aSymbol! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:18'!
stepTime

	^stepTime! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
stepTime: aNumber
	stepTime _ aNumber! !

!UpdatingStringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:15'!
target: anObject
	target _ anObject! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/4/2013 13:24'!
step
	self contents: self world activeHand morphPosition printString! !

!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 1/4/2013 13:18'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !

!UpdatingStringMorph methodsFor: 'initialization' stamp: 'jmv 1/4/2013 13:16'!
initialize
	super initialize.
	getSelector printString.
	stepTime _ 50! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 1/4/2013 13:24'!
contents: newContents
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis _ newContents emphasisAt: 1.
			 newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded! !

