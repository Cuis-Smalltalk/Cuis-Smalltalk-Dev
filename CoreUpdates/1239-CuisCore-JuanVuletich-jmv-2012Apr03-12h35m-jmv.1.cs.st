'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 3 April 2012 at 12:37:21 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/3/2012 12:26'!
                    lastUsedNumber: anIntegerOrNil
	"
	ChangeSet lastUsedNumber: nil
	"
	LastUsedNumber _ anIntegerOrNil! !


!String methodsFor: 'comparing' stamp: 'jmv 4/3/2012 12:20'!
                         is: aString substingAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize |
	((sequenceSize _ aString size) = 0 or: [ self size - index + 1 < sequenceSize ]) ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (aString basicAt: 1) ifFalse: [ ^false ].
	^(self findSubstring: aString in: self startingAt: index matchTable: CaseSensitiveOrder) = index! !


!ChangeSet methodsFor: 'testing' stamp: 'jmv 4/3/2012 12:24'!
                       isForBaseSystem
	"Answer true if we were created calling #baseSystemNameFor:"

	^ (name at: 1) isDigit
		and: [ (name at: 1) isDigit
			and: [ (name at: 1) isDigit
				and: [ (name at: 1) isDigit
					and: [ name is: '-CuisCore-' substingAt: 5 ]]]]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/3/2012 12:33'!
                          fileOutAndRemove
	"File out the current change set."

	myChangeSet fileOut.
	self removePrompting: false.

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/3/2012 12:34'!
     remove
	"Completely destroy my change set.  Check if it's OK first"

	| currentNumber |
	currentNumber _ myChangeSet isForBaseSystem ifTrue: [ myChangeSet name asInteger ].
	self removePrompting: true.

	"If the ChangeSet we're destroying was using the next Cuis Core change set number,
	assume that the user wants a new change set that reuses that number."
	currentNumber ifNotNil: [
		ChangeSet lastUsedNumber = currentNumber ifTrue: [
			ChangeSet lastUsedNumber: currentNumber - 1 ]].

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/3/2012 12:33'!
      removePrompting: doPrompt
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |

	"Tiene sentido? Preguntar cosas? Sugerir hacer fileOut?"
	self flag: #ojo.

	aName _ myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message _ 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg _ myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	changeSetNumber _ myChangeSet name initialIntegerOrNil.
	changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber].
	ChangeSorter removeChangeSet: myChangeSet.! !


!String methodsFor: 'comparing' stamp: 'jmv 4/3/2012 12:22'!
     beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."

	^self is: prefix substingAt: 1! !

!String methodsFor: 'comparing' stamp: 'jmv 4/3/2012 12:22'!
    endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substingAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
"! !

