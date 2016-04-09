'From Cuis 4.2 of 25 July 2013 [latest update: #2727] on 9 April 2016 at 7:33:38.383585 pm'!

!Array methodsFor: 'testing' stamp: 'jmv 4/9/2016 19:33'!
rangeIncludes: aNumber
	^ self includes: aNumber! !


!Array methodsFor: 'testing' stamp: 'jmv 4/9/2016 19:32'!
is: aSymbol
	^ aSymbol == #Array or: [ super is: aSymbol ]! !


!Number methodsFor: 'intervals' stamp: 'jmv 4/9/2016 19:17'!
to: stop
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by 1."
self <= stop ifFalse: [ ^ #() ].
	^self to: stop by: "(self <= stop ifTrue: [1] ifFalse: [-1])"1! !

!Number methodsFor: 'intervals' stamp: 'jmv 4/9/2016 19:18'!
to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by step."

	| answer count i |
	count _ stop - self / step + 1.
	answer _ Array new: count.
	i _ 1.
	self to: stop by: step do: [ :v |
		answer at: i put: v.
		i _ i + 1 ].
	^ answer.
"	^Interval from: self to: stop by: step"! !


!TextModelMorph methodsFor: 'model access' stamp: 'jmv 4/9/2016 19:22'!
setSelection: sel

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ 
					sel isEmpty ifTrue: [ self editor selectFrom:1 to: 0 ]
					ifFalse: [
					self editor selectFrom: sel first to: sel last ]]].
	self scrollSelectionIntoView! !


!Array reorganize!
('comparing' literalEqual:)
('converting' asArray elementsExchangeIdentityWith: elementsForwardIdentityTo: elementsForwardIdentityTo:copyHash: evalStrings)
('printing' isLiteral printOn: storeOn:)
('private' hasLiteral: hasLiteralSuchThat: replaceFrom:to:with:startingAt:)
('accessing' atWrap: atWrap:put:)
('testing' is: isArray rangeIncludes:)
('sorting' sorted:)
!

