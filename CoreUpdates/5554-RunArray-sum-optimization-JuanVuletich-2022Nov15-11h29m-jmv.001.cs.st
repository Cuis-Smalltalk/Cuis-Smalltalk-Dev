'From Cuis 6.0 [latest update: #5553] on 15 November 2022 at 11:34:38 am'!

!RunArray methodsFor: 'statistics' stamp: 'jmv 11/15/2022 11:34:19'!
sum: aBlock ifEmpty: emptyBlockOrValue
        "
        (RunArray new: 4096 withAll: 3) sum =  12288.
        ([(RunArray new) sum] on: Error do: [:ex | ex messageText] ) = 'this collection is empty'.
        "
	| initial sum |
	self isEmpty ifTrue: [^ emptyBlockOrValue value].
        initial _ aBlock value: self first.
        sum _ initial.
        self runsAndValuesDo: [:run :value | sum _ sum + (run * (aBlock value: value))].
        ^ sum - initial! !


!RunArray reorganize!
('accessing' = at: canJoinMessage: first isEmpty last runLengthAt: size values withStartStopAndValueDo:)
('adding' add: addFirst: addLast: coalesce rangeOf:startingAt:)
('copying' , copy copyFrom:to: copyReplaceFrom:to:with:)
('printing' printOn: storeOn: writeOn:)
('private' at:setRunOffsetAndValue: basicReplaceAttributesFrom:to:with: canJoin:and: find: mapValues: runs setRuns:setValues:)
('enumerating' runsAndValuesDo: runsFrom:to:do:)
('converting' reversed)
('testing' is:)
('statistics' sum:ifEmpty:)
!

