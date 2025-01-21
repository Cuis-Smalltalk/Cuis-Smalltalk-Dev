'From Cuis7.3 [latest update: #6995] on 21 January 2025 at 9:00:36 am'!

!EphemeronIdentityDictionary methodsFor: 'as yet unclassified' stamp: 'jmv 1/21/2025 08:57:52'!
slowSize
	"Careful!! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	self keysDo: [ :k | count := count + 1 ].
	^count! !


!EphemeronIdentityDictionary reorganize!
('accessing' associationClass)
('adding' add:)
('access protected protocol' associationAt:ifAbsent: at:ifAbsent: at:ifPresent:ifAbsent: at:put: collect: do: keys keysAndValuesDo: keysAndValuesRemove: keysDo: rehash removeAll removeKey:ifAbsent: select: valuesDo:)
('private' atNewIndex:put:)
('copying' postCopy)
('finalization' finalizeEphemeron: finalizeValues)
('initialize-release' init:)
('as yet unclassified' size slowSize)
('public')
!

