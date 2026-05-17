'From Cuis7.7 [latest update: #7937] on 15 May 2026 at 3:43:00 pm'!

!RunArray methodsFor: 'inspecting' stamp: 'zrdz 15/May/2026 15:42:17'!
explorerContents	
	(Preferences allPreferences at: #compressRunArrayValues :: value) ifTrue: [
		| vals |
		vals := OrderedCollection new.
		vals add: (ObjectExplorerWrapper with: runs name: 'runs' model: self).
		1 = runs size :: ifTrue: [
			| sym | 
			sym := '1->',  (runs at: 1) asString.
			vals add: (ObjectExplorerWrapper
				 with: values
				 name: sym model: self).
		] ifFalse: [
			1 to: runs size do: [ :i | 
				| sym symst |
				(i = 1) ifTrue: [ symst := '1'. ] ifFalse: [ symst := runs at: (i - 1) :: + 1 :: asString. ].
				sym :=  symst, '->', (runs at: i :: asString) :: asSymbol. 
				vals add: (ObjectExplorerWrapper
					with: (values at: i)
					name: sym model: self).
			]. 
		].
		^ vals.
	] ifFalse: [
		^ super explorerContents.
	].! !


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
('inspecting' explorerContents)
!
| pref name | 
name := #compressRunArrayValues.
pref := Preference new name: name
 description: 'If set, RunArray will have a compressed view, not repeating every value at every index, but instead showing what is present at a given run.'
 category: #inspection
 type: Boolean
 value: false.
Preferences allPreferences at: name put: pref.
"Postscript: Lodge compressRunArrayValues as a preference to alter the
 behaviour of `RunArray`s when being explored."
!

!

