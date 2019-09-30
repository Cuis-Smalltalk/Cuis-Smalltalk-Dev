'From Cuis 5.0 [latest update: #3891] on 29 September 2019 at 1:56:19 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/29/2019 13:55:44'!
submorphBehind: aMorph

	self submorphsBehind: aMorph do: [ :m | ^m ].
	^nil! !

