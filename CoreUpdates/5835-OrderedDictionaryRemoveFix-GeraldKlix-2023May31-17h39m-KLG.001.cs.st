'From Haver 6.0 [latest update: #5832] on 2 June 2023 at 7:51:08 pm'!

!OrderedDictionary methodsFor: 'removing' stamp: 'KLG 6/2/2023 19:49:22'!
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."

	| answer |
	answer := super removeKey: key ifAbsent: [
		^ aBlock value ].
	orderedKeys remove: key.
	^ answer! !

