'From Cuis 6.0 [latest update: #5717] on 23 March 2023 at 12:22:07 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 3/23/2023 12:19:56'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!!"

	| answer |
	answer := self basicDisplayBounds.
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsDo: [ :m |
			answer := answer
				ifNil: [m displayFullBounds]
				ifNotNil: [answer quickMerge: m displayFullBounds]]].
	^answer! !

