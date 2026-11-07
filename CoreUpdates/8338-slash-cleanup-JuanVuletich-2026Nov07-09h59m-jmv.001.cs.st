'From Cuis7.9 [latest update: #8336] on 7 November 2026 at 10:01:08 am'!

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 7/Nov/2026, 1:00:44 pm (UTC)'!
slashCharacter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash := self primPathNameDelimiter ].
	^slash! !

!methodRemoval: FileIOAccessor #slash stamp: 'jmv 7/Nov/2026, 1:00:38 pm (UTC)'!
FileIOAccessor removeSelector: #slash!
