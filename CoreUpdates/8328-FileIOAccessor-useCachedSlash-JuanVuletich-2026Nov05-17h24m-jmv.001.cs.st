'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:26:59 pm'!

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 8:26:24 pm (UTC)'!
pathNameDelimiter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self slash! !

!FileIOAccessor methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 3:02:49 pm (UTC)'!
slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash := String with: self primPathNameDelimiter ].
	^slash! !

!FileIOAccessor methodsFor: 'testing' stamp: 'jmv 5/Nov/2026, 3:03:08 pm (UTC)'!
onUnix
	^self slash == $/! !

!FileIOAccessor methodsFor: 'testing' stamp: 'jmv 5/Nov/2026, 3:03:00 pm (UTC)'!
onWindows
	^self slash == $\! !

