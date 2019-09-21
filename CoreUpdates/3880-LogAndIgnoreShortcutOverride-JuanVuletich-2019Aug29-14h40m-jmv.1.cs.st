'From Cuis 5.0 [latest update: #3852] on 29 August 2019 at 2:41:42 pm'!

!Editor class methodsFor: 'class initialization' stamp: 'jmv 8/29/2019 10:42:05'!
putIntoCmdShortcuts: shortcutsSpec

	shortcutsSpec do: [ :ary | | i previous |
		i _ ary first numericValue + 1.
		previous _ cmdShortcuts at: i.
		previous = #noop:
			ifTrue: [
				cmdShortcuts at: i put: ary second ]
			ifFalse: [ ('Editor shortcut: ', ary first printString, ' already taken for: ', previous, '. Override request for: ', ary second, '  ignored') print ]]! !

