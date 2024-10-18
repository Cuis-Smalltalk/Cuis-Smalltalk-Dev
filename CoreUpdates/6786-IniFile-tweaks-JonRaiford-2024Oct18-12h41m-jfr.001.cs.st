'From Cuis7.1 [latest update: #6784] on 18 October 2024 at 2:13:32 pm'!

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jfr 10/18/2024 14:03:29'!
sectionNames
	"Answer the names of the sections. Note these are in order as they appear in the file."

	^namedSections keys
! !


!IniFileSection methodsFor: 'accessing' stamp: 'jfr 10/18/2024 14:07:22'!
entries
	^entries! !

!IniFileSection methodsFor: 'accessing' stamp: 'jfr 10/18/2024 14:08:34'!
size
	^entries size! !


!IniFile methodsFor: 'writing' stamp: 'jfr 10/18/2024 14:09:06'!
writeOn: aStream
	
	globalSection ifNotNil: [
		globalSection writeOn: aStream.
		((globalSection size > 0) and:[namedSections size > 0])
			ifTrue:[ aStream newLine ]].
	namedSections
		do: [:aSection |
			aSection writeOn: aStream.
			aStream newLine ] separatedBy: [aStream newLine].! !


!IniFileSection reorganize!
('accessing' at: at:orDefault: at:orPutDefault: at:put: entries size)
('initialization' initializeGlobal initializeNamed:)
('writing' writeOn:)
('enumerating' entriesDo:)
!

