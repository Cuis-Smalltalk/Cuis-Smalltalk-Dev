'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 1:33:38 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 13:32:02'!
findSelector
	"Revised to use scanner for better removal of extraneous stuff"
	| potentialSelector colonIndex |
	potentialSelector _ self withBlanksTrimmed withoutEnclosing: $".
	colonIndex _ potentialSelector indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (potentialSelector at: colonIndex - 1) isValidInIdentifiers ]) ifTrue: [
		potentialSelector _ [ Scanner findSelectorIn: potentialSelector ] on: Error do: [ :anError | 
			anError messageText = Scanner unmatechedCommentQuoteErrorDescription 
				ifTrue: [ 
					potentialSelector _ potentialSelector copyWithout: $".
					anError retry ]
				ifFalse: [ anError return: '']]].
		
	potentialSelector isEmpty ifTrue: [ ^ nil ].
	Symbol
		hasInterned: potentialSelector
		ifTrue: [ :aSymbol | ^ aSymbol ].
		
	^ nil.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/8/2022 13:32:51'!
withoutEnclosing: aCharacter
	"
	'*Hello*' withoutEnclosing: $*
	'*Hello*' asUtf8String withoutEnclosing: $*
	"
	| s |
	s _ self size.
	s = 0 ifTrue: [ ^ self ].
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 6/8/2022 13:33:10'!
asFileEntry
	"

Windows	
	'C:\Windows' asFileEntry exists false
	'C:\Windows' asDirectoryEntry exists true
	'/' asFileEntry exists false
	'/' asDirectoryEntry exists false
	'C:\' asFileEntry exists false
	'C:\' asDirectoryEntry exists true
	('C:' asDirectoryEntry // 'Windows') exists false
	('C:' asDirectoryEntry / 'Windows') exists true
	
Linux
    '/var' asFileEntry exists
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists true
    '/media/cdrom' asFileEntry exists false
    '/media/cdrom' asDirectoryEntry exists true
    ('/bin' asDirectoryEntry / 'more') exists false
    ('/bin' asDirectoryEntry // 'more') exists true

MacOsX
    '/var' asFileEntry exists false
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists  true
    '/Volumes/SanDisk32-NTFS' asFileEntry exists false
    '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
    'SanDisk32-NTFS' asFileEntry exists false
    'SanDisk32-NTFS' asDirectoryEntry exists false
	
	"
	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!!' ].
	^FileEntry withPathName: self! !

!methodRemoval: String #findSelector stamp: 'jmv 6/8/2022 13:32:02'!
String removeSelector: #findSelector!
!methodRemoval: String #withoutEnclosing: stamp: 'jmv 6/8/2022 13:32:37'!
String removeSelector: #withoutEnclosing:!
!methodRemoval: String #asFileEntry stamp: 'jmv 6/8/2022 13:33:10'!
String removeSelector: #asFileEntry!
