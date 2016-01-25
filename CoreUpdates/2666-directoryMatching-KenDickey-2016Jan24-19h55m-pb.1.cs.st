'From Cuis 4.2 of 25 July 2013 [latest update: #2665] on 24 January 2016 at 7:58:18.64377 pm'!

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'pb 1/24/2016 19:57'!
directoryMatching: pattern
"
	FmDirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'.
"
	| dirNameFound |

	dirNameFound := 
		self directoryNames 
				detect: [ :dirName | pattern match: dirName ] 
				ifNone: [nil].
	^ dirNameFound ifNotNil: [ :dirName | self / dirName ] ifNil: [ nil ]! !

!FmDirectoryEntry methodsFor: 'cuis specific' stamp: 'KenD 1/14/2016 14:16'!
directoryNamesMatching: pat
	"
	FmDirectoryEntry currentDirectory directoryNamesMatching: '*'
	"

	^ self directoryNames select: [ :each | pat match: each ]! !

