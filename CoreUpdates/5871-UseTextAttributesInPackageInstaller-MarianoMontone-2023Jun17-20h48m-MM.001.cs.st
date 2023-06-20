'From Cuis 6.0 [latest update: #5867] on 19 June 2023 at 8:21:51 pm'!

!PackageInstallerWindow methodsFor: 'as yet unclassified' stamp: 'MM 6/19/2023 20:19:15'!
descriptionOfPackage: aPackage

	^ Text streamContents: [ :s |
		s nextPutAll: (Text string: 'Code Package File: ' attribute: TextEmphasis bold).
		s nextPutAll: aPackage fullName; newLine; newLine.
		s nextPutAll: (Text string: 'Provides: ' attribute: TextEmphasis bold).
		aPackage provides printDetailsOn: s.
		s newLine; newLine.
		aPackage requires ifNotEmpty: [ :requirements | 
			s nextPutAll: (Text string: 'Requires: ' attribute: TextEmphasis bold).
			(requirements sorted: [:a :b | a name < b name]) do: [ :req |
				s newLine; space.
				req printDetailsOn: s ].
			s newLine ].
		s newLine.
		aPackage sourceSystem ifNotEmpty: [:sourceSystem | 
			s nextPutAll: sourceSystem; newLine; newLine ]]! !


!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 6/19/2023 20:16:07'!
packageDescription

	| package |
	
	package := self selectedPackage.
	
	package ifNil: [^''].
	
	^ Text streamContents: [:s | 
		s nextPutAll: (Text string: package packageName attribute: TextEmphasis bold).
		s newLine; newLine.
		package packageDescription ifNotEmpty: [:description |
			s nextPutAll: description.
			s  newLine;		newLine].
		
		s nextPutAll: (self descriptionOfPackage: package)]! !

